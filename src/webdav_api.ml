open Lwt.Infix

module Fs = Webdav_fs

module Xml = Webdav_xml

type state = Webdav_fs.Fs.t
type tree = Webdav_xml.tree

let write state ~name ~content_type data =
  match name with
  | `Dir _ -> Lwt.return (Error `Method_not_allowed)
  | `File file ->
    let parent = Fs.parent (`File file) in
    Fs.dir_exists state parent >>= function
    | false ->
      Printf.printf "parent directory of %s does not exist\n" (Fs.to_string (`File file)) ;
      Lwt.return (Error `Conflict)
    | true ->
      let props =
        Xml.create_properties ~content_type false
          (Ptime.to_rfc3339 (Ptime_clock.now ()))
          (String.length data) (Fs.to_string (`File file))
      in
      Fs.write state (`File file) (Cstruct.of_string data) props >|= function
      | Error e -> Error `Internal_server_error
      | Ok () -> Ok state

let statuscode_to_string res =
  Format.sprintf "%s %s"
    (Cohttp.Code.string_of_version `HTTP_1_1)
    (Cohttp.Code.string_of_status res)

let propstat_node (code, props) =
  Xml.dav_node "propstat" [
    Xml.dav_node "prop" props ;
    Xml.dav_node "status" [ Xml.pcdata (statuscode_to_string code) ] ]

let parse_depth = function
  | None -> Ok `Infinity
  | Some "0" -> Ok `Zero
  | Some "1" -> Ok `One
  | Some "infinity" -> Ok `Infinity
  | _ -> Error `Bad_request

let propfind_request_to_selector = function
   | `Propname -> (fun m -> [`OK, List.map ( fun (ns, k) -> Xml.node ~ns k []) @@ List.map fst (Xml.PairMap.bindings m)])
   | `All_prop includes -> (fun m -> [`OK, Xml.props_to_tree m]) (* TODO: finish this *)
   | `Props ps -> (fun m -> Xml.find_props ps m)

let property_selector fs prefix request f_or_d =
  Printf.printf "processing properties of %s\n" (Fs.to_string f_or_d) ;
  Fs.get_property_map fs f_or_d >|= function
  | None -> `Not_found
  | Some map ->
    Printf.printf "read map %s\n" (Xml.props_to_string map) ;
    let propstats = (propfind_request_to_selector request) map in
    let ps = List.map propstat_node propstats in
    let selected_properties =
      Xml.dav_node "response"
        (Xml.dav_node "href" [ Xml.Pcdata (prefix ^ "/" ^ (Fs.to_string f_or_d)) ] :: ps)
    in
    `Single_response selected_properties

let multistatus nodes = Xml.dav_node "multistatus" nodes

let error_xml element = Xml.dav_node "error" [ Xml.dav_node element [] ]

let propfind fs f_or_d prefix req depth =
  let process_files fs prefix dir req els =
    Lwt_list.map_s (property_selector fs prefix req) (dir :: els) >|= fun answers ->
    (* answers : [ `Not_found | `Single_response of Tyxml.Xml.node ] list *)
    let nodes = List.fold_left (fun acc element ->
        match element with
        | `Not_found -> acc
        | `Single_response node -> node :: acc) [] answers
    in
    Ok (multistatus nodes)
  in
  match depth, f_or_d with
  | `Infinity, _ ->
    let body = error_xml "propfind-finite-depth" in
    Lwt.return (Error (`Forbidden body))
  | `Zero, _
  | _, `File _ ->
    begin
      property_selector fs prefix req f_or_d >|= function
      | `Not_found -> Error `Property_not_found
      | `Single_response t ->
        let outer =
          Xml.dav_node "multistatus" [ t ]
        in
        Ok outer
    end
  | `One, `Dir data ->
    Fs.listdir fs (`Dir data) >>= function
    | Error _ -> assert false
    | Ok els -> process_files fs prefix (`Dir data) req els

let propfind state ~prefix ~name tree ~depth =
  match parse_depth depth with
  | Error `Bad_request -> Lwt.return (Error `Bad_request)
  | Ok depth ->
    match Xml.parse_propfind_xml tree with
    | Error _ -> Lwt.return (Error `Property_not_found)
    | Ok req ->
      propfind state name prefix req depth >|= function
      | Ok body -> Ok body
      | Error e -> Error e

let apply_updates ?(validate_key = fun _ -> Ok ()) m updates =
  let set_prop k v m = match validate_key k with
    | Error e -> None, (e, k)
    | Ok () ->
      (* set needs to be more expressive: forbidden, conflict, insufficient storage needs to be added *)
      let map = Xml.PairMap.add k v m in
      Format.printf "map after set %a %s\n" Xml.pp_fqname k (Xml.props_to_string map) ;
      Some map, (`OK, k)
  in
  (* if an update did not apply, m will be None! *)
  let apply (m, propstats) update = match m, update with
    | None, `Set (_, k, _) -> None, (`Failed_dependency, k) :: propstats
    | None, `Remove k   -> None, (`Failed_dependency, k) :: propstats
    | Some m, `Set (a, k, v) -> let (m, p) = set_prop k (a, v) m in (m, p :: propstats)
    | Some m, `Remove k ->
      let map = Xml.PairMap.remove k m in
      Format.printf "map after remove %a %s\n" Xml.pp_fqname k (Xml.props_to_string map) ;
      Some map, (`OK, k) :: propstats
  in
  match List.fold_left apply (m, []) updates with
  | Some m, xs -> Some m, xs
  | None, xs ->
    (* some update did not apply -> tree: None *)
    let ok_to_failed (s, k) =
      ((match s with
        | `OK -> `Failed_dependency
        | x -> x), k)
    in
    None, List.map ok_to_failed xs

let update_properties ?validate_key fs f_or_d updates =
  Fs.get_property_map fs f_or_d >>= fun map ->
  let map', xs = apply_updates ?validate_key map updates in
  let propstats =
    List.map (fun (s, (ns, n)) -> propstat_node (s, [ Xml.node ~ns n [] ])) xs 
  in
  (match map' with
  | None -> Lwt.return (Ok ())
  | Some m -> Fs.write_property_map fs f_or_d m ) >|= function
    | Error e -> Error e
    | Ok () -> Ok propstats

let proppatch state ~prefix ~name body =
  match Xml.parse_propupdate_xml body with
  | Error _ -> Lwt.return (Error `Bad_request)
  | Ok updates ->
    let validate_key (ns, k) = 
      if ns = Xml.dav_ns then match k with
        | "resourcetype" -> Error `Forbidden
        | _ -> Ok ()
      else Ok ()
    in
    update_properties ~validate_key state name updates >|= function
    | Error _      -> Error `Bad_request
    | Ok propstats ->
      let nodes =
        Xml.dav_node "response"
          (Xml.dav_node "href" [ Xml.Pcdata (prefix ^ "/" ^ (Fs.to_string name)) ] :: propstats)
      in
      let status = multistatus [ nodes ] in
      Ok (state, status)

let body_to_props body default_props = 
  match body with
  | None -> Ok default_props
  | Some body' -> 
  match Xml.parse_mkcol_xml body' with
  | Error _ -> Error `Bad_request
  | Ok set_props ->
    match apply_updates (Some default_props) set_props with
    | None, errs ->
      let propstats =
        List.map (fun (s, (ns, n)) -> propstat_node (s, [ Xml.node ~ns n [] ])) errs 
      in
      let xml = Xml.dav_node "mkcol-response" propstats in
      Printf.printf "forbidden from body_to_props!\n" ;
      Error (`Forbidden xml)
    | Some map, _ -> Ok map

(* assumption: name is a relative path! *)
let mkcol ?(now = Ptime_clock.now ()) state (`Dir dir) body =
  (* TODO: move to caller *)
  let parent = Fs.parent (`Dir dir) in
  Fs.dir_exists state parent >>= function
  | false -> Lwt.return (Error `Conflict)
  | true ->
    let default_props =
      Xml.create_properties ~content_type:"text/directory"
        true (Ptime.to_rfc3339 now) 0
        (Fs.to_string (`Dir dir))
    in
    match body_to_props body default_props with
    | Error e -> Lwt.return (Error e)
    | Ok map -> Fs.mkdir state (`Dir dir) map >|= function
      | Error _ -> Error `Conflict
      | Ok () -> Ok state

let check_in_bounds p s e = true
let apply_to_params pfs p = true
let text_matches s c n p = true

let apply_to_props props =
  let key p = Icalendar.Writer.calprop_to_ics_key p in
  function
  | (name, `Is_defined) ->
    List.exists (String.equal name) (List.map key props)
  | (name, `Is_not_defined) ->
    not (List.exists (String.equal name) (List.map key props))
  | (name, `Range ((s, e), pfs)) ->
    let property = List.find_opt (fun p -> String.equal name (key p)) props in
    (match property with
    | None -> false
    | Some p -> check_in_bounds p s e && apply_to_params pfs p)
  | (name, `Text ((substring, collate, negate), pfs)) ->
    let property = List.find_opt (fun p -> String.equal name (key p)) props in
    (match property with
    | None -> false
    | Some p -> text_matches substring collate negate p && apply_to_params pfs p)

let apply_to_vcalendar (query: Xml.report_prop option * Xml.component_filter) data = match query, data with
  | (None, ("VCALENDAR", `Is_defined)), data -> Some(data)
  | (None, ("VCALENDAR", `Is_not_defined)), data -> None
  | (None, ( _ , `Is_defined)), data -> None
  | (None, ( _ , `Is_not_defined)), data -> Some(data)
    (*`Comp_filter of timerange option * prop_filter list * component_filter list*) 
  | (None, ("VCALENDAR", `Comp_filter (tr_opt, pfs, cfs))), (props, comps) ->
    if List.for_all (apply_to_props props) pfs
    then Some (props, comps)
    else None (* TODO handle tr_opt *)

let report state ~prefix ~name req =
  match Xml.parse_calendar_query_xml req with
  | Error e -> Lwt.return (Error `Bad_request)
  | Ok calendar_query -> match name with
    | `Dir d -> Lwt.return (Error `Bad_request)
    | `File f ->
      Fs.read state (`File f) >>= function
      | Error _ -> Lwt.return (Error `Bad_request)
      | Ok (data, map) ->
        match Icalendar.parse (Cstruct.to_string data) with
        | Ok ics -> let _ = apply_to_vcalendar calendar_query ics in
          Lwt.return (Ok req)
(*     | `Dir d -> Fs.listdir state name >>= fun files ->
       Lwt_list.map (fun f -> Fs.read state f) files  *)