open Lwt.Infix

module Fs = Webdav_fs

module Xml = Webdav_xml

type state = Webdav_fs.Fs.t
type tree = Webdav_xml.tree

let parse_depth = function
  | None -> Ok `Infinity
  | Some "0" -> Ok `Zero
  | Some "1" -> Ok `One
  | Some "infinity" -> Ok `Infinity
  | _ -> Error `Bad_request

let process_properties fs prefix f_or_d f =
  Printf.printf "processing properties of %s\n" (Fs.to_string f_or_d) ;
  Fs.get_property_map fs f_or_d >|= function
  | None -> `Not_found
  | Some map ->
    Printf.printf "read map %s\n" (Xml.props_to_string map) ;
    let propstats = f map in
    let status res =
      Format.sprintf "%s %s"
        (Cohttp.Code.string_of_version `HTTP_1_1)
        (Cohttp.Code.string_of_status res)
    in
    let ps = List.map (fun (code, props) ->
      Xml.node "propstat" [
        Xml.node "prop" props ;
        Xml.node "status" [ Xml.Pcdata (status code) ] ])
      propstats
    in
    let tree =
      Xml.node "response"
        (Xml.node "href" [ Xml.Pcdata (prefix ^ "/" ^ (Fs.to_string f_or_d)) ] :: ps)
    in
    `Single_response tree

let process_property_leaf fs prefix req f_or_d =
  let f = match req with
   | `Propname -> (fun m -> [`OK, List.map ( fun k -> Xml.node k []) @@ List.map fst (Xml.M.bindings m)])
   | `All_prop includes -> (fun m -> [`OK, Xml.props_to_tree m]) (* TODO: finish this *)
   | `Props ps -> (fun m -> Xml.find_props ps m)
  in process_properties fs prefix f_or_d f

let multistatus nodes = Xml.node ~ns:Xml.dav_ns "multistatus" nodes

let error_xml element = Xml.node ~ns:Xml.dav_ns "error" [ Xml.node element [] ]

let propfind fs f_or_d prefix req depth =
  let process_files fs prefix dir req els =
    Lwt_list.map_s (process_property_leaf fs prefix req) (dir :: els) >|= fun answers ->
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
      process_property_leaf fs prefix req f_or_d >|= function
      | `Not_found -> Error `Property_not_found
      | `Single_response t ->
        let outer =
          Xml.node ~ns:Xml.dav_ns "multistatus" [ t ]
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
      | Ok body -> Ok (state, body)
      | Error e -> Error e

let apply_updates ?(validate_key = fun _ -> Ok ()) m updates =
  let set_prop k v m = match validate_key k with
    | Error e -> None, (k, e)
    | Ok () ->
      (* set needs to be more expressive: forbidden, conflict, insufficient storage needs to be added *)
      let map = Xml.M.add k v m in
      Printf.printf "map after set %s %s\n" k (Xml.props_to_string map) ;
      Some map, (k, `OK)
  in
  (* if an update did not apply, m will be None! *)
  let apply (m, propstats) update = match m, update with
    | None, `Set (_, k, _) -> None, (k, `Failed_dependency) :: propstats
    | None, `Remove k   -> None, (k, `Failed_dependency) :: propstats
    | Some m, `Set (a, k, v) -> let (m, p) = set_prop k (a, v) m in (m, p :: propstats)
    | Some m, `Remove k ->
      let map = Xml.M.remove k m in
      Printf.printf "map after remove %s %s\n" k (Xml.props_to_string map) ;
      Some map, (k, `OK) :: propstats
  in
  match List.fold_left apply (m, []) updates with
  | Some m, xs -> Some m, xs
  | None, xs ->
    (* some update did not apply -> tree: None *)
    let ok_to_failed (k, s) =
      (k, match s with
        | `OK -> `Failed_dependency
        | x -> x)
    in
    None, List.map ok_to_failed xs

let update_properties ?validate_key fs f_or_d updates =
  Fs.get_property_map fs f_or_d >>= fun map ->
  let map', xs = apply_updates ?validate_key map updates in
  let propstats =
    List.map (fun (name, status) ->
        let status_code =
          Format.sprintf "%s %s"
            (Cohttp.Code.string_of_version `HTTP_1_1)
            (Cohttp.Code.string_of_status status)
        in
        Xml.node "propstat" [
          Xml.node "prop" [ Xml.node name [] ] ;
          Xml.node "status" [ Xml.Pcdata status_code ] ])
      xs in
  (match map' with
  | None -> Lwt.return (Ok ())
  | Some m -> Fs.write_property_map fs f_or_d m ) >|= function
    | Error e -> Error e
    | Ok () -> Ok propstats

let proppatch state ~prefix ~name body =
  match Xml.parse_propupdate_xml body with
  | Error _ -> Lwt.return (Error `Bad_request)
  | Ok updates ->
    let validate_key = function
      | "resourcetype" -> Error `Forbidden
      | _ -> Ok ()
    in
    update_properties ~validate_key state name updates >|= function
    | Error _      -> Error `Bad_request
    | Ok propstats ->
      let nodes =
        Xml.node "response"
          (Xml.node "href" [ Xml.Pcdata (prefix ^ "/" ^ (Fs.to_string name)) ] :: propstats)
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
        List.map (fun (name, status) ->
            let status_code =
              Format.sprintf "%s %s"
                (Cohttp.Code.string_of_version `HTTP_1_1)
                (Cohttp.Code.string_of_status status)
            in
            Xml.node "propstat" [
              Xml.node "prop" [ Xml.node name [] ] ;
              Xml.node "status" [ Xml.Pcdata status_code ] ])
          errs
      in
      let xml = Xml.node "mkcol-response" propstats in
      Printf.printf "forbidden from body_to_props!\n" ;
      Error (`Forbidden xml)
    | Some map, _ -> Ok map

(* assumption: name is a relative path! *)
let mkcol ?(now = Ptime_clock.now ()) state (`Dir dir) body =
  (* TODO: move to caller *)
  let parent =
    match List.rev dir with
    | _ :: tl -> `Dir (List.rev tl)
    | [] -> `Dir []
  in
  Fs.exists state (Fs.to_string parent) >>= function
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
