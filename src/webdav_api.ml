open Lwt.Infix

module Fs = Webdav_fs

type state = Webdav_fs.Fs.t

let parse_depth = function
  | None -> Ok `Infinity
  | Some "0" -> Ok `Zero
  | Some "1" -> Ok `One
  | Some "infinity" -> Ok `Infinity
  | _ -> Error `Bad_request

let process_properties fs prefix url f =
  Printf.printf "processing properties of %s\n" url ;
  Fs.get_property_map fs url >|= function
  | None -> `Not_found
  | Some map ->
    Printf.printf "read map %s\n" (Webdav.props_to_string map) ;
    let (success, fail) = f map in
    let status res =
      Format.sprintf "%s %s"
        (Cohttp.Code.string_of_version `HTTP_1_1)
        (Cohttp.Code.string_of_status res)
    in
    let open Tyxml.Xml in
    let success_propstats = node "prop" success
    and fail_propstats = node "prop" fail in
    let tree =
      node "response"
        [ node "href" [ pcdata (prefix ^ "/" ^ url) ] ;
          node "propstat" [
            success_propstats ; node "status" [ pcdata (status `OK) ] ];
          node "propstat" [
            fail_propstats ; node "status" [ pcdata (status `Forbidden) ] ];
          ]
    in
    `Single_response tree

let process_property_leaf fs prefix req url =
  let f = match req with
   | `Propname -> (fun m -> List.map ( fun k -> Tyxml.Xml.node k [] ) @@ List.map fst (Webdav.M.bindings m), [])
   | `All_prop includes -> (fun m -> Webdav.props_to_tree m, []) (* TODO: finish this *)
   | `Props ps -> (fun m -> Webdav.find_props ps m)
  in process_properties fs prefix url f

let dav_ns = Tyxml.Xml.string_attrib "xmlns" (Tyxml_xml.W.return "DAV:")

let multistatus nodes = Tyxml.Xml.node ~a:[ dav_ns ] "multistatus" nodes

let process_files fs prefix url req els =
  Lwt_list.map_s (process_property_leaf fs prefix req) (url :: els) >|= fun answers ->
  (* answers : [ `Not_found | `Single_response of Tyxml.Xml.node ] list *)
  let nodes = List.fold_left (fun acc element ->
      match element with
      | `Not_found -> acc
      | `Single_response node -> node :: acc) [] answers
  in
  `Response (Webdav.tyxml_to_body (multistatus nodes))

let propfind fs url prefix req =
  Fs.stat fs url >>= function
  | Error _ -> assert false
  | Ok stat when stat.directory ->
    begin
      Fs.listdir fs url >>= function
      | Error _ -> assert false
      | Ok els -> process_files fs prefix url req els
    end
  | Ok _ ->
    process_property_leaf fs prefix req url >|= function
    | `Not_found -> `Property_not_found
    | `Single_response t ->
      let outer =
        Tyxml.Xml.(node ~a:[ dav_ns ] "multistatus" [ t ])
      in
      `Response (Webdav.tyxml_to_body outer)

let error_xml element =
  Tyxml.Xml.(node ~a:[ dav_ns ] "error" [ node element [] ])
  |> Webdav.tyxml_to_body

let propfind state ~prefix ~name ~body ~depth =
  match parse_depth depth with
  | Error `Bad_request -> Lwt.return (Error `Bad_request)
  | Ok `Infinity ->
    let body = error_xml "propfind-finite-depth" in
    Lwt.return (Error (`Forbidden body))
  | Ok d ->
    (* TODO actually deal with depth d (`Zero or `One) *)
    match Webdav.parse_propfind_xml body with
    | None -> Lwt.return (Error `Property_not_found)
    | Some req ->
      propfind state name prefix req >|= function
      | `Response body -> Ok (state, body)
      | `Property_not_found -> Error `Property_not_found

let apply_updates fs id updates =
  let set_prop k v m = match k with
    | "resourcetype" -> None, (k, `Forbidden)
    | _ -> 
      (* set needs to be more expressive: forbidden, conflict, insufficient storage needs to be added *)
      let map = Webdav.M.add k v m in
      Printf.printf "map after set %s %s\n" k (Webdav.props_to_string map) ;
      Some map, (k, `OK)
  in
  let update_fun m =
    (* if an update did not apply, m will be None! *)
    let apply (m, propstats) update = match m, update with
      | None, `Set (_, k, _) -> None, (k, `Failed_dependency) :: propstats
      | None, `Remove k   -> None, (k, `Failed_dependency) :: propstats
      | Some m, `Set (a, k, v) -> let (m, p) = set_prop k (a, v) m in (m, p :: propstats)
      | Some m, `Remove k ->
        let map = Webdav.M.remove k m in
        Printf.printf "map after remove %s %s\n" k (Webdav.props_to_string map) ;
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
  in
  Fs.get_property_map fs id >>= fun map -> 
  let map', xs = update_fun map in
  let propstats =
    List.map (fun (name, status) ->
        let status_code =
          Format.sprintf "%s %s"
            (Cohttp.Code.string_of_version `HTTP_1_1)
            (Cohttp.Code.string_of_status status)
        in
        Tyxml.Xml.(node "propstat" [
            node "prop" [node name []] ;
            node "status" [ pcdata status_code ] ]))
      xs in
  (match map' with
  | None -> Lwt.return (Ok ())
  | Some m -> Fs.write_property_map fs id m) >|= function
    | Error e -> Error e
    | Ok () -> Ok propstats

let proppatch state ~prefix ~name ~body =
  match Webdav.parse_propupdate_xml body with
  | None -> Lwt.return (Error `Bad_request)
  | Some updates ->
    apply_updates state name updates >|= function
    | Error _      -> Error `Bad_request
    | Ok propstats ->
      let nodes =
        Tyxml.Xml.(node "response"
                     (node "href" [ pcdata (prefix ^ "/" ^ name) ] :: propstats))
      in
      let status = multistatus [ nodes ] in
      Ok (state, Webdav.tyxml_to_body status)
