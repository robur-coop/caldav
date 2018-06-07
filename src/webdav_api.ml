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
  Fs.get_property_tree fs url >|= function
  | None -> `Not_found
  | Some xml ->
    Printf.printf "read tree %s\n" (Webdav.tree_to_string xml) ;
    let xml' = f xml in
    Printf.printf "^^^^ read tree %s\n" (Webdav.tree_to_string xml') ;
    let res = `OK in
    let status =
      Format.sprintf "%s %s"
        (Cohttp.Code.string_of_version `HTTP_1_1)
        (Cohttp.Code.string_of_status res)
    in
    let open Tyxml.Xml in
    let tree =
      node "response"
        [ node "href" [ pcdata (prefix ^ "/" ^ url) ] ;
          node "propstat" [
            Webdav.tree_to_tyxml xml' ;
            node "status" [ pcdata status ] ] ]
    in
    `Single_response tree

let process_property_leaf fs prefix req url =
  let f = match req with
   | `Propname -> Webdav.drop_pcdata
   | `All_prop includes -> (fun id -> id)
   | `Props ps -> Webdav.filter_in_ps ps
  in process_properties fs prefix url f

let process_files fs prefix url req els =
  Lwt_list.map_s (process_property_leaf fs prefix req) (url :: els) >|= fun answers ->
  (* answers : [ `Not_found | `Single_response of Tyxml.Xml.node ] list *)
  let nodes = List.fold_left (fun acc element ->
      match element with
      | `Not_found -> acc
      | `Single_response node -> node :: acc) [] answers
  in
  let multistatus =
    Tyxml.Xml.(node
                 ~a:[ string_attrib "xmlns" (Tyxml_xml.W.return "DAV:") ]
                 "multistatus" nodes)
  in
  `Response (Webdav.tyxml_to_body multistatus)

let dav_ns = Tyxml.Xml.string_attrib "xmlns" (Tyxml_xml.W.return "DAV:")

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
  let remove name t =
    let f node kids tail = match node with
      | `Node (a, n, _) -> if n = name then tail else `Node (a, n, kids) :: tail
      | `Pcdata d -> `Pcdata d :: tail
    in
    match Webdav.tree_fold f [] [t] with
    | [tree] -> Some tree
    | _ -> None
  and set name v t =
    match Webdav.get_prop name t with
    | Some _ ->
      begin
        let f node kids tail = match node with
          | `Node (a, n, _) -> if n = name then `Node (a, n, v) :: tail else `Node (a, n, kids) :: tail
          | `Pcdata d -> `Pcdata d :: tail
        in
        match Webdav.tree_fold f [] [t] with
        | [tree] -> Some tree
        | _ -> None
      end
    | None ->
      match t with
      | `Node (a, "prop", children) -> Some (`Node (a, "prop", `Node ([], name, v) :: children))
      | _ -> None
  in
  let update_fun t =
    let apply t update = match t, update with
      | None, _ -> None
      | Some t, `Set (k, v) -> set k v t
      | Some t, `Remove k   -> remove k t
    in
    List.fold_left apply t updates
  in
  Fs.get_property_tree fs id >>= fun tree -> match update_fun tree with
  | None -> assert false
  | Some t -> Fs.write_property_tree fs id false (Webdav.tree_to_tyxml t)

let proppatch state ~name ~body =
  match Webdav.parse_propupdate_xml body with
  | None -> Lwt.return (Error `Bad_request)
  | Some updates ->
    apply_updates state name updates >|= function
    | Error _ -> Error `Bad_request
    | Ok ()   -> Ok state
