(** An example CRUD API for id-based operations on JSON objects. The objects are
    stored in-memory and therefore will not persist across runs of the database.
    The application does not perform any JSON validation at this time.

    Build by running `jbuilder`:

      [jbuilder build _build/default/examples/crud_lwt.exe]

    Run using the following command, which will display the path that each
    request takes through the decision diagram:

      [DEBUG_PATH= ./crud_lwt.exe]

    Here are some sample CURL commands to test on a running server:

      - Get a complete list of items:
        [curl -i -w "\n" -X GET http://localhost:8080/items]

      - Get the item with id 1:
        [curl -i -w "\n" -X GET http://localhost:8080/item/1]

      - Create a new item:
        [curl -i -w "\n" -X POST -d '{"name":"new item"}' http://localhost:8080/items]

      - Modify the item with id 1:
        [curl -i -w "\n" -X PUT -H 'Content-Type: application/json'\
          -d '{"name":"modified item"}' http://localhost:8080/item/1]

      - Delete the item with id 1:
        [curl -i -w "\n" -X DELETE http://localhost:8080/item/1] *)

open Cohttp_lwt_unix
open Lwt.Infix

module Fs = Mirage_fs_mem

(* Apply the [Webmachine.Make] functor to the Lwt_unix-based IO module
 * exported by cohttp. For added convenience, include the [Rd] module
 * as well so you don't have to go reaching into multiple modules to
 * access request-related information. *)
module Wm = struct
  module Rd = Webmachine.Rd
  include Webmachine.Make(Cohttp_lwt_unix__Io)
end

let file_to_propertyfile filename =
  filename ^ ".prop.xml"

let get_properties fs filename =
  let propfile = file_to_propertyfile filename in
  Fs.size fs propfile >>= function
  | Error e -> Lwt.return (Error e)
  | Ok size -> Fs.read fs propfile 0 (Int64.to_int size)

let string_to_tree str =
  let data str = `Pcdata str
  and el ((ns, name), attrs) children =
    let a =
      let namespace = match ns with
        | "" -> []
        | ns -> [ ("xmlns", ns) ]
      in
      namespace @ List.map (fun ((_ns, name), value) -> (name, value)) attrs
    in
    `Node (a, name, children)
  in
  try
    let input = Xmlm.make_input (`String (0, str)) in
    ignore (Xmlm.input input) ; (* ignore DTD *)
    Some (Xmlm.input_tree ~el ~data input)
  with _ -> None

let rec tree_fold f s forest = match forest with
  | `Node (a, name, children) :: tail ->
    let children' = tree_fold f s children
    and tail' = tree_fold f s tail in
    f (`Node (a, name, children)) children' tail'
  | (`Pcdata _ as t') :: tail ->
    let tail' = tree_fold f s tail in
    f t' s tail'
  | [] -> s

let tree_to_string t =
  let f s children tail = match s with
  | `Node (a, name, _) -> " Node: " ^ name ^ "(" ^ children ^ ")(" ^ tail ^ ")"
  | `Pcdata str -> " PCDATA: (" ^ str ^ ") " ^ tail in
  tree_fold f "" [t]

let drop_pcdata t =
  let f s children tail = match s with
  | `Node (a, n, c) -> `Node (a, n, children) :: tail
  | `Pcdata str -> tail in
  List.hd @@ tree_fold f [] [t]

let filter_in_ps ps xml =
  let f s children tail = match s with
  | `Node (a, n, c) -> if List.mem n ps || n = "prop" then `Node (a, n, children) :: tail else tail
  | `Pcdata str -> `Pcdata str :: tail in
  List.hd @@ tree_fold f [] [xml]

let tree_to_tyxml t =
  let attrib_to_tyxml (name, value) =
    Tyxml.Xml.string_attrib name (Tyxml_xml.W.return value)
  in
  let f s children tail = match s with
  | `Node (a, n, c) ->
    let a' = List.map attrib_to_tyxml a in
    Tyxml.Xml.node ~a:a' n (Tyxml_xml.W.return children) :: tail
  | `Pcdata str -> Tyxml.Xml.pcdata (Tyxml_xml.W.return str) :: tail
  in List.hd @@ tree_fold f [] [t]

let tyxml_to_body t =
  Format.asprintf "<?xml version=\"1.0\" encoding=\"utf-8\" ?>\n%a"
    (Tyxml.Xml.pp ()) t

let process_properties fs prefix url f =
  Printf.printf "processing properties of %s\n" url ;
  get_properties fs url >|= function
  | Error _ ->
    Printf.printf "not found\n" ;
    `Not_found
  | Ok data ->
    match string_to_tree Cstruct.(to_string @@ concat data) with
    | None -> `Not_found
    | Some xml ->
      Printf.printf "read tree %s\n" (tree_to_string xml) ;
      let xml' = f xml in
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
              tree_to_tyxml xml' ;
              node "status" [ pcdata status ] ] ]
      in
      `Single_response tree


(** A resource for querying an individual item in the database by id via GET,
    modifying an item via PUT, and deleting an item via DELETE. *)
class handler prefix fs = object(self)
  inherit [Cohttp_lwt.Body.t] Wm.resource

  method private of_json rd =
    Cohttp_lwt.Body.to_string rd.Wm.Rd.req_body >>= fun body ->
    Fs.write fs (self#id rd) 0 (Cstruct.of_string body) >>= fun modified ->
    let modified', resp_body =
      match modified with
      | Error _ -> false, `String "{\"status\":\"not found\"}"
      | Ok () -> true, `String "{\"status\":\"ok\"}"
    in
    Wm.continue modified' { rd with Wm.Rd.resp_body }

  method private to_json rd =
    let file = self#id rd in
    Fs.size fs file >>= function
    | Error _ -> assert false
    | Ok bytes ->
      Fs.read fs (self#id rd) 0 (Int64.to_int bytes) >>= function
      | Error _ -> assert false
      | Ok data ->
        let value = String.concat "" @@ List.map Cstruct.to_string data in
        Wm.continue (`String value) rd

  method allowed_methods rd =
    Wm.continue [`GET; `HEAD; `PUT; `DELETE; `OPTIONS; `Other "PROPFIND"; `Other "PROPPATCH"; `Other "COPY" ; `Other "MOVE"] rd

  method known_methods rd =
    Wm.continue [`GET; `HEAD; `PUT; `DELETE; `OPTIONS; `Other "PROPFIND"; `Other "PROPPATCH"; `Other "COPY" ; `Other "MOVE"] rd

  method resource_exists rd =
    Fs.stat fs (self#id rd) >>= function
    | Error _ -> Wm.continue false rd
    | Ok _ -> Wm.continue true rd

  method content_types_provided rd =
    Wm.continue [
      "application/json", self#to_json
    ] rd

  method content_types_accepted rd =
    Wm.continue [
      "application/json", self#of_json
    ] rd

  method process_property rd =
    Printf.printf "processing property!!!!\n%!" ;
    let process_property_leaf url body =
      match Webdav.read_propfind body with
       | None -> Lwt.return `Not_found
       | Some `Propname -> process_properties fs prefix url drop_pcdata
       | Some (`All_prop includes) -> process_properties fs prefix url (fun id -> id)
       | Some (`Props ps) -> process_properties fs prefix url (filter_in_ps ps)
    in

    Cohttp_lwt.Body.to_string rd.Wm.Rd.req_body >>= fun body ->
    (* single resource vs collection? *)
    let url = self#id rd in
    let rd = Wm.Rd.with_resp_headers (fun header ->
      let header' = Cohttp.Header.remove header "Content-Type" in
      Cohttp.Header.add header' "Content-Type" "application/xml") rd in
    Printf.printf "url %s\n" url ;
    Fs.stat fs url >>= function
    | Error _ -> assert false
    | Ok stat when stat.directory ->
      begin
        Fs.listdir fs url >>= function
        | Error _ -> assert false
        | Ok els ->
          let els =
            List.filter (fun f ->
                not @@ Astring.String.is_suffix ~affix:"prop.xml" f)
              els
          in
          Printf.printf "properties for %s and %s\n" url (String.concat ", " els) ;
          Lwt_list.map_s (fun url -> process_property_leaf url body) (url :: els) >>= fun answers ->
          (* answers : [ `Not_found | `Single_response of Tyxml.Xml.node ] list *)
          let nodes = List.fold_left (fun acc element ->
              match element with
              | `Not_found -> acc
              | `Single_response node -> node :: acc)
              [] answers
          in
          let multistatus =
            Tyxml.Xml.(node
                         ~a:[ string_attrib "xmlns" (Tyxml_xml.W.return "DAV:") ]
                         "multistatus" nodes)
          in
          let str = tyxml_to_body multistatus in
          Wm.continue `Multistatus { rd with Wm.Rd.resp_body = `String str }
      end
    | Ok _ ->
      process_property_leaf url body >>= function
      | `Not_found -> Wm.continue `Property_not_found rd
      | `Single_response t ->
        let outer =
          Tyxml.Xml.(node
                       ~a:[ string_attrib "xmlns" (Tyxml_xml.W.return "DAV:") ]
                       "multistatus" [ t ])
        in
        let str = tyxml_to_body outer in
        Wm.continue `Multistatus { rd with Wm.Rd.resp_body = `String str }

  method delete_resource rd =
    Fs.destroy fs (self#id rd) >>= fun res ->
    let deleted, resp_body =
      match res with
      | Ok () -> true, `String "{\"status\":\"ok\"}"
      | Error _ -> false, `String "{\"status\":\"not found\"}"
    in
    Wm.continue deleted { rd with Wm.Rd.resp_body }

  method generate_etag rd =
    Wm.continue (Some "foo") rd

  method finish_request rd =
    let rd = Wm.Rd.with_resp_headers (fun header ->
        Cohttp.Header.add header "DAV" "1") rd
    in
    Cohttp_lwt.Body.to_string rd.Wm.Rd.resp_body >>= fun body ->
    Printf.printf "returning %s %s\n%!"
      (Cohttp.Header.to_string rd.Wm.Rd.resp_headers) body ;
    Wm.continue () rd

  method private id rd =
    let url = Uri.path (rd.Wm.Rd.uri) in
    let pl = String.length prefix in
    let path =
      let p = String.sub url pl (String.length url - pl) in
      if String.length p > 0 && String.get p 0 = '/' then
        String.sub p 1 (String.length p - 1)
      else
        p
    in
    Printf.printf "path is %s\n" path ;
    path
end

let create_properties fs name is_dir =
  let props file =
    Webdav.create_properties ~content_type:"application/json"
      is_dir (Ptime.to_rfc3339 (Ptime_clock.now ())) file
  in
  let propfile filename = tyxml_to_body (props filename) in
  Fs.write fs (file_to_propertyfile name) 0 (Cstruct.of_string (propfile name))

let create_file_and_property fs name data =
  Fs.write fs name 0 (Cstruct.of_string data) >>= fun _ ->
  create_properties fs name false

let main () =
  (* listen on port 8080 *)
  let port = 8080 in
  (* create the file system *)
  Fs.connect "" >>= fun fs ->
  (* the route table *)
  let routes = [
    ("/item/*", fun () -> new handler "/item" fs) ;
  ] in
  let callback (ch, conn) request body =
    let open Cohttp in
    (* Perform route dispatch. If [None] is returned, then the URI path did not
     * match any of the route patterns. In this case the server should return a
     * 404 [`Not_found]. *)
    Cohttp_lwt.Body.to_string body >>= fun mbody ->
    Printf.printf "resource %s meth %s headers %s body %s\n"
      (Request.resource request)
      (Code.string_of_method (Request.meth request))
      (Header.to_string (Request.headers request))
      mbody ;
    Wm.dispatch' routes ~body ~request
    >|= begin function
      | None        -> (`Not_found, Header.init (), `String "Not found", [])
      | Some result -> result
    end
    >>= fun (status, headers, body, path) ->
      (* If you'd like to see the path that the request took through the
       * decision diagram, then run this example with the [DEBUG_PATH]
       * environment variable set. This should suffice:
       *
       *  [$ DEBUG_PATH= ./crud_lwt.native]
       *
       *)
      let path =
        match Sys.getenv "DEBUG_PATH" with
        | _ -> Printf.sprintf " - %s" (String.concat ", " path)
        | exception Not_found   -> ""
      in
      Printf.eprintf "%d - %s %s%s"
        (Code.code_of_status status)
        (Code.string_of_method (Request.meth request))
        (Uri.path (Request.uri request))
        path;
      (* Finally, send the response to the client *)
      Server.respond ~headers ~body ~status ()
  in
  (* create the server and handle requests with the function defined above *)
  let conn_closed (ch, conn) =
    Printf.printf "connection %s closed\n%!"
      (Sexplib.Sexp.to_string_hum (Conduit_lwt_unix.sexp_of_flow ch))
  in
  (* init the database with two items *)
  create_properties fs "" true >>= fun _ ->
  create_file_and_property fs "1" "{\"name\":\"item 1\"}" >>= fun _ ->
  create_file_and_property fs "2" "{\"name\":\"item 2\"}" >>= fun _ ->
  let config = Server.make ~callback ~conn_closed () in
  Server.create  ~mode:(`TCP(`Port port)) config
  >>= (fun () -> Printf.eprintf "hello_lwt: listening on 0.0.0.0:%d%!" port;
      Lwt.return_unit)

let () =  Lwt_main.run (main ())
