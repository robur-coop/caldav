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

let process_properties fs prefix url f =
  Printf.printf "processing properties of %s\n" url ;
  get_properties fs url >|= function
  | Error _ ->
    Printf.printf "not found\n" ;
    `Not_found
  | Ok data ->
    match Webdav.string_to_tree Cstruct.(to_string @@ concat data) with
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
    | Error _ -> Wm.continue `Empty rd
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
    Printf.printf "RESOURCE exists %s \n" (self#id rd);
    Fs.stat fs (self#id rd) >>= function
    | Error _ -> 
      Printf.printf "FALSE\n";
      Wm.continue false rd
    | Ok _ -> 
      Printf.printf "TRUE\n";
      Wm.continue true rd

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
       | Some `Propname -> process_properties fs prefix url Webdav.drop_pcdata
       | Some (`All_prop includes) -> process_properties fs prefix url (fun id -> id)
       | Some (`Props ps) -> process_properties fs prefix url (Webdav.filter_in_ps ps)
    in

    Cohttp_lwt.Body.to_string rd.Wm.Rd.req_body >>= fun body ->
    (* single resource vs collection? *)
    assert (body <> "");
    Printf.printf "BODY::: %s\n" body; 
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
              | `Single_response node -> node :: acc) [] answers
          in
          let multistatus =
            Tyxml.Xml.(node
                         ~a:[ string_attrib "xmlns" (Tyxml_xml.W.return "DAV:") ]
                         "multistatus" nodes)
          in
          let str = Webdav.tyxml_to_body multistatus in
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
        let str = Webdav.tyxml_to_body outer in
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
    Printf.printf "returning %s\n%!"
      (Cohttp.Header.to_string rd.Wm.Rd.resp_headers) ;
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

let create_properties fs name is_dir length =
  let props file =
    Webdav.create_properties ~content_type:"application/json"
      is_dir (Ptime.to_rfc3339 (Ptime_clock.now ())) length file
  in
  let propfile = Webdav.tyxml_to_body (props name) in
  let trailing_slash = if is_dir then "/" else "" in
  let filename = file_to_propertyfile (name ^ trailing_slash) in
  Fs.write fs filename 0 (Cstruct.of_string propfile)

let create_file_and_property fs name data =
  Fs.write fs name 0 (Cstruct.of_string data) >>= fun _ ->
  create_properties fs name false (String.length data)

let main () =
  (* listen on port 8080 *)
  let port = 8080 in
  (* create the file system *)
  Fs.connect "" >>= fun fs ->
  (* the route table *)
  let routes = [
    ("/calendars", fun () -> new handler "/calendars" fs) ;
    ("/calendars/*", fun () -> new handler "/calendars" fs) ;
  ] in
  let callback (ch, conn) request body =
    let open Cohttp in
    (* Perform route dispatch. If [None] is returned, then the URI path did not
     * match any of the route patterns. In this case the server should return a
     * 404 [`Not_found]. *)
    Printf.printf "resource %s meth %s headers %s\n"
      (Request.resource request)
      (Code.string_of_method (Request.meth request))
      (Header.to_string (Request.headers request)) ;
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
  Mirage_fs_mem.mkdir fs "" >>= fun _ ->
  create_properties fs "" true 0 >>= fun _ ->
  Mirage_fs_mem.mkdir fs "__uids__" >>= fun _ ->
  create_properties fs "__uids__" true 0 >>= fun _ ->
  Mirage_fs_mem.mkdir fs "__uids__/10000000-0000-0000-0000-000000000001" >>= fun _ ->
  create_properties fs "__uids__/10000000-0000-0000-0000-000000000001" true 0 >>= fun _ ->
  create_file_and_property fs "1" "{\"name\":\"item 1\"}" >>= fun _ ->
  create_file_and_property fs "2" "{\"name\":\"item 2\"}" >>= fun _ ->
  let config = Server.make ~callback ~conn_closed () in
  Server.create  ~mode:(`TCP(`Port port)) config
  >>= (fun () -> Printf.eprintf "hello_lwt: listening on 0.0.0.0:%d%!" port;
      Lwt.return_unit)

let () =  Lwt_main.run (main ())
