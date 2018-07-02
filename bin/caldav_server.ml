open Cohttp_lwt_unix
open Lwt.Infix

module Fs = Caldav.Webdav_fs
module Xml = Caldav.Webdav_xml
module Dav = Caldav.Webdav_api

(* Apply the [Webmachine.Make] functor to the Lwt_unix-based IO module
 * exported by cohttp. For added convenience, include the [Rd] module
 * as well so you don't have to go reaching into multiple modules to
 * access request-related information. *)
module Wm = struct
  module Rd = Webmachine.Rd
  include Webmachine.Make(Cohttp_lwt_unix__Io)
end

let to_status x = Cohttp.Code.code_of_status x

let create_properties name content_type is_dir length =
  Printf.printf "Creating properties!!! %s \n" name;
  Xml.create_properties ~content_type
    is_dir (Ptime.to_rfc3339 (Ptime_clock.now ())) length name

let etag str = Digest.to_hex @@ Digest.string str

(* assumption: path is a directory - otherwise we return none *)
(* out: ( name * typ * last_modified ) list - non-recursive *)
let list_dir fs (`Dir dir) =
  let list_file f_or_d =
    Fs.last_modified fs f_or_d >|= function
    | Error _ -> assert false
    | Ok last_modified ->
      let is_dir = match f_or_d with
        | `File _ -> false | `Dir _ -> true
      in
      (Fs.to_string f_or_d, is_dir, last_modified)
  in
  Fs.listdir fs (`Dir dir) >>= function
  | Error e -> assert false
  | Ok files -> Lwt_list.map_p list_file files

let directory_as_html prefix fs (`Dir dir) =
  list_dir fs (`Dir dir) >|= fun files ->
  let print_file (file, is_dir, last_modified) =
    Printf.sprintf "<tr><td><a href=\"%s/%s\">%s</a></td><td>%s</td><td>%s</td></tr>"
      prefix file file (if is_dir then "directory" else "text/calendar") last_modified in
  String.concat "\n" (List.map print_file files)

let directory_as_ics fs (`Dir dir) =
  let calendar_components = function
    | `Dir d ->
      Printf.printf "calendar components of directory %s\n%!" (Fs.to_string (`Dir d)) ;
      Lwt.return []
      (* assert false (* CalDAV forbids nested calendars *) *)
    | `File f ->
      Fs.read fs (`File f) >|= function
      | Error _ -> Printf.printf "error while reading file!\n" ; []
      | Ok (data, _props) ->
        match Icalendar.parse (Cstruct.to_string data) with
        | Ok calendar -> snd calendar
        | Error e -> Printf.printf "error %s while parsing ics\n" e ; []
  in
  Fs.listdir fs (`Dir dir) >>= function
  | Error _ -> assert false (* previously checked that directory exists *)
  | Ok files ->
    (* TODO: hardcoded calprops, put them elsewhere *)
    Fs.get_property_map fs (`Dir dir) >>= function
    | None -> assert false (* invariant: each file and directory has a property map *)
    | Some props ->
      let name =
        match Xml.get_prop (Xml.dav_ns, "displayname") props with
        | Some (_, [ Xml.Pcdata name ]) -> [ `Xprop (("WR", "CALNAME"), [], name) ]
        | _ -> []
      in
      let calprops = [
        `Prodid ([], "-//ROBUR.IO//EN") ;
        `Version ([], "2.0")
      ] @ name in
      Lwt_list.map_p calendar_components files >|= fun components ->
      Icalendar.to_ics (calprops, List.flatten components)

let calendar_to_collection data =
  if data = "" then Ok "" else
  match Xml.string_to_tree data with
  | Some (Xml.Node (ns, "mkcalendar", a, c)) when ns = Xml.dav_ns -> Ok (Xml.tyxml_to_body (Xml.tree_to_tyxml (Xml.node ~ns:Xml.dav_ns ~a "mkcol" c)))
  | _ -> Error `Bad_request

(** A resource for querying an individual item in the database by id via GET,
    modifying an item via PUT, and deleting an item via DELETE. *)
class handler prefix fs = object(self)
  inherit [Cohttp_lwt.Body.t] Wm.resource

  method private write_calendar rd =
    Format.printf "write_calendar, fs is: %a\n" Mirage_fs_mem.pp fs ;
    Cohttp_lwt.Body.to_string rd.Wm.Rd.req_body >>= fun body ->
    let name = self#id rd in
    let file = Fs.file_from_string name in
    let parent = Fs.parent (file :> Fs.file_or_dir) in
    Fs.dir_exists fs parent >>= function
    | false ->
      Printf.printf "parent directory of %s does not exist\n" name ;
      Wm.continue false rd
    | true ->
      let content_type =
        match Cohttp.Header.get rd.Wm.Rd.req_headers "Content-Type" with
        | None -> "text/calendar"
        | Some x -> x
      in
      match Icalendar.parse body with
      | Error e ->
        Printf.printf "error %s while parsing calendar\n" e ;
        Format.printf "write_calendar end, fs is now: %a\n" Mirage_fs_mem.pp fs ;
        Wm.continue false rd
      | Ok cal ->
        let ics = Icalendar.to_ics cal in
        let props = create_properties name content_type false (String.length ics) in
        Fs.write fs file (Cstruct.of_string ics) props >>= fun _ ->
        let etag = etag ics in
        let rd = Wm.Rd.with_resp_headers (fun header ->
            let header' = Cohttp.Header.remove header "ETag" in
            Cohttp.Header.add header' "Etag" etag) rd
        in
        Format.printf "write_calendar end, fs is now: %a\n" Mirage_fs_mem.pp fs ;
        Wm.continue true rd

  method private read_calendar rd =
    let file = self#id rd in
    let gecko =
      match Cohttp.Header.get rd.Wm.Rd.req_headers "User-Agent" with
      | None -> false
      | Some x ->
        (* Apple seems to use the regular expression 'Mozilla/.*Gecko.*' *)
        Astring.String.is_prefix ~affix:"Mozilla/" x &&
        Astring.String.is_infix ~affix:"Gecko" x
    in

    let (>>==) a f = a >>= function
    | Error e ->
      Format.printf "Error %s: %a\n" file Fs.pp_error e ;
      Wm.continue `Empty rd
    | Ok res  -> f res in

    Fs.from_string fs file >>== function
    | `Dir dir ->
      (* TODO: append / to name! *)
      Printf.printf "is a directory\n" ;
      if gecko then
        directory_as_html prefix fs (`Dir dir) >>= fun listing ->
        Wm.continue (`String listing) rd
      else
        (* TODO: check wheter CalDAV:calendar property is set as resourcetype!
           otherwise: standard WebDAV directory listing *)
        directory_as_ics fs (`Dir dir) >>= fun data ->
        Wm.continue (`String data) rd
    | `File f ->
      Fs.read fs (`File f) >>== fun (data, props) ->
      let ct = match Xml.get_prop (Xml.dav_ns, "getcontenttype") props with
        | Some (_, [ Xml.Pcdata ct ]) -> ct
        | _ -> "text/calendar" in
      let rd =
        Wm.Rd.with_resp_headers (fun header ->
            let header' = Cohttp.Header.remove header "Content-Type" in
            Cohttp.Header.add header' "Content-Type" ct)
          rd
      in
      Wm.continue (`String (Cstruct.to_string data)) rd

  method allowed_methods rd =
    Wm.continue [`GET; `HEAD; `PUT; `DELETE; `OPTIONS; `Other "PROPFIND"; `Other "PROPPATCH"; `Other "COPY" ; `Other "MOVE"; `Other "MKCOL"; `Other "MKCALENDAR" ; `Other "REPORT" ] rd

  method known_methods rd =
    Wm.continue [`GET; `HEAD; `PUT; `DELETE; `OPTIONS; `Other "PROPFIND"; `Other "PROPPATCH"; `Other "COPY" ; `Other "MOVE"; `Other "MKCOL"; `Other "MKCALENDAR" ; `Other "REPORT" ] rd

  method charsets_provided rd =
    Wm.continue [
      "utf-8", (fun id -> id)
    ] rd

  method resource_exists rd =
    Fs.exists fs (self#id rd) >>= fun v ->
    Wm.continue v rd

  method content_types_provided rd =
    Wm.continue [
      "text/calendar", self#read_calendar
    ] rd

  method content_types_accepted rd =
    Wm.continue [
      "text/calendar", self#write_calendar
    ] rd

  method private process_propfind rd name =
    let depth = Cohttp.Header.get rd.Wm.Rd.req_headers "Depth" in
    Cohttp_lwt.Body.to_string rd.Wm.Rd.req_body >>= fun body ->
    match Xml.string_to_tree body with
    | None -> Wm.respond (to_status `Bad_request) rd
    | Some tree ->
      Dav.propfind fs ~prefix ~name tree ~depth >>= function
      | Ok b -> Wm.continue `Multistatus { rd with Wm.Rd.resp_body = `String (Xml.tree_to_string b) }
      | Error `Property_not_found -> Wm.continue `Property_not_found rd
      | Error (`Forbidden b) -> Wm.respond ~body:(`String (Xml.tree_to_string b)) (to_status `Forbidden) rd
      | Error `Bad_request -> Wm.respond (to_status `Bad_request) rd

  method private process_proppatch rd name =
    Cohttp_lwt.Body.to_string rd.Wm.Rd.req_body >>= fun body ->
    Printf.printf "PROPPATCH:%s\n" body;
    match Xml.string_to_tree body with
    | None -> Wm.respond (to_status `Bad_request) rd
    | Some tree ->
      Dav.proppatch fs ~prefix ~name tree >>= function
      | Ok (_, b) -> Wm.continue `Multistatus { rd with Wm.Rd.resp_body = `String (Xml.tree_to_string b) }
      | Error `Bad_request -> Wm.respond (to_status `Bad_request) rd

  method process_property rd =
    let replace_header h = Cohttp.Header.replace h "Content-Type" "application/xml" in
    let rd' = Wm.Rd.with_resp_headers replace_header rd in
    Fs.from_string fs (self#id rd) >>= function
    | Error _ -> Wm.respond (to_status `Bad_request) rd
    | Ok f_or_d ->
      match rd'.Wm.Rd.meth with
      | `Other "PROPFIND" -> self#process_propfind rd' f_or_d
      | `Other "PROPPATCH" -> self#process_proppatch rd' f_or_d
      | _ -> assert false

  method report rd =
    Wm.continue `Multistatus rd

  method cannot_create rd =
    let xml = Xml.node ~ns:Xml.dav_ns "error" [Xml.node ~ns:Xml.dav_ns "resource-must-be-null" []] in
    let err = Xml.tree_to_string xml in
    let rd' = { rd with Wm.Rd.resp_body = `String err } in
    Wm.continue () rd'

  method create_collection rd =
    Cohttp_lwt.Body.to_string rd.Wm.Rd.req_body >>= fun body ->
    let body' = match rd.Wm.Rd.meth with
    | `Other "MKCALENDAR" -> calendar_to_collection body
    | `Other "MKCOL" -> Ok body 
    | _ -> assert false in
    match body' with
    | Error _ -> Wm.continue `Conflict rd
    | Ok body'' -> 
      match Xml.string_to_tree body'' with
      | None when body'' <> "" -> Wm.continue `Conflict rd
      | tree ->
        let dir = Fs.dir_from_string (self#id rd) in
        Dav.mkcol fs dir tree >>= function
        | Ok _ -> Wm.continue `Created rd
        | Error (`Forbidden t) -> Wm.continue `Forbidden { rd with Wm.Rd.resp_body = `String (Xml.tree_to_string t) }
        | Error `Conflict -> Wm.continue `Conflict rd
        | Error `Bad_request -> Wm.continue `Conflict rd

  method delete_resource rd =
    Format.printf "deleting %s in FS %a\n" (self#id rd) Mirage_fs_mem.pp fs ;
    Fs.from_string fs (self#id rd) >>= function
    | Error _ ->
      Wm.continue false rd
    | Ok f_or_d ->
      Fs.destroy fs f_or_d >>= fun res ->
      Format.printf "deleted - FS now: %a\n" Mirage_fs_mem.pp fs ;
      Wm.continue true rd

  method last_modified rd =
    Fs.from_string fs (self#id rd) >>= function
    | Error _ -> Wm.continue None rd
    | Ok f_or_d ->
      let to_lwt_option res =
        Lwt.return (match res with
            | Error _ -> None
            | Ok x -> Some x)
      in
      Fs.last_modified fs f_or_d >>= to_lwt_option >>= fun res ->
      Wm.continue res rd

  method generate_etag rd =
    Fs.from_string fs (self#id rd) >>= function
    | Error _ -> Wm.continue None rd
    | Ok f_or_d ->
      (Fs.last_modified fs f_or_d >|= function
        | Error _ -> rd
        | Ok lm ->
          let add_headers h = Cohttp.Header.add_list h [ ("Last-Modified", lm) ] in
          Wm.Rd.with_resp_headers add_headers rd) >>= fun rd' ->
      (match f_or_d with
       | `Dir dir ->
         directory_as_html prefix fs (`Dir dir) >|= fun data ->
         Some (etag data)
       | `File file ->
         Fs.read fs (`File file) >|= function
         | Error _ -> None
         | Ok (data, _) -> Some (etag @@ Cstruct.to_string data)) >>= fun result ->
      Wm.continue result rd'

  method finish_request rd =
    let rd' = if rd.Wm.Rd.meth = `OPTIONS then
      let add_headers h = Cohttp.Header.add_list h [ ("DAV", "1, extended-mkcol") ] in
      Wm.Rd.with_resp_headers add_headers rd
    else
      rd in
    Printf.printf "returning %s\n%!"
      (Cohttp.Header.to_string rd'.Wm.Rd.resp_headers) ;
    Wm.continue () rd'

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
    (* Printf.printf "path is %s\n" path ; *)
    path
end

let initialise_fs fs =
  let props = create_properties "/" "text/directory" true 0 in
  Fs.write_property_map fs (`Dir []) props >>= fun _ ->
  let create_dir name =
    let props = create_properties name "text/directory" true 0 in
    Fs.mkdir fs (Fs.dir_from_string name) props
  in
  create_dir "users" >>= fun _ ->
  create_dir "__uids__" >>= fun _ ->
  create_dir "__uids__/10000000-0000-0000-0000-000000000001" >>= fun _ ->
  create_dir "__uids__/10000000-0000-0000-0000-000000000001/calendar" >>= fun _ ->
  create_dir "__uids__/10000000-0000-0000-0000-000000000001/tasks" >>= fun _ ->
  Format.printf "FS is:\n%a\n" Mirage_fs_mem.pp fs ;
  Lwt.return_unit

let main () =
  (* listen on port 8080 *)
  let port = 8080 in
  (* create the file system *)
  Fs.connect () >>= fun fs ->
  (* the route table *)
  let routes = [
    ("/", fun () -> new handler "/" fs) ;
    ("/principals", fun () -> new handler "/principals" fs) ;
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
      Printf.eprintf "%d - %s %s%s, body: %s\n"
        (Code.code_of_status status)
        (Code.string_of_method (Request.meth request))
        (Uri.path (Request.uri request))
        path
        (match body with `String s -> s | `Empty -> "empty" | _ -> "unknown") ;
      (* Finally, send the response to the client *)
      Server.respond ~headers ~body ~status ()
  in
  (* create the server and handle requests with the function defined above *)
  let conn_closed (ch, conn) =
    Printf.printf "connection %s closed\n%!"
      (Sexplib.Sexp.to_string_hum (Conduit_lwt_unix.sexp_of_flow ch))
  in
  initialise_fs fs >>= fun () ->
  let config = Server.make ~callback ~conn_closed () in
  Server.create  ~mode:(`TCP(`Port port)) config
  >>= (fun () -> Printf.eprintf "hello_lwt: listening on 0.0.0.0:%d%!" port;
      Lwt.return_unit)

let () =  Lwt_main.run (main ())
