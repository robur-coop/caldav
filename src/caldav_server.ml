open Cohttp_lwt_unix
open Lwt.Infix

module Fs = Webdav_fs

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
  Webdav_xml.create_properties ~content_type
    is_dir (Webdav_xml.ptime_to_http_date (Ptime_clock.now ())) length name

let etag str = Digest.to_hex @@ Digest.string str

let get_last_modified_prop fs file =
  Fs.get_property_map fs file >|= function
  | None -> Error `Invalid_xml
  | Some map ->
    match Webdav_xml.get_prop "getlastmodified" map with
    | Some (_, [ `Pcdata last_modified ]) -> Ok last_modified
    | _ -> Error `Unknown_prop

(* assumption: path is a directory - otherwise we return none *)
(* out: ( name * typ * last_modified ) list - non-recursive *)
let list_dir fs dir =
  let list_file file =
    (* TODO: figure out whether _file_ is a file or a directory *)
    let full_filename = dir ^ file in
    get_last_modified_prop fs full_filename >>= function
    | Error _ -> assert false
    | Ok last_modified ->
      Fs.isdir fs full_filename >|= function
      | Error _ -> assert false
      | Ok is_dir -> full_filename, is_dir, last_modified in
  Fs.listdir fs dir >>= function
  | Error e -> assert false
  | Ok files -> Lwt_list.map_p list_file files

let directory_listing prefix files =
  let print_file (file, is_dir, last_modified) =
    Printf.sprintf "<tr><td><a href=\"%s/%s\">%s</a></td><td>%s</td><td>%s</td></tr>"
      prefix file file (if is_dir then "directory" else "text/calendar") last_modified in
  String.concat "\n" (List.map print_file files)

let collate_directory fs files =
  let extract_components (filename, is_dir, _) =
    Fs.read fs filename >|= function
    | Error _ -> Printf.printf "error while reading file!\n" ; []
    | Ok (data, _props) ->
      match Icalendar.parse (Cstruct.to_string data) with
      | Ok calendar -> snd calendar
      | Error e -> Printf.printf "error %s while parsing ics\n" e ; []
  in
  (* TODO: hardcoded calprops, put them elsewhere *)
  let calprops = [
    `Prodid ([], "-//ROBUR.IO//EN") ;
    `Version ([], "2.0")
  ] in
  Lwt_list.map_p extract_components files >|= fun components ->
  Icalendar.to_ics (calprops, List.flatten components)

let create_dir_rec fs name =
  let segments = Astring.String.cuts ~sep:"/" name in
  let rec prefixes = function
    | [] -> []
    | h :: t -> [] :: (List.map (fun a -> h :: a) (prefixes t)) in
  let directories = prefixes segments in
  let create_dir path =
    let filename = String.concat "/" path in
    Fs.mkdir fs filename >>= fun _ ->
    let props = create_properties filename "text/directory" true 0 in
    Fs.write_property_map fs filename props
    >|= fun _ ->
    Printf.printf "creating properties %s\n" filename;
    () in 
  Lwt_list.iter_s create_dir directories

let calendar_to_collection data =
  if data = "" then Ok "" else
  match Webdav_xml.string_to_tree data with
  | Some (`Node (a, "mkcalendar", c)) -> Ok (Webdav_xml.tyxml_to_body (Webdav_xml.tree_to_tyxml (`Node (a, "mkcol", c))))
  | _ -> Error `Bad_request

(** A resource for querying an individual item in the database by id via GET,
    modifying an item via PUT, and deleting an item via DELETE. *)
class handler prefix fs = object(self)
  inherit [Cohttp_lwt.Body.t] Wm.resource

  method private write_calendar rd =
    Format.printf "write_calendar, fs is: %a\n" Mirage_fs_mem.pp fs ;
    Cohttp_lwt.Body.to_string rd.Wm.Rd.req_body >>= fun body ->
    let name = self#id rd in
    (* figure out whether it is a directory or not, and maybe append / *)
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
      create_dir_rec fs name >>= fun () ->
      let props = create_properties name content_type false (String.length ics) in 
      Fs.write fs name (Cstruct.of_string ics) props >>= fun _ ->
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

    Fs.isdir fs file >>== function
    | true ->
      (* TODO: append / to name! *)
      Printf.printf "is a directory\n" ;
      list_dir fs file >>= fun files ->
      if gecko then
        let listing = directory_listing prefix files in
        Wm.continue (`String listing) rd
      else
        collate_directory fs files >>= fun data ->
        Wm.continue (`String data) rd
    | false ->
      Fs.read fs file >>== fun (data, props) ->
      let ct = match Webdav_xml.get_prop "getcontenttype" props with
        | Some (_, [ `Pcdata ct ]) -> ct
        | _ -> "text/calendar" in
      let rd =
        Wm.Rd.with_resp_headers (fun header ->
            let header' = Cohttp.Header.remove header "Content-Type" in
            Cohttp.Header.add header' "Content-Type" ct)
          rd
      in
      Wm.continue (`String (Cstruct.to_string data)) rd

  method allowed_methods rd =
    Wm.continue [`GET; `HEAD; `PUT; `DELETE; `OPTIONS; `Other "PROPFIND"; `Other "PROPPATCH"; `Other "COPY" ; `Other "MOVE"; `Other "MKCOL"; `Other "MKCALENDAR" ] rd

  method known_methods rd =
    Wm.continue [`GET; `HEAD; `PUT; `DELETE; `OPTIONS; `Other "PROPFIND"; `Other "PROPPATCH"; `Other "COPY" ; `Other "MOVE"; `Other "MKCOL"; `Other "MKCALENDAR" ] rd

  method charsets_provided rd =
    Wm.continue [
      "utf-8", (fun id -> id)
    ] rd

  method resource_exists rd =
    (* Printf.printf "RESOURCE exists %s \n" (self#id rd); *)
    Fs.stat fs (self#id rd) >>= function
    | Error _ ->
      (* Printf.printf "FALSE\n"; *)
      Wm.continue false rd
    | Ok _ ->
      (* Printf.printf "TRUE\n"; *)
      Wm.continue true rd

  method content_types_provided rd =
    Wm.continue [
      "text/calendar", self#read_calendar
    ] rd

  method content_types_accepted rd =
    Wm.continue [
      "text/calendar", self#write_calendar
    ] rd

  method private process_propfind rd =
    let depth = Cohttp.Header.get rd.Wm.Rd.req_headers "Depth" in
    Cohttp_lwt.Body.to_string rd.Wm.Rd.req_body >>= fun body ->
    Webdav_api.propfind fs ~prefix ~name:(self#id rd) ~body ~depth >>= function
    | Ok (_, answer) -> Wm.continue `Multistatus { rd with Wm.Rd.resp_body = `String answer }
    | Error `Property_not_found -> Wm.continue `Property_not_found rd
    | Error (`Forbidden b) -> Wm.respond ~body:(`String b) (to_status `Forbidden) rd
    | Error `Bad_request -> Wm.respond (to_status `Bad_request) rd

  method private process_proppatch rd =
    Cohttp_lwt.Body.to_string rd.Wm.Rd.req_body >>= fun body ->
    Printf.printf "PROPPATCH:%s\n" body;
    Webdav_api.proppatch fs ~prefix ~name:(self#id rd) ~body >>= function
    | Ok (_, answer) -> Wm.continue `Multistatus { rd with Wm.Rd.resp_body = `String answer }
    | Error `Bad_request -> Wm.respond (to_status `Bad_request) rd

  method process_property rd =
    let replace_header h = Cohttp.Header.replace h "Content-Type" "application/xml" in
    let rd' = Wm.Rd.with_resp_headers replace_header rd in
    match rd'.Wm.Rd.meth with
    | `Other "PROPFIND" -> self#process_propfind rd'
    | `Other "PROPPATCH" -> self#process_proppatch rd'
    | _ -> assert false

  method cannot_create rd =
    let xml = Tyxml.Xml.(node ~a:[Webdav_api.dav_ns] "error" [node "resource-must-be-null" []]) in
    let err = Webdav_xml.tyxml_to_body xml in
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
      match Webdav_xml.string_to_tree body'' with
      | None when body'' <> "" -> Wm.continue `Conflict rd
      | tree ->
        Webdav_api.mkcol fs (self#id rd) tree >>= function
        | Ok _ -> Wm.continue `Created rd
        | Error (`Forbidden t) -> Wm.continue `Forbidden { rd with Wm.Rd.resp_body = `String (Webdav_xml.tree_to_string t) }
        | Error `Conflict -> Wm.continue `Conflict rd
        | Error `Bad_request -> Wm.continue `Conflict rd

  method delete_resource rd =
    Fs.destroy fs (self#id rd) >>= fun res ->
    let deleted, resp_body =
      match res with
      | Ok () -> true, `String "{\"status\":\"ok\"}"
      | Error _ -> false, `String "{\"status\":\"not found\"}"
    in
    Wm.continue deleted { rd with Wm.Rd.resp_body }

  method last_modified rd =
    let file = self#id rd in
    Printf.printf "last modified in webmachine %s\n" file;
    let to_lwt_option = function
    | Error _ -> Lwt.return None
    | Ok x -> Lwt.return (Some x) in
    get_last_modified_prop fs file >>= to_lwt_option >>= fun res ->
    Wm.continue res rd
    
  method generate_etag rd =
    let file = self#id rd in
    Fs.isdir fs file >>= function
    | Error _ -> Wm.continue None rd
    | Ok is_dir ->
      (get_last_modified_prop fs (self#id rd) >|= function
      | Error _ -> rd
      | Ok lm ->
        let add_headers h = Cohttp.Header.add_list h [ ("Last-Modified", lm) ] in
        Wm.Rd.with_resp_headers add_headers rd) >>= fun rd ->
      (if is_dir
      then
        list_dir fs file >>= fun files ->
        collate_directory fs files >|= fun data ->
        Some (etag data)
      else
        Fs.read fs file >|= function
        | Error _ -> None
        | Ok (data, _) -> Some (etag @@ Cstruct.to_string data)) >>= fun result ->
      Wm.continue result rd

  method finish_request rd =
    let add_headers h = Cohttp.Header.add_list h [ ("DAV", "1, extended-mkcol") ] in
    let rd = Wm.Rd.with_resp_headers add_headers rd in
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
    (* Printf.printf "path is %s\n" path ; *)
    path
end

let initialise_fs fs =
  let create_file name data =
    let props = create_properties name "application/json" false (String.length data) in
    Fs.write fs name (Cstruct.of_string data) props
  in
  create_dir_rec fs "users" >>= fun _ ->
  create_dir_rec fs "__uids__/10000000-0000-0000-0000-000000000001/calendar/" >>= fun _ ->
  create_file "1" "{\"name\":\"item 1\"}" >>= fun _ ->
  create_file "2" "{\"name\":\"item 2\"}" >>= fun _ ->
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
