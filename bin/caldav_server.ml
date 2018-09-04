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

let to_status x = Cohttp.Code.code_of_status (x :> Cohttp.Code.status_code)

let etag str = Digest.to_hex @@ Digest.string str

(* assumption: path is a directory - otherwise we return none *)
(* out: ( name * typ * last_modified ) list - non-recursive *)
let list_dir fs (`Dir dir) =
  let list_file f_or_d =
    (* maybe implement a Fs.get_property? *)
    Fs.get_property_map fs f_or_d >|= function
    | None -> assert false
    | Some m ->
      let last_modified = match Xml.get_prop (Xml.dav_ns, "getlastmodified") m with
        | Some (_, [ Xml.Pcdata lm ]) -> lm
        | _ -> assert false
      in
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

let directory_etag prefix fs (`Dir dir) =
  directory_as_html prefix fs (`Dir dir) >|= fun data ->
  etag data

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
      let empty = Icalendar.Params.empty in
      let name =
        match Xml.get_prop (Xml.dav_ns, "displayname") props with
        | Some (_, [ Xml.Pcdata name ]) -> [ `Xprop (("WR", "CALNAME"), empty, name) ]
        | _ -> []
      in
      let calprops = [
        `Prodid (empty, "-//ROBUR.IO//EN") ;
        `Version (empty, "2.0")
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
    Cohttp_lwt.Body.to_string rd.Wm.Rd.req_body >>= fun body ->
    Printf.printf "write_calendar: %s\n%!" body ;
    let name = self#id rd in
    let content_type =
      match Cohttp.Header.get rd.Wm.Rd.req_headers "Content-Type" with
      | None -> "text/calendar"
      | Some x -> x
    in
    match Icalendar.parse body with
    | Error e ->
      Printf.printf "error %s while parsing calendar\n" e ;
      Wm.continue false rd
    | Ok cal ->
      let ics = Icalendar.to_ics cal in
      let etag = etag ics in
      let file = Fs.file_from_string name in
      Dav.write fs ~name:file ~etag ~content_type ics >>= function
      | Error e -> Wm.respond (to_status e) rd
      | Ok _ ->
        Printf.printf "wrote calendar %s\n%!" name ;
        let rd = Wm.Rd.with_resp_headers (fun header ->
            (* let header' = Cohttp.Header.remove header "ETag" in
             * let header'' = Cohttp.Header.add header' "Etag" etag in *)
            Cohttp.Header.add header "Location" ("http://127.0.0.1:8080" ^ prefix ^ "/" ^ name)
          ) rd
        in
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
      "text/xml", self#read_calendar ;
      "text/calendar", self#read_calendar
    ] rd

  method content_types_accepted rd =
    Wm.continue [
      "text/xml", self#write_calendar ;
      "text/calendar", self#write_calendar
    ] rd

  method private process_propfind rd name =
    let depth = Cohttp.Header.get rd.Wm.Rd.req_headers "Depth" in
    Cohttp_lwt.Body.to_string rd.Wm.Rd.req_body >>= fun body ->
    Printf.printf "PROPFIND: %s\n%!" body;
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
    Printf.printf "PROPPATCH: %s\n%!" body;
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
    Cohttp_lwt.Body.to_string rd.Wm.Rd.req_body >>= fun body ->
    Printf.printf "REPORT: %s\n%!" body;
    Fs.from_string fs (self#id rd) >>= function
    | Error _ -> Wm.respond (to_status `Bad_request) rd
    | Ok f_or_d ->
      match Xml.string_to_tree body with
      | None -> Wm.respond (to_status `Bad_request) rd
      | Some tree ->
        Dav.report fs ~prefix ~name:f_or_d tree >>= function
        | Ok b -> Wm.continue `Multistatus { rd with Wm.Rd.resp_body = `String (Xml.tree_to_string b) }
        | Error `Bad_request -> Wm.respond (to_status `Bad_request) rd

  method cannot_create rd =
    let xml = Xml.node ~ns:Xml.dav_ns "error" [Xml.node ~ns:Xml.dav_ns "resource-must-be-null" []] in
    let err = Xml.tree_to_string xml in
    let rd' = { rd with Wm.Rd.resp_body = `String err } in
    Wm.continue () rd'

  method create_collection rd =
    Cohttp_lwt.Body.to_string rd.Wm.Rd.req_body >>= fun body ->
    Printf.printf "MKCOL/MKCALENDAR: %s\n%!" body;
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
    Fs.from_string fs (self#id rd) >>= function
    | Error _ -> Wm.continue false rd
    | Ok f_or_d ->
      Dav.delete fs ~name:f_or_d >>= fun _ ->
      Wm.continue true rd

  method last_modified rd =
    Fs.from_string fs (self#id rd) >>= function
    | Error _ -> Wm.continue None rd
    | Ok f_or_d ->
      Fs.get_property_map fs f_or_d >|= (function
          | None -> None
          | Some map -> match Xml.get_prop (Xml.dav_ns, "getlastmodified") map with
            | Some (_, [ Xml.Pcdata lm]) -> Some lm
            | _ -> None) >>= fun res ->
      Wm.continue res rd

  method generate_etag rd =
    Fs.from_string fs (self#id rd) >>= function
    | Error _ -> Wm.continue None rd
    | Ok f_or_d ->
      Fs.get_property_map fs f_or_d >>= function
      | None -> Wm.continue None rd
      | Some map ->
        let rd' =
          match Xml.get_prop (Xml.dav_ns, "getlastmodified") map with
          | Some (_, [ Xml.Pcdata lm ]) ->
            let add_headers h = Cohttp.Header.add_list h [ ("Last-Modified", lm) ] in
            Wm.Rd.with_resp_headers add_headers rd
          | _ -> rd
        in
        let etag = match Xml.get_prop (Xml.dav_ns, "getetag") map with
          | Some (_, [ Xml.Pcdata etag ]) -> Some etag
          | _ -> None
        in
        Printf.printf "etag of %s is %s\n%!" (Fs.to_string f_or_d)
          (match etag with None -> "none" | Some x -> x) ;
        Wm.continue etag rd'

  method finish_request rd =
    let rd' = if rd.Wm.Rd.meth = `OPTIONS then
        let add_headers h = Cohttp.Header.add_list h [ ("DAV", "1, extended-mkcol, calendar-access") ] in
        (* access-control, access-control, calendar-access, calendar-schedule, calendar-auto-schedule,
           calendar-availability, inbox-availability, calendar-proxy, calendarserver-private-events,
           calendarserver-private-comments, calendarserver-sharing, calendarserver-sharing-no-scheduling,
           calendarserver-group-sharee, calendar-query-extended, calendar-default-alarms,
           calendar-managed-attachments, calendarserver-partstat-changes, calendarserver-group-attendee,
           calendar-no-timezone, calendarserver-recurrence-split, addressbook, addressbook, extended-mkcol,
           calendarserver-principal-property-search, calendarserver-principal-search, calendarserver-home-sync *)
      Wm.Rd.with_resp_headers add_headers rd
    else
      rd in
    Wm.continue () rd'

  method private id rd =
    let url = Uri.path (rd.Wm.Rd.uri) in
    let pl = String.length prefix in
    let p = String.sub url pl (String.length url - pl) in
    if String.length p > 0 && String.get p 0 = '/' then
      String.sub p 1 (String.length p - 1)
    else
      p
end

let server_ns = "http://calendarserver.org/ns/"
let carddav_ns = "urn:ietf:params:xml:ns:carddav"

let initialise_fs fs =
  let create_properties name content_type is_dir length =
    Xml.create_properties ~content_type
      is_dir (Ptime.to_rfc3339 (Ptime_clock.now ())) length name
  in
  let props =
    let p = create_properties "/" "text/directory" true 0 in
    Xml.PairMap.add (Xml.caldav_ns, "calendar-home-set")
      ([], [Xml.node "href" ~ns:Xml.dav_ns [Xml.pcdata "http://127.0.0.1:8080/calendars/__uids__/10000000-0000-0000-0000-000000000001/calendar"]])
      p
  in
  Fs.write_property_map fs (`Dir []) props >>= fun _ ->
  let create_dir ?(props=[]) name =
    let dir = Fs.dir_from_string name in
    let propmap = create_properties name "text/directory" true 0 in
    let propmap' = List.fold_left (fun p (k, v) -> Xml.PairMap.add k v p) propmap props in
    Fs.mkdir fs dir propmap'
  in
  let create_calendar name =
    let props =
      let reports = [
        Xml.caldav_ns, "calendar-query" ;
        Xml.caldav_ns, "calendar-multiget" ;
(*        Xml.dav_ns, "acl-principal-prop-set" ;
        Xml.dav_ns, "principal-match" ;
        Xml.dav_ns, "principal-property-search" ;
        Xml.dav_ns, "expand-property" ;
        server_ns, "calendarserver-principal-search" ;
        Xml.caldav_ns, "free-busy-query" ;
        carddav_ns, "addressbook-query" ;
          carddav_ns, "addressbook-multiget" *)
        (* Xml.dav_ns, "sync-collection" *)
      ] in
      let report_nodes =
        List.map (fun (ns, s) ->
            Xml.node ~ns:Xml.dav_ns "supported-report"
              [ Xml.node ~ns:Xml.dav_ns "report"
                  [ Xml.node ~ns s [] ] ])
          reports
      in
      let comps =
        List.map (fun s ->
            Xml.node ~ns:Xml.caldav_ns "comp" ~a:[(("", "name"), s)] [])
          [ "VEVENT" ; "VTODO" ; "VTIMEZONE" ; "VFREEBUSY" ]
      in
      [
      (Xml.dav_ns, "resourcetype"), ([], [Xml.node ~ns:Xml.caldav_ns "calendar" []; Xml.node ~ns:Xml.dav_ns "collection" []]) ;
      (Xml.dav_ns, "supported-report-set"), ([], report_nodes) ;
      (Xml.caldav_ns, "supported-calendar-component-set"), ([], comps) ;
      (* (server_ns, "getctag"), ([], [ Xml.pcdata "hallo" ]) *)
      (* (Xml.dav_ns, "owner"), ([], [ Xml.pcdata "/principals/__uids__/10000000-0000-0000-0000-000000000001" ]) ; *)
      (* (Xml.dav_ns, "current-user-principal"), ([], [ Xml.pcdata "/principals/__uids__/10000000-0000-0000-0000-000000000001" ]) ; *)
    ] in
    create_dir ~props name
  in
  create_dir "users" >>= fun _ ->
  create_dir "__uids__" >>= fun _ ->
  create_dir "__uids__/10000000-0000-0000-0000-000000000001" >>= fun _ ->
  create_calendar "__uids__/10000000-0000-0000-0000-000000000001/calendar" >>= fun _ ->
  (*  create_calendar "__uids__/10000000-0000-0000-0000-000000000001/calendar/geburtstage" >>= fun _ -> *)
  create_dir "__uids__/10000000-0000-0000-0000-000000000001/tasks" >>= fun _ ->
  Lwt.return_unit

type config = {
  users : string list ;
  hostname : string ;
}

let main () =
  (* listen on port 8080 *)
  let port = 8080 in
  (* create the file system *)
  Fs.connect "/tmp/calendar" >>= fun fs ->
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
    Printf.printf "REQUEST %s %s headers %s\n%!"
      (Code.string_of_method (Request.meth request))
      (Request.resource request)
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
      Printf.printf "\nRESPONSE %d - %s %s%s, body: %s\n\n%!"
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
  (* only for apple test suite *)
  (*initialise_fs fs >>= fun () ->*)
  let config = Server.make ~callback ~conn_closed () in
  Server.create  ~mode:(`TCP(`Port port)) config
  >>= (fun () -> Printf.eprintf "hello_lwt: listening on 0.0.0.0:%d%!" port;
      Lwt.return_unit)

let () =  Lwt_main.run (main ())
