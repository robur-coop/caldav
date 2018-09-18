open Cohttp_lwt_unix
open Lwt.Infix
open Caldav.Webdav_config

module Fs = Caldav.Webdav_fs.Make(FS_unix)
module Xml = Caldav.Webdav_xml
module Dav = Caldav.Webdav_api.Make(Fs)
module Properties = Caldav.Properties
module Privileges = Caldav.Privileges
type file_or_dir = Caldav.Webdav_fs.file_or_dir

module Wm = struct
  module Rd = Webmachine.Rd
  include Webmachine.Make(Cohttp_lwt_unix__Io)
end

open Wm.Rd

module Headers = struct
  let get_content_type headers =
    match Cohttp.Header.get headers "Content-Type" with
    | None -> "text/calendar"
    | Some x -> x

  let get_authorization headers = Cohttp.Header.get headers "Authorization"

  let get_user headers = match get_authorization headers with
    | None -> assert false
    | Some v -> v

  let get_depth headers = Cohttp.Header.get headers "Depth"

  let is_user_agent_mozilla headers =
    match Cohttp.Header.get headers "User-Agent" with
    | None -> false
    | Some x ->
      (* Apple seems to use the regular expression 'Mozilla/.*Gecko.*' *)
      Astring.String.is_prefix ~affix:"Mozilla/" x &&
      Astring.String.is_infix ~affix:"Gecko" x

  let replace_etag_add_location etag location headers =
    let headers' = Cohttp.Header.replace headers "Etag" etag in
    Cohttp.Header.replace headers' "Location" (Uri.to_string @@ location)

  let replace_content_type content_type headers =
    Cohttp.Header.replace headers "Content-Type" content_type

  let replace_authorization auth headers =
    Cohttp.Header.replace headers "Authorization" auth

  let replace_last_modified last_modified headers =
    Cohttp.Header.replace headers "Last-Modified" last_modified

  let replace_dav dav headers =
    Cohttp.Header.replace headers "DAV" dav
end

let to_status x = Cohttp.Code.code_of_status (x :> Cohttp.Code.status_code)

(** A resource for querying an individual item in the database by id via GET,
    modifying an item via PUT, and deleting an item via DELETE. *)
class handler config fs = object(self)
  inherit [Cohttp_lwt.Body.t] Wm.resource

  method private write_component rd =
    Cohttp_lwt.Body.to_string rd.req_body >>= fun body ->
    let path = self#path rd in
    let content_type = Headers.get_content_type rd.req_headers in
    let user = Headers.get_user rd.req_headers in
    Dav.write_component fs config ~path (Ptime_clock.now ()) ~content_type ~user ~data:body >>= function
    | Error e -> Wm.respond (to_status e) rd
    | Ok etag ->
      let location = Uri.with_path config.host path in
      let rd' = with_resp_headers (Headers.replace_etag_add_location etag location) rd in
      Wm.continue true rd'

  method private read_calendar rd =
    let path = self#path rd in
    let is_mozilla = Headers.is_user_agent_mozilla rd.req_headers in
    Dav.read fs ~path ~is_mozilla >>= function
    | Error e -> Wm.respond (to_status e) rd 
    | Ok (body, content_type) ->
      let rd' = with_resp_headers (Headers.replace_content_type content_type) rd in
      Wm.continue (`String body) rd'

  method allowed_methods rd =
    Wm.continue [`GET; `HEAD; `PUT; `DELETE; `OPTIONS; `Other "PROPFIND"; `Other "PROPPATCH"; `Other "MKCOL"; `Other "MKCALENDAR" ; `Other "REPORT" ; `Other "ACL" ] rd

  method known_methods rd =
    Wm.continue [`GET; `HEAD; `PUT; `DELETE; `OPTIONS; `Other "PROPFIND"; `Other "PROPPATCH"; `Other "MKCOL"; `Other "MKCALENDAR" ; `Other "REPORT" ; `Other "ACL"] rd

  method charsets_provided rd =
    Wm.continue [
      "utf-8", (fun id -> id)
    ] rd

  method resource_exists rd =
    Fs.exists fs (self#path rd) >>= fun v ->
    Wm.continue v rd

  method content_types_provided rd =
    Wm.continue [
      "text/xml", self#read_calendar ;
      "text/calendar", self#read_calendar
    ] rd

  method content_types_accepted rd =
    Wm.continue [
      "text/xml", self#write_component ;
      "text/calendar", self#write_component
    ] rd

  method is_authorized rd =
    (* TODO implement digest authentication! *)
    match Headers.get_authorization rd.req_headers with
     | None -> Wm.continue (`Basic "calendar") rd
     | Some v -> Dav.verify_auth_header fs config v >>= function
       | Error msg -> Wm.continue (`Basic "invalid authorization") rd
       | Ok user ->
         let rd' = with_req_headers (Headers.replace_authorization user) rd in
         Wm.continue `Authorized rd'

  method forbidden rd =
    let path = self#path rd in
    let user = Headers.get_user rd.req_headers in
    Dav.access_granted_for_acl fs config ~path rd.meth ~user >>= fun granted ->
    Wm.continue (not granted) rd

  method process_property rd =
    let path = self#path rd in
    let user = Headers.get_user rd.req_headers in
    let depth = Headers.get_depth rd.req_headers in
    Cohttp_lwt.Body.to_string rd.req_body >>= fun body ->
    let dispatch_on_verb = match rd.meth with
    | `Other "PROPFIND" -> Dav.propfind fs config ~path ~user ~depth ~data:body
    | `Other "PROPPATCH" -> Dav.proppatch fs config ~path ~user ~data:body
    | _ -> assert false in
    dispatch_on_verb >>= function
    | Error (`Forbidden body) -> Wm.respond ~body:(`String body) (to_status `Forbidden) rd
    | Error (`Bad_request as e) -> Wm.respond (to_status e) rd
    | Error `Property_not_found -> Wm.continue `Property_not_found rd
    | Ok body -> 
      let rd' = with_resp_headers (Headers.replace_content_type "application/xml") rd in
      Wm.continue `Multistatus { rd' with resp_body = `String body }

  method report rd =
    let path = self#path rd in
    let user = Headers.get_user rd.req_headers in
    Cohttp_lwt.Body.to_string rd.req_body >>= fun body ->
    Dav.report fs config ~path ~user ~data:body >>= function
    | Error (`Bad_request as e) -> Wm.respond (to_status e) rd
    | Ok body -> 
      let rd' = with_resp_headers (Headers.replace_content_type "application/xml") rd in
      Wm.continue `Multistatus { rd' with resp_body = `String body }

  (* required by webmachine API *)
  method cannot_create rd =
    let xml = Xml.node ~ns:Xml.dav_ns "error" [Xml.node ~ns:Xml.dav_ns "resource-must-be-null" []] in
    let err = Xml.tree_to_string xml in
    let rd' = { rd with resp_body = `String err } in
    Wm.continue () rd'

  method create_collection rd =
    let path = self#path rd in
    let user = Headers.get_user rd.req_headers in
    Cohttp_lwt.Body.to_string rd.req_body >>= fun body ->
    Printf.printf "MKCOL/MKCALENDAR: %s\n%!" body;
    Dav.mkcol fs ~path config ~user rd.meth (Ptime_clock.now ()) ~data:body >>= function
    | Error (`Bad_request as e) -> Wm.respond (to_status e) rd
    | Error (`Forbidden body) -> Wm.continue `Forbidden { rd with resp_body = `String body }
    | Error `Conflict -> Wm.continue `Conflict rd
    | Ok _ -> Wm.continue `Created rd

  method delete_resource rd =
    let path = self#path rd in
    Dav.delete fs ~path (Ptime_clock.now ()) >>= fun deleted ->
    Wm.continue deleted rd

  method last_modified rd =
    let path = self#path rd in
    Dav.last_modified fs ~path >>= fun lm ->
    Wm.continue lm rd 

  method generate_etag rd =
    Fs.from_string fs (self#path rd) >>= function
    | Error _ -> Wm.continue None rd
    | Ok f_or_d ->
      Fs.get_property_map fs f_or_d >>= fun map ->
      let rd' =
        (* no special property, already checked for resource *)
        match Properties.unsafe_find (Xml.dav_ns, "getlastmodified") map with
        | Some (_, [ Xml.Pcdata lm ]) ->
          with_resp_headers (Headers.replace_last_modified lm) rd
        | _ -> rd
      in
      (* no special property, already checked for resource *)
      let etag = match Properties.unsafe_find (Xml.dav_ns, "getetag") map with
        | Some (_, [ Xml.Pcdata etag ]) -> Some etag
        | _ -> None
      in
      Printf.printf "etag of %s is %s\n%!" (Fs.to_string f_or_d)
        (match etag with None -> "none" | Some x -> x) ;
      Wm.continue etag rd'

  method finish_request rd =
    let rd' = if rd.meth = `OPTIONS then
        (* access-control, access-control, calendar-access, calendar-schedule, calendar-auto-schedule,
           calendar-availability, inbox-availability, calendar-proxy, calendarserver-private-events,
           calendarserver-private-comments, calendarserver-sharing, calendarserver-sharing-no-scheduling,
           calendarserver-group-sharee, calendar-query-extended, calendar-default-alarms,
           calendar-managed-attachments, calendarserver-partstat-changes, calendarserver-group-attendee,
           calendar-no-timezone, calendarserver-recurrence-split, addressbook, addressbook, extended-mkcol,
           calendarserver-principal-property-search, calendarserver-principal-search, calendarserver-home-sync *)
      with_resp_headers (Headers.replace_dav "1, extended-mkcol, calendar-access") rd
    else
      rd in
    Wm.continue () rd'

  method private path rd =
    Uri.path (rd.uri)
end

class redirect config = object(self)
  inherit [Cohttp_lwt.Body.t] Wm.resource

  method allowed_methods rd =
    Wm.continue [`GET ; `Other "PROPFIND"] rd

  method content_types_provided rd =
    Wm.continue [
      ("*/*"       , self#redirect);
    ] rd

  method content_types_accepted rd =
    Wm.continue [] rd

  method private redirect rd =
    let rd' = redirect (Uri.to_string @@ Uri.with_path config.host config.calendars) rd in
    Wm.respond 301 rd'
end

(* TODO access control for /user *)
(* TODO delete user, delete all existing references (in acls, calendars) *)
(* TODO force create user, uses delete user *)
class create_user config fs = object(self)
  inherit [Cohttp_lwt.Body.t] Wm.resource

  method allowed_methods rd =
    Wm.continue [`PUT; `OPTIONS ] rd

  method known_methods rd =
    Wm.continue [`PUT; `OPTIONS ] rd

  method private create_user rd =
    let uri = rd.uri in
    match Uri.get_query_param uri "user", Uri.get_query_param uri "password" with
    | None, _ | _, None -> Wm.respond (to_status `Bad_request) rd
    | Some name, Some pass ->
      (* TODO may fail if user exists *)
      Dav.make_user fs config name pass >>= fun () ->
      Wm.continue true rd

  method content_types_provided rd =
    Wm.continue [ ("*/*", Wm.continue `Empty) ] rd

  method content_types_accepted rd =
    Wm.continue [
      ("application/octet-stream", self#create_user)
    ] rd
end

let init_users fs config user_password =
  Lwt_list.iter_p (fun (u, p) -> Dav.make_user fs config u p) user_password >>= fun () ->
  Dav.make_group fs config "group" "group-password" ["root" ; "test"]

let main () =
  (* listen on port 8080 *)
  let port = 8080
  and scheme = "http"
  and hostname = "127.0.0.1"
  in
  let host = Uri.make ~port ~scheme ~host:hostname () in
  let principals = "principals" in
  let config = {
    principals ;
    calendars = "calendars" ;
    host ;
    default_acl = [ (`Href (Uri.with_path host @@ "/" ^ principals ^ "/root/"),
                     `Grant [ `All ]) ; (`All, `Grant [`Read]) ]
  } in
  (* create the file system *)
  FS_unix.connect "/tmp/calendar" >>= fun fs ->
  (* only for apple test suite *)
  (* initialize_fs_for_apple_testsuite fs config >>= fun () -> *)
  Dav.initialize_fs fs config >>= fun () ->
  let user_password = [
    ("test", "password") ;
    ("root", "toor") ;
    ("nobody", "1")
  ] in
  init_users fs config user_password >>= fun () ->
  (* the route table *)
  let routes = [
    ("/.well-known/caldav", fun () -> new redirect config) ;
    ("/user", fun () -> new create_user config fs) ;
    ("/" ^ config.principals, fun () -> new handler config fs) ;
    ("/" ^ config.principals ^ "/*", fun () -> new handler config fs) ;
    ("/" ^ config.calendars, fun () -> new handler config fs) ;
    ("/" ^ config.calendars ^ "/*", fun () -> new handler config fs) ;
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
  let config = Server.make ~callback ~conn_closed () in
  Server.create  ~mode:(`TCP(`Port port)) config
  >>= (fun () -> Printf.eprintf "hello_lwt: listening on 0.0.0.0:%d%!" port;
      Lwt.return_unit)

let () =  Lwt_main.run (main ())
