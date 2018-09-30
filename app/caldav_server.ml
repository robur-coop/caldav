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

let now = Ptime_clock.now

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

  let replace_location location headers =
    Cohttp.Header.replace headers "Location" (Uri.to_string @@ location)

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
    Logs.debug (fun m -> m "write_component path %s user %s body @.%s" path user body);
    Dav.write_component fs config ~path (now ()) ~content_type ~user ~data:body >>= function
    | Error e -> Wm.respond (to_status e) rd
    | Ok etag ->
      let location = Uri.with_path config.host path in
      let rd' = with_resp_headers (Headers.replace_etag_add_location etag location) rd in
      Wm.continue true rd'

  method private read_calendar rd =
    let path = self#path rd in
    let is_mozilla = Headers.is_user_agent_mozilla rd.req_headers in
    Logs.debug (fun m -> m "read_calendar path %s is_mozilla %b" path is_mozilla);
    Dav.read fs ~path ~is_mozilla >>= function
    | Error e -> Wm.respond (to_status e) rd
    | Ok (content_type, body) ->
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
       | Error msg ->
         Logs.warn (fun m -> m "is_authorized failed with header value %s and message %s" v msg);
         Wm.continue (`Basic "invalid authorization") rd
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
    Logs.debug (fun m -> m "process_property verb %s path %s user %s body @.%s" (Cohttp.Code.string_of_method rd.meth) path user body);
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
    Logs.debug (fun m -> m "report path %s user %s body @.%s" path user body);
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
    Logs.debug (fun m -> m "create_collection verb %s path %s user %s body @.%s" (Cohttp.Code.string_of_method rd.meth) path user body);
    Dav.mkcol fs ~path config ~user rd.meth (now ()) ~data:body >>= function
    | Error (`Bad_request as e) -> Wm.respond (to_status e) rd
    | Error (`Forbidden body) -> Wm.continue `Forbidden { rd with resp_body = `String body }
    | Error `Conflict -> Wm.continue `Conflict rd
    | Ok _ -> Wm.continue `Created rd

  method delete_resource rd =
    let path = self#path rd in
    Logs.debug (fun m -> m "delete_resource path %s" path);
    Dav.delete fs ~path (now ()) >>= fun deleted ->
    Wm.continue deleted rd

  method last_modified rd =
    let path = self#path rd in
    Dav.last_modified fs ~path >>= fun lm ->
    Wm.continue lm rd

  method generate_etag rd =
    let path = self#path rd in
    (Dav.last_modified fs ~path >|= function
    | None -> rd
    | Some lm -> with_resp_headers (Headers.replace_last_modified lm) rd) >>= fun rd' ->
    Dav.compute_etag fs ~path >>= fun etag ->
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

  method known_methods = self#allowed_methods

  method content_types_provided rd =
    Wm.continue [ ("*/*", self#redirect) ] rd

  method content_types_accepted rd =
    Wm.continue [] rd

  method process_property rd =
    let rd' = redirect (Uri.to_string @@ Uri.with_path config.host config.calendars) rd in
    Wm.respond 301 rd'

  method private redirect rd =
    let rd' = redirect (Uri.to_string @@ Uri.with_path config.host config.calendars) rd in
    Wm.respond 301 rd'
end

let sane username =
  username <> "" && Astring.String.for_all Astring.Char.Ascii.is_alphanum username

(* TODO force create user, uses delete user *)
class user config fs = object(self)
  inherit [Cohttp_lwt.Body.t] Wm.resource

  method private requested_user rd =
    match Uri.get_query_param rd.uri "name" with
    | None -> Error `Bad_request
    | Some x -> if not (sane x) then Error `Bad_request else Ok x

  method private requested_password rd =
    match Uri.get_query_param rd.uri "password" with
    | None -> Error `Bad_request
    | Some x -> Ok x

  method allowed_methods rd =
    Wm.continue [`PUT; `OPTIONS; `DELETE ] rd

  method known_methods rd =
    Wm.continue [`PUT; `OPTIONS; `DELETE ] rd

  method private create_user rd =
    match self#requested_user rd, self#requested_password rd with
    | Error _, _ | _, Error _ -> Wm.respond (to_status `Bad_request) rd
    | Ok name, Ok pass ->
      let now = now () in
      Dav.make_user fs now config name pass >>= fun principal_url ->
      let rd' = with_resp_headers (Headers.replace_location principal_url) rd in
      Wm.continue true rd'

  (* TODO? allow a user to delete themselves *)
  (* TODO? soft-delete: "mark as deleted" *)
  method delete_resource rd =
    match self#requested_user rd with
    | Error `Bad_request -> Wm.respond (to_status `Bad_request) rd
    | Ok name ->
      Dav.delete_user fs config name >>= function
      | Error e -> Wm.respond (to_status e) rd
      | Ok () -> Wm.continue true rd

  method is_conflict rd =
    match self#requested_user rd with
    | Error `Bad_request -> Wm.respond (to_status `Bad_request) rd
    | Ok name ->
      Fs.dir_exists fs (`Dir [config.principals ; name ]) >>= fun user_exists ->
      (* TODO this is a hack to overload PUT /user for creation of new users and password changes *)
      match user_exists, self#requested_password rd with
      | true, Ok new_pass ->
        begin
          Dav.change_user_password fs config name new_pass >>= function
          | Ok () -> Wm.respond (to_status `OK) rd
          | Error e -> Wm.respond (to_status e) rd
        end
      | _, _ -> Wm.continue user_exists rd

  method content_types_provided rd =
    Wm.continue [ ("*/*", Wm.continue `Empty) ] rd

  method content_types_accepted rd =
    Wm.continue [
      ("application/octet-stream", self#create_user)
    ] rd

  method is_authorized rd =
    (* TODO implement digest authentication! *)
    match Headers.get_authorization rd.req_headers with
     | None -> Wm.continue (`Basic "calendar") rd
     | Some v -> Dav.verify_auth_header fs config v >>= function
       | Error msg ->
         Logs.warn (fun m -> m "is_authorized failed with header value %s and message %s" v msg);
         Wm.continue (`Basic "invalid authorization") rd
       | Ok user ->
         let rd' = with_req_headers (Headers.replace_authorization user) rd in
         Wm.continue `Authorized rd'

  method forbidden rd =
    let user = Headers.get_user rd.req_headers in
    match self#requested_user rd with
    | Error `Bad_request -> Wm.respond (to_status `Bad_request) rd
    | Ok requested_user ->
      Dav.access_granted_for_acl fs config ~path:(config.principals ^ "/" ^ requested_user) rd.meth ~user >>= fun principals_granted ->
      Dav.access_granted_for_acl fs config ~path:(config.calendars ^ "/" ^ requested_user) rd.meth ~user >>= fun calendars_granted ->
      Wm.continue (not (principals_granted && calendars_granted)) rd
end

class group config fs = object(self)
  inherit [Cohttp_lwt.Body.t] Wm.resource

  method private requested_group rd =
    match Uri.get_query_param rd.uri "name" with
    | None -> Error `Bad_request
    | Some x -> if not (sane x) then Error `Bad_request else Ok x

  method private requested_members rd =
    match Uri.get_query_param rd.uri "members" with
    | None -> Error `Bad_request
    | Some x ->
      let members = Astring.String.cuts ~sep:"," x in
      if List.for_all sane members
      then Ok members
      else Error `Bad_request

  method allowed_methods rd =
    Wm.continue [`PUT; `OPTIONS; `DELETE ] rd

  method known_methods rd =
    Wm.continue [`PUT; `OPTIONS; `DELETE ] rd

  method private create_group rd =
    match self#requested_group rd, self#requested_members rd with
    | Error _, _ | _, Error _ -> Wm.respond (to_status `Bad_request) rd
    | Ok name, Ok members ->
      let now = now () in
      Dav.make_group fs now config name members >>= fun principal_url ->
      let rd' = with_resp_headers (Headers.replace_location principal_url) rd in
      Wm.continue true rd'

  method delete_resource rd =
    match self#requested_group rd with
    | Error `Bad_request -> Wm.respond (to_status `Bad_request) rd
    | Ok name ->
      Dav.delete_group fs config name >>= function
      | Error e -> Wm.respond (to_status e) rd
      | Ok () -> Wm.continue true rd

  method is_conflict rd =
    match self#requested_group rd with
    | Error `Bad_request -> Wm.respond (to_status `Bad_request) rd
    | Ok name ->
      Fs.dir_exists fs (`Dir [config.principals ; name ]) >>= fun group_exists ->
      match group_exists, self#requested_members rd with
      | true, Ok members ->
        begin
          Dav.change_group_members fs config name members >>= function
          | Ok () -> Wm.respond (to_status `OK) rd
          | Error e -> Wm.respond (to_status e) rd
        end
      | _ -> Wm.continue group_exists rd

  method content_types_provided rd =
    Wm.continue [ ("*/*", Wm.continue `Empty) ] rd

  method content_types_accepted rd =
    Wm.continue [
      ("application/octet-stream", self#create_group)
    ] rd

  method is_authorized rd =
    (* TODO implement digest authentication! *)
    match Headers.get_authorization rd.req_headers with
     | None -> Wm.continue (`Basic "calendar") rd
     | Some v -> Dav.verify_auth_header fs config v >>= function
       | Error msg ->
         Logs.warn (fun m -> m "is_authorized failed with header value %s and message %s" v msg);
         Wm.continue (`Basic "invalid authorization") rd
       | Ok user ->
         let rd' = with_req_headers (Headers.replace_authorization user) rd in
         Wm.continue `Authorized rd'

  method forbidden rd =
    let user = Headers.get_user rd.req_headers in
    match self#requested_group rd with
    | Error `Bad_request -> Wm.respond (to_status `Bad_request) rd
    | Ok requested_group ->
      Dav.access_granted_for_acl fs config ~path:(config.principals ^ "/" ^ requested_group) rd.meth ~user >>= fun principals_granted ->
      Dav.access_granted_for_acl fs config ~path:(config.calendars ^ "/" ^ requested_group) rd.meth ~user >>= fun calendars_granted ->
      Wm.continue (not (principals_granted && calendars_granted)) rd
end


let init_users fs now config user_password =
  Lwt_list.iter_p (fun (u, p) -> Dav.make_user fs now config u p >|= fun _ -> ()) user_password >>= fun () ->
  Dav.make_group fs now config "group" ["root" ; "test"]

let main () =
  (* avoid ECONNRESET, see https://github.com/mirage/ocaml-cohttp/issues/511 *)
  Lwt.async_exception_hook := (function
      | Unix.Unix_error (error, func, arg) ->
        Logs.warn (fun m -> m  "Client connection error %s: %s(%S)"
                      (Unix.error_message error) func arg)
      | exn -> Logs.err (fun m -> m "Unhandled exception: %a" Fmt.exn exn)
    );
  Logs.set_reporter (Logs_fmt.reporter ());
  Logs.set_level (Some Logs.Debug);
  let now = now () in
  (* listen on port 8080 *)
  let port = 8080
  and scheme = "http"
  and hostname = "127.0.0.1"
  in
  let host =
    (* strip port if default for scheme, this should be handled by the Uri library *)
    let port = match scheme, port with
      | "http", 80 -> None
      | "https", 443 -> None
      | _ -> Some port
    in
    Uri.make ?port ~scheme ~host:hostname ()
  in
  let principals = "principals" in
  let config = {
    principals ;
    calendars = "calendars" ;
    host ;
    admin_only_acl = [
      (`Href (Uri.with_path host @@ "/" ^ principals ^ "/root/"), `Grant [ `All ]) ;
      (`All, `Grant [ `Read ])
    ]
  } in
  (* create the file system *)
  FS_unix.connect "/tmp/calendar" >>= fun fs ->
  (* only for apple test suite *)
  (* initialize_fs_for_apple_testsuite fs now config >>= fun () -> *)
  Dav.initialize_fs fs now config >>= fun () ->
  let user_password = [
    ("test", "password") ;
    ("root", "toor") ;
    ("nobody", "1")
  ] in
  init_users fs now config user_password >>= fun _ ->
  (* the route table *)
  let routes = [
    ("/.well-known/caldav", fun () -> new redirect config) ;
    ("/user", fun () -> new user config fs) ;
    ("/group", fun () -> new group config fs) ;
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
    Logs.info (fun m -> m "REQUEST %s %s headers %s"
      (Code.string_of_method (Request.meth request))
      (Request.resource request)
      (Header.to_string (Request.headers request)) );
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
      Logs.info (fun m -> m "\nRESPONSE %d - %s %s%s, body: %s"
        (Code.code_of_status status)
        (Code.string_of_method (Request.meth request))
        (Uri.path (Request.uri request))
        path
        (match body with `String s -> s | `Empty -> "empty" | _ -> "unknown") ) ;
      (* Finally, send the response to the client *)
      Server.respond ~headers ~body ~status ()
  in
  (* create the server and handle requests with the function defined above *)
  let conn_closed (ch, conn) =
    Logs.info (fun m -> m "connection %s closed"
      (Sexplib.Sexp.to_string_hum (Conduit_lwt_unix.sexp_of_flow ch)))
  in
  let config = Server.make ~callback ~conn_closed () in
  Server.create  ~mode:(`TCP(`Port port)) config
  >>= (fun () -> Logs.app (fun m -> m "caldav_server.exe: listening on 0.0.0.0:%d%!" port);
      Lwt.return_unit)

let () =  Lwt_main.run (main ())
