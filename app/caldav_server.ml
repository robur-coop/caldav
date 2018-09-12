open Cohttp_lwt_unix
open Lwt.Infix
open Caldav.Webdav_config

module Fs = Caldav.Webdav_fs.Make(FS_unix)
module Xml = Caldav.Webdav_xml
module Dav = Caldav.Webdav_api.Make(Fs)
module Properties = Caldav.Properties
type file_or_dir = Caldav.Webdav_fs.file_or_dir

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
    Fs.get_property_map fs f_or_d >|= fun m ->
    let last_modified = match Properties.find (Xml.dav_ns, "getlastmodified") m with
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

let directory_as_html fs (`Dir dir) =
  list_dir fs (`Dir dir) >|= fun files ->
  let print_file (file, is_dir, last_modified) =
    Printf.sprintf "<tr><td><a href=\"%s\">%s</a></td><td>%s</td><td>%s</td></tr>"
      file file (if is_dir then "directory" else "text/calendar") last_modified in
  String.concat "\n" (List.map print_file files)

let directory_etag fs (`Dir dir) =
  directory_as_html fs (`Dir dir) >|= fun data ->
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
    Fs.get_property_map fs (`Dir dir) >>= fun props ->
    let empty = Icalendar.Params.empty in
    let name =
      match Properties.find (Xml.dav_ns, "displayname") props with
      | Some (_, [ Xml.Pcdata name ]) -> [ `Xprop (("WR", "CALNAME"), empty, name) ]
      | _ -> []
    in
    let calprops = [
      `Prodid (empty, "-//ROBUR.IO//EN") ;
      `Version (empty, "2.0")
    ] @ name in
    Lwt_list.map_p calendar_components files >|= fun components ->
    Icalendar.to_ics (calprops, List.flatten components)

let hash_password password =
  let server_secret = "server_secret--" in
  Cstruct.to_string @@
  Nocrypto.Hash.SHA256.digest @@
  Cstruct.of_string (server_secret ^ password)

let verify_auth_header user_password v =
  match Astring.String.cut ~sep:"Basic " v with
  | Some ("", b64) ->
    begin match Nocrypto.Base64.decode (Cstruct.of_string b64) with
      | None -> Error "invalid base64 encoding"
      | Some data -> match Astring.String.cut ~sep:":" (Cstruct.to_string data) with
        | None -> Error "invalid user:password encoding"
        | Some (user, password) ->
          Printf.printf "user is %s, password %s\n%!" user password ;
          let hashed = hash_password password in
          if List.mem (user, hashed) user_password
          then Ok user
          else Error "invalid user or wrong password"
    end
  | _ -> Error "bad header"

let calendar_to_collection data =
  if data = "" then Ok "" else
  match Xml.string_to_tree data with
  | Some (Xml.Node (ns, "mkcalendar", a, c)) when ns = Xml.dav_ns -> Ok (Xml.tyxml_to_body (Xml.tree_to_tyxml (Xml.node ~ns:Xml.dav_ns ~a "mkcol" c)))
  | _ -> Error `Bad_request

(** A resource for querying an individual item in the database by id via GET,
    modifying an item via PUT, and deleting an item via DELETE. *)
class handler config fs = object(self)
  inherit [Cohttp_lwt.Body.t] Wm.resource

  method private write_calendar rd =
    Cohttp_lwt.Body.to_string rd.Wm.Rd.req_body >>= fun body ->
    Printf.printf "write_calendar: %s\n%!" body ;
    let path = self#path rd in
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
      let file = Fs.file_from_string path in
      Dav.write fs ~path:file ~etag ~content_type ics >>= function
      | Error e -> Wm.respond (to_status e) rd
      | Ok _ ->
        Printf.printf "wrote calendar %s\n%!" path ;
        let rd = Wm.Rd.with_resp_headers (fun header ->
            let header' = Cohttp.Header.remove header "ETag" in
            let header'' = Cohttp.Header.add header' "Etag" etag in
            Cohttp.Header.add header'' "Location"
              (Uri.to_string @@ Uri.with_path config.host path)
          ) rd
        in
        Wm.continue true rd

  method private read_calendar rd =
    let file = self#path rd in
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
        directory_as_html fs (`Dir dir) >>= fun listing ->
        Wm.continue (`String listing) rd
      else
        (* TODO: check wheter CalDAV:calendar property is set as resourcetype!
           otherwise: standard WebDAV directory listing *)
        directory_as_ics fs (`Dir dir) >>= fun data ->
        Wm.continue (`String data) rd
    | `File f ->
      Fs.read fs (`File f) >>== fun (data, props) ->
      let ct = match Properties.find (Xml.dav_ns, "getcontenttype") props with
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
      "text/xml", self#write_calendar ;
      "text/calendar", self#write_calendar
    ] rd

  method is_authorized rd =
    (* TODO implement digest authentication! *)
    let res, rd' =
      match Cohttp.Header.get rd.Wm.Rd.req_headers "Authorization" with
      | None -> `Basic "calendar", rd
      | Some v ->
        match verify_auth_header config.user_password v with
        | Ok user ->
          let replace_header h =
            Cohttp.Header.replace h "Authorization" user
          in
          let rd' = Wm.Rd.with_req_headers replace_header rd in
          `Authorized, rd'
        | Error msg ->
          Printf.printf "ivalid authorization: %s\n" msg ;
          `Basic "invalid authorization", rd
    in
    Wm.continue res rd'

  method forbidden rd =
    let path = self#path rd in
    let user =
      match Cohttp.Header.get rd.Wm.Rd.req_headers "Authorization" with
      | None -> assert false
      | Some v -> v
    in
    let get_property_map_for_user name =
      let user_path = `Dir [ config.principals ; name ] in
      Fs.get_property_map fs user_path
    in
    get_property_map_for_user user >>= fun auth_user_props ->
    Dav.access_granted_for_acl fs path rd.Wm.Rd.meth auth_user_props >>= fun granted ->
    Wm.continue (not granted) rd

  method private process_propfind rd path =
    let depth = Cohttp.Header.get rd.Wm.Rd.req_headers "Depth" in
    let user =
      match Cohttp.Header.get rd.Wm.Rd.req_headers "Authorization" with
      | None -> assert false
      | Some user -> (`Dir [ config.principals ; user ])
    in
    Cohttp_lwt.Body.to_string rd.Wm.Rd.req_body >>= fun body ->
    Printf.printf "PROPFIND: %s\n%!" body;
    match Xml.string_to_tree body with
    | None -> Wm.respond (to_status `Bad_request) rd
    | Some tree ->
      Dav.propfind fs ~host:config.host ~path tree ~user ~depth >>= function
      | Ok b -> Wm.continue `Multistatus { rd with Wm.Rd.resp_body = `String (Xml.tree_to_string b) }
      | Error `Property_not_found -> Wm.continue `Property_not_found rd
      | Error (`Forbidden b) -> Wm.respond ~body:(`String (Xml.tree_to_string b)) (to_status `Forbidden) rd
      | Error `Bad_request -> Wm.respond (to_status `Bad_request) rd

  method private process_proppatch rd path =
    Cohttp_lwt.Body.to_string rd.Wm.Rd.req_body >>= fun body ->
    Printf.printf "PROPPATCH: %s\n%!" body;
    match Xml.string_to_tree body with
    | None -> Wm.respond (to_status `Bad_request) rd
    | Some tree ->
      Dav.proppatch fs ~host:config.host ~path tree >>= function
      | Ok (_, b) -> Wm.continue `Multistatus { rd with Wm.Rd.resp_body = `String (Xml.tree_to_string b) }
      | Error `Bad_request -> Wm.respond (to_status `Bad_request) rd

  method process_property rd =
    let replace_header h = Cohttp.Header.replace h "Content-Type" "application/xml" in
    let rd' = Wm.Rd.with_resp_headers replace_header rd in
    Fs.from_string fs (self#path rd) >>= function
    | Error _ -> Wm.respond (to_status `Bad_request) rd
    | Ok f_or_d ->
      match rd'.Wm.Rd.meth with
      | `Other "PROPFIND" -> self#process_propfind rd' f_or_d
      | `Other "PROPPATCH" -> self#process_proppatch rd' f_or_d
      | _ -> assert false

  method report rd =
    Cohttp_lwt.Body.to_string rd.Wm.Rd.req_body >>= fun body ->
    Printf.printf "REPORT: %s\n%!" body;
    Fs.from_string fs (self#path rd) >>= function
    | Error _ -> Wm.respond (to_status `Bad_request) rd
    | Ok f_or_d ->
      match Xml.string_to_tree body with
      | None -> Wm.respond (to_status `Bad_request) rd
      | Some tree ->
        let user =
          match Cohttp.Header.get rd.Wm.Rd.req_headers "Authorization" with
          | None -> assert false
          | Some user -> (`Dir [ config.principals ; user ])
        in
        Dav.report fs ~host:config.host ~path:f_or_d tree ~user >>= function
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
        let dir = Fs.dir_from_string (self#path rd) in
        Dav.mkcol fs dir tree >>= function
        | Ok _ -> Wm.continue `Created rd
        | Error (`Forbidden t) -> Wm.continue `Forbidden { rd with Wm.Rd.resp_body = `String (Xml.tree_to_string t) }
        | Error `Conflict -> Wm.continue `Conflict rd
        | Error `Bad_request -> Wm.continue `Conflict rd

  method delete_resource rd =
    Fs.from_string fs (self#path rd) >>= function
    | Error _ -> Wm.continue false rd
    | Ok f_or_d ->
      Dav.delete fs ~path:f_or_d >>= fun _ ->
      Wm.continue true rd

  method last_modified rd =
    Fs.from_string fs (self#path rd) >>= function
    | Error _ -> Wm.continue None rd
    | Ok f_or_d ->
      Fs.get_property_map fs f_or_d >|= (fun map ->
        match Properties.find (Xml.dav_ns, "getlastmodified") map with
          | Some (_, [ Xml.Pcdata lm]) -> Some lm
          | _ -> None) >>= fun res ->
      Wm.continue res rd

  method generate_etag rd =
    Fs.from_string fs (self#path rd) >>= function
    | Error _ -> Wm.continue None rd
    | Ok f_or_d ->
      Fs.get_property_map fs f_or_d >>= fun map ->
      let rd' =
        match Properties.find (Xml.dav_ns, "getlastmodified") map with
        | Some (_, [ Xml.Pcdata lm ]) ->
          let add_headers h = Cohttp.Header.add_list h [ ("Last-Modified", lm) ] in
          Wm.Rd.with_resp_headers add_headers rd
        | _ -> rd
      in
      let etag = match Properties.find (Xml.dav_ns, "getetag") map with
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

  method private path rd =
    Uri.path (rd.Wm.Rd.uri)
    (*if String.length p > 0 && String.get p 0 = '/' then
      String.sub p 1 (String.length p - 1)
    else 
      p *)
end

let server_ns = "http://calendarserver.org/ns/"
let carddav_ns = "urn:ietf:params:xml:ns:carddav"

let make_dir fs ?(resourcetype = []) ?(props=[]) dir =
  let propmap =
    let resourcetype' = Xml.node ~ns:Xml.dav_ns "collection" [] :: resourcetype in
    Properties.create ~content_type:"text/directory" ~resourcetype:resourcetype'
      (Ptime.to_rfc3339 (Ptime_clock.now ())) 0 (Fs.basename (dir :> file_or_dir))
  in
  let propmap' = List.fold_left (fun p (k, v) -> Properties.add k v p) propmap props in
  Fs.mkdir fs dir propmap'

let make_dir_if_not_present fs ?resourcetype ?props dir =
  Fs.dir_exists fs dir >>= fun exists ->
  (*  if not exists then *)
    make_dir fs ?resourcetype ?props dir >|= fun _ -> ()
(*  else
    Lwt.return_unit *)

let grant_test config =
  let url = Uri.with_path config.host (Fs.to_string (`Dir [ config.principals ; "test" ])) in
  (Xml.dav_ns, "acl"), ([], [ Xml.ace_to_xml (`Href url, `Grant [ `Read ]) ; Xml.ace_to_xml (`Href url, `Grant [ `Write ]) ])

let deny_all = (Xml.dav_ns, "acl"), ([], [ Xml.ace_to_xml (`All, `Deny [ `All ]) ])
let grant_all = (Xml.dav_ns, "acl"), ([], [ Xml.ace_to_xml (`All, `Grant [ `All ]) ])

let initialise_fs fs config =
  let create_calendar fs name =
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
      (Xml.dav_ns, "supported-report-set"), ([], report_nodes) ;
      (Xml.caldav_ns, "supported-calendar-component-set"), ([], comps) ;
      (* (server_ns, "getctag"), ([], [ Xml.pcdata "hallo" ]) *)
      (* (Xml.dav_ns, "owner"), ([], [ Xml.pcdata "/principals/__uids__/10000000-0000-0000-0000-000000000001" ]) ; *)
      (* (Xml.dav_ns, "current-user-principal"), ([], [ Xml.pcdata "/principals/__uids__/10000000-0000-0000-0000-000000000001" ]) ; *)
    ] in
    let resourcetype = [ Xml.node ~ns:Xml.caldav_ns "calendar" [] ] in
    make_dir_if_not_present fs ~resourcetype ~props:(grant_test config :: props) name
  in
  let calendars_properties =
    let url = Uri.with_path config.host (config.calendars ^ "/__uids__/10000000-0000-0000-0000-000000000001/calendar") in
    [
    (Xml.caldav_ns, "calendar-home-set"),
    ([], [Xml.node "href" ~ns:Xml.dav_ns [Xml.pcdata (Uri.to_string url) ]])
  ] in
  make_dir_if_not_present fs ~props:calendars_properties (`Dir [config.calendars]) >>= fun _ ->
  make_dir_if_not_present fs (`Dir [config.calendars ; "users"]) >>= fun _ ->
  make_dir_if_not_present fs (`Dir [config.calendars ; "__uids__"]) >>= fun _ ->
  make_dir_if_not_present fs (`Dir [config.calendars ; "__uids__" ; "10000000-0000-0000-0000-000000000001"]) >>= fun _ ->
  create_calendar fs (`Dir [config.calendars ; "__uids__" ; "10000000-0000-0000-0000-000000000001" ; "calendar" ]) >>= fun _ ->
  make_dir_if_not_present fs (`Dir [config.calendars ; "__uids__" ; "10000000-0000-0000-0000-000000000001" ; "tasks"]) >>= fun _ ->
  Lwt.return_unit

let make_user ?(props = []) fs config name =
  make_dir_if_not_present fs (`Dir [ config.principals ]) >>= fun _ ->
  let resourcetype = [ Xml.node ~ns:Xml.dav_ns "principal" [] ] in
  let user_home = `Dir [ config.principals ; name ] in
  let url = Uri.with_path config.host (Fs.to_string (user_home :> file_or_dir)) in
  let props' =
    ((Xml.dav_ns, "principal-URL"),
     ([], [ Xml.node ~ns:Xml.dav_ns "href" [ Xml.pcdata (Uri.to_string url) ] ]))
    :: props
  in
  make_dir_if_not_present fs ~resourcetype ~props:props' user_home >>= fun _ ->
  Lwt.return_unit

let make_group fs config name members =
  let principal_path user = Fs.to_string (`Dir [ config.principals ; user ]) in
  let new_member_paths = List.map principal_path members in
  let new_member_urls =
    List.map
      (fun path -> Uri.to_string @@ Uri.with_path config.host path)
      new_member_paths
  in
  let group_props = [
    (Xml.dav_ns, "group-member-set"),
    ([], List.map (fun u -> Xml.dav_node "href" [ Xml.pcdata u ]) new_member_urls)
  ] in
  make_user ~props:group_props fs config name >>= fun () ->
  let group_node =
    Xml.dav_node "href"
      [ Xml.pcdata (Uri.to_string @@ Uri.with_path config.host (principal_path name)) ]
  in
  let group_key = (Xml.dav_ns, "group-membership") in
  Lwt_list.iter_p (fun path ->
      let f_or_d = (Fs.dir_from_string path :> file_or_dir) in
      Fs.get_property_map fs f_or_d >>= fun props ->
      let props' = match Properties.find group_key props with
        | None -> Properties.add group_key ([], [ group_node ]) props
        | Some (attrs, groups) -> Properties.add group_key (attrs, group_node :: groups) props
      in
      Fs.write_property_map fs f_or_d props' >>= fun _ ->
      Lwt.return_unit)
    new_member_paths

let init_users fs config =
  make_user fs config "root" >>= fun () ->
  make_user fs config "nobody" >>= fun () ->
  make_user fs config "test" >>= fun () ->
  make_group fs config "group" ["root" ; "test"]

let main () =
  (* listen on port 8080 *)
  let port = 8080
  and scheme = "http"
  and hostname = "127.0.0.1"
  in
  let host = Uri.make ~port ~scheme ~host:hostname () in
  let config = {
    principals = "principals" ;
    calendars = "calendars" ;
    host ;
    user_password = [
      ("test", hash_password "password") ;
      ("root", hash_password "toor") ;
      ("nobody", hash_password "1") ]
  } in
  (* create the file system *)
  FS_unix.connect "/tmp/calendar" >>= fun fs ->
  (* only for apple test suite *)
  initialise_fs fs config >>= fun () ->
  init_users fs config >>= fun () ->
  (* the route table *)
  let routes = [
    ("/" ^ config.principals, fun () -> new handler config fs) ;
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
