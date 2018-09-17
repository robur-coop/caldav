open Cohttp_lwt_unix
open Lwt.Infix
open Caldav.Webdav_config

module Fs = Caldav.Webdav_fs.Make(FS_unix)
module Xml = Caldav.Webdav_xml
module Dav = Caldav.Webdav_api.Make(Fs)
module Properties = Caldav.Properties
module Privileges = Caldav.Privileges
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
    (* restore last modified from properties as a workaround because the file system does not provide it *)
    let last_modified = match Properties.unsafe_find (Xml.dav_ns, "getlastmodified") m with
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
      (* instantiate the calendar name from the displayname property *)
      match Properties.unsafe_find (Xml.dav_ns, "displayname") props with
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
  Cstruct.to_string @@ Nocrypto.Base64.encode @@
  Nocrypto.Hash.SHA256.digest @@ Cstruct.of_string (server_secret ^ password)

let verify_auth_header fs config v =
  match Astring.String.cut ~sep:"Basic " v with
  | Some ("", b64) ->
    begin match Nocrypto.Base64.decode (Cstruct.of_string b64) with
      | None -> Lwt.return @@ Error "invalid base64 encoding"
      | Some data -> match Astring.String.cut ~sep:":" (Cstruct.to_string data) with
        | None -> Lwt.return @@ Error "invalid user:password encoding"
        | Some (user, password) ->
          let hashed = hash_password password in
          Fs.get_property_map fs (`Dir [config.principals ; user]) >|= fun props ->
          (* no user context yet *)
          match Properties.unsafe_find (Xml.robur_ns, "password") props with
          | Some (_, [ Xml.Pcdata stored ]) ->
            if String.equal hashed stored
            then Ok user
            else Error "wrong password"
          | _ -> Error "invalid user"
    end
  | _ -> Lwt.return @@ Error "bad header"

let calendar_to_collection data =
  if data = "" then Ok "" else
  match Xml.string_to_tree data with
  | Some (Xml.Node (ns, "mkcalendar", a, c)) when ns = Xml.caldav_ns -> Ok (Xml.tyxml_to_body (Xml.tree_to_tyxml (Xml.node ~ns:Xml.dav_ns ~a "mkcol" c)))
  | _ -> Error `Bad_request

let parent_is_calendar fs file =
  Fs.get_property_map fs (Fs.parent (file :> file_or_dir) :> file_or_dir) >|= fun map ->
  (* TODO access check missing *)
  match Properties.unsafe_find (Xml.dav_ns, "resourcetype") map with
  | None -> false
  | Some (_, trees) ->
     let calendar_node = function
     | Xml.Node (ns, "calendar", _, _) when ns = Xml.caldav_ns -> true
     | _ -> false in
     List.exists calendar_node trees

let properties_for_current_user fs config req_headers =
  let user =
    match Cohttp.Header.get req_headers "Authorization" with
    | None -> assert false
    | Some v -> v
  in
  let user_path = `Dir [ config.principals ; user ] in
  Fs.get_property_map fs user_path

let parent_acl fs config req_headers path =
  properties_for_current_user fs config req_headers >>= fun auth_user_props ->
  Fs.get_property_map fs (Fs.parent (path :> file_or_dir) :> file_or_dir) >|= fun parent_resource_props ->
  if not (Privileges.is_met ~requirement:`Read_acl @@
          Properties.privileges ~auth_user_props parent_resource_props)
  then Error `Forbidden
  (* we check above that Read_acl is allowed, TODO express with find_many *)
  else match Properties.unsafe_find (Xml.dav_ns, "acl") parent_resource_props with
    | None -> Ok []
    | Some (_, aces) ->
      let aces' = List.map Xml.xml_to_ace aces in
      Ok (List.fold_left (fun acc -> function Ok ace -> ace :: acc | _ -> acc) [] aces')

(** A resource for querying an individual item in the database by id via GET,
    modifying an item via PUT, and deleting an item via DELETE. *)
class handler config fs = object(self)
  inherit [Cohttp_lwt.Body.t] Wm.resource

  method private write_component rd =
    Cohttp_lwt.Body.to_string rd.Wm.Rd.req_body >>= fun body ->
    Printf.printf "write_component: %s\n%!" body ;
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
      parent_acl fs config rd.Wm.Rd.req_headers file >>= function
      | Error e -> Wm.respond (to_status `Forbidden) rd
      | Ok acl ->
        Dav.write_component fs ~path:file ~etag ~content_type acl (Ptime_clock.now ()) ics >>= function
        | Error e -> Wm.respond (to_status e) rd
        | Ok _ ->
          Printf.printf "wrote component %s\n%!" path ;
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
      (* this property only needs read, which has been checked on the resource already *)
      let ct = match Properties.unsafe_find (Xml.dav_ns, "getcontenttype") props with
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
      "text/xml", self#write_component ;
      "text/calendar", self#write_component
    ] rd

  method is_authorized rd =
    (* TODO implement digest authentication! *)
    (match Cohttp.Header.get rd.Wm.Rd.req_headers "Authorization" with
     | None -> Lwt.return (`Basic "calendar", rd)
     | Some v ->
       verify_auth_header fs config v >|= function
       | Ok user ->
         let replace_header h =
           Cohttp.Header.replace h "Authorization" user
         in
         let rd' = Wm.Rd.with_req_headers replace_header rd in
         `Authorized, rd'
       | Error msg ->
         Printf.printf "ivalid authorization: %s\n" msg ;
         `Basic "invalid authorization", rd) >>= fun (res, rd') ->
    Wm.continue res rd'

  method forbidden rd =
    let path = self#path rd in
    properties_for_current_user fs config rd.Wm.Rd.req_headers >>= fun auth_user_props ->
    Dav.access_granted_for_acl fs path rd.Wm.Rd.meth auth_user_props >>= fun granted ->
    Wm.continue (not granted) rd

  method private process_propfind rd auth_user_props path =
    let depth = Cohttp.Header.get rd.Wm.Rd.req_headers "Depth" in
    Cohttp_lwt.Body.to_string rd.Wm.Rd.req_body >>= fun body ->
    Printf.printf "PROPFIND: %s\n%!" body;
    match Xml.string_to_tree body with
    | None -> Wm.respond (to_status `Bad_request) rd
    | Some tree ->
      Dav.propfind fs ~host:config.host ~path tree ~auth_user_props ~depth >>= function
      | Ok b -> Wm.continue `Multistatus { rd with Wm.Rd.resp_body = `String (Xml.tree_to_string b) }
      | Error `Property_not_found -> Wm.continue `Property_not_found rd
      | Error (`Forbidden b) -> Wm.respond ~body:(`String (Xml.tree_to_string b)) (to_status `Forbidden) rd
      | Error `Bad_request -> Wm.respond (to_status `Bad_request) rd

  method private process_proppatch rd auth_user_props path =
    Cohttp_lwt.Body.to_string rd.Wm.Rd.req_body >>= fun body ->
    Printf.printf "PROPPATCH: %s\n%!" body;
    match Xml.string_to_tree body with
    | None -> Wm.respond (to_status `Bad_request) rd
    | Some tree ->
      Dav.proppatch fs ~host:config.host ~path tree ~auth_user_props >>= function
      | Ok (_, b) -> Wm.continue `Multistatus { rd with Wm.Rd.resp_body = `String (Xml.tree_to_string b) }
      | Error `Bad_request -> Wm.respond (to_status `Bad_request) rd

  method process_property rd =
    let replace_header h = Cohttp.Header.replace h "Content-Type" "application/xml" in
    let rd' = Wm.Rd.with_resp_headers replace_header rd in
    Fs.from_string fs (self#path rd) >>= function
    | Error _ -> Wm.respond (to_status `Bad_request) rd
    | Ok f_or_d ->
      properties_for_current_user fs config rd.Wm.Rd.req_headers >>= fun auth_user_props ->
      match rd'.Wm.Rd.meth with
      | `Other "PROPFIND" -> self#process_propfind rd' auth_user_props f_or_d
      | `Other "PROPPATCH" -> self#process_proppatch rd' auth_user_props f_or_d
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
        properties_for_current_user fs config rd.Wm.Rd.req_headers >>= fun auth_user_props ->
        Dav.report fs ~host:config.host ~path:f_or_d tree ~auth_user_props >>= function
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
    let is_calendar, body' = match rd.Wm.Rd.meth with
    | `Other "MKCALENDAR" -> true, calendar_to_collection body
    | `Other "MKCOL" -> false, Ok body
    | _ -> assert false in
    match body' with
    | Error _ -> Wm.continue `Conflict rd
    | Ok body'' ->
      match Xml.string_to_tree body'' with
      | None when body'' <> "" -> Wm.continue `Conflict rd
      | tree ->
        let path = Fs.dir_from_string (self#path rd) in
        parent_is_calendar fs path >>= fun parent_is_calendar ->
        if is_calendar && parent_is_calendar
        then Wm.continue `Conflict rd
        else
          parent_acl fs config rd.Wm.Rd.req_headers path >>= function
          | Error e -> Wm.continue e rd
          | Ok acl ->
            Dav.mkcol fs ~path acl (Ptime_clock.now ()) ~is_calendar tree >>= function
            | Ok _ -> Wm.continue `Created rd
            | Error (`Forbidden t) -> Wm.continue `Forbidden { rd with Wm.Rd.resp_body = `String (Xml.tree_to_string t) }
            | Error `Conflict -> Wm.continue `Conflict rd
            | Error `Bad_request -> Wm.continue `Conflict rd

  method delete_resource rd =
    Fs.from_string fs (self#path rd) >>= function
    | Error _ ->
      Wm.continue false rd
    | Ok f_or_d ->
      Dav.delete fs ~path:f_or_d (Ptime_clock.now ()) >>= fun _ ->
      Wm.continue true rd

  method last_modified rd =
    Fs.from_string fs (self#path rd) >>= function
    | Error _ -> Wm.continue None rd
    | Ok f_or_d ->
      Fs.get_property_map fs f_or_d >|= (fun map ->
          (* no special property, already checked for resource *)
        match Properties.unsafe_find (Xml.dav_ns, "getlastmodified") map with
          | Some (_, [ Xml.Pcdata lm]) -> Some lm
          | _ -> None) >>= fun res ->
      Wm.continue res rd

  method generate_etag rd =
    Fs.from_string fs (self#path rd) >>= function
    | Error _ -> Wm.continue None rd
    | Ok f_or_d ->
      Fs.get_property_map fs f_or_d >>= fun map ->
      let rd' =
        (* no special property, already checked for resource *)
        match Properties.unsafe_find (Xml.dav_ns, "getlastmodified") map with
        | Some (_, [ Xml.Pcdata lm ]) ->
          let add_headers h = Cohttp.Header.add_list h [ ("Last-Modified", lm) ] in
          Wm.Rd.with_resp_headers add_headers rd
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
    let rd' = Wm.Rd.redirect (Uri.to_string @@ Uri.with_path config.host config.calendars) rd in
    Wm.respond 301 rd'
end

let server_ns = "http://calendarserver.org/ns/"
let carddav_ns = "urn:ietf:params:xml:ns:carddav"

let make_dir fs acl ?(resourcetype = []) ?(props=[]) dir =
  let propmap =
    Properties.create_dir ~initial_props:props ~resourcetype acl (Ptime_clock.now ()) (Fs.basename (dir :> file_or_dir))
  in
  Fs.mkdir fs dir propmap

let make_dir_if_not_present fs acl ?resourcetype ?props dir =
  Fs.dir_exists fs dir >>= fun exists ->
  if not exists then
    make_dir fs acl ?resourcetype ?props dir >|= fun _ -> ()
  else
    Lwt.return_unit

let grant_test config =
  let url = Uri.with_path config.host (Fs.to_string (`Dir [ config.principals ; "test" ])) in
  (Xml.dav_ns, "acl"), ([], [ Xml.ace_to_xml (`Href url, `Grant [ `Read ]) ; Xml.ace_to_xml (`Href url, `Grant [ `Write ]) ])

let deny_all = (Xml.dav_ns, "acl"), ([], [ Xml.ace_to_xml (`All, `Deny [ `All ]) ])
let grant_all = (Xml.dav_ns, "acl"), ([], [ Xml.ace_to_xml (`All, `Grant [ `All ]) ])

let create_calendar fs acl name =
  let props =
    let reports = [
      Xml.caldav_ns, "calendar-query" ;
      Xml.caldav_ns, "calendar-multiget" ;
   (* Xml.dav_ns, "acl-principal-prop-set" ;
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
          Xml.dav_node "supported-report"
            [ Xml.dav_node "report" [ Xml.node ~ns s [] ] ])
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
    ] in
  let resourcetype = [ Xml.node ~ns:Xml.caldav_ns "calendar" [] ] in
  make_dir_if_not_present fs acl ~resourcetype ~props name

let initialise_fs_for_apple_testsuite fs config =
  let calendars_properties =
    let url =
      Uri.with_path config.host
        (config.calendars ^ "/__uids__/10000000-0000-0000-0000-000000000001/calendar")
    in
    [
    (Xml.caldav_ns, "calendar-home-set"),
    ([], [Xml.node "href" ~ns:Xml.dav_ns [Xml.pcdata (Uri.to_string url) ]])
  ] in
  let acl = config.default_acl in
  make_dir_if_not_present fs acl ~props:calendars_properties (`Dir [config.calendars]) >>= fun _ ->
  make_dir_if_not_present fs acl (`Dir [config.calendars ; "users"]) >>= fun _ ->
  make_dir_if_not_present fs acl (`Dir [config.calendars ; "__uids__"]) >>= fun _ ->
  make_dir_if_not_present fs acl (`Dir [config.calendars ; "__uids__" ; "10000000-0000-0000-0000-000000000001"]) >>= fun _ ->
  create_calendar fs acl (`Dir [config.calendars ; "__uids__" ; "10000000-0000-0000-0000-000000000001" ; "calendar" ]) >>= fun _ ->
  make_dir_if_not_present fs acl (`Dir [config.calendars ; "__uids__" ; "10000000-0000-0000-0000-000000000001" ; "tasks"]) >>= fun _ ->
  Lwt.return_unit

let initialise_fs fs config =
  make_dir_if_not_present fs config.default_acl (`Dir [config.principals]) >>= fun _ ->
  make_dir_if_not_present fs config.default_acl (`Dir [config.calendars]) >>= fun _ ->
  Lwt.return_unit

(* use config.user_password for initial structure
  /principals/ -- config.principals WebDAV
  /principals/user/ -- WebDAV -- principal-URL for user user, prop.xml <- contains calendar-home-set
  /calendars/  -- config.calendars WebDAV
  /calendars/user/ -- WebDAV
  /calendars/user/calendar/ -- CalDAV - default calendar
  /calendars/user/my_other_calendar/ -- CalDAV

PROPFIND /calendars -- eingeloggt als user -- <principal-URL>
--> <principal-URL>http://.../principals/user/

PROPFIND /principals/user -- <calendar-home-set>
--> <calendar-home-set><href>http://.../calendars/user/</calendar-home-set>
 *)

let make_user ?(props = []) fs config name password =
  let resourcetype = [ Xml.node ~ns:Xml.dav_ns "principal" [] ] in
  let get_url dir = Uri.with_path config.host (Fs.to_string (dir :> file_or_dir)) in
  let principal_dir = `Dir [ config.principals ; name ] in
  let principal_url = get_url principal_dir in
  let home_set_dir = `Dir [ config.calendars ; name ] in
  let home_set_url = get_url home_set_dir in
  let props' =
    ((Xml.dav_ns, "principal-URL"),
     ([], [ Xml.node ~ns:Xml.dav_ns "href" [ Xml.pcdata @@ Uri.to_string principal_url ] ]))
    :: ((Xml.caldav_ns, "calendar-home-set"),
        ([], [Xml.dav_node "href" [Xml.pcdata @@ Uri.to_string home_set_url ]]))
    :: ((Xml.robur_ns, "password"),
        ([], [Xml.pcdata @@ hash_password password]))
    :: props
  in
  let acl = [ (`Href principal_url, `Grant [ `All ]) ; (`All, `Grant [ `Read ]) ] in
  (* TODO should root have access to principals/user? *)
  make_dir_if_not_present fs acl ~resourcetype ~props:props' principal_dir >>= fun _ ->
  make_dir_if_not_present fs acl home_set_dir >>= fun _ ->
  create_calendar fs acl (`Dir [config.calendars ; name ; "calendar"]) >>= fun _ ->
  Lwt.return_unit

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
    let uri = rd.Wm.Rd.uri in
    match Uri.get_query_param uri "user", Uri.get_query_param uri "password" with
    | None, _ | _, None -> Wm.respond (to_status `Bad_request) rd
    | Some name, Some pass ->
      make_user fs config name pass >>= fun () ->
      Wm.continue true rd

  method content_types_provided rd =
    Wm.continue [ ("*/*", Wm.continue `Empty) ] rd

  method content_types_accepted rd =
    Wm.continue [
      ("application/octet-stream", self#create_user)
    ] rd
end

let make_group fs config name password members =
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
  make_user ~props:group_props fs config name password >>= fun () ->
  let group_node =
    Xml.dav_node "href"
      [ Xml.pcdata (Uri.to_string @@ Uri.with_path config.host (principal_path name)) ]
  in
  let group_key = (Xml.dav_ns, "group-membership") in
  Lwt_list.iter_p (fun path ->
      let f_or_d = (Fs.dir_from_string path :> file_or_dir) in
      Fs.get_property_map fs f_or_d >>= fun props ->
      (* TODO should use find_many *)
      let props' = match Properties.unsafe_find group_key props with
        | None -> Properties.unsafe_add group_key ([], [ group_node ]) props
        | Some (attrs, groups) -> Properties.unsafe_add group_key (attrs, group_node :: groups) props
      in
      Fs.write_property_map fs f_or_d props' >>= fun _ ->
      Lwt.return_unit)
    new_member_paths

let init_users fs config user_password =
  Lwt_list.iter_p (fun (u, p) -> make_user fs config u p) user_password >>= fun () ->
  make_group fs config "group" "group-password" ["root" ; "test"]

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
  (* initialise_fs_for_apple_testsuite fs config >>= fun () -> *)
  initialise_fs fs config >>= fun () ->
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
