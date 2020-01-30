module Xml = Webdav_xml
open Webdav_config
type tree = Xml.tree
type content_type = string

module type S =
sig
  type state

  val mkcol : state -> config -> path:string -> user:string -> Cohttp.Code.meth -> Ptime.t -> data:string ->
    (unit, [ `Bad_request | `Conflict | `Forbidden of string ]) result Lwt.t

  val propfind : state -> config -> path:string -> user:string -> depth:string option -> data:string ->
    (string, [> `Bad_request | `Forbidden of string | `Property_not_found ]) result Lwt.t

  val proppatch : state -> config -> path:string -> user:string -> data:string ->
    (string, [> `Bad_request ]) result Lwt.t

  val report : state -> config -> path:string -> user:string -> data:string ->
    (string, [> `Bad_request ]) result Lwt.t

  val write_component : state -> config -> path:string -> user:string -> Ptime.t -> content_type:content_type -> data:string ->
    (string, [> `Bad_request | `Conflict | `Forbidden | `Internal_server_error ]) result Lwt.t

  val delete : state -> path:string -> Ptime.t -> bool Lwt.t

  val read : state -> path:string -> is_mozilla:bool -> (string * content_type, [> `Not_found ]) result Lwt.t

  val access_granted_for_acl : state -> config -> Cohttp.Code.meth -> path:string -> user:string -> bool Lwt.t

  val last_modified : state -> path:string -> string option Lwt.t

  val compute_etag : state -> path:string -> string option Lwt.t

  val verify_auth_header : state -> Webdav_config.config -> string -> (string, [> `Msg of string | `Unknown_user of string * string ]) result Lwt.t

  val make_user : ?props:(Webdav_xml.fqname * Properties.property) list -> state -> Ptime.t -> config -> name:string -> password:string -> salt:Cstruct.t ->
    Uri.t Lwt.t
  val change_user_password : state -> config -> name:string -> password:string -> salt:Cstruct.t -> (unit, [> `Internal_server_error ]) result Lwt.t
  val delete_user : state -> config -> string -> (unit, [> `Internal_server_error | `Not_found ]) result Lwt.t

  val make_group : state -> Ptime.t -> config -> string -> string list -> Uri.t Lwt.t
  val enroll : state -> config -> member:string -> group:string -> unit Lwt.t
  val resign : state -> config -> member:string -> group:string -> unit Lwt.t
  val replace_group_members : state -> config -> string -> string list -> unit Lwt.t
  val delete_group : state -> config -> string -> (unit, [> `Internal_server_error | `Not_found ]) result Lwt.t

  val initialize_fs : state -> Ptime.t -> config -> unit Lwt.t
  val initialize_fs_for_apple_testsuite : state -> Ptime.t -> config -> unit Lwt.t

  val generate_salt : unit -> Cstruct.t

  val connect : state -> config -> string option -> state Lwt.t

end

let src = Logs.Src.create "webdav.robur.io" ~doc:"webdav api logs"
module Log = (val Logs.src_log src : Logs.LOG)

module Make(R : Mirage_random.S)(Clock : Mirage_clock.PCLOCK)(Fs: Webdav_fs.S) = struct
  open Lwt.Infix

  type state = Fs.t

  let is_calendar fs file =
    Fs.get_property_map fs file >|= fun map ->
    (* unsafe is ok, used internally for decision *)
    match Properties.unsafe_find (Xml.dav_ns, "resourcetype") map with
    | None -> false
    | Some (_, trees) ->
       let calendar_node = function
       | Xml.Node (ns, "calendar", _, _) when ns = Xml.caldav_ns -> true
       | _ -> false in
       List.exists calendar_node trees

  let unsafe_read_acl fs path =
    Fs.get_property_map fs path >|= fun resource_props ->
    match Properties.unsafe_find (Xml.dav_ns, "acl") resource_props with
    | None ->
      Log.warn (fun m -> m "unsafe_read_acl: encountered empty ACL for %s"
                   (Fs.to_string path)) ;
      []
    | Some (_, aces) ->
      let aces' = List.map Xml.xml_to_ace aces in
      List.fold_left (fun acc -> function Ok ace -> ace :: acc | _ -> acc) [] aces'

  let properties_for_current_user fs config user =
    let user_path = `Dir [ config.principals ; user ] in
    Fs.get_property_map fs user_path

  let acl fs config user path =
    properties_for_current_user fs config user >>= fun auth_user_props ->
    Fs.get_property_map fs path >|= fun resource_props ->
    match Properties.find ~auth_user_props ~resource_props (Xml.dav_ns, "acl") with
    | Error `Forbidden as e -> e
    | Error `Not_found -> Ok []
    | Ok (_, aces) ->
      let aces' = List.map Xml.xml_to_ace aces in
      Ok (List.fold_left (fun acc -> function Ok ace -> ace :: acc | _ -> acc) [] aces')

  let privilege_met fs requirement ~auth_user_props resource_props =
    let privileges = Properties.privileges ~auth_user_props resource_props in
    (match Properties.inherited_acls ~auth_user_props resource_props with
    | [ url ] -> 
      Log.debug (fun m -> m "Inherited %s" (Uri.to_string url)) ;
      (Fs.from_string fs (Uri.to_string url) >>= function
      | Error e -> 
      Log.warn (fun m -> m "privilege_met: Could not convert to file %a" Fs.pp_error e) ;
      Lwt.return []
      | Ok inherited -> 
      Fs.get_property_map fs inherited >|= fun inherited_props ->
      Properties.privileges ~auth_user_props inherited_props)
    | urls -> 
      Log.debug (fun m -> m "Inherited %s" (String.concat "\n" @@ List.map Uri.to_string urls)) ;
     Lwt.return []) >|= fun inherited_privileges ->
    let privileges = privileges @ inherited_privileges in 
    Log.debug (fun m -> m "Privileges size: %d " (List.length privileges)) ;
    if not (Privileges.is_met ~requirement privileges) then `Forbidden else `Ok
    
  let parse_calendar ~path data =
    match Icalendar.parse data with
    | Error e ->
      Log.err (fun m -> m "%s while parsing calendar" e) ;
      Error `Bad_request
    | Ok cal ->
      let ics = Icalendar.to_ics cal in
      let file = Fs.file_from_string path in
      Ok (ics, file)

  (* out: ( name * typ * last_modified ) list - non-recursive *)
  let list_dir fs (`Dir dir) =
    let list_file f_or_d =
      begin Fs.last_modified fs f_or_d >|= function
      | Error e -> Ptime.epoch
      | Ok ts -> ts 
      end >|= fun ts ->
      let is_dir = match f_or_d with | `File _ -> false | `Dir _ -> true in
      (Fs.to_string f_or_d, is_dir, Ptime.to_rfc3339 ts)
    in
    Fs.listdir fs (`Dir dir) >>= function
    | Error e -> assert false
    | Ok files -> Lwt_list.map_p list_file files

  let directory_as_html fs (`Dir dir) =
    list_dir fs (`Dir dir) >|= fun files ->
    let print_file (file, is_dir, last_modified) =
      Printf.sprintf "<tr><td><a href=\"%s\">%s</a></td><td>%s</td><td>%s</td></tr>"
        file file (if is_dir then "directory" else "text/calendar") last_modified in
    let data = String.concat "\n" (List.map print_file files) in
    ("text/html", data)

  (* When a resource is modified or deleted, its parent's getlastmodified property needs to be updated. *)
  let update_parent_after_child_write fs f_or_d last_modified =
    let last_modified = Ptime.to_rfc3339 last_modified in
    let (`Dir parent) = Fs.parent f_or_d in
    Fs.get_property_map fs (`Dir parent) >>= fun map ->
    let map' = Properties.unsafe_add (Xml.dav_ns, "getlastmodified") ([], [ Xml.Pcdata last_modified ]) map in
    Fs.write_property_map fs (`Dir parent) map' >|= function
    | Error e -> assert false
    | Ok () -> ()

  let write_if_parent_exists fs config (file : Webdav_fs.file) timestamp content_type user ics =
    let file' = (file :> Webdav_fs.file_or_dir) in
    let parent = Fs.parent file' in
    let parent' = (parent :> Webdav_fs.file_or_dir) in
    Fs.dir_exists fs parent >>= function
    | false ->
      Log.err (fun m -> m "parent directory of %s does not exist" (Fs.to_string file')) ;
      Lwt.return (Error `Conflict)
    | true ->
      is_calendar fs parent' >>= function
      | false ->
        Log.err (fun m -> m "is_calendar was false when trying to write %s" (Fs.to_string file')) ;
        Lwt.return @@ Error `Bad_request
      | true ->
        let acl = [(`All, `Inherited (Uri.of_string (Fs.to_string parent')))] in
        let props = Properties.create ~content_type acl timestamp (String.length ics) (Fs.to_string file') in
        Fs.write fs file ics props >>= function
        (* TODO map error to internal server error and log it, as function *)
        | Error e -> Lwt.return @@ Error `Internal_server_error
        | Ok () ->
          update_parent_after_child_write fs file' timestamp >>= fun () ->
          Fs.etag fs file' >|= function
          | Error e -> Error `Internal_server_error
          | Ok etag -> Ok etag

  let write_component fs config ~path ~user timestamp ~content_type ~data =
    match parse_calendar ~path data with
    | Error e -> Lwt.return @@ Error e
    | Ok (ics, file) -> write_if_parent_exists fs config file timestamp content_type user ics

  let directory_as_ics fs (`Dir dir) =
    let calendar_components = function
      | `Dir d ->
        Log.info (fun m -> m "empty calendar components of directory %s" (Fs.to_string (`Dir d))) ;
        Lwt.return []
        (* assert false (* CalDAV forbids nested calendars *) *)
      | `File f ->
        Fs.read fs (`File f) >|= function
        | Error e -> Log.err (fun m -> m "error %a while reading file" Fs.pp_error e) ; []
        | Ok (data, _props) ->
          match Icalendar.parse data with
          | Error e -> Log.err (fun m -> m "error %s while parsing ics" e ); []
          | Ok calendar -> snd calendar
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
      let data = Icalendar.to_ics (calprops, List.flatten components) in
      ("text/calendar", data)

  let read fs ~path ~is_mozilla =
    Fs.from_string fs path >>= function
    | Error _ -> Lwt.return @@ Error `Not_found
    | Ok (`Dir dir) ->
      (if is_mozilla then
        directory_as_html fs (`Dir dir)
      else
        (* TODO: check wheter CalDAV:calendar property is set as resourcetype!
           otherwise: standard WebDAV directory listing *)
        directory_as_ics fs (`Dir dir)) >|= fun res ->
      Ok res
    | Ok (`File f) ->
      Fs.read fs (`File f) >|= function
      | Error _ -> Error `Not_found
      | Ok (data, props) ->
      (* this property only needs read, which has been checked on the resource already *)
      let ct = match Properties.unsafe_find (Xml.dav_ns, "getcontenttype") props with
        | Some (_, [ Xml.Pcdata ct ]) -> ct
        | _ -> "text/calendar" in
      Ok (ct, data)

  let delete fs ~path now =
    Fs.from_string fs path >>= function
    | Error _ -> Lwt.return false
    | Ok f_or_d ->
      Fs.destroy fs f_or_d >>= fun res ->
      update_parent_after_child_write fs f_or_d now >|= fun () ->
      true

  let statuscode_to_string res =
    Format.sprintf "%s %s"
      (Cohttp.Code.string_of_version `HTTP_1_1)
      (Cohttp.Code.string_of_status res)

  let propstat_node (code, props) =
    Xml.dav_node "propstat" [
      Xml.dav_node "prop" props ;
      Xml.dav_node "status" [ Xml.pcdata (statuscode_to_string code) ] ]

  let parse_depth = function
    | None -> Ok `Infinity
    | Some "0" -> Ok `Zero
    | Some "1" -> Ok `One
    | Some "infinity" -> Ok `Infinity
    | _ -> Error `Bad_request

  let property_selector fs propfind_request auth_user_props f_or_d =
    Fs.get_property_map fs f_or_d >>= fun resource_props ->
    privilege_met fs `Read ~auth_user_props resource_props >|= function
    | `Forbidden -> `Forbidden
    | `Ok -> 
      if resource_props = Properties.empty
      then `Not_found
      else
        (* results for props, grouped by code *)
        let propstats = match propfind_request with
          | `Propname -> [`OK, Properties.names resource_props]
          | `All_prop includes -> [`OK, Properties.all resource_props] (* TODO: finish this *)
          | `Props ps -> Properties.find_many ~auth_user_props ~resource_props ps
        in
        let ps = List.map propstat_node propstats in
        let selected_properties =
          Xml.dav_node "response"
            (Xml.dav_node "href" [ Xml.Pcdata (Fs.to_string f_or_d) ] :: ps)
        in
        `Single_response selected_properties

  let multistatus nodes = Xml.dav_node "multistatus" nodes

  let error_xml element = Xml.dav_node "error" [ Xml.dav_node element [] ]

  let propfind fs f_or_d req auth_user_props depth =
    let process_files fs dir req els =
      Lwt_list.map_s (property_selector fs req auth_user_props) (dir :: els) >|= fun answers ->
      (* answers : [ `Not_found | `Single_response of Tyxml.Xml.node ] list *)
      let nodes = List.fold_left (fun acc element ->
          match element with
          | `Not_found | `Forbidden -> acc
          | `Single_response node -> node :: acc) [] answers
      in
      Ok (multistatus nodes)
    in
    match depth, f_or_d with
    | `Infinity, _ ->
      let data = error_xml "propfind-finite-depth" in
      Lwt.return (Error (`Forbidden (Xml.tree_to_string data)))
    | `Zero, _
    | _, `File _ ->
      begin
        property_selector fs req auth_user_props f_or_d >|= function
        | `Not_found | `Forbidden -> Error `Property_not_found
        | `Single_response t -> Ok (multistatus [t])
      end
    | `One, `Dir data ->
      Fs.listdir fs (`Dir data) >>= function
      | Error _ -> assert false
      | Ok els -> process_files fs (`Dir data) req els

  let build_req_tree fs config path user data =
    Fs.from_string fs path >>= function
    | Error _ -> Lwt.return @@ Error `Bad_request
    | Ok f_or_d ->
      properties_for_current_user fs config user >|= fun auth_user_props ->
      match Xml.string_to_tree data with
      | None -> Error `Bad_request
      | Some req_tree -> Ok (f_or_d, auth_user_props, req_tree)

  let propfind fs config ~path ~user ~depth ~data =
    build_req_tree fs config path user data >>= function
    | Error e -> Lwt.return @@ Error e
    | Ok (f_or_d, auth_user_props, req_tree) -> match parse_depth depth with
      | Error `Bad_request -> Lwt.return (Error `Bad_request)
      | Ok depth -> match Xml.parse_propfind_xml req_tree with
        | Error _ -> Lwt.return (Error `Property_not_found)
        | Ok req -> propfind fs f_or_d req auth_user_props depth >|= function
          | Error e -> Error e
          | Ok resp_tree -> Ok (Xml.tree_to_string resp_tree)

  let update_properties fs f_or_d updates =
    Fs.get_property_map fs f_or_d >>= fun map ->
    let map', xs = Properties.patch map updates in
    let propstats = List.map propstat_node xs in
    (match map' with
     | None -> Lwt.return (Ok ())
     | Some m -> Fs.write_property_map fs f_or_d m) >|= function
    | Error e -> Error e
    | Ok () -> Ok propstats

  let proppatch fs config ~path ~user ~data =
    build_req_tree fs config path user data >>= function
    | Error e -> Lwt.return @@ Error e
    | Ok (f_or_d, auth_user_props, req_tree) -> match Xml.parse_propupdate_xml req_tree with
      | Error _ -> Lwt.return @@ Error `Bad_request
      | Ok updates -> update_properties fs f_or_d updates >|= function
        | Error _      -> Error `Bad_request
        | Ok propstats ->
          let nodes =
            Xml.dav_node "response"
              (Xml.dav_node "href" [ Xml.Pcdata (Fs.to_string f_or_d) ] :: propstats)
          in
          let resp = multistatus [ nodes ] in
          Ok (Xml.tree_to_string resp)

  let mkcol_tree_to_proppatch = function
    | None -> Ok []
    | Some data' -> match Xml.parse_mkcol_xml data' with
      | Error _ -> Error `Bad_request
      | Ok set_props -> Ok set_props

  let create_collection_dir fs acl set_props now resourcetype dir =
    let col_props = Properties.create_dir ~resourcetype acl now (Fs.to_string (dir :> Webdav_fs.file_or_dir)) in
    match Properties.patch ~is_mkcol:true col_props set_props with
    | None, errs ->
      let propstats = List.map propstat_node errs in
      let xml = Xml.dav_node "mkcol-response" propstats in
      let res = Xml.tree_to_string xml in
      Lwt.return @@ Error (`Forbidden res)
    | Some map, _ ->
      Fs.mkdir fs dir map >|= function
      | Error _ -> Error `Conflict
      | Ok () -> Ok ()

  (* assumption: path is a relative path! *)
  let mkcol fs config ~path ~user http_verb now ~data =
    let dir = Fs.dir_from_string path in
    let parent = Fs.parent (dir :> Webdav_fs.file_or_dir) in
    let parent' = (parent :> Webdav_fs.file_or_dir) in
    Fs.dir_exists fs parent >>= function
    | false -> Lwt.return @@ Error `Conflict
    | true ->
      let resource_is_calendar, resourcetype = match http_verb with
      | `Other "MKCALENDAR" -> true, [Xml.node ~ns:Xml.caldav_ns "calendar" []]
      | `Other "MKCOL" -> false, []
      | _ -> assert false in
      match Xml.string_to_tree data with
      | None when data <> "" -> Lwt.return @@ Error `Conflict
      | mkcol_tree -> match mkcol_tree_to_proppatch mkcol_tree with
        | Error e -> Lwt.return @@ Error e
        | Ok set_props ->
          is_calendar fs parent' >>= fun parent_is_calendar ->
          if resource_is_calendar && parent_is_calendar
          then Lwt.return @@ Error `Conflict
          else 
            let acl = [ ( `Href (Uri.of_string (Fs.to_string (`Dir [config.principals ; user]))), `Grant [`All])] in
            create_collection_dir fs acl set_props now resourcetype dir

  let check_in_bounds p s e = true
  let apply_to_params pfs p = true
  let text_matches s c n p = true

  let apply_to_props props =
    let key p = Icalendar.Writer.cal_prop_to_ics_key p in
    function
    | (name, `Is_defined) ->
      List.exists (String.equal name) (List.map key props)
    | (name, `Is_not_defined) ->
      not (List.exists (String.equal name) (List.map key props))
    | (name, `Range ((s, e), pfs)) ->
      let property = List.find_opt (fun p -> String.equal name (key p)) props in
      (match property with
      | None -> false
      | Some p -> check_in_bounds p s e && apply_to_params pfs p)
    | (name, `Text ((substring, collate, negate), pfs)) ->
      let property = List.find_opt (fun p -> String.equal name (key p)) props in
      (match property with
      | None -> false
      | Some p -> text_matches substring collate negate p && apply_to_params pfs p)

  let get_time_properties props =
    let dtstart = match List.find_opt (function `Dtstart _ -> true | _ -> false) props with
    | None -> None
    | Some (`Dtstart (_, startdate)) -> Some startdate
    | _ -> assert false
    in
    let duration = match List.find_opt (function `Duration _ -> true | _ -> false) props with
    | None -> None
    | Some (`Duration (_, s)) -> Some s
    | _ -> assert false
    in (dtstart, duration)

  let add_span ts span = match Ptime.add_span ts span with
    | None -> assert false
    | Some ts' -> ts'

  (*
        +---------------------------------------------------------------+
        | VEVENT has the DTEND property?                                |
        |   +-----------------------------------------------------------+
        |   | VEVENT has the DURATION property?                         |
        |   |   +-------------------------------------------------------+
        |   |   | DURATION property value is greater than 0 seconds?    |
        |   |   |   +---------------------------------------------------+
        |   |   |   | DTSTART property is a DATE-TIME value?            |
        |   |   |   |   +-----------------------------------------------+
        |   |   |   |   | Condition to evaluate                         |
        +---+---+---+---+-----------------------------------------------+
        | Y | N | N | * | (start <  DTEND AND end > DTSTART)            |
        +---+---+---+---+-----------------------------------------------+
        | N | Y | Y | * | (start <  DTSTART+DURATION AND end > DTSTART) |
        |   |   +---+---+-----------------------------------------------+
        |   |   | N | * | (start <= DTSTART AND end > DTSTART)          |
        +---+---+---+---+-----------------------------------------------+
        | N | N | N | Y | (start <= DTSTART AND end > DTSTART)          |
        +---+---+---+---+-----------------------------------------------+
        | N | N | N | N | (start <  DTSTART+P1D AND end > DTSTART)      |
        +---+---+---+---+-----------------------------------------------+
  *)

  let date_or_datetime_to_utc_ptime timezones = function
    | `Datetime (`Utc dtstart) -> dtstart, true
    | `Datetime (`Local dtstart) -> dtstart, true
    | `Datetime (`With_tzid (dtstart, tzid)) ->
       (match Icalendar.normalize_timezone dtstart tzid timezones with
       | None -> dtstart, true
       | Some ts' -> ts', true)
    | `Date start -> match Ptime.of_date_time (start, ((0, 0, 0), 0)) with
      | None -> assert false
      | Some dtstart -> dtstart, false

  (* start and end_ mark the range,
     dtstart and dtend and duration mark the component, e.g. event *)
  let span_in_timerange range dtstart dtend duration dtstart_is_datetime =
    let (start, end_) = range in
    let duration_gt_0 = match duration with
      | None -> false
      | Some d -> Ptime.Span.compare Ptime.Span.zero d = -1
    in
    let (<) a b = Ptime.is_earlier a ~than:b
    and (>) a b = Ptime.is_later a ~than:b
    in
    let (<=) a b = a < b || Ptime.equal a b
    and (+) a b = add_span a b
    and p1d = Ptime.Span.of_int_s (24 * 60 * 60)
    in
    match dtend, duration, duration_gt_0, dtstart_is_datetime with
    | Some dtend, None, false, _    -> start < dtend && end_ > dtstart
    | None, Some duration, true, _  -> start < (dtstart + duration) && end_ > dtstart
    | None, Some duration, false, _ -> start <= dtstart && end_ > dtstart
    | None, None, false, true       -> start <= dtstart && end_ > dtstart
    | None, None, false, false      -> start < (dtstart + p1d) && end_ > dtstart
    | _                             -> assert false (* duration_gt_0 is dependent on duration *)

  let real_event_in_timerange timezones event range =
    let dtstart, dtstart_is_datetime = date_or_datetime_to_utc_ptime timezones (snd event.Icalendar.dtstart) in
    let dtend, duration = match event.Icalendar.dtend_or_duration with
      | None -> None, None
      | Some (`Duration (_, span)) -> None, Some span
      | Some (`Dtend (_, v)) -> Some (fst (date_or_datetime_to_utc_ptime timezones v)), None
    in
    span_in_timerange range dtstart dtend duration dtstart_is_datetime

  let expand_event_in_range timezones f acc range exceptions event =
    let (s, e) = range in
    let next_event = Icalendar.recur_events event in
    let rec next_r () = match next_event () with
      | None -> None
      | Some event ->
        let date_or_datetime_to_date t d = fst @@ Ptime.to_date_time @@ fst @@ date_or_datetime_to_utc_ptime t d in
        let date = date_or_datetime_to_date timezones (snd event.dtstart) in
        if List.mem date exceptions
        then next_r ()
        else Some event
    in
    let rec in_timerange acc = function
     | Some event when Ptime.is_earlier ~than:s (fst (date_or_datetime_to_utc_ptime timezones (snd event.Icalendar.dtstart))) ->
       in_timerange acc (next_r ())
     | Some event when real_event_in_timerange timezones event range ->
       let acc' = f acc event in
       in_timerange acc' (next_r ())
     | _ -> acc in
    in_timerange acc (Some event)

  let event_in_timerange timezones range exceptions event =
    let f _ _ = true in
    expand_event_in_range timezones f false range exceptions event

  (* TODO does not match freebusy table in RFC 4791 Sec 9.9 *)
  let freebusy_in_timerange range fb =
    match
      List.find_opt (function `Dtstart_utc _ -> true | _ -> false) fb,
      List.find_opt (function `Dtend_utc _ -> true | _ -> false) fb
    with
    | Some (`Dtstart_utc (_, dtstart)), Some (`Dtend_utc (_, dtend)) ->
      span_in_timerange range dtstart (Some dtend) None true
    | _ -> false

  (* TODO `Todo is missing *)
  let comp_in_timerange timezones range exceptions = function
    | `Event e -> event_in_timerange timezones range exceptions e
    | `Freebusy fb -> freebusy_in_timerange range fb
    | `Timezone _  -> true
    | _ -> false

  let date_to_ptime date = match Ptime.of_date_time (date, ((0, 0, 0), 0)) with
    | None -> assert false
    | Some t -> t

  let ptime_to_date ts = fst @@ Ptime.to_date_time ts

  let normalize_timestamp timezones = function
    | `Utc ts -> `Utc ts
    | `Local ts -> `Local ts
    | `With_tzid (ts, tzid) as d ->
      match Icalendar.normalize_timezone ts tzid timezones with
      | None -> d
      | Some ts' -> `Utc ts'

  let normalize_date_or_datetime timezones = function
    | `Date date -> `Date date
    | `Datetime dt -> `Datetime (normalize_timestamp timezones dt)

  let normalize_dates_or_datetimes timezones = function
    | `Dates ds -> `Dates ds
    | `Datetimes dts -> `Datetimes (List.map (normalize_timestamp timezones) dts)

  let normalize_dates_or_datetimes_or_periods timezones = function
    | #Icalendar.dates_or_datetimes as ds_or_dts -> normalize_dates_or_datetimes timezones ds_or_dts
    | `Periods ps -> `Periods (List.map (fun (ts, span, was_explicit) -> (normalize_timestamp timezones ts, span, was_explicit)) ps)

  let expand_event range exceptions timezones event =
    let normalize = normalize_date_or_datetime timezones in
    let normalize' = normalize_dates_or_datetimes timezones in
    let normalize'' = normalize_dates_or_datetimes_or_periods timezones in
    let add_recur_id_normalize_tz acc event' =
      let dtstart =
        let params, value = event'.Icalendar.dtstart in
        params, normalize value
      in
      let dtend_or_duration = match event'.Icalendar.dtend_or_duration with
      | None -> None
      | Some (`Duration d) -> Some (`Duration d)
      | Some (`Dtend (params, v)) -> Some (`Dtend (params, normalize v)) in
      let props = List.map (function
          | `Recur_id (params, v) -> `Recur_id (params, normalize v)
          | `Rdate (params, v) -> `Rdate (params, normalize'' v)
          | `Exdate (params, v) -> `Exdate (params, normalize' v)
          | x -> x) event'.Icalendar.props
      in
      let recur_id : Icalendar.event_prop = `Recur_id dtstart in
      let props' = match event'.Icalendar.rrule with None -> props | Some _ -> recur_id :: props in
      { event' with Icalendar.dtstart ; dtend_or_duration ; props = props' ; rrule = None } :: acc
    in
    expand_event_in_range timezones add_recur_id_normalize_tz [] range exceptions event

  let expand_comp range exceptions timezones = function
    | `Event e -> List.map (fun e -> `Event e) (expand_event range exceptions timezones e)
    (* TODO normalise other timestamps with TZID as well: DUE *)
    (*| `Todo e -> List.map (fun e -> `Todo e) (expand_todo range exceptions timezones e)*)
    | _ -> []

  (* both range and freebusy are in utc *)
  let fb_in_timerange range = function
    | `Freebusy fb ->
      let in_range (s, span, was_explicit) =
        let e = add_span s span in
        let (s_req, e_req) = range in
        let (<) a b = Ptime.is_later ~than:a b in
        let (<=) a b = a < b || Ptime.equal a b in
        (s_req <= s && e < e_req) ||
        (s_req <= e && e < e_req) ||
        (s_req <= s && s < e_req)
      in
      let prop_in_timerange = function
        | `Freebusy (_, ranges) -> List.exists in_range ranges
        | _ -> true
      in
      [ `Freebusy (List.filter prop_in_timerange fb) ]
    | _ -> []

  (* transformation step *)
  let select_calendar_data (calprop, (comps : Icalendar.component list)) (requested_data: Xml.calendar_data) =
    let (_, range, freebusy) = requested_data in
    let exceptions =
      let events =
        List.filter (function
            | `Event event -> List.exists (function `Recur_id _ -> true | _ -> false) event.Icalendar.props
            | _ -> false)
          comps
      in
      List.map (function
          | `Event event ->
            begin match event.Icalendar.dtstart with
              | (_, `Date d)          -> d
              | (_, `Datetime (`Utc d)) -> fst @@ Ptime.to_date_time d
              | (_, `Datetime (`Local d)) -> fst @@ Ptime.to_date_time d
              | (_, `Datetime (`With_tzid (d, tzid))) -> fst @@ Ptime.to_date_time d
            end
          | _ -> assert false)
        events
    in
    let timezones = List.fold_left (fun acc -> function `Timezone tz -> tz :: acc | _ -> acc) [] comps in
    let limit_rec_set comps = match range with
      | Some (`Limit_recurrence_set range) -> List.filter (comp_in_timerange timezones range exceptions) comps
      | Some (`Expand range) -> List.flatten (List.map (expand_comp range exceptions timezones) comps)
      | _ -> comps in
    let limit_freebusy_set comps = match freebusy with
      | None -> comps
      | Some (`Limit_freebusy_set range) ->
        List.flatten (List.map (fb_in_timerange range) comps)
    in
    (calprop, limit_freebusy_set @@ limit_rec_set comps)

  (*
  type comp_filter = [
    | `Is_defined (* represents empty filter in RFC *)
    | `Is_not_defined
    | `Comp_filter of timerange option * prop_filter list * component_filter list
  ]
  and component_filter = string * comp_filter
  *)

  let ts_in_range ts range =
    let (s, e) = range in
    Ptime.is_later ~than:s ts && Ptime.is_later ~than:ts e

  (* TODO deal with repeating alarms *)
  let alarm_in_timerange range alarm by_parent =
    let trigger = match alarm with
    | `Email e -> e.Icalendar.trigger
    | `Audio a -> a.Icalendar.trigger
    | `Display d -> d.Icalendar.trigger
    | `None d -> d.Icalendar.trigger
    in
    match snd trigger with
    | `Datetime ts -> ts_in_range ts range
    | `Duration d -> (* is start or end; get duration, add to start / end *)
      let trig_rel = match Icalendar.Params.find Icalendar.Related (fst trigger) with
      | None -> `Start
      | Some x -> x
      in
      by_parent range d trig_rel


  let matches_alarm_filter by_parent alarm (comp_name, comp_filter) =
    let is_match = String.equal comp_name "VALARM" in
    match comp_filter, is_match with
    | `Is_defined, true -> true
    | `Is_not_defined, true -> false
    | `Is_defined, false -> false
    | `Is_not_defined, false -> true
    | `Comp_filter (_, _, _), false -> false
    (* no further cfs below alarm, and pfs is handled by Icalendar.to_ics (filtering on output) *)
    | `Comp_filter (tr_opt, _, _), true ->
      match tr_opt with
      | None -> true
      | Some range -> alarm_in_timerange range alarm by_parent

  let matches_comp_filter timezones component (comp_name, comp_filter) =
    let is_match =
      String.equal comp_name (Icalendar.component_to_ics_key component)
    in
    Log.debug (fun m -> m "component matches filter %s %b (component key %s)"
                  comp_name is_match (Icalendar.component_to_ics_key component)) ;
    match comp_filter, is_match with
    | `Is_defined, true -> true
    | `Is_not_defined, true -> false
    | `Is_defined, false -> false
    | `Is_not_defined, false -> true
    | `Comp_filter (_, _, _), false -> false
    | `Comp_filter (tr_opt, pfs, cfs), true ->
      let matches_timerange = match tr_opt with
      | None -> true
      | Some range ->
        let exceptions = [] in (* TODO *)
        comp_in_timerange timezones range exceptions component in
      let matches_cfs = match cfs, component with
      | [], _ -> true
      | _, `Todo (props, alarms) ->
        let by_parent range d trigrel =
          let (dtstart_opt, duration) = get_time_properties props in
          match trigrel with
            | `Start ->
              (match dtstart_opt with
              | None -> assert false
              | Some dtstart ->
              let todo_start, _ = date_or_datetime_to_utc_ptime timezones dtstart in
              let alarm_start = add_span todo_start d in
              ts_in_range alarm_start range)
            | `End ->
              let due = List.find_opt (function `Due _ -> true | _ -> false) props in
              match due, dtstart_opt, duration with
              | Some (`Due (_, date_or_time)), _, _ ->
                let todo_end, _ = date_or_datetime_to_utc_ptime timezones date_or_time in
                let alarm_start = add_span todo_end d in
                ts_in_range alarm_start range
              | None, Some dtstart, Some duration ->
                let todo_start, _ = date_or_datetime_to_utc_ptime timezones dtstart in
                let todo_end = add_span todo_start duration in
                let alarm_start = add_span todo_end d in
                ts_in_range alarm_start range
              | _ -> assert false
            in

        List.exists (fun alarm -> List.exists (matches_alarm_filter by_parent alarm) cfs) alarms
      | _, `Event event ->
        let exceptions = [] in (* TODO missing! *)
        let by_parent range d trigrel =
          (* TODO may miss some events if their alarm is outside of range *)
          let events = expand_event range exceptions timezones event in
          List.exists (fun event ->
              let event_start, _ = date_or_datetime_to_utc_ptime timezones (snd event.Icalendar.dtstart) in
              match trigrel with
              | `Start ->
                let alarm_start = add_span event_start d in
                ts_in_range alarm_start range
              | `End ->
                let event_end = match event.Icalendar.dtend_or_duration with
                  | Some (`Dtend (_, dtend)) -> fst (date_or_datetime_to_utc_ptime timezones dtend)
                  | Some (`Duration (_, span)) -> add_span event_start span
                  | None -> assert false
                in
                let alarm_start = add_span event_end d in
                ts_in_range alarm_start range)
            events
        in
        List.exists (fun alarm -> List.exists (matches_alarm_filter by_parent alarm) cfs) event.Icalendar.alarms
      | _, _ -> false in
      (* TODO: treat pfs *)
      matches_timerange && matches_cfs

  let get_timezones_for_resp calendar tzids =
    let get_timezone tzid =
      let has_matching_tzid props = List.exists (function `Timezone_id (_, (_, tzid')) -> tzid = tzid' | _ -> false) props in
      List.find (function `Timezone props -> has_matching_tzid props | _ -> false ) (snd calendar) in
    List.map get_timezone @@ Astring.String.Set.elements tzids

  (* returns the entire calendar if any component matches the component filter.
     this is fine, because in caldav rfc and apple testsuite, each calendar only uses one component per file *)
  let vcalendar_matches_comp_filter filter (props, comps) =
    match filter with
    | ("VCALENDAR", `Is_defined) -> true
    | ("VCALENDAR", `Is_not_defined) -> false
    | ( _ , `Is_defined) -> false
    | ( _ , `Is_not_defined) -> true
      (*`Comp_filter of timerange option * prop_filter list * component_filter list*)
    | ("VCALENDAR", `Comp_filter (tr_opt, pfs, cfs)) ->
      let timezones = List.flatten @@ List.map (function `Timezone tz -> [tz] | _ -> []) comps in
      let matches_timerange = match tr_opt with
        | None -> true
        | Some range ->
          let exceptions = [] in (* TODO *)
          List.exists (comp_in_timerange timezones range exceptions) comps
      in
      let matches_cfs =
        (* TODO abstract *)
        List.exists (fun c -> List.exists (matches_comp_filter timezones c) cfs) comps
      in
      let matches_pfs = List.for_all (apply_to_props props) pfs in
      matches_timerange && matches_cfs && matches_pfs
    | _ -> false

  let apply_transformation (t : Xml.report_prop option) d resource_props ~auth_user_props = match t with
    | None -> [`OK, [Xml.node ~ns:Xml.caldav_ns "calendar-data" [Xml.pcdata (Icalendar.to_ics ~cr:false d)]]]
    | Some `All_props -> [`OK, Properties.all resource_props]
    | Some `Propname -> [`OK, Properties.names resource_props]
    | Some `Proplist ps ->
      let props, calendar_data_transform = List.partition (function `Prop _ -> true | _ -> false) ps in
      let calendar_data_transform' = match calendar_data_transform with
        | [ `Calendar_data c ] -> Some c
        | [] -> None
        | _ -> assert false
      in
      let props' = List.map (function `Prop p -> p | _ -> assert false) props in
      let output, filter = match calendar_data_transform' with
        | None -> d, None
        | Some ((filter, _, _) as tr) -> select_calendar_data d tr, filter
      in
      let found_props = Properties.find_many ~auth_user_props ~resource_props props' in
      let ok_props, rest_props = List.partition (fun (st, _) -> st = `OK) found_props in
      let ok_props' = List.flatten (List.map snd ok_props) in
      match snd output with (* kill whole calendar if comps are empty *)
      | [] -> []
      | _ ->
        let ics = Icalendar.to_ics ~cr:false ~filter output in
        let cs = [ Xml.node ~ns:Xml.caldav_ns "calendar-data" [Xml.pcdata ics] ] in
        [`OK, ok_props' @ cs ] @ rest_props

  let apply_to_vcalendar ((transform, filter): Xml.report_prop option * Xml.component_filter) data map ~auth_user_props =
    if vcalendar_matches_comp_filter filter data then
      apply_transformation transform data map ~auth_user_props
    else
      []

  let handle_calendar_query_report calendar_query fs path ~auth_user_props =
    let report_one query = function
      | `Dir _ -> Lwt.return (Error `Bad_request)
      | `File f ->
        Fs.read fs (`File f) >>= function
        | Error _ -> Lwt.return @@ Error `Bad_request
        | Ok (data, resource_props) ->
          privilege_met fs `Read ~auth_user_props resource_props >|= function
          | `Forbidden -> 
            let node = Xml.dav_node "response"
                [ Xml.dav_node "href" [ Xml.pcdata (Fs.to_string (`File f)) ] ;
                  Xml.dav_node "status" [ Xml.pcdata (statuscode_to_string `Forbidden) ] ]
            in
            Ok (Some node)
          | `Ok -> match Icalendar.parse data with
            | Error e ->
              Log.err (fun m -> m "Error %s while parsing %s" e data);
              Error `Bad_request
            | Ok ics ->
              match apply_to_vcalendar query ics resource_props ~auth_user_props with
              | [] -> Ok None
              | xs ->
                let node =
                  Xml.dav_node "response"
                    (Xml.dav_node "href" [ Xml.pcdata (Fs.to_string (`File f)) ]
                     :: List.map propstat_node xs)
                in
                Ok (Some node)
    in
    match path with
    | `File f ->
      begin
        report_one calendar_query (`File f) >|= function
        | Ok (Some node) -> Ok (Xml.tree_to_string @@ multistatus [ node ])
        | Ok None -> Ok (Xml.tree_to_string @@ multistatus [])
        | Error e -> Error e
      end
    | `Dir d ->
      Fs.listdir fs (`Dir d) >>= function
      | Error _ -> Lwt.return (Error `Bad_request)
      | Ok files ->
        Lwt_list.map_p (report_one calendar_query) files >|= fun responses ->
        (* TODO we remove individual file errors, should we report them back?
           be consistent in respect to other HTTP verbs taking directories (e.g. propfind) *)
        let responses' = List.fold_left (fun acc -> function
            | Ok (Some r) -> r :: acc
            | Ok None -> acc
            | Error _ -> acc) [] responses in
        let resp_tree = multistatus responses' in
        Ok (Xml.tree_to_string resp_tree)

  let handle_calendar_multiget_report (transformation, filenames) fs path ~auth_user_props =
    let report_one (filename : string) =
      Log.debug (fun m -> m "calendar_multiget: filename %s" filename) ;
      let file = Fs.file_from_string filename in
      Fs.read fs file >>= function
      | Error _ ->
        let node =
          Xml.dav_node "response"
            [ Xml.dav_node "href" [ Xml.pcdata (Fs.to_string (file :> Webdav_fs.file_or_dir)) ] ;
              Xml.dav_node "status" [ Xml.pcdata (statuscode_to_string `Not_found) ] ]
        in
        Lwt.return @@ Ok node
      | Ok (data, resource_props) ->
        privilege_met fs `Read ~auth_user_props resource_props >|= function
        | `Forbidden -> 
          let node = Xml.dav_node "response"
              [ Xml.dav_node "href" [ Xml.pcdata (Fs.to_string (file :> Webdav_fs.file_or_dir)) ] ;
                Xml.dav_node "status" [ Xml.pcdata (statuscode_to_string `Forbidden) ] ]
          in
          Ok node
        | `Ok ->
          match Icalendar.parse data with
          | Error e ->
            Log.err (fun m -> m "Error %s while parsing %s" e data);
            Error `Bad_request
          | Ok ics ->
            let xs = apply_transformation transformation ics resource_props ~auth_user_props in
            let node =
              Xml.dav_node "response"
                (Xml.dav_node "href" [ Xml.pcdata (Fs.to_string (file :> Webdav_fs.file_or_dir)) ]
                 :: List.map propstat_node xs)
            in
            Ok node
    in
    Lwt_list.map_p report_one filenames >|= fun responses ->
    (* TODO we remove individual file parse errors, should we report them back? *)
    let responses' = List.fold_left (fun acc -> function
        | Ok r -> r :: acc
        | Error _ -> acc) [] responses in
    let resp_tree = multistatus @@ List.rev responses' in
    Ok (Xml.tree_to_string resp_tree)

  let report fs config ~path ~user ~data =
    build_req_tree fs config path user data >>= function
    | Error e -> Lwt.return @@ Error e
    | Ok (f_or_d, auth_user_props, req_tree) ->
      match Xml.parse_calendar_query_xml req_tree, Xml.parse_calendar_multiget_xml req_tree with
      | Ok calendar_query, _ -> handle_calendar_query_report calendar_query fs f_or_d ~auth_user_props
      | _, Ok calendar_multiget -> handle_calendar_multiget_report calendar_multiget fs f_or_d ~auth_user_props
      | Error e, Error _ -> Lwt.return (Error `Bad_request)

  let read_target_or_parent_properties fs path target_or_parent =
    (match target_or_parent with
     | `Target -> Fs.from_string fs path
     | `Parent -> Lwt.return @@ Ok (Fs.parent @@ (Fs.file_from_string path :> Webdav_fs.file_or_dir) :> Webdav_fs.file_or_dir)) >>= function
    | Error _ -> Lwt.return Properties.empty
    | Ok f_or_d -> Fs.get_property_map fs f_or_d

  let access_granted_for_acl fs config http_verb ~path ~user =
    properties_for_current_user fs config user >>= fun auth_user_props ->
    Fs.exists fs path >>= fun target_exists ->
    let requirement, target_or_parent = Privileges.required http_verb ~target_exists in
    read_target_or_parent_properties fs path target_or_parent >>= fun resource_props ->
    privilege_met fs requirement ~auth_user_props resource_props >|= function
    | `Forbidden -> false
    | `Ok -> true

  (* moved from Caldav_server *)

let base64_encode data = Base64.encode_string @@ Cstruct.to_string data

let hash_password password salt =
  base64_encode @@ Mirage_crypto.Hash.SHA256.digest @@ Cstruct.of_string (salt ^ "-" ^ password)

let verify_auth_header fs config v =
  match Astring.String.cut ~sep:"Basic " v with
  | Some ("", b64) ->
    begin match Base64.decode b64 with
      | Error `Msg msg ->
        Lwt.return @@ Rresult.R.error_msgf "invalid base64 encoding %s: %s" msg b64
      | Ok data -> match Astring.String.cut ~sep:":" data with
        | None -> Lwt.return @@ Error (`Msg ("invalid user:pass encoding" ^ data))
        | Some (user, password) ->
          Fs.get_property_map fs (`Dir [config.principals ; user]) >|= fun props ->
          (* no user context yet *)
          match
            Properties.unsafe_find (Xml.robur_ns, "password") props,
            Properties.unsafe_find (Xml.robur_ns, "salt") props
          with
          | Some (_, [ Xml.Pcdata stored_password ]), Some (_, [ Xml.Pcdata salt ]) ->
            let computed_password = hash_password password salt in
            if String.equal computed_password stored_password
            then Ok user
            else Error (`Msg "password does not match")
          | _ -> Error (`Unknown_user (user, password))
    end
  | _ -> Lwt.return @@ Error (`Msg ("invalid auth header " ^ v))

let last_modified fs ~path =
  Fs.from_string fs path >>= function
  | Error _ -> Lwt.return None
  | Ok f_or_d ->
    Fs.last_modified fs f_or_d >|= function
    | Error _ -> None
    | Ok ts -> Some (Xml.ptime_to_http_date ts)

let compute_etag fs ~path =
  Fs.from_string fs path >>= function
  | Error _ -> Lwt.return None
  | Ok f_or_d ->
    Fs.etag fs f_or_d >|= function
    | Error _ -> None
    | Ok etag -> Some etag

let server_ns = "http://calendarserver.org/ns/"
let carddav_ns = "urn:ietf:params:xml:ns:carddav"

let make_dir fs now acl ?(resourcetype = []) ?(props=[]) dir =
  let propmap =
    Properties.create_dir ~initial_props:props ~resourcetype acl now (Fs.basename (dir :> Webdav_fs.file_or_dir))
  in
  Fs.mkdir fs dir propmap

let make_dir_if_not_present fs now acl ?resourcetype ?props dir =
  Fs.dir_exists fs dir >>= fun exists ->
  if not exists then
    make_dir fs now acl ?resourcetype ?props dir >|= fun _ -> ()
  else
    Lwt.return_unit

let create_calendar fs now acl name =
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
  make_dir_if_not_present fs now acl ~resourcetype ~props name

let initialize_fs_for_apple_testsuite fs now config =
  let acl = [ (`All, `Grant [ `All ]) ] in
  let calendars_properties =
    let url =
      Uri.with_path config.host
        (config.calendars ^ "/__uids__/10000000-0000-0000-0000-000000000001/calendar")
    in
    [
    (Xml.caldav_ns, "calendar-home-set"),
    ([], [Xml.node "href" ~ns:Xml.dav_ns [Xml.pcdata (Uri.to_string url) ]])
  ] in
  make_dir_if_not_present fs now acl ~props:calendars_properties (`Dir [config.calendars]) >>= fun _ ->
  make_dir_if_not_present fs now acl (`Dir [config.calendars ; "users"]) >>= fun _ ->
  make_dir_if_not_present fs now acl (`Dir [config.calendars ; "__uids__"]) >>= fun _ ->
  make_dir_if_not_present fs now acl (`Dir [config.calendars ; "__uids__" ; "10000000-0000-0000-0000-000000000001"]) >>= fun _ ->
  create_calendar fs now acl (`Dir [config.calendars ; "__uids__" ; "10000000-0000-0000-0000-000000000001" ; "calendar" ]) >>= fun _ ->
  make_dir_if_not_present fs now acl (`Dir [config.calendars ; "__uids__" ; "10000000-0000-0000-0000-000000000001" ; "tasks"]) >>= fun _ ->
  make_dir_if_not_present fs now acl (`Dir [config.principals]) >>= fun _ ->
  Lwt.return_unit

let initialize_fs fs now config =
  make_dir_if_not_present fs now (admin_acl config) (`Dir [config.principals]) >>= fun _ ->
  make_dir_if_not_present fs now (calendars_acl config) (`Dir [config.calendars]) >>= fun _ ->
  Lwt.return_unit

let change_user_password fs config ~name ~password ~salt =
  let principal_dir = `Dir [ config.principals ; name ] in
  Fs.get_property_map fs principal_dir >>= fun auth_user_props ->
  let auth_user_props' =
    let salt = base64_encode salt in
    Properties.unsafe_add (Xml.robur_ns, "salt")
      ([], [Xml.pcdata @@ salt])
      (Properties.unsafe_add (Xml.robur_ns, "password")
         ([], [Xml.pcdata @@ hash_password password salt]) auth_user_props)
  in
  Fs.write_property_map fs principal_dir auth_user_props' >|= function
  | Error e ->
    Log.err (fun m -> m "error %a while writing properties" Fs.pp_write_error e) ;
    Error `Internal_server_error
  | Ok () -> Ok ()

let make_principal props fs now config name =
  let resourcetype = [ Xml.node ~ns:Xml.dav_ns "principal" [] ] in
  let principal_dir = `Dir [ config.principals ; name ] in
  let principal_url_string = Fs.to_string (principal_dir :> Webdav_fs.file_or_dir) in
  let props' =
    ((Xml.dav_ns, "principal-URL"),
     ([], [ Xml.node ~ns:Xml.dav_ns "href" [ Xml.pcdata principal_url_string ] ]))
    :: props
  in
  let principal_url = Uri.of_string principal_url_string in
  let acl = (`Href principal_url, `Grant [ `All ]) in
  unsafe_read_acl fs (`Dir [config.principals]) >>= fun principal_acl ->
  (* maybe only allow root to write principal_dir (for password reset) *)
  make_dir_if_not_present fs now (acl :: principal_acl) ~resourcetype ~props:props' principal_dir >>= fun _ ->
  create_calendar fs now [acl] (`Dir [config.calendars ; name ]) >|= fun _ ->
  principal_url

let principal_to_href principals_directory principal_string =
  let uri_string = Fs.to_string (`Dir [ principals_directory ; principal_string]) in
  Xml.dav_node "href" [ Xml.pcdata uri_string ]

let href_to_principal principals_directory tree =
  match Xml.href_parser tree with
  | Ok principal ->
    let path = Uri.path (Uri.of_string principal) in
    let dir = Fs.to_string (`Dir [principals_directory]) in
    if Astring.String.is_prefix ~affix:dir path then
      match Astring.String.cut ~sep:dir path with
      | Some ("", p) -> Ok p
      | _ -> Error "invalid path"
    else
      Error "invalid path"
  | Error msg -> Error msg

let enroll_or_resign modify_membership fs config ~member ~group =
  let update_properties modify_property_map home =
    Fs.get_property_map fs home >>= fun prop_map ->
    let prop_map' = modify_property_map prop_map in
    Fs.write_property_map fs home prop_map' >|= function
    | Error we -> Log.err (fun m -> m "failed to write properties for %s: %a"
                              (Fs.to_string home) Fs.pp_write_error we)
    | Ok () -> ()
  in
  let principals prop_map key =
    match Properties.unsafe_find (Xml.dav_ns, key) prop_map with
      | None -> ([], [])
      | Some (attrs, all_principals) -> (attrs, all_principals)
  in
  let modify_member_in_group prop_map =
    let member_href = principal_to_href config.principals member in
    let values = principals prop_map "group-member-set" in
    let values' = modify_membership member_href values in
    Properties.unsafe_add (Xml.dav_ns, "group-member-set") values' prop_map
  in
  let modify_group_in_member prop_map =
    let group_href = principal_to_href config.principals group in
    let values = principals prop_map "group-membership" in
    let values' = modify_membership group_href values in
    Properties.unsafe_add (Xml.dav_ns, "group-membership") values' prop_map
  in
  update_properties modify_member_in_group (`Dir [ config.principals ; group ]) >>= fun () ->
  update_properties modify_group_in_member (`Dir [ config.principals ; member ])

let enroll =
  let modify_membership href (attrs, values) =
    if List.mem href values
    then (attrs, values)
    else (attrs, href :: values)
  in
  enroll_or_resign modify_membership

let resign =
  let remove_href href (attrs, values) =
    (attrs, List.filter (fun href' -> not (href = href')) values)
  in
  enroll_or_resign remove_href

let collect_principals fs config principal_dir key =
  Fs.get_property_map fs principal_dir >|= fun prop_map ->
  match Properties.unsafe_find key prop_map with
  | None -> []
  | Some (_, principals) ->
    List.fold_left (fun acc href -> match href_to_principal config.principals href with
        | Ok principal -> principal :: acc
        | Error e ->
          Log.err (fun m -> m "couldn't convert %a to principal: %s" Xml.pp_tree href e) ;
          acc) [] principals

let make_user ?(props = []) fs now config ~name ~password ~salt =
  let home_set_dir = `Dir [ config.calendars ] in
  let home_set_url_string = Fs.to_string (home_set_dir :> Webdav_fs.file_or_dir) in
  let props' =
    let salt = base64_encode salt in
    ((Xml.caldav_ns, "calendar-home-set"),
     ([], [Xml.dav_node "href" [Xml.pcdata home_set_url_string ]]))
    :: ((Xml.robur_ns, "password"),
        ([], [Xml.pcdata @@ hash_password password salt]))
    :: ((Xml.robur_ns, "salt"),
        ([], [Xml.pcdata salt]))
    :: props
  in
  make_principal props' fs now config name >>= fun principal_url ->
  collect_principals fs config (`Dir [config.principals]) (Xml.robur_ns, "default_groups") >>= fun groups ->
  Lwt_list.iter_s (fun group -> enroll fs config ~member:name ~group) groups >|= fun () ->
  principal_url

let delete_home_and_calendars fs principal_dir user_calendar_dir =
  Fs.destroy fs principal_dir >>= function
  | Error e -> Lwt.return @@ Error e
  | Ok () -> Fs.destroy fs user_calendar_dir

let delete_user fs config name =
  let principal_dir = `Dir [config.principals ; name] in
  let user_calendar_dir = `Dir [config.calendars ; name] in
  (* TODO also delete an users other calendars besides their default calendar *)
  (* TODO delete user from all acls *)
  (* TODO events in other people calendars? *)
  Fs.dir_exists fs principal_dir >>= function
  | false -> Lwt.return @@ Error `Not_found
  | true ->
    collect_principals fs config principal_dir (Xml.dav_ns, "group-membership") >>= fun groups ->
    Lwt_list.iter_s (fun group -> resign fs config ~member:name ~group) groups >>= fun () ->
    delete_home_and_calendars fs principal_dir user_calendar_dir >|= function
    | Error e ->
      Log.err (fun m -> m "error %a while removing home and calendars" Fs.pp_write_error e) ;
      Error `Internal_server_error
    | Ok () -> Ok ()

let delete_group_members fs config group =
  let principal_dir = `Dir [ config.principals ; group ] in
  collect_principals fs config principal_dir (Xml.dav_ns, "group-member-set") >>= fun members ->
  Lwt_list.iter_s (fun member -> resign fs config ~member ~group) members

let delete_group fs config name =
  delete_group_members fs config name >>= fun () ->
  delete_user fs config name

let replace_group_members fs config group new_members =
  delete_group_members fs config group >>= fun () ->
  Lwt_list.iter_s (fun member -> enroll fs config ~member ~group) new_members

(* TODO find out whether we should modify calendar-home-set of group or members *)
let make_group fs now config name members =
  make_principal [] fs now config name >>= fun principal_uri ->
  Lwt_list.iter_s (fun member -> enroll fs config ~member ~group:name) members >|= fun () ->
  principal_uri

  let generate_salt () = R.generate 15

  let connect fs config admin_pass =
    let now = Ptime.v (Clock.now_d_ps ()) in
    Fs.valid fs config >>= fun fs_is_valid ->
    match fs_is_valid, admin_pass with
    | Error (`Msg msg), None ->
      Lwt.fail_with ("got an uninitalized file system (error: " ^ msg ^ "), please provide admin password")
    | Error _, Some password ->
      initialize_fs fs now config >>= fun () ->
      let salt = generate_salt () in
      make_user fs now config ~name:"root" ~password ~salt >|= fun _ ->
      fs
    | Ok (), None -> Lwt.return fs
    | Ok (), Some password ->
      let salt = generate_salt () in
      change_user_password fs config ~name:"root" ~password ~salt >|= fun _ ->
      fs

end
