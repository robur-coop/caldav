module type S =
sig
  type state
  type tree = Webdav_xml.tree

  val mkcol : ?now:Ptime.t -> state -> Webdav_fs.dir -> tree option ->
    (state, [ `Bad_request | `Conflict | `Forbidden of tree ])
      result Lwt.t

  val propfind : state -> host:Uri.t -> path:Webdav_fs.file_or_dir -> tree -> user:Webdav_fs.file_or_dir -> depth:string option ->
    (tree, [ `Bad_request | `Forbidden of tree | `Property_not_found ]) result Lwt.t

  val proppatch : state -> host:Uri.t -> path:Webdav_fs.file_or_dir -> tree ->
    (state * tree, [ `Bad_request ]) result Lwt.t

  val report : state -> host:Uri.t -> path:Webdav_fs.file_or_dir -> tree ->
    (tree, [`Bad_request]) result Lwt.t

  val write : state -> path:Webdav_fs.file -> ?etag:string -> content_type:string -> string ->
    (state, [ `Conflict | `Internal_server_error | `Method_not_allowed ]) result Lwt.t

  val delete : ?now:Ptime.t -> state -> path:Webdav_fs.file_or_dir -> state Lwt.t

  (*
  val get : state -> string ->

  val head : state -> string ->

  val post : state -> string -> ?? -> state

  val put : state -> string ->

  val read : state -> string ->
  *)

  val access_granted_for_acl : state -> string -> Cohttp.Code.meth -> Properties.t -> bool Lwt.t
end

module Make(Fs: Webdav_fs.S) = struct

  open Lwt.Infix

  module Xml = Webdav_xml

  type state = Fs.t
  type tree = Webdav_xml.tree

  let compute_etag str = Digest.to_hex @@ Digest.string str

  let write state ~path ?etag ~content_type data =
    match path with
    | `Dir _ -> Lwt.return (Error `Method_not_allowed)
    | `File file ->
      let parent = Fs.parent (`File file) in
      Fs.dir_exists state parent >>= function
      | false ->
        Printf.printf "parent directory of %s does not exist\n" (Fs.to_string (`File file)) ;
        Lwt.return (Error `Conflict)
      | true ->
        let props =
          let etag = match etag with None -> compute_etag data | Some e -> e in
          Properties.create ~content_type ~etag
            (Ptime.to_rfc3339 (Ptime_clock.now ()))
            (String.length data) (Fs.to_string (`File file))
        in
        Fs.write state (`File file) (Cstruct.of_string data) props >|= function
        | Error e -> Error `Internal_server_error
        | Ok () -> Ok state

  let delete ?(now = Ptime_clock.now ()) state ~path =
    Fs.destroy state path >>= fun res ->
    let now = Ptime.to_rfc3339 now in
    let rec update_parent f_or_d =
      let (`Dir parent) = Fs.parent f_or_d in
      Fs.get_property_map state (`Dir parent) >>= function
      | None -> assert false
      | Some map ->
        let map' = Properties.add (Xml.dav_ns, "getlastmodified") ([], [ Xml.pcdata now ]) map in
        Fs.write_property_map state (`Dir parent) map' >>= function
        | Error e -> assert false
        | Ok () -> match parent with
          | [] -> Lwt.return_unit
          | dir -> update_parent (`Dir dir)
    in
    update_parent path >|= fun () ->
    state

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

  let uri_string host f_or_d =
    Uri.to_string @@ Uri.with_path host (Fs.to_string f_or_d)

  (* TODO extract map and sort *)
  let find_props userprops ps m =
    let props = List.map (fun ((ns, name) as k) ->
        if ns = Xml.dav_ns && name = "current-user-privilege-set" then
          k, Properties.current_user_privilege_set ~userprops m
        else
          k, Properties.find k m) ps in
    let (found, not_found) =
      List.fold_left (fun (found, not_found) ((ns, name), v) -> match v with
          | None        -> (found, Xml.node ~ns name [] :: not_found)
          | Some (a, v) -> (Xml.node ~ns ~a name v :: found, not_found))
        ([], []) props
    in
    match found, not_found with
    | [], [] -> []
    | [], nf -> [ (`Not_found, nf) ]
    | f, [] -> [ (`OK, f) ]
    | f, nf -> [ (`OK, f) ; (`Not_found, nf) ]

  let property_selector fs host propfind_request user f_or_d =
    Fs.get_property_map fs f_or_d >>= function
    | None -> Lwt.return `Not_found
    | Some map ->
      Fs.get_property_map fs user >|= fun user_map ->
      let user_map' = match user_map with
        | None -> Properties.empty
        | Some usermap -> usermap
      in
      (* results for props, grouped by code *)
      let propstats = match propfind_request with
        | `Propname -> [`OK, Properties.propname map]
        | `All_prop includes -> [`OK, Properties.allprop map] (* TODO: finish this *)
        | `Props ps -> find_props user_map' ps map
      in
      let ps = List.map propstat_node propstats in
      let selected_properties =
        Xml.dav_node "response"
          (Xml.dav_node "href" [ Xml.Pcdata (uri_string host f_or_d) ] :: ps)
      in
      `Single_response selected_properties

  let multistatus nodes = Xml.dav_node "multistatus" nodes

  let error_xml element = Xml.dav_node "error" [ Xml.dav_node element [] ]

  let propfind fs f_or_d host req user depth =
    let process_files fs host dir req els =
      Lwt_list.map_s (property_selector fs host req user) (dir :: els) >|= fun answers ->
      (* answers : [ `Not_found | `Single_response of Tyxml.Xml.node ] list *)
      let nodes = List.fold_left (fun acc element ->
          match element with
          | `Not_found -> acc
          | `Single_response node -> node :: acc) [] answers
      in
      Ok (multistatus nodes)
    in
    match depth, f_or_d with
    | `Infinity, _ ->
      let body = error_xml "propfind-finite-depth" in
      Lwt.return (Error (`Forbidden body))
    | `Zero, _
    | _, `File _ ->
      begin
        property_selector fs host req user f_or_d >|= function
        | `Not_found -> Error `Property_not_found
        | `Single_response t -> Ok (multistatus [t])
      end
    | `One, `Dir data ->
      Fs.listdir fs (`Dir data) >>= function
      | Error _ -> assert false
      | Ok els -> process_files fs host (`Dir data) req els

  let propfind state ~host ~path tree ~user ~depth =
    match parse_depth depth with
    | Error `Bad_request -> Lwt.return (Error `Bad_request)
    | Ok depth ->
      match Xml.parse_propfind_xml tree with
      | Error _ -> Lwt.return (Error `Property_not_found)
      | Ok req ->
        propfind state path host req user depth >|= function
        | Ok body -> Ok body
        | Error e -> Error e

  let apply_updates ?(validate_key = fun _ -> Ok ()) m updates =
    let set_prop k v m = match validate_key k with
      | Error e -> None, (e, k)
      | Ok () ->
        (* set needs to be more expressive: forbidden, conflict, insufficient storage needs to be added *)
        let map = Properties.add k v m in
        Some map, (`OK, k)
    in
    (* if an update did not apply, m will be None! *)
    let apply (m, propstats) update = match m, update with
      | None, `Set (_, k, _) -> None, (`Failed_dependency, k) :: propstats
      | None, `Remove k   -> None, (`Failed_dependency, k) :: propstats
      | Some m, `Set (a, k, v) ->
        let (m, p) = set_prop k (a, v) m in
        (m, p :: propstats)
      | Some m, `Remove k ->
        let map = Properties.remove k m in
        Some map, (`OK, k) :: propstats
    in
    match List.fold_left apply (m, []) updates with
    | Some m, xs -> Some m, xs
    | None, xs ->
      (* some update did not apply -> tree: None *)
      let ok_to_failed (s, k) =
        ((match s with
          | `OK -> `Failed_dependency
          | x -> x), k)
      in
      None, List.map ok_to_failed xs

  let update_properties ?validate_key fs f_or_d updates =
    Fs.get_property_map fs f_or_d >>= fun map ->
    let map', xs = apply_updates ?validate_key map updates in
    let propstats =
      List.map (fun (s, (ns, n)) -> propstat_node (s, [ Xml.node ~ns n [] ])) xs
    in
    (match map' with
    | None -> Lwt.return (Ok ())
    | Some m -> Fs.write_property_map fs f_or_d m ) >|= function
      | Error e -> Error e
      | Ok () -> Ok propstats

  let proppatch state ~host ~path body =
    match Xml.parse_propupdate_xml body with
    | Error _ -> Lwt.return (Error `Bad_request)
    | Ok updates ->
      let validate_key (ns, k) =
        if ns = Xml.dav_ns then match k with
          | "resourcetype" -> Error `Forbidden
          | _ -> Ok ()
        else Ok ()
      in
      update_properties ~validate_key state path updates >|= function
      | Error _      -> Error `Bad_request
      | Ok propstats ->
        let nodes =
          Xml.dav_node "response"
            (Xml.dav_node "href" [ Xml.Pcdata (uri_string host path) ] :: propstats)
        in
        let status = multistatus [ nodes ] in
        Ok (state, status)

  let body_to_props body default_props =
    match body with
    | None -> Ok default_props
    | Some body' ->
    match Xml.parse_mkcol_xml body' with
    | Error _ -> Error `Bad_request
    | Ok set_props ->
      match apply_updates (Some default_props) set_props with
      | None, errs ->
        let propstats =
          List.map (fun (s, (ns, n)) -> propstat_node (s, [ Xml.node ~ns n [] ])) errs
        in
        let xml = Xml.dav_node "mkcol-response" propstats in
        Printf.printf "forbidden from body_to_props!\n" ;
        Error (`Forbidden xml)
      | Some map, _ -> Ok map

  (* assumption: path is a relative path! *)
  let mkcol ?(now = Ptime_clock.now ()) state (`Dir dir) body =
    (* TODO: move to caller *)
    let parent = Fs.parent (`Dir dir) in
    Fs.dir_exists state parent >>= function
    | false -> Lwt.return (Error `Conflict)
    | true ->
      let default_props =
        Properties.create ~content_type:"text/directory"
          ~resourcetype:[ Xml.node ~ns:Xml.dav_ns "collection" [] ]
          (Ptime.to_rfc3339 now) 0
          (Fs.to_string (`Dir dir))
      in
      match body_to_props body default_props with
      | Error e -> Lwt.return (Error e)
      | Ok map -> Fs.mkdir state (`Dir dir) map >|= function
        | Error _ -> Error `Conflict
        | Ok () -> Ok state

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
    Format.printf "component matches filter %s %b (component key %s)\n" comp_name is_match (Icalendar.component_to_ics_key component) ;
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

  let apply_transformation (t : Xml.report_prop option) d map = match t with
    | None -> [`OK, [Xml.node ~ns:Xml.caldav_ns "calendar-data" [Xml.pcdata (Icalendar.to_ics ~cr:false d)]]]
    | Some `All_props -> [`OK, Properties.allprop map]
    | Some `Propname -> [`OK, Properties.propname map]
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
      (* TODO real user props! *)
      let found_props = find_props Properties.empty props' map in
      let ok_props, rest_props = List.partition (fun (st, _) -> st = `OK) found_props in
      let ok_props' = List.flatten (List.map snd ok_props) in
      match snd output with (* kill whole calendar if comps are empty *)
      | [] -> []
      | _ ->
        let ics = Icalendar.to_ics ~cr:false ~filter output in
        let cs = [ Xml.node ~ns:Xml.caldav_ns "calendar-data" [Xml.pcdata ics] ] in
        [`OK, ok_props' @ cs ] @ rest_props

  let apply_to_vcalendar ((transform, filter): Xml.report_prop option * Xml.component_filter) data map =
    if vcalendar_matches_comp_filter filter data then
      apply_transformation transform data map
    else
      []

  let handle_calendar_query_report calendar_query state host path =
    let report_one query = function
      | `Dir _ -> Lwt.return (Error `Bad_request)
      | `File f ->
        Fs.read state (`File f) >|= function
        | Error _ -> Error `Bad_request
        | Ok (data, map) ->
          match Icalendar.parse (Cstruct.to_string data) with
          | Error e ->
            Printf.printf "Error %s while parsing %s\n" e (Cstruct.to_string data);
            Error `Bad_request
          | Ok ics ->
            match apply_to_vcalendar query ics map with
            | [] -> Ok None
            | xs ->
              let node =
                Xml.dav_node "response"
                  (Xml.dav_node "href" [ Xml.pcdata (uri_string host (`File f)) ]
                   :: List.map propstat_node xs)
              in
              Ok (Some node)
    in
    match path with
    | `File f ->
      begin
        report_one calendar_query (`File f) >|= function
        | Ok (Some node) -> Ok (multistatus [ node ])
        | Ok None -> Ok (multistatus [])
        | Error e -> Error e
      end
    | `Dir d ->
      Fs.listdir state (`Dir d) >>= function
      | Error _ -> Lwt.return (Error `Bad_request)
      | Ok files ->
        Lwt_list.map_p (report_one calendar_query) files >>= fun responses ->
        (* TODO we remove individual file errors, should we report them back?
           be consistent in respect to other HTTP verbs taking directories (e.g. propfind) *)
        let responses' = List.fold_left (fun acc -> function
            | Ok (Some r) -> r :: acc
            | Ok None -> acc
            | Error _ -> acc) [] responses in
        Lwt.return (Ok (multistatus responses'))

  let handle_calendar_multiget_report (transformation, filenames) state host path =
    let report_one (filename : string) =
      Printf.printf "calendar_multiget: filename %s\n%!" filename ;
      let file = Fs.file_from_string filename in
      Fs.read state file >|= function
      | Error _ ->
        let node =
          Xml.dav_node "response"
            [ Xml.dav_node "href" [ Xml.pcdata (uri_string host (file :> Webdav_fs.file_or_dir)) ] ;
              Xml.dav_node "status" [ Xml.pcdata (statuscode_to_string `Not_found) ] ]
        in
        Ok node
      | Ok (data, map) ->
        match Icalendar.parse (Cstruct.to_string data) with
        | Error e ->
          Printf.printf "Error %s while parsing %s\n" e (Cstruct.to_string data);
          Error `Bad_request
        | Ok ics ->
          let xs = apply_transformation transformation ics map in
          let node =
            Xml.dav_node "response"
              (Xml.dav_node "href" [ Xml.pcdata (uri_string host (file :> Webdav_fs.file_or_dir)) ]
               :: List.map propstat_node xs)
          in
          Ok node
    in
    Lwt_list.map_p report_one filenames >>= fun responses ->
    (* TODO we remove individual file parse errors, should we report them back? *)
    let responses' = List.fold_left (fun acc -> function
        | Ok r -> r :: acc
        | Error _ -> acc) [] responses in
    Lwt.return (Ok (multistatus @@ List.rev responses'))

  let report state ~host ~path req =
    match Xml.parse_calendar_query_xml req, Xml.parse_calendar_multiget_xml req with
    | Ok calendar_query, _ -> handle_calendar_query_report calendar_query state host path
    | _, Ok calendar_multiget -> handle_calendar_multiget_report calendar_multiget state host path
    | Error e, Error _ -> Lwt.return (Error `Bad_request)

  let required_privs verb target_exists = match verb with
    | `GET -> `Read, `Target
    | `HEAD -> `Read, `Target
    | `OPTIONS -> `Read, `Target
    | `PUT when target_exists     -> `Write_content, `Target
    | `PUT (* no target exists *) -> `Bind, `Parent
    | `Other "PROPPATCH" -> `Write_properties, `Target
    | `Other "ACL" -> `Write_acl, `Target
    | `Other "PROPFIND" -> `Read, `Target (* plus <D:read-acl> and <D:read-current-user-privilege-set> as needed *)
    | `DELETE -> `Unbind, `Parent
    | `Other "MKCOL" -> `Bind, `Parent
    | `Other "MKCALENDAR" -> `Bind, `Parent
    | `Other "REPORT" -> `Read, `Target (* referenced_resources body *)
    | _ -> assert false
  (* | COPY (target exists)            | <D:read>, <D:write-content> and |
     |                                 | <D:write-properties> on target  |
     |                                 | resource                        |
     | COPY (no target exists)         | <D:read>, <D:bind> on target    |
     |                                 | collection                      |
     | MOVE (no target exists)         | <D:unbind> on source collection |
     |                                 | and <D:bind> on target          |
     |                                 | collection                      |
     | MOVE (target exists)            | As above, plus <D:unbind> on    |
     |                                 | the target collection           |
     | LOCK (target exists)            | <D:write-content>               |
     | LOCK (no target exists)         | <D:bind> on parent collection   |
     | UNLOCK                          | <D:unlock>                      |
     | CHECKOUT                        | <D:write-properties>            |
     | CHECKIN                         | <D:write-properties>            |
     | VERSION-CONTROL                 | <D:write-properties>            |
     | MERGE                           | <D:write-content>               |
     | MKWORKSPACE                     | <D:write-content> on parent     |
     |                                 | collection                      |
     | BASELINE-CONTROL                | <D:write-properties> and        |
     |                                 | <D:write-content>               |
     | MKACTIVITY                      | <D:write-content> on parent     |
     |                                 | collection                      | *)

  let privilege_met requirements privilege =
    match requirements, privilege with
    | _, `All -> true
    | `Read, `Read -> true
    | `Read_acl, `Read_acl -> true
    | `Write, `Write -> true
    | `Write_content, `Write -> true
    | `Write_properties, `Write -> true
    | `Write_acl, `Write -> true
    | `Bind, `Write -> true
    | `Unbind, `Write -> true
    | `Write_content, `Write_content -> true
    | `Write_properties, `Write_properties -> true
    | `Write_acl, `Write_acl -> true
    | `Bind, `Bind -> true
    | `Unbind, `Unbind -> true
    | _ -> false

  let read_target_or_parent_properties fs path target_or_parent =
    (match target_or_parent with
     | `Target -> Fs.from_string fs path
     | `Parent -> Lwt.return @@ Ok (Fs.parent @@ (Fs.file_from_string path :> Webdav_fs.file_or_dir) :> Webdav_fs.file_or_dir)) >>= function
    | Error _ -> Lwt.return Properties.empty
    | Ok f_or_d -> Fs.get_property_map fs f_or_d >|= function
      | None ->
        Printf.printf "forbidden: no property map found!\n" ;
        Properties.empty
      | Some props -> props

  let access_granted_for_acl fs path http_verb userprops =
    Fs.exists fs path >>= fun target_exists ->
    let requirements, target_or_parent = required_privs http_verb target_exists in
    read_target_or_parent_properties fs path target_or_parent >|= fun propmap ->
    let privileges = Properties.privileges ~userprops propmap in
    Format.printf "privileges are %a\n%!" Fmt.(list ~sep:(unit "; ") Xml.pp_privilege) privileges ;
    List.exists (privilege_met requirements) privileges
end
