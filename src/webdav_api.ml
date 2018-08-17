open Lwt.Infix

module Fs = Webdav_fs

module Xml = Webdav_xml

type state = Webdav_fs.Fs.t
type tree = Webdav_xml.tree

let compute_etag str = Digest.to_hex @@ Digest.string str

let write state ~name ?etag ~content_type data =
  match name with
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
        Xml.create_properties ~content_type false
          ~etag (Ptime.to_rfc3339 (Ptime_clock.now ()))
          (String.length data) (Fs.to_string (`File file))
      in
      Fs.write state (`File file) (Cstruct.of_string data) props >|= function
      | Error e -> Error `Internal_server_error
      | Ok () -> Ok state

let delete ?(now = Ptime_clock.now ()) state ~name =
  Fs.destroy state name >>= fun res ->
  let now = Ptime.to_rfc3339 now in
  let rec update_parent f_or_d =
    let (`Dir parent) = Fs.parent f_or_d in
    Fs.get_property_map state (`Dir parent) >>= function
    | None -> assert false
    | Some map ->
      let map' = Xml.PairMap.add (Xml.dav_ns, "getlastmodified") ([], [ Xml.pcdata now ]) map in
      Fs.write_property_map state (`Dir parent) map' >>= function
      | Error e -> assert false
      | Ok () -> match parent with
        | [] -> Lwt.return_unit
        | dir -> update_parent (`Dir dir)
  in
  update_parent name >|= fun () ->
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

let propfind_request_to_selector = function
  | `Propname -> (fun m -> [`OK, List.map (fun (ns, k) -> Xml.node ~ns k []) @@ List.map fst (Xml.PairMap.bindings m)])
  | `All_prop includes -> (fun m -> [`OK, Xml.props_to_tree m]) (* TODO: finish this *)
  | `Props ps -> (fun m -> Xml.find_props ps m)

let property_selector fs prefix request f_or_d =
  Printf.printf "processing properties of %s\n" (Fs.to_string f_or_d) ;
  Fs.get_property_map fs f_or_d >|= function
  | None -> `Not_found
  | Some map ->
    Printf.printf "read map %s\n" (Xml.props_to_string map) ;
    let propstats = (propfind_request_to_selector request) map in
    let ps = List.map propstat_node propstats in
    let selected_properties =
      Xml.dav_node "response"
        (Xml.dav_node "href" [ Xml.Pcdata (prefix ^ "/" ^ (Fs.to_string f_or_d)) ] :: ps)
    in
    `Single_response selected_properties

let multistatus nodes = Xml.dav_node "multistatus" nodes

let error_xml element = Xml.dav_node "error" [ Xml.dav_node element [] ]

let propfind fs f_or_d prefix req depth =
  let process_files fs prefix dir req els =
    Lwt_list.map_s (property_selector fs prefix req) (dir :: els) >|= fun answers ->
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
      property_selector fs prefix req f_or_d >|= function
      | `Not_found -> Error `Property_not_found
      | `Single_response t -> Ok (multistatus [t])
    end
  | `One, `Dir data ->
    Fs.listdir fs (`Dir data) >>= function
    | Error _ -> assert false
    | Ok els -> process_files fs prefix (`Dir data) req els

let propfind state ~prefix ~name tree ~depth =
  match parse_depth depth with
  | Error `Bad_request -> Lwt.return (Error `Bad_request)
  | Ok depth ->
    match Xml.parse_propfind_xml tree with
    | Error _ -> Lwt.return (Error `Property_not_found)
    | Ok req ->
      propfind state name prefix req depth >|= function
      | Ok body -> Ok body
      | Error e -> Error e

let apply_updates ?(validate_key = fun _ -> Ok ()) m updates =
  let set_prop k v m = match validate_key k with
    | Error e -> None, (e, k)
    | Ok () ->
      (* set needs to be more expressive: forbidden, conflict, insufficient storage needs to be added *)
      let map = Xml.PairMap.add k v m in
      Format.printf "map after set %a %s\n" Xml.pp_fqname k (Xml.props_to_string map) ;
      Some map, (`OK, k)
  in
  (* if an update did not apply, m will be None! *)
  let apply (m, propstats) update = match m, update with
    | None, `Set (_, k, _) -> None, (`Failed_dependency, k) :: propstats
    | None, `Remove k   -> None, (`Failed_dependency, k) :: propstats
    | Some m, `Set (a, k, v) -> let (m, p) = set_prop k (a, v) m in (m, p :: propstats)
    | Some m, `Remove k ->
      let map = Xml.PairMap.remove k m in
      Format.printf "map after remove %a %s\n" Xml.pp_fqname k (Xml.props_to_string map) ;
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

let proppatch state ~prefix ~name body =
  match Xml.parse_propupdate_xml body with
  | Error _ -> Lwt.return (Error `Bad_request)
  | Ok updates ->
    let validate_key (ns, k) = 
      if ns = Xml.dav_ns then match k with
        | "resourcetype" -> Error `Forbidden
        | _ -> Ok ()
      else Ok ()
    in
    update_properties ~validate_key state name updates >|= function
    | Error _      -> Error `Bad_request
    | Ok propstats ->
      let nodes =
        Xml.dav_node "response"
          (Xml.dav_node "href" [ Xml.Pcdata (prefix ^ "/" ^ (Fs.to_string name)) ] :: propstats)
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

(* assumption: name is a relative path! *)
let mkcol ?(now = Ptime_clock.now ()) state (`Dir dir) body =
  (* TODO: move to caller *)
  let parent = Fs.parent (`Dir dir) in
  Fs.dir_exists state parent >>= function
  | false -> Lwt.return (Error `Conflict)
  | true ->
    let default_props =
      Xml.create_properties ~content_type:"text/directory"
        true (Ptime.to_rfc3339 now) 0
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
  let key p = Icalendar.Writer.calprop_to_ics_key p in
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

(*
type comp = [ `Allcomp | `Comp of component list ]
and prop = [ `Allprop | `Prop of (string * bool) list ]
and component = string * prop * comp [@@deriving show, eq]

type timerange = string * string [@@deriving show, eq]

type calendar_data =
  component option *
  [ `Expand of timerange | `Limit_recurrence_set of timerange ] option *
  [ `Limit_freebusy_set of timerange ] option [@@deriving show, eq]

type report_prop = [
  | `All_props
  | `Proplist of [ `Calendar_data of calendar_data | `Prop of fqname ] list
  | `Propname
] [@@deriving show, eq]
*)

let propfilter to_key req_data props =
  match req_data with
  | `Allprop | `Prop [] -> props
  | `Prop ps -> List.filter (fun p -> List.exists (fun (key, _) -> String.equal (to_key p) key) ps) props


let event_propfilter = propfilter Icalendar.Writer.eventprop_to_ics_key
let todo_propfilter = propfilter Icalendar.Writer.todoprop_to_ics_key
let freebusy_propfilter = propfilter Icalendar.Writer.freebusyprop_to_ics_key
let timezone_propfilter = propfilter Icalendar.Writer.timezoneprop_to_ics_key
let calprop_propfilter = propfilter Icalendar.Writer.calprop_to_ics_key

let alarm_compfilter (comp: Xml.comp) alarms =
  match comp with
  | `Allcomp -> alarms
  | `Comp [("VALARM", _, _)] -> alarms
  | _ -> []
(*
  match alarms with
  | `Audio a -> Some(`Audio (audio_propfilter prop a))
  | `Display d -> Some(`Display (display_propfilter prop d))
  | `Email e -> Some(`Email (email_propfilter prop e))
*)

let select_component component ((name, prop, comp): Xml.component) =
  match component, name with
  | `Event (props, alarms), "VEVENT" -> Some(`Event (event_propfilter prop props, alarm_compfilter comp alarms))
  | `Todo (props, alarms), "VTODO" -> Some(`Todo (todo_propfilter prop props, alarm_compfilter comp alarms))
  | `Freebusy props, "VFREEBUSY" -> Some(`Freebusy (freebusy_propfilter prop props))
  | `Timezone props, "VTIMEZONE" -> Some(`Timezone (timezone_propfilter prop props))
  | _ -> None

let get_time_properties (props:Icalendar.eventprop list) =
  let dtstart = match List.find_opt (function `Dtstart _ -> true | _ -> false) props with
  | None -> assert false
  | Some (`Dtstart (_, startdate)) -> startdate
  in
  let dtend = match List.find_opt (function `Dtend _ -> true | _ -> false) props with
  | Some (`Dtend (_, `Date enddate)) -> begin match Ptime.of_date_time (enddate, ((0, 0, 0), 0)) with
    | None -> assert false
    | Some end_ts -> Some end_ts
  end 
  | Some (`Dtend (_, `Datetime (ts, utc))) -> Some ts
  | None -> None
  in 
  let duration = match List.find_opt (function `Duration _ -> true | _ -> false) props with
  | None -> None
  | Some (`Duration (_, s)) -> Some s
  in (dtstart, dtend, duration)

let real_event_in_timerange s e dtstart dtend duration is_datetime =
  match dtend, duration with
  | Some dtend, None -> Ptime.is_earlier s ~than:dtend && Ptime.is_later e ~than:dtstart
  | None, Some secs when secs > 0 -> 
    begin match Ptime.add_span dtstart (Ptime.Span.of_int_s secs) with
    | None -> false
    | Some dtend -> Ptime.is_earlier s ~than:dtend && Ptime.is_later e ~than:dtstart
    end
  | None, Some 0 -> (Ptime.is_earlier s ~than:dtstart || Ptime.equal s dtstart) && Ptime.is_later e ~than:dtstart
  | None, None -> if is_datetime 
    then (Ptime.is_earlier s ~than:dtstart || Ptime.equal s dtstart) && Ptime.is_later e ~than:dtstart 
    else match Ptime.add_span dtstart (Ptime.Span.of_int_s (24 * 60 * 60)) with
      | None -> false
      | Some dtend -> Ptime.is_earlier s ~than:dtend && Ptime.is_later e ~than:dtstart 

let fold_event f acc range exceptions (`Event (props, alarms)) = 
  let ((s, _), (e, _)) = range in
  let (dtstart, dtend, duration) = get_time_properties props in
  let ts_of_dtstart = function
  | `Datetime (dtstart, utc) -> dtstart, true
  | `Date start -> match Ptime.of_date_time (start, ((0, 0, 0), 0)) with
    | None -> assert false
    | Some dtstart -> dtstart, false
  in 
  let dtstart', is_datetime = ts_of_dtstart dtstart in
  let rrule = List.find_opt (function `Rrule _ -> true | _ -> false) props in
  let next_event = match rrule with
  | None -> (fun () -> None)
  | Some (`Rrule (_, rrule)) -> Icalendar.recur_events dtstart' rrule
  in
  let rec next_r () = match next_event () with 
  | None -> None 
  | Some dtstart -> 
    let date, _ = Ptime.to_date_time dtstart in
    if List.mem date exceptions then next_r () else Some dtstart
  in
  let rec in_timerange acc = function
   | Some dtstart when Ptime.is_earlier ~than:s dtstart -> 
     in_timerange acc (next_r ())
   | Some dtstart when real_event_in_timerange s e dtstart dtend duration is_datetime -> 
     let acc' = f acc dtstart in
     in_timerange acc' (next_r ())
   | _ -> acc in
  in_timerange acc (Some dtstart')

let event_in_timerange range exceptions e =
  let f acc _ = true in
  fold_event f false range exceptions e
 
let comp_in_timerange r exceptions = function
  | `Event _ as e -> event_in_timerange r exceptions e
  | `Timezone _  -> true
  | _ -> false

let date_to_ptime date = match Ptime.of_date_time (date, ((0, 0, 0), 0)) with
  | None -> assert false
  | Some t -> t

let ptime_to_date ts = fst @@ Ptime.to_date_time ts

let normalize_tz timestamp params timezones =
  match Icalendar.Params.find Icalendar.Tzid params with
  | None -> params, timestamp
  | Some tzid ->
      let params' = Icalendar.Params.remove Icalendar.Tzid params in
      params', Icalendar.normalize_timezone timestamp tzid timezones

let normalize_date_or_datetime params timezones = function
  | `Datetime (ts, utc) ->
       let params', ts' = normalize_tz ts params timezones in
       params', `Datetime (ts', utc)
  | `Date date ->
       let ts = date_to_ptime date in
       let params', ts' = normalize_tz ts params timezones in
       params', `Date (ptime_to_date ts')

let expand_event range exceptions timezones ((props: Icalendar.eventprop list), alarms) =
  let f acc dtstart =
    let recur_id : Icalendar.eventprop =
      match List.find_opt (function `Dtstart _ -> true | _ -> false) props with
      | Some (`Dtstart (params, v)) ->
        let params', v' = normalize_date_or_datetime params timezones (`Datetime (dtstart, false)) in
        `Recur_id (params', v')
      | _ -> assert false
    in
    let props' = List.map (function
      | `Dtstart (params, `Datetime (_, utc)) ->
        `Dtstart (normalize_date_or_datetime params timezones (`Datetime (dtstart, utc)))
      | `Dtstart (params, `Date _) ->
        `Dtstart (normalize_date_or_datetime params timezones (`Date (ptime_to_date dtstart)))
      | `Recur_id (params, v) ->
        `Recur_id (normalize_date_or_datetime params timezones v)
      | `Rrule _ -> recur_id
      | x -> x) props in
    `Event (props', alarms) :: acc
  in
  fold_event f [] range exceptions (`Event (props, alarms))

let expand_comp range exceptions timezones = function
  | `Event e -> expand_event range exceptions timezones e
  | _ -> []

let select_calendar_data (calprop, (comps : Icalendar.component list)) (requested_data: Xml.calendar_data) =
  let (comp, range, freebusy) = requested_data in
  let exceptions = 
    let events = List.filter (function `Event (props, _) -> List.exists (function `Recur_id _ -> true | _ -> false) props | _ -> false) comps in
    List.map (function `Event ((props: Icalendar.eventprop list), _) -> begin match List.find (function `Dtstart _ -> true | _ -> false) props with
      | `Dtstart (_, `Date d)          -> d
      | `Dtstart (_, `Datetime (d, _)) -> fst @@ Ptime.to_date_time d end
    | _ -> assert false
  ) events in
  let timezones = List.fold_left (fun acc -> function `Timezone tz -> tz :: acc | _ -> acc) [] comps in
  let limit_rec_set comps = match range with
  | Some (`Limit_recurrence_set range) -> List.filter (comp_in_timerange range exceptions) comps
  | Some (`Expand range) -> List.flatten (List.map (expand_comp range exceptions timezones) comps)
  | _ -> comps in
  match comp with
  | None -> Some (calprop, limit_rec_set comps)
  | Some ("VCALENDAR", prop, comp) -> 
    let comps' = match comp with
    | `Allcomp -> comps
    | `Comp cs -> 
       let select_and_filter c acc' comp = match select_component c comp with None -> acc' | Some c -> c :: acc' in
       let comps' = List.fold_left (fun acc c -> List.fold_left (select_and_filter c) acc cs) [] comps in
       List.rev comps'
    in
    Some (calprop_propfilter prop calprop, limit_rec_set comps')
  | _ -> None


(*
type comp_filter = [
  | `Is_defined (* represents empty filter in RFC *)
  | `Is_not_defined
  | `Comp_filter of timerange option * prop_filter list * component_filter list
]
and component_filter = string * comp_filter
*)
let apply_comp_filter (comp_name, comp_filter) component =
  let is_match =
    String.equal comp_name (Icalendar.component_to_ics_key component)
  in
  match comp_filter, is_match with
  | `Is_defined, true -> true
  | `Is_not_defined, true -> false
  | `Is_defined, false -> false
  | `Is_not_defined, false -> true
  | `Comp_filter (_, _, _), false -> false
  | `Comp_filter (tr_opt, pfs, cfs), true ->
    match tr_opt with
    | None -> true
    | Some range -> 
       let exceptions = [] in (* TODO *)
       comp_in_timerange range exceptions component
        (* TODO: treat pfs and cfs *)

let get_timezones_for_resp calendar tzids =
  let get_timezone tzid =
    let has_matching_tzid props = List.exists (function `Timezone_id (_, (_, tzid')) -> tzid = tzid' | _ -> false) props in
    List.find (function `Timezone props -> has_matching_tzid props | _ -> false ) (snd calendar) in
  List.map get_timezone @@ Astring.String.Set.elements tzids

let apply_comp_filter_to_vcalendar filter data =
  Format.printf "apply to vcalendar, snd query is %a\n" Xml.pp_component_filter filter ;
  match filter, data with
  | ("VCALENDAR", `Is_defined), data -> Some data
  | ("VCALENDAR", `Is_not_defined), data -> None
  | ( _ , `Is_defined), data -> None
  | ( _ , `Is_not_defined), data -> Some data
    (*`Comp_filter of timerange option * prop_filter list * component_filter list*) 
  | ("VCALENDAR", `Comp_filter (tr_opt, pfs, cfs)), (props, comps) ->
    let comps' = match tr_opt with
    | None -> comps
    | Some range -> 
      let exceptions = [] in (* TODO *)
      if List.exists (comp_in_timerange range exceptions) comps then comps else []
    in
    let comps'' =
      (* TODO abstract *)
      if List.exists (fun c -> List.exists (fun cf -> apply_comp_filter cf c) cfs) comps'
      then comps'
      else []
    in
    if List.for_all (apply_to_props props) pfs
    then Some (props, comps'')
    else None
  | _ -> Printf.printf "something else\n" ; None



let apply_to_vcalendar ((transform, filter): Xml.report_prop option * Xml.component_filter) data map =
  let filtered_data = apply_comp_filter_to_vcalendar filter data in
  Format.printf "filtered data is %a" Fmt.(option ~none:(unit "none") Icalendar.pp) filtered_data ;
  let apply_transformation t d = match t with
  | `All_props -> [`OK, Xml.props_to_tree map]
  | `Proplist ps ->
     let props, calendar_data = List.fold_left (fun (ps, cs) -> function
        | `Calendar_data c -> (ps, c :: cs)
        | `Prop p -> (p :: ps, cs)) ([], []) ps
     in
     let outputs = List.fold_left (fun acc c -> match select_calendar_data d c with
         | None -> acc
         | Some c -> c :: acc) [] calendar_data
     in
     let found_props = Xml.find_props props map in
     let ok_props, rest_props = List.partition (fun (st, _) -> st = `OK) found_props in
     let ok_props' = List.flatten (List.map snd ok_props) in
     let calendars = List.flatten @@ List.map
       (fun c -> 
         match snd c with 
         | [] -> [] 
         | _  -> 
           let ics = Icalendar.to_ics ~cr:false c in
           [ Xml.node ~ns:Xml.caldav_ns "calendar-data" [Xml.pcdata ics] ]
       )
       outputs in
       begin match calendars with [] -> [] | cs -> [`OK, ok_props' @ cs ] @ rest_props end
  | `Propname -> [`OK, List.map ( fun (ns, k) -> Xml.node ~ns k []) @@ List.map fst (Xml.PairMap.bindings map)]
  in
  match transform, filtered_data with
  | None, Some c -> [`OK, [Xml.node ~ns:Xml.caldav_ns "calendar-data" [Xml.pcdata (Icalendar.to_ics ~cr:false c)]]]
  | _ , None -> []
  | Some t, Some d -> apply_transformation t d

let report state ~prefix ~name req =
  let report_one query = function
    | `Dir _ -> Lwt.return (Error `Bad_request)
    | `File f ->
      Fs.read state (`File f) >>= function
      | Error _ -> Lwt.return (Error `Bad_request)
      | Ok (data, map) ->
        match Icalendar.parse (Cstruct.to_string data) with
        | Error e ->
          Printf.printf "Error %s while parsing %s\n" e (Cstruct.to_string data);
          Lwt.return (Error `Bad_request)
        | Ok ics ->
          match apply_to_vcalendar query ics map with
          | [] -> Lwt.return (Ok None)
          | xs -> 
            let filename = Fs.to_string (`File f) in
            Format.printf "xs for %s are:\n%a\n" filename
              Fmt.(list ~sep:(unit "\n\n") Xml.pp_tree) (List.map propstat_node xs) ;
            let node =
              Xml.dav_node "response"
                (Xml.dav_node "href" [ Xml.pcdata (prefix ^ filename) ]
                 :: List.map propstat_node xs)
            in
            Lwt.return (Ok (Some node)) in
  match Xml.parse_calendar_query_xml req with
  | Error e -> Lwt.return (Error `Bad_request)
  | Ok calendar_query -> match name with
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
        Lwt_list.map_p (report_one calendar_query) files >>= fun reports ->
        (* TODO we remove individual file errors, should we report them back?
           be consistent in respect to other HTTP verbs taking directories (e.g. propfind) *)
        let report' = List.fold_left (fun acc -> function
            | Ok (Some r) -> r :: acc
            | Ok None -> acc
            | Error _ -> acc) [] reports in
        Lwt.return (Ok (multistatus report'))
