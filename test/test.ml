module Xml = Caldav.Webdav_xml
module KV_mem = Mirage_kv_mem.Make(Pclock)
module Fs = Caldav.Webdav_fs.Make(Pclock)(KV_mem)
module Dav = Caldav.Webdav_api.Make(Mirage_random_test)(Pclock)(Fs)
module Properties = Caldav.Properties

open Caldav.Webdav_config

let header = "<?xml version=\"1.0\" encoding=\"utf-8\" ?>"

let tree xml = match Xml.string_to_tree xml with Some t -> t | None -> Alcotest.fail "Invalid xml."

let to_ptime date time =
  match Ptime.of_date_time (date, (time, 0)) with
  | None -> Alcotest.fail "invalid date time"
  | Some p -> p

let prop =
  let module M = struct
    type t = Xml.propfind
    let pp = Xml.pp_propfind
    let equal a b = compare a b = 0
  end in
  (module M : Alcotest.TESTABLE with type t = M.t)

let str_err =
  let module M = struct
    type t = string
    let pp = Fmt.string
    let equal = String.equal
  end in
  (module M : Alcotest.TESTABLE with type t = M.t)

let empty_propfind () =
  let xml = header ^ "<propfind xmlns=\"" ^ Xml.dav_ns ^ "\"/>" in
  Alcotest.(check (result prop str_err) "parsing <propfind/>"
              (Error "broken") (Xml.parse_propfind_xml (tree xml)))

let propname () =
  let xml = header ^ "<propfind xmlns=\"" ^ Xml.dav_ns ^ "\"><propname/></propfind>" in
  Alcotest.(check (result prop str_err) "parsing <propfind><propname/></propfind>"
              (Ok `Propname) (Xml.parse_propfind_xml (tree xml)))

let two_propname () =
  let xml = header ^ "<propfind xmlns=\"" ^ Xml.dav_ns ^ "\"><propname/><propname/></propfind>" in
  Alcotest.(check (result prop str_err) "parsing <propfind><propname/><propname/></propfind>"
              (Error "broken") (Xml.parse_propfind_xml (tree xml)))

let allprop () =
  let xml = header ^ "<propfind xmlns=\"" ^ Xml.dav_ns ^ "\"><allprop/></propfind>" in
  Alcotest.(check (result prop str_err) "parsing <propfind><allprop/></propfind>"
              (Ok (`All_prop [])) (Xml.parse_propfind_xml (tree xml)))

let allprop_include () =
  let xml = header ^ "<propfind xmlns=\"" ^ Xml.dav_ns ^ "\"><allprop/><include><foo/><bar/></include></propfind>" in
  Alcotest.(check (result prop str_err) "parsing all prop with includes"
              (Ok (`All_prop ["foo";"bar"])) (Xml.parse_propfind_xml (tree xml)))

let invalid_xml () =
  let error_tree xml = match Xml.string_to_tree xml with
    | Some _ -> ()
    | None -> invalid_arg "Invalid xml" in
  Alcotest.(check_raises "parsing header only" (Invalid_argument "Invalid xml")
              (fun () -> error_tree header));
  let xml = header ^ "<propfind xmlns=\"" ^ Xml.dav_ns ^ "\">" in
  Alcotest.(check_raises "hanging paren"
              (Invalid_argument "Invalid xml") (fun () -> error_tree xml)) ;
  let xml = header ^ "<propfind" in
  Alcotest.(check_raises "missing bracket"
              (Invalid_argument "Invalid xml") (fun () -> error_tree xml)) ;
  let xml = header ^ "<propname xmlns=\"" ^ Xml.dav_ns ^ "\"/>" in
  Alcotest.(check (result prop str_err) "missing propfind"
              (Error "expected propfind, but found propname")
              (Xml.parse_propfind_xml (tree xml))) ;
  let xml = header ^ "<propfind xmlns=\"" ^ Xml.dav_ns ^ "\"/>" in
  Alcotest.(check (result prop str_err) "empty propfind"
              (Error "broken")
              (Xml.parse_propfind_xml (tree xml))) ;
  let xml = header ^ "<propfind xmlns=\"" ^ Xml.dav_ns ^ "\"><foo/></propfind>" in
  Alcotest.(check (result prop str_err) "unexpected element foo"
              (Error "broken")
              (Xml.parse_propfind_xml (tree xml))) ;
  let xml = header ^ "<propfind xmlns=\"" ^ Xml.dav_ns ^ "\"><prop><foo><bar/></foo></prop></propfind>" in
  Alcotest.(check (result prop str_err) "non-flat property list"
              (Ok (`Props [ (Xml.dav_ns, "foo") ]))
              (Xml.parse_propfind_xml (tree xml)))

let parse_propfind_xml_tests = [
  "Empty", `Quick, empty_propfind ;
  "Propname", `Quick, propname ;
  "Two propnames", `Quick, two_propname ;
  "Allprop", `Quick, allprop ;
  "Allprop with includes", `Quick, allprop_include ;
  "Invalid XML", `Quick, invalid_xml ;
]

let calendar_query =
  let module M = struct
    type t = Xml.calendar_query
    let pp = Xml.pp_calendar_query
    let equal a b = compare a b = 0
  end in
  (module M : Alcotest.TESTABLE with type t = M.t)

let parse_simple_report_query () =
  let xml = header ^ {_|
   <C:calendar-query xmlns:D="DAV:"
                 xmlns:C="urn:ietf:params:xml:ns:caldav">
     <D:prop>
       <D:getetag/>
     </D:prop>
     <C:filter><C:comp-filter name="VCALENDAR"/></C:filter>
   </C:calendar-query>
|_} in
  let expected = Ok (Some (`Proplist [ `Prop (Xml.dav_ns, "getetag") ]),
                     ("VCALENDAR", `Is_defined)) in
  Alcotest.(check (result calendar_query string) __LOC__ expected
              (Xml.parse_calendar_query_xml (tree xml)))

let parse_simple_report_query_with_calendar_data () =
  let xml = header ^ {_|
   <C:calendar-query xmlns:D="DAV:"
                 xmlns:C="urn:ietf:params:xml:ns:caldav">
     <D:prop>
       <D:getetag/>
       <C:calendar-data>
         <C:comp name="VCALENDAR">
           <C:prop name="VERSION"/>
           <C:comp name="VEVENT">
             <C:prop name="SUMMARY"/>
             <C:prop name="UID"/>
             <C:prop name="DTSTART"/>
             <C:prop name="DTEND"/>
             <C:prop name="DURATION"/>
             <C:prop name="RRULE"/>
             <C:prop name="RDATE"/>
             <C:prop name="EXRULE"/>
             <C:prop name="EXDATE"/>
             <C:prop name="RECURRENCE-ID"/>
           </C:comp>
           <C:comp name="VTIMEZONE"/>
         </C:comp>
       </C:calendar-data>
     </D:prop>
     <C:filter><C:comp-filter name="VCALENDAR"/></C:filter>
   </C:calendar-query>
|_} in
  let expected =
    Ok (Some (`Proplist [
        `Prop (Xml.dav_ns, "getetag") ;
        `Calendar_data
          (Some ("VCALENDAR",
           `Prop [ ("VERSION", false) ],
           `Comp [ ("VEVENT",
                    `Prop [
                      ("SUMMARY", false) ;
                      ("UID", false) ;
                      ("DTSTART", false) ;
                      ("DTEND", false) ;
                      ("DURATION", false) ;
                      ("RRULE", false);
                      ("RDATE", false) ;
                      ("EXRULE", false) ;
                      ("EXDATE", false) ;
                      ("RECURRENCE-ID", false) ],
                    `Comp []) ;
                   ("VTIMEZONE", `Prop [], `Comp []) ])
            , None, None) ] ),
        ("VCALENDAR", `Is_defined))
  in
  Alcotest.(check (result calendar_query string) __LOC__ expected
              (Xml.parse_calendar_query_xml (tree xml)))

let report_7_8_1 = header ^ {|<C:calendar-query xmlns:D="DAV:"
                 xmlns:C="urn:ietf:params:xml:ns:caldav">
     <D:prop>
       <D:getetag/>
       <C:calendar-data>
         <C:comp name="VCALENDAR">
           <C:prop name="VERSION"/>
           <C:comp name="VEVENT">
             <C:prop name="SUMMARY"/>
             <C:prop name="UID"/>
             <C:prop name="DTSTART"/>
             <C:prop name="DTEND"/>
             <C:prop name="DURATION"/>
             <C:prop name="RRULE"/>
             <C:prop name="RDATE"/>
             <C:prop name="EXRULE"/>
             <C:prop name="EXDATE"/>
             <C:prop name="RECURRENCE-ID"/>
           </C:comp>
           <C:comp name="VTIMEZONE"/>
         </C:comp>
       </C:calendar-data>
     </D:prop>
     <C:filter>
       <C:comp-filter name="VCALENDAR">
         <C:comp-filter name="VEVENT">
           <C:time-range start="20060104T000000Z"
                         end="20060105T000000Z"/>
         </C:comp-filter>
       </C:comp-filter>
     </C:filter>
                          </C:calendar-query>|}

let parse_report_query_7_8_1 () =
  let xml = report_7_8_1
  and expected =
    Ok (Some (`Proplist [
        `Prop (Xml.dav_ns, "getetag") ;
        `Calendar_data (Some
          ("VCALENDAR",
           `Prop [ ("VERSION", false) ],
           `Comp [ ("VEVENT",
                    `Prop [
                      ("SUMMARY", false) ;
                      ("UID", false) ;
                      ("DTSTART", false) ;
                      ("DTEND", false) ;
                      ("DURATION", false) ;
                      ("RRULE", false);
                      ("RDATE", false) ;
                      ("EXRULE", false) ;
                      ("EXDATE", false) ;
                      ("RECURRENCE-ID", false) ],
                    `Comp []) ;
                   ("VTIMEZONE", `Prop [], `Comp []) ]), None, None) ] ),
        ("VCALENDAR", `Comp_filter (None, [], [ ("VEVENT", `Comp_filter (Some (to_ptime (2006,01,04) (00,00,00), to_ptime (2006,01,05) (00,00,00)), [], [])) ])))
  in
  Alcotest.(check (result calendar_query string) __LOC__ expected
              (Xml.parse_calendar_query_xml (tree xml)))

let report_7_8_2 =
  header ^ {|<C:calendar-query xmlns:D="DAV:"
                     xmlns:C="urn:ietf:params:xml:ns:caldav">
     <D:prop>
       <D:getetag/>
       <C:calendar-data>
         <C:limit-recurrence-set start="20060103T000000Z"
                                 end="20060105T000000Z"/>
       </C:calendar-data>
     </D:prop>
     <C:filter>
       <C:comp-filter name="VCALENDAR">
         <C:comp-filter name="VEVENT">
           <C:time-range start="20060103T000000Z"
                         end="20060105T000000Z"/>
         </C:comp-filter>
       </C:comp-filter>
     </C:filter>
                          </C:calendar-query>|}

let parse_report_query_7_8_2 () =
  let xml = report_7_8_2
  and expected =
    Ok (Some (`Proplist [
        `Prop (("DAV:", "getetag")); `Calendar_data (None, Some (`Limit_recurrence_set (to_ptime (2006,01,03) (00,00,00), to_ptime (2006,01,05) (00,00,00))), None) ] ),
        ("VCALENDAR", `Comp_filter (None, [], [ ("VEVENT", `Comp_filter (Some (to_ptime (2006,01,03) (00,00,00), to_ptime (2006,01,05) (00,00,00)), [], [])) ])))
  in
  Alcotest.(check (result calendar_query string) __LOC__ expected
              (Xml.parse_calendar_query_xml (tree xml)))

let report_7_8_3 =
  header ^ {|<C:calendar-query xmlns:D="DAV:"
                     xmlns:C="urn:ietf:params:xml:ns:caldav">
     <D:prop>
     <D:getetag/>
       <C:calendar-data>
         <C:expand start="20060103T000000Z"
                   end="20060105T000000Z"/>
       </C:calendar-data>
     </D:prop>
     <C:filter>
       <C:comp-filter name="VCALENDAR">
         <C:comp-filter name="VEVENT">
           <C:time-range start="20060103T000000Z"
                         end="20060105T000000Z"/>
         </C:comp-filter>
       </C:comp-filter>
     </C:filter>
   </C:calendar-query>|}

let parse_report_query_7_8_3 () =
  let xml = report_7_8_3
  and expected =
    Ok (Some (`Proplist [
        `Prop (("DAV:", "getetag"));
        `Calendar_data (None, Some (`Expand (to_ptime (2006,01,03) (00,00,00), to_ptime (2006,01,05) (00,00,00))), None) ] ),
        ("VCALENDAR", `Comp_filter (None, [], [ ("VEVENT", `Comp_filter (Some (to_ptime (2006,01,03) (00,00,00), to_ptime (2006,01,05) (00,00,00)), [], [])) ])))
  in
  Alcotest.(check (result calendar_query string) __LOC__ expected
              (Xml.parse_calendar_query_xml (tree xml)))

let report_7_8_4 =
  header ^ {|<C:calendar-query xmlns:D="DAV:"
                 xmlns:C="urn:ietf:params:xml:ns:caldav">
     <D:prop>
        <D:getetag/>
    <C:calendar-data>
         <C:limit-freebusy-set start="20060102T000000Z"
                                 end="20060103T000000Z"/>
       </C:calendar-data>
     </D:prop>
     <C:filter>
       <C:comp-filter name="VCALENDAR">
         <C:comp-filter name="VFREEBUSY">
           <C:time-range start="20060102T000000Z"
                           end="20060103T000000Z"/>
         </C:comp-filter>
       </C:comp-filter>
     </C:filter>
   </C:calendar-query>|}

let parse_report_query_7_8_4 () =
  let xml = report_7_8_4
  and expected =
    Ok (Some (`Proplist [
        `Prop (("DAV:", "getetag"));
        `Calendar_data (None, None, Some (`Limit_freebusy_set (to_ptime (2006,01,02) (00,00,00), to_ptime (2006,01,03) (00,00,00)))) ] ),
        ("VCALENDAR", `Comp_filter (None, [], [ ("VFREEBUSY", `Comp_filter (Some (to_ptime (2006,01,02) (00,00,00), to_ptime (2006,01,03) (00,00,00)), [], [])) ])))
  in
  Alcotest.(check (result calendar_query string) __LOC__ expected
              (Xml.parse_calendar_query_xml (tree xml)))

let report_7_8_5 =
  header ^ {|<C:calendar-query xmlns:C="urn:ietf:params:xml:ns:caldav">
     <D:prop xmlns:D="DAV:">
       <D:getetag/>
       <C:calendar-data/>
     </D:prop>
     <C:filter>
       <C:comp-filter name="VCALENDAR">
         <C:comp-filter name="VTODO">
           <C:comp-filter name="VALARM">
             <C:time-range start="20060106T100000Z"
                             end="20060107T100000Z"/>
           </C:comp-filter>
         </C:comp-filter>
       </C:comp-filter>
     </C:filter>
   </C:calendar-query>|}


let parse_report_query_7_8_5 () =
  let xml = report_7_8_5
  and expected =
    Ok (Some (`Proplist [ `Prop (Xml.dav_ns, "getetag"); `Calendar_data (None, None, None)]),
        ("VCALENDAR", `Comp_filter (None, [], [ ("VTODO", `Comp_filter ((None, [],
                                    [("VALARM",
                                      `Comp_filter (((Some (to_ptime (2006,01,06) (10,00,00),
                                                            to_ptime (2006,01,07) (10,00,00))),
                                                     [], [])))
                                      ])))
                     ])))
  in
  Alcotest.(check (result calendar_query string) __LOC__ expected
              (Xml.parse_calendar_query_xml (tree xml)))

let parse_report_query_7_8_6 () =
  let xml = header ^ {|<C:calendar-query xmlns:C="urn:ietf:params:xml:ns:caldav">
     <D:prop xmlns:D="DAV:">
       <D:getetag/>
       <C:calendar-data/>
     </D:prop>
     <C:filter>
       <C:comp-filter name="VCALENDAR">
         <C:comp-filter name="VEVENT">
           <C:prop-filter name="UID">
             <C:text-match collation="i;octet"
             >DC6C50A017428C5216A2F1CD@example.com</C:text-match>
           </C:prop-filter>
         </C:comp-filter>
       </C:comp-filter>
     </C:filter>
   </C:calendar-query>|}
  and expected =
    Ok (Some (`Proplist [ `Prop (Xml.dav_ns, "getetag"); `Calendar_data (None, None, None)]),
        ("VCALENDAR", `Comp_filter (None, [], [("VEVENT",
                     `Comp_filter ((None,
                                    [("UID",
                                      `Text (("DC6C50A017428C5216A2F1CD@example.com" , "i;octet", false), []))
                                      ],
                                    [])))
                     ])))
  in
  Alcotest.(check (result calendar_query string) __LOC__ expected
              (Xml.parse_calendar_query_xml (tree xml)))

let parse_report_query_7_8_7 () =
  let xml = header ^ {|<C:calendar-query xmlns:C="urn:ietf:params:xml:ns:caldav">
     <D:prop xmlns:D="DAV:">
       <D:getetag/>
       <C:calendar-data/>
     </D:prop>
     <C:filter>
       <C:comp-filter name="VCALENDAR">
         <C:comp-filter name="VEVENT">
           <C:prop-filter name="ATTENDEE">
             <C:text-match collation="i;ascii-casemap"
              >mailto:lisa@example.com</C:text-match>
             <C:param-filter name="PARTSTAT">
               <C:text-match collation="i;ascii-casemap"
                >NEEDS-ACTION</C:text-match>
             </C:param-filter>
           </C:prop-filter>
         </C:comp-filter>
       </C:comp-filter>
     </C:filter>
   </C:calendar-query>|}
  and expected =
    Ok (Some (`Proplist [ `Prop (Xml.dav_ns, "getetag"); `Calendar_data (None, None, None)]),
        ("VCALENDAR", `Comp_filter (None, [], [("VEVENT",
                     `Comp_filter ((None, [("ATTENDEE",
                                      `Text ((("mailto:lisa@example.com",
                                               "i;ascii-casemap", false),
                                              [`Param_filter (("PARTSTAT",
                                                               `Text_match (
                                                                 ("NEEDS-ACTION",
                                                                  "i;ascii-casemap",
                                                                  false))
                                                                 ))
                                                ])))
                                      ],
                                    [])))
                     ])))
  in
  Alcotest.(check (result calendar_query string) __LOC__ expected
              (Xml.parse_calendar_query_xml (tree xml)))


let parse_report_query_7_8_8 () =
  let xml = header ^ {|<C:calendar-query xmlns:C="urn:ietf:params:xml:ns:caldav">
     <D:prop xmlns:D="DAV:">
       <D:getetag/>
       <C:calendar-data/>
     </D:prop>
     <C:filter>
       <C:comp-filter name="VCALENDAR">
         <C:comp-filter name="VEVENT"/>
       </C:comp-filter>
     </C:filter>
   </C:calendar-query>|}
  and expected =
    Ok (Some (`Proplist [ `Prop (Xml.dav_ns, "getetag"); `Calendar_data (None, None, None)]),
       ("VCALENDAR", `Comp_filter ((None, [], [("VEVENT", `Is_defined)]))))
  in
  Alcotest.(check (result calendar_query string) __LOC__ expected
              (Xml.parse_calendar_query_xml (tree xml)))

let parse_report_query_7_8_9 () =
  let xml = header ^ {|<C:calendar-query xmlns:C="urn:ietf:params:xml:ns:caldav">
     <D:prop xmlns:D="DAV:">
       <D:getetag/>
       <C:calendar-data/>
     </D:prop>
     <C:filter>
       <C:comp-filter name="VCALENDAR">
         <C:comp-filter name="VTODO">
           <C:prop-filter name="COMPLETED">
             <C:is-not-defined/>
           </C:prop-filter>
           <C:prop-filter name="STATUS">
             <C:text-match
                negate-condition="yes">CANCELLED</C:text-match>
           </C:prop-filter>
         </C:comp-filter>
       </C:comp-filter>
     </C:filter>
   </C:calendar-query>|}
  and expected =
    Ok (Some (`Proplist [ `Prop (Xml.dav_ns, "getetag"); `Calendar_data (None, None, None)]),
       ("VCALENDAR", `Comp_filter ((None, [],
                   [("VTODO",
                     `Comp_filter ((None,
                                    [("COMPLETED", `Is_not_defined);
                                      ("STATUS",
                                       `Text ((("CANCELLED",
                                                "i;ascii-casemap", true),
                                               [])))
                                      ],
                                    [])))
                     ]))))
  in
  Alcotest.(check (result calendar_query string) __LOC__ expected
              (Xml.parse_calendar_query_xml (tree xml)))

let parse_report_query_7_8_10 () =
  let xml = header ^ {|<C:calendar-query xmlns:C="urn:ietf:params:xml:ns:caldav">
     <D:prop xmlns:D="DAV:">
       <D:getetag/>
       <C:calendar-data/>
     </D:prop>
     <C:filter>
       <C:comp-filter name="VCALENDAR">
         <C:comp-filter name="VEVENT">
           <C:prop-filter name="X-ABC-GUID">
             <C:text-match>ABC</C:text-match>
           </C:prop-filter>
         </C:comp-filter>
       </C:comp-filter>
     </C:filter>
   </C:calendar-query>|}
  and expected =
    Ok (Some (`Proplist [ `Prop (Xml.dav_ns, "getetag"); `Calendar_data (None, None, None)]),
       ("VCALENDAR", `Comp_filter ((None, [],
                   [("VEVENT",
                     `Comp_filter ((None,
                                    [("X-ABC-GUID",
                                      `Text ((("ABC", "i;ascii-casemap",
                                               false),
                                              [])))
                                      ],
                                    [])))
                     ]))))
  in
  Alcotest.(check (result calendar_query string) __LOC__ expected
              (Xml.parse_calendar_query_xml (tree xml)))

let report_parse_tests = [
  "Parse simple report query", `Quick, parse_simple_report_query ;
  "Parse simple report query with calendar data", `Quick, parse_simple_report_query_with_calendar_data ;
  "Parse report query from section 7.8.1", `Quick, parse_report_query_7_8_1 ;
  "Parse report query from section 7.8.2", `Quick, parse_report_query_7_8_2 ;
  "Parse report query from section 7.8.3", `Quick, parse_report_query_7_8_3 ;
  "Parse report query from section 7.8.4", `Quick, parse_report_query_7_8_4 ;
  "Parse report query from section 7.8.5", `Quick, parse_report_query_7_8_5 ;
  "Parse report query from section 7.8.6", `Quick, parse_report_query_7_8_6 ;
  "Parse report query from section 7.8.7", `Quick, parse_report_query_7_8_7 ;
  "Parse report query from section 7.8.8", `Quick, parse_report_query_7_8_8 ;
  "Parse report query from section 7.8.9", `Quick, parse_report_query_7_8_9 ;
  "Parse report query from section 7.8.10", `Quick, parse_report_query_7_8_10
]

let config = {
  principals = "principals" ;
  calendars = "calendars" ;
  host = Uri.of_string "http://cal.example.com" ;
  do_trust_on_first_use = false ;
}

let allow_all_acl = [ (`All, `Grant [ `All ]) ]

let appendix_b_data acl =
  let open Lwt.Infix in
  Lwt_main.run (
    let now = Ptime.v (1, 0L) in
    KV_mem.connect () >>= fun res_fs ->
    let props name = Properties.create_dir acl now name in
    Fs.mkdir res_fs (`Dir [ "bernard" ]) (props "bernard") >>= fun _ ->
    Fs.mkdir res_fs (`Dir [ "bernard" ; "work" ]) (props "bernard/work") >>= fun _ ->
    Lwt_list.iter_s (fun (fn, _etag, data) ->
        let props = Properties.create ~content_type:"text/calendar"
            acl now (String.length data) ("bernard/work/" ^ fn)
        in
        Fs.write res_fs (`File [ "bernard" ; "work" ; fn ])
          data props >|= fun _ ->
        ())
      Appendix_b.all >|= fun () ->
    res_fs)

let appendix_b_1_data acl =
  let open Lwt.Infix in
  Lwt_main.run (
    let now = Ptime.v (1, 0L) in
    KV_mem.connect () >>= fun res_fs ->
    let props name = Properties.create_dir acl now name in
    Fs.mkdir res_fs (`Dir [ "bernard" ]) (props "bernard") >>= fun _ ->
    Fs.mkdir res_fs (`Dir [ "bernard" ; "work" ]) (props "bernard/work") >>= fun _ ->
    (match Appendix_b.all with
     | [] -> assert false
     | (fn, _etag, data) :: _ ->
       let props = Properties.create ~content_type:"text/calendar"
           acl now (String.length data) ("bernard/work/" ^ fn)
       in
       Fs.write res_fs (`File [ "bernard" ; "work" ; fn ])
         data props >|= fun _ ->
       ())
      >|= fun () ->
    res_fs)

let t_tree =
  let module M = struct
    type t = Xml.tree
    let pp = Xml.pp_tree
    let equal = Xml.equal_tree
  end in
  (module M : Alcotest.TESTABLE with type t = M.t)

let report path body fs =
  let open Lwt.Infix in
  Lwt_main.run (
    Dav.report fs config ~path ~user:"nobody" ~data:body >|= function
    | Ok resp -> Ok (tree resp)
    | Error _ -> Error "failed")

let test_report_1 () =
  let xml = report_7_8_1
  and expected = header ^ {|
<D:multistatus xmlns:D="DAV:"></D:multistatus>|}
  in
  Alcotest.(check (result t_tree string) __LOC__
              (Ok (tree expected))
              (report "bernard/work" xml
                 (appendix_b_1_data allow_all_acl)))

let test_report_7_8_1 () =
  let xml = report_7_8_1
  and expected = header ^ {|
<D:multistatus xmlns:D="DAV:"
              xmlns:C="urn:ietf:params:xml:ns:caldav">
     <D:response>
       <D:href>/bernard/work/abcd2.ics</D:href>
       <D:propstat>
         <D:prop>
           <D:getetag>0f034329a44585adc51b4038070b968d</D:getetag>
           <C:calendar-data>BEGIN:VCALENDAR
VERSION:2.0
BEGIN:VTIMEZONE
LAST-MODIFIED:20040110T032845Z
TZID:US/Eastern
BEGIN:DAYLIGHT
DTSTART:20000404T020000
RRULE:FREQ=YEARLY;BYDAY=1SU;BYMONTH=4
TZNAME:EDT
TZOFFSETFROM:-0500
TZOFFSETTO:-0400
END:DAYLIGHT
BEGIN:STANDARD
DTSTART:20001026T020000
RRULE:FREQ=YEARLY;BYDAY=-1SU;BYMONTH=10
TZNAME:EST
TZOFFSETFROM:-0400
TZOFFSETTO:-0500
END:STANDARD
END:VTIMEZONE
BEGIN:VEVENT
UID:00959BC664CA650E933C892C@example.com
DTSTART;TZID=US/Eastern:20060102T120000
DURATION:PT1H
RRULE:FREQ=DAILY;COUNT=5
SUMMARY:Event #2
END:VEVENT
BEGIN:VEVENT
UID:00959BC664CA650E933C892C@example.com
DTSTART;TZID=US/Eastern:20060104T140000
DURATION:PT1H
RECURRENCE-ID;TZID=US/Eastern:20060104T120000
SUMMARY:Event #2 bis
END:VEVENT
BEGIN:VEVENT
UID:00959BC664CA650E933C892C@example.com
DTSTART;TZID=US/Eastern:20060106T140000
DURATION:PT1H
RECURRENCE-ID;TZID=US/Eastern:20060106T120000
SUMMARY:Event #2 bis bis
END:VEVENT
END:VCALENDAR
</C:calendar-data>
      </D:prop>
      <D:status>HTTP/1.1 200 OK</D:status>
    </D:propstat>
  </D:response>
  <D:response>
    <D:href>/bernard/work/abcd3.ics</D:href>
    <D:propstat>
      <D:prop>
        <D:getetag>f5ef220a9072ece829f086cfd9b137ba</D:getetag>
        <C:calendar-data>BEGIN:VCALENDAR
VERSION:2.0
BEGIN:VTIMEZONE
LAST-MODIFIED:20040110T032845Z
TZID:US/Eastern
BEGIN:DAYLIGHT
DTSTART:20000404T020000
RRULE:FREQ=YEARLY;BYDAY=1SU;BYMONTH=4
TZNAME:EDT
TZOFFSETFROM:-0500
TZOFFSETTO:-0400
END:DAYLIGHT
BEGIN:STANDARD
DTSTART:20001026T020000
RRULE:FREQ=YEARLY;BYDAY=-1SU;BYMONTH=10
TZNAME:EST
TZOFFSETFROM:-0400
TZOFFSETTO:-0500
END:STANDARD
END:VTIMEZONE
BEGIN:VEVENT
UID:DC6C50A017428C5216A2F1CD@example.com
DTSTART;TZID=US/Eastern:20060104T100000
DURATION:PT1H
SUMMARY:Event #3
END:VEVENT
END:VCALENDAR
</C:calendar-data>
         </D:prop>
         <D:status>HTTP/1.1 200 OK</D:status>
       </D:propstat>
     </D:response>
   </D:multistatus>
|}
  in
  Alcotest.(check (result t_tree string) __LOC__
              (Ok (tree expected))
              (report "bernard/work" xml
                 (appendix_b_data allow_all_acl)))


let test_report_7_8_2 () =
  let xml = report_7_8_2
  and expected = header ^ {|
   <D:multistatus xmlns:D="DAV:"
              xmlns:C="urn:ietf:params:xml:ns:caldav">
     <D:response>
       <D:href>/bernard/work/abcd2.ics</D:href>
       <D:propstat>
         <D:prop>
           <D:getetag>0f034329a44585adc51b4038070b968d</D:getetag>
           <C:calendar-data>BEGIN:VCALENDAR
VERSION:2.0
PRODID:-//Example Corp.//CalDAV Client//EN
BEGIN:VTIMEZONE
LAST-MODIFIED:20040110T032845Z
TZID:US/Eastern
BEGIN:DAYLIGHT
DTSTART:20000404T020000
RRULE:FREQ=YEARLY;BYDAY=1SU;BYMONTH=4
TZNAME:EDT
TZOFFSETFROM:-0500
TZOFFSETTO:-0400
END:DAYLIGHT
BEGIN:STANDARD
DTSTART:20001026T020000
RRULE:FREQ=YEARLY;BYDAY=-1SU;BYMONTH=10
TZNAME:EST
TZOFFSETFROM:-0400
TZOFFSETTO:-0500
END:STANDARD
END:VTIMEZONE
BEGIN:VEVENT
UID:00959BC664CA650E933C892C@example.com
DTSTAMP:20060206T001121Z
DTSTART;TZID=US/Eastern:20060102T120000
DURATION:PT1H
RRULE:FREQ=DAILY;COUNT=5
SUMMARY:Event #2
END:VEVENT
BEGIN:VEVENT
UID:00959BC664CA650E933C892C@example.com
DTSTAMP:20060206T001121Z
DTSTART;TZID=US/Eastern:20060104T140000
DURATION:PT1H
RECURRENCE-ID;TZID=US/Eastern:20060104T120000
SUMMARY:Event #2 bis
END:VEVENT
END:VCALENDAR
</C:calendar-data>
         </D:prop>
         <D:status>HTTP/1.1 200 OK</D:status>
       </D:propstat>
     </D:response>
     <D:response>
       <D:href>/bernard/work/abcd3.ics</D:href>
       <D:propstat>
         <D:prop>
           <D:getetag>f5ef220a9072ece829f086cfd9b137ba</D:getetag>
           <C:calendar-data>BEGIN:VCALENDAR
VERSION:2.0
PRODID:-//Example Corp.//CalDAV Client//EN
BEGIN:VTIMEZONE
LAST-MODIFIED:20040110T032845Z
TZID:US/Eastern
BEGIN:DAYLIGHT
DTSTART:20000404T020000
RRULE:FREQ=YEARLY;BYDAY=1SU;BYMONTH=4
TZNAME:EDT
TZOFFSETFROM:-0500
TZOFFSETTO:-0400
END:DAYLIGHT
BEGIN:STANDARD
DTSTART:20001026T020000
RRULE:FREQ=YEARLY;BYDAY=-1SU;BYMONTH=10
TZNAME:EST
TZOFFSETFROM:-0400
TZOFFSETTO:-0500
END:STANDARD
END:VTIMEZONE
BEGIN:VEVENT
UID:DC6C50A017428C5216A2F1CD@example.com
DTSTAMP:20060206T001220Z
DTSTART;TZID=US/Eastern:20060104T100000
DURATION:PT1H
ATTENDEE;PARTSTAT=ACCEPTED;ROLE=CHAIR:mailto:cyrus@example.com
ATTENDEE;PARTSTAT=NEEDS-ACTION:mailto:lisa@example.com
LAST-MODIFIED:20060206T001330Z
ORGANIZER:mailto:cyrus@example.com
SEQUENCE:1
STATUS:TENTATIVE
SUMMARY:Event #3
X-ABC-GUID:E1CX5Dr-0007ym-Hz@example.com
END:VEVENT
END:VCALENDAR
</C:calendar-data>
         </D:prop>
         <D:status>HTTP/1.1 200 OK</D:status>
       </D:propstat>
     </D:response>
   </D:multistatus>
 |}
  in
  Alcotest.(check (result t_tree string) __LOC__
              (Ok (tree expected))
              (report "bernard/work" xml
                 (appendix_b_data allow_all_acl)))

let test_report_7_8_3 () =
  let xml = report_7_8_3
  and expected = header ^ {|
   <D:multistatus xmlns:D="DAV:"
              xmlns:C="urn:ietf:params:xml:ns:caldav">
     <D:response>
       <D:href>/bernard/work/abcd2.ics</D:href>
       <D:propstat>
         <D:prop>
           <D:getetag>0f034329a44585adc51b4038070b968d</D:getetag>
           <C:calendar-data>BEGIN:VCALENDAR
VERSION:2.0
PRODID:-//Example Corp.//CalDAV Client//EN
BEGIN:VEVENT
UID:00959BC664CA650E933C892C@example.com
DTSTAMP:20060206T001121Z
DTSTART:20060103T170000Z
DURATION:PT1H
RECURRENCE-ID:20060103T170000Z
SUMMARY:Event #2
END:VEVENT
BEGIN:VEVENT
UID:00959BC664CA650E933C892C@example.com
DTSTAMP:20060206T001121Z
DTSTART:20060104T190000Z
DURATION:PT1H
RECURRENCE-ID:20060104T170000Z
SUMMARY:Event #2 bis
END:VEVENT
END:VCALENDAR
</C:calendar-data>
         </D:prop>
         <D:status>HTTP/1.1 200 OK</D:status>
       </D:propstat>
     </D:response>
     <D:response>
       <D:href>/bernard/work/abcd3.ics</D:href>
       <D:propstat>
         <D:prop>
           <D:getetag>f5ef220a9072ece829f086cfd9b137ba</D:getetag>
           <C:calendar-data>BEGIN:VCALENDAR
VERSION:2.0
PRODID:-//Example Corp.//CalDAV Client//EN
BEGIN:VEVENT
UID:DC6C50A017428C5216A2F1CD@example.com
DTSTAMP:20060206T001220Z
DTSTART:20060104T150000Z
DURATION:PT1H
ATTENDEE;PARTSTAT=ACCEPTED;ROLE=CHAIR:mailto:cyrus@example.com
ATTENDEE;PARTSTAT=NEEDS-ACTION:mailto:lisa@example.com
LAST-MODIFIED:20060206T001330Z
ORGANIZER:mailto:cyrus@example.com
SEQUENCE:1
STATUS:TENTATIVE
SUMMARY:Event #3
X-ABC-GUID:E1CX5Dr-0007ym-Hz@example.com
END:VEVENT
END:VCALENDAR
</C:calendar-data>
         </D:prop>
         <D:status>HTTP/1.1 200 OK</D:status>
       </D:propstat>
     </D:response>
   </D:multistatus>
 |}
  in
  Alcotest.(check (result t_tree string) __LOC__
              (Ok (tree expected))
              (report "bernard/work" xml
                 (appendix_b_data allow_all_acl)))

let test_report_7_8_4 () =
  let xml = report_7_8_4
  and expected = header ^ {|
<D:multistatus xmlns:D="DAV:"
                  xmlns:C="urn:ietf:params:xml:ns:caldav">
     <D:response>
       <D:href>/bernard/work/abcd8.ics</D:href>
       <D:propstat>
         <D:prop>
           <D:getetag>0556572c63ff541a1512cb3494ed1fe7</D:getetag>
           <C:calendar-data>BEGIN:VCALENDAR
VERSION:2.0
PRODID:-//Example Corp.//CalDAV Client//EN
BEGIN:VFREEBUSY
ORGANIZER;CN="Bernard Desruisseaux":mailto:bernard@example.com
UID:76ef34-54a3d2@example.com
DTSTAMP:20050530T123421Z
DTSTART:20060101T000000Z
DTEND:20060108T000000Z
FREEBUSY;FBTYPE=BUSY-TENTATIVE:20060102T100000Z/20060102T120000Z
END:VFREEBUSY
END:VCALENDAR
</C:calendar-data>
         </D:prop>
         <D:status>HTTP/1.1 200 OK</D:status>
       </D:propstat>
     </D:response>
   </D:multistatus>
 |}
  in
  Alcotest.(check (result t_tree string) __LOC__
              (Ok (tree expected))
              (report "bernard/work" xml
                 (appendix_b_data allow_all_acl)))

let test_report_7_8_5 () =
  let xml = report_7_8_5
  and expected = header ^ {|
<D:multistatus xmlns:D="DAV:"
                  xmlns:C="urn:ietf:params:xml:ns:caldav">
     <D:response>
       <D:href>/bernard/work/abcd5.ics</D:href>
       <D:propstat>
         <D:prop>
           <D:getetag>c8ef519bd9b7316a51611ebf2d99991b</D:getetag>
           <C:calendar-data>BEGIN:VCALENDAR
VERSION:2.0
PRODID:-//Example Corp.//CalDAV Client//EN
BEGIN:VTIMEZONE
LAST-MODIFIED:20040110T032845Z
TZID:US/Eastern
BEGIN:DAYLIGHT
DTSTART:20000404T020000
RRULE:FREQ=YEARLY;BYDAY=1SU;BYMONTH=4
TZNAME:EDT
TZOFFSETFROM:-0500
TZOFFSETTO:-0400
END:DAYLIGHT
BEGIN:STANDARD
DTSTART:20001026T020000
RRULE:FREQ=YEARLY;BYDAY=-1SU;BYMONTH=10
TZNAME:EST
TZOFFSETFROM:-0400
TZOFFSETTO:-0500
END:STANDARD
END:VTIMEZONE
BEGIN:VTODO
DTSTAMP:20060205T235300Z
DUE;TZID=US/Eastern:20060106T120000
LAST-MODIFIED:20060205T235308Z
SEQUENCE:1
STATUS:NEEDS-ACTION
SUMMARY:Task #2
UID:E10BA47467C5C69BB74E8720@example.com
BEGIN:VALARM
ACTION:AUDIO
TRIGGER;RELATED=END:-PT10M
END:VALARM
END:VTODO
END:VCALENDAR
</C:calendar-data>
         </D:prop>
         <D:status>HTTP/1.1 200 OK</D:status>
       </D:propstat>
     </D:response>
   </D:multistatus>
 |}
  in
  Alcotest.(check (result t_tree string) __LOC__
              (Ok (tree expected))
              (report "bernard/work" xml
                 (appendix_b_data allow_all_acl)))

let multiget_7_9_1 = header ^ {|
   <C:calendar-multiget xmlns:D="DAV:"
                    xmlns:C="urn:ietf:params:xml:ns:caldav">
     <D:prop>
       <D:getetag/>
       <C:calendar-data/>
     </D:prop>
     <D:href>/bernard/work/abcd1.ics</D:href>
     <D:href>/bernard/work/mtg1.ics</D:href>
   </C:calendar-multiget>|}

let calendar_multiget =
  let module M = struct
    type t = Xml.calendar_multiget
    let pp = Xml.pp_calendar_multiget
    let equal a b = compare a b = 0
  end in
  (module M : Alcotest.TESTABLE with type t = M.t)

let test_parse_multiget_7_9_1 () =
  let xml = multiget_7_9_1 in
  let expected =
    Ok
      ((Some (`Proplist ([`Prop (("DAV:", "getetag"));
                          `Calendar_data ((None, None, None))]))),
       ["/bernard/work/abcd1.ics"; "/bernard/work/mtg1.ics"])
  in
  Alcotest.(check (result calendar_multiget string) __LOC__ expected
              (Xml.parse_calendar_multiget_xml (tree xml)))

let test_multiget_7_9_1 () =
  let xml = multiget_7_9_1
  and expected = header ^ {|
   <D:multistatus xmlns:D="DAV:"
                  xmlns:C="urn:ietf:params:xml:ns:caldav">
     <D:response>
       <D:href>/bernard/work/abcd1.ics</D:href>
       <D:propstat>
         <D:prop>
           <D:getetag>9e4b2e35cd7588524c1285a57746c17b</D:getetag>
           <C:calendar-data>BEGIN:VCALENDAR
VERSION:2.0
PRODID:-//Example Corp.//CalDAV Client//EN
BEGIN:VTIMEZONE
LAST-MODIFIED:20040110T032845Z
TZID:US/Eastern
BEGIN:DAYLIGHT
DTSTART:20000404T020000
RRULE:FREQ=YEARLY;BYDAY=1SU;BYMONTH=4
TZNAME:EDT
TZOFFSETFROM:-0500
TZOFFSETTO:-0400
END:DAYLIGHT
BEGIN:STANDARD
DTSTART:20001026T020000
RRULE:FREQ=YEARLY;BYDAY=-1SU;BYMONTH=10
TZNAME:EST
TZOFFSETFROM:-0400
TZOFFSETTO:-0500
END:STANDARD
END:VTIMEZONE
BEGIN:VEVENT
UID:74855313FA803DA593CD579A@example.com
DTSTAMP:20060206T001102Z
DTSTART;TZID=US/Eastern:20060102T100000
DURATION:PT1H
SUMMARY:Event #1
Description:Go Steelers!
END:VEVENT
END:VCALENDAR
</C:calendar-data>
         </D:prop>
         <D:status>HTTP/1.1 200 OK</D:status>
       </D:propstat>
     </D:response>
     <D:response>
       <D:href>/bernard/work/mtg1.ics</D:href>
       <D:status>HTTP/1.1 404 Not Found</D:status>
     </D:response>
   </D:multistatus>|}
  in
  Alcotest.(check (result t_tree string) __LOC__
              (Ok (tree expected))
              (report "bernard/work" xml
                 (appendix_b_data allow_all_acl)))

let report_7_8_2_range =
  header ^ {|<C:calendar-query xmlns:D="DAV:"
                     xmlns:C="urn:ietf:params:xml:ns:caldav">
     <D:prop>
       <D:getetag/>
       <C:calendar-data>
       </C:calendar-data>
     </D:prop>
     <C:filter>
       <C:comp-filter name="VCALENDAR">
         <C:comp-filter name="VEVENT">
           <C:time-range start="20060102T143000Z"
                         end="20060102T153000Z"/>
         </C:comp-filter>
       </C:comp-filter>
     </C:filter>
                          </C:calendar-query>|}

let ics_example = "BEGIN:VCALENDAR\nVERSION:2.0\nPRODID:-//Example Corp.//CalDAV Client//EN\nBEGIN:VTIMEZONE\nLAST-MODIFIED:20040110T032845Z\nTZID:US/Eastern\nBEGIN:DAYLIGHT\nDTSTART:20000404T020000\nRRULE:FREQ=YEARLY;BYDAY=1SU;BYMONTH=4\nTZNAME:EDT\nTZOFFSETFROM:-0500\nTZOFFSETTO:-0400\nEND:DAYLIGHT\nBEGIN:STANDARD\nDTSTART:20001026T020000\nRRULE:FREQ=YEARLY;BYDAY=-1SU;BYMONTH=10\nTZNAME:EST\nTZOFFSETFROM:-0400\nTZOFFSETTO:-0500\nEND:STANDARD\nEND:VTIMEZONE\nBEGIN:VEVENT\nUID:74855313FA803DA593CD579A@example.com\nDTSTAMP:20060206T001102Z\nDTSTART;TZID=US/Eastern:20060102T100000\nDURATION:PT1H\nSUMMARY:Event #1\nDescription:Go Steelers!\nEND:VEVENT\nEND:VCALENDAR\n"

let test_report_7_8_2_range () =
  let xml = report_7_8_2_range
  and expected = (Xml.Node ("DAV:", "multistatus",
     [(("http://www.w3.org/2000/xmlns/", "D"), "DAV:");
       (("http://www.w3.org/2000/xmlns/", "C"),
        "urn:ietf:params:xml:ns:caldav")
       ],
     [(Xml.Node ("DAV:", "response", [],
         [(Xml.Node ("DAV:", "href", [],
             [(Xml.Pcdata
                 "/bernard/work/abcd1.ics")
               ]
             ));
           (Xml.Node ("DAV:", "propstat", [],
              [(Xml.Node ("DAV:", "prop", [],
                  [(Xml.Node ("DAV:", "getetag", [],
                      [(Xml.Pcdata "9e4b2e35cd7588524c1285a57746c17b")]));
                    (Xml.Node ("urn:ietf:params:xml:ns:caldav",
                       "calendar-data", [],
                       [(Xml.Pcdata ics_example)
                         ]
                       ))
                    ]
                  ));
                (Xml.Node ("DAV:", "status", [],
                   [(Xml.Pcdata "HTTP/1.1 200 OK")]))
                ]
              ))
           ]
         ))
       ]
     ))  in
  Alcotest.(check (result t_tree string) __LOC__
              (Ok expected)
              (report "bernard/work" xml
                 (appendix_b_data allow_all_acl)))


let report_tests = [
  "Report from section 7.8.1 - first file only", `Quick, test_report_1 ;
  "Report from section 7.8.1", `Quick, test_report_7_8_1 ;
  "Report from section 7.8.2", `Quick, test_report_7_8_2 ;
  "Report from section 7.8.3", `Quick, test_report_7_8_3 ;
  "Report from section 7.8.4", `Quick, test_report_7_8_4 ;
  "Report from section 7.8.5", `Quick, test_report_7_8_5 ;
(*  "Parse report query from section 7.8.6", `Quick, test_report_7_8_6 ;
  "Parse report query from section 7.8.7", `Quick, test_report_7_8_7 ;
  "Parse report query from section 7.8.8", `Quick, test_report_7_8_8 ;
  "Parse report query from section 7.8.9", `Quick, test_report_7_8_9 ;
    "Parse report query from section 7.8.10", `Quick, test_report_7_8_10 *)
  "Parse multiget report 7.9.1", `Quick, test_parse_multiget_7_9_1 ;
  "Multiget report 7.9.1", `Quick, test_multiget_7_9_1 ;
  "Test range for report 7.8.2, modified", `Quick, test_report_7_8_2_range ;
]

let propupdate =
  let module M = struct
    type t = Xml.propupdate
    let pp = Xml.pp_propupdate
    let equal a b = compare a b = 0
  end in
  (module M : Alcotest.TESTABLE with type t = M.t)


let proppatch () =
  let xml = header ^
    {|<D:propertyupdate xmlns:D="DAV:"
             xmlns:Z="http://ns.example.com/standards/z39.50/">
       <D:set>
         <D:prop>
           <Z:Authors>
             <Z:Author>Jim Whitehead</Z:Author>
             <Z:Author>Roy Fielding</Z:Author>
           </Z:Authors>
         </D:prop>
       </D:set>
       <D:remove>
         <D:prop><Z:Copyright-Owner/></D:prop>
       </D:remove>
      </D:propertyupdate>|}
  in
  Alcotest.(check (result (list propupdate) str_err) __LOC__
              (Ok [`Set ([],
                           ("http://ns.example.com/standards/z39.50/", "Authors"),
                           [Node ("http://ns.example.com/standards/z39.50/", "Author", [], [ Pcdata "Jim Whitehead"]) ;
                            Node ("http://ns.example.com/standards/z39.50/", "Author", [], [ Pcdata "Roy Fielding" ]) ]) ;
                     `Remove ("http://ns.example.com/standards/z39.50/", "Copyright-Owner") ])
    (Xml.parse_propupdate_xml (tree xml)))

let parse_propupdate_xml_tests = [
  "propertyupdate RFC example", `Quick, proppatch
]

let state_testable =
  let module M = struct
    type t = Fs.t
    let pp = KV_mem.pp
    let equal = KV_mem.equal
  end in
  (module M : Alcotest.TESTABLE with type t = M.t)

let err_testable =
  let module M = struct
    type t = [ `Bad_request | `Conflict | `Forbidden of string ]
    let pp ppf = function
      | `Bad_request -> Fmt.string ppf "bad request"
      | `Conflict -> Fmt.string ppf "conflict"
      | `Forbidden s -> Fmt.pf ppf "forbidden %s" s 
    let equal a b = match a, b with
      | `Bad_request, `Bad_request -> true
      | `Conflict, `Conflict -> true
      | `Forbidden x, `Forbidden y -> String.equal x y
      | _ -> false
  end in
  (module M : Alcotest.TESTABLE with type t = M.t)


let mkcol_success () =
  let open Lwt.Infix in
  let body = {|<?xml version="1.0" encoding="utf-8" ?>
   <D:mkcol xmlns:D="DAV:"
                 xmlns:E="http://example.com/ns/">
     <D:set>
       <D:prop>
         <D:resourcetype>
           <D:collection/>
           <E:special-resource/>
         </D:resourcetype>
         <D:displayname>Special Resource</D:displayname>
       </D:prop>
     </D:set>
               </D:mkcol>|}
  in
  let res_fs, r =
    Lwt_main.run (
      let now = Ptime.v (1, 0L) in
      KV_mem.connect () >>= fun res_fs ->
      let props =
        let resourcetype = [ Xml.node ~ns:"http://example.com/ns/" "special-resource" [] ] in
        let acl = [ (`Href (Uri.of_string "/principals/testuser/"), `Grant [`All])] in
        Properties.create_dir ~resourcetype acl now "Special Resource"
      in
      let properties = Properties.create_dir allow_all_acl now "home" in
      Fs.mkdir res_fs (`Dir ["home"]) properties >>= fun _ ->
      Fs.mkdir res_fs (`Dir [ "home" ; "special" ]) props >>= fun _ ->
      KV_mem.connect () >>= fun fs ->
      Fs.mkdir fs (`Dir ["home"]) properties >>= fun _ ->
      Dav.mkcol fs config ~path:"home/special/" ~user:"testuser" (`Other "MKCOL") now ~data:body >|= function
      | Error e -> (res_fs, Error e)
      | Ok () -> (res_fs, Ok fs))
  in
  Alcotest.(check (result state_testable err_testable) __LOC__
              (Ok res_fs) r)

let delete_test () =
  let res_fs, r =
    Lwt_main.run (
      let open Lwt.Infix in
      KV_mem.connect () >>= fun res_fs ->
      let creation_time = Ptime.v (1, 0L) in
      let resourcetype = [ Xml.node ~ns:"http://example.com/ns/" "special-resource" [] ] in
      let dir_props = Properties.create_dir ~resourcetype [] creation_time "Special Resource" in
      Fs.write_property_map res_fs (`Dir []) dir_props >>= fun _ -> 
      Fs.mkdir res_fs (`Dir ["parent"]) dir_props >>= fun _ ->
      KV_mem.connect () >>= fun fs ->
      let updated_time = Ptime.v (10, 0L) in
      let dir_props' = Properties.unsafe_add (Xml.dav_ns, "getlastmodified") ([], [ Pcdata (Ptime.to_rfc3339 updated_time) ]) dir_props in
      let dir_props'' = Properties.unsafe_add (Xml.dav_ns, "getetag") ([], [ Pcdata "01dd76faf69851ed6896ae419391363c" ]) dir_props' in
      Fs.write_property_map fs (`Dir []) dir_props'' >>= fun _ -> 
      Dav.delete res_fs ~path:"parent" updated_time >|= fun deleted ->
      (fs, res_fs))
  in
  Alcotest.(check state_testable __LOC__ res_fs r)

let delete_and_update_parent_mtime_and_etag () =
  let res_fs, r =
    Lwt_main.run (
      let open Lwt.Infix in
      KV_mem.connect () >>= fun res_fs ->
      let creation_time = Ptime.v (1, 0L) in
      let resourcetype = [ Xml.node ~ns:"http://example.com/ns/" "special-resource" [] ] in
      let initial_props = [ ((Xml.dav_ns, "getetag"), ([], [Xml.Pcdata "myetag"]))] in
      let dir_props = Properties.create_dir ~initial_props ~resourcetype [] creation_time "Special Resource"
      and file_props = Properties.create [] creation_time 0 "Special Resource" in 
      Fs.write_property_map res_fs (`Dir []) dir_props >>= fun _ -> 
      Fs.mkdir res_fs (`Dir ["parent"]) dir_props >>= fun _ ->
      Fs.write res_fs (`File ["parent" ; "child"]) "" file_props >>= fun _ ->
      KV_mem.connect () >>= fun fs ->
      let updated_time = Ptime.v (10, 0L) in
      let dir_props' = Properties.unsafe_add (Xml.dav_ns, "getlastmodified") ([], [ Pcdata (Ptime.to_rfc3339 updated_time) ]) dir_props in
      let dir_props'' = Properties.unsafe_add (Xml.dav_ns, "getetag") ([], [ Pcdata "01dd76faf69851ed6896ae419391363c" ]) dir_props' in
      Fs.write_property_map fs (`Dir []) dir_props >>= fun _ -> 
      Fs.mkdir fs (`Dir ["parent"]) dir_props'' >>= fun _ ->
      Dav.delete res_fs ~path:"parent/child" updated_time >|= fun deleted ->
      (fs, res_fs))
  in
  Alcotest.(check state_testable __LOC__ res_fs r)

let write_and_update_parent_mtime () =
  let res_fs, r =
    Lwt_main.run (
      let open Lwt.Infix in
      KV_mem.connect () >>= fun res_fs ->
      let creation_time = Ptime.v (1, 0L) in
      let resourcetype = [ Xml.node ~ns:"http://example.com/ns/" "special-resource" [] ; Xml.node ~ns:Xml.caldav_ns "calendar" [] ] in
      let dir_props = Properties.create_dir ~resourcetype allow_all_acl creation_time "Special Resource" in
      Fs.write_property_map res_fs (`Dir []) dir_props >>= fun _ -> 
      Fs.mkdir res_fs (`Dir ["principals"]) dir_props >>= fun _ ->
      Fs.mkdir res_fs (`Dir ["principals" ; "karl"]) dir_props >>= fun _ ->
      Fs.mkdir res_fs (`Dir ["parent"]) dir_props >>= fun _ ->
      KV_mem.connect () >>= fun fs ->
      let updated_time = Ptime.v (20, 0L) in
      let dir_props' = Properties.unsafe_add (Xml.dav_ns, "getlastmodified") ([], [ Xml.Pcdata (Ptime.to_rfc3339 updated_time) ]) dir_props in
      let dir_props'' = Properties.unsafe_add (Xml.dav_ns, "getetag") ([], [ Xml.Pcdata "7f3af2eea3e815059f400874ebbad45c" ]) dir_props' in
      let initial_props = [ ((Xml.dav_ns, "getetag"), ([], [ Xml.Pcdata "fac1da4d0bf639292a15d5a98c9b02e8"])) ] in
      let file_props = Properties.create ~content_type:"text/calendar" ~initial_props [(`All, `Inherited (Uri.of_string "/parent/"))] updated_time 654 "/parent/child" in 
      Fs.write_property_map fs (`Dir []) dir_props >>= fun _ -> 
      Fs.mkdir fs (`Dir ["principals"]) dir_props >>= fun _ ->
      Fs.mkdir fs (`Dir ["principals" ; "karl"]) dir_props >>= fun _ ->
      Fs.mkdir fs (`Dir ["parent"]) dir_props'' >>= fun _ -> (* getlastmodified needs to be updated since we wrote a child in parent *)
      let ics = Astring.String.fold_left (fun str -> function '\n' -> str ^ "\r\n" | c -> str ^ String.make 1 c ) "" ics_example in
      Fs.write fs (`File ["parent"; "child"]) ics file_props >>= fun _ ->
      let user = "karl" in
      let data = ics_example in
      Dav.write_component res_fs config ~content_type:"text/calendar" ~path:"parent/child" ~user ~data updated_time >|= fun r ->
      ( match r with
      | Ok _ -> ()
      | Error `Bad_request -> Printf.printf "bad request \n"
      | Error `Conflict -> Printf.printf "conflict \n"
      | Error `Forbidden -> Printf.printf "forbidden \n"
      | Error `Internal_server_error -> Printf.printf "internal server error \n");
      (fs, res_fs))
  in
  Alcotest.(check state_testable __LOC__ res_fs r)


let proppatch_success () =
  let open Lwt.Infix in
  let body = {|<?xml version="1.0" encoding="utf-8" ?>
   <D:propertyupdate xmlns:D="DAV:">
     <D:set>
       <D:prop>
         <D:displayname>Special Resource</D:displayname>
       </D:prop>
     </D:set>
               </D:propertyupdate>|}
  in
  let res_fs, r =
    Lwt_main.run (
      let now = Ptime.v (1, 0L) in
      KV_mem.connect () >>= fun res_fs ->
      let properties = Properties.create_dir allow_all_acl now "home" in
      let props = Properties.unsafe_add (Xml.dav_ns, "displayname") ([], [ Xml.Pcdata "Special Resource"]) properties in
      Fs.mkdir res_fs (`Dir ["home"]) props >>= fun _ ->
      KV_mem.connect () >>= fun fs ->
      Fs.mkdir fs (`Dir ["home"]) properties >>= fun _ ->
      Dav.proppatch fs config ~path:"home" ~user:"testuser" ~data:body >|= function
      | Error e -> (res_fs, Error e)
      | Ok _ -> (res_fs, Ok fs))
  in
  Alcotest.(check (result state_testable err_testable) __LOC__
              (Ok res_fs) r)


let webdav_api_tests = [
  "successful mkcol", `Quick, mkcol_success ;
  "delete", `Quick, delete_test ;
  "delete and update parent mtime", `Quick, delete_and_update_parent_mtime_and_etag ;
  "write and update parent mtime", `Quick, write_and_update_parent_mtime ;
  "proppatch", `Quick, proppatch_success ;
]

let principal_url principal = Uri.with_path config.host (Fs.to_string (`Dir [ config.principals ; principal ]))

let test_fs_with_acl path acl user user_props = Lwt_main.run (
  let open Lwt.Infix in
  KV_mem.connect () >>= fun fs ->
  let props = Properties.create_dir acl (Ptime_clock.now ()) path in
  Fs.mkdir fs (`Dir [path]) props >>= fun _ ->
  Fs.mkdir fs (`Dir [config.principals ; user] ) user_props >|= fun _ ->
  fs)

let deny_all = "deny all", [ (`All, `Deny [ `All ]) ]
let grant_all ="grant all",  [ (`All, `Grant [ `All ]) ]

let grant_read_write =
  let url = principal_url "read-write" in
  "grant for user test", [ (`Href url, `Grant [ `Read ]) ; (`Href url, `Grant [ `Write ]) ]

let grant_read =
  let url = principal_url "read" in
  "grant for user read", [ (`Href url, `Grant [ `Read ]) ]

let grant_read_acl =
  let url = principal_url "read-acl" in
  "grant for user read-acl", [ (`Href url, `Grant [ `Read_acl ]) ]

let grant_bind =
  let url = principal_url "bind" in
  "grant for user bind", [ (`Href url, `Grant [ `Bind ]) ]

let grant_unbind =
  let url = principal_url "unbind" in
  "grant for user unbind", [ (`Href url, `Grant [ `Unbind ]) ]

let grant_write =
  let url = principal_url "write" in
  "grant for user write", [ (`Href url, `Grant [ `Write ]) ]

let grant_write_properties =
  let url = principal_url "write-props" in
  "grant for user write-props", [ (`Href url, `Grant [ `Write_properties ]) ]

let grant_write_content =
  let url = principal_url "write-content" in
  "grant for user write-content", [ (`Href url, `Grant [ `Write_content ]) ]

let grant_write_acl =
  let url = principal_url "write-acl" in
  "grant for user write-acl", [ (`Href url, `Grant [ `Write_acl ]) ]

let grant_read_current_user_privilege_set =
  let url = principal_url "read-current-user-privilege-set" in
  "grant for user read-current-user-privilege-set", [ (`Href url, `Grant [ `Read_current_user_privilege_set ]) ]

let make_properties key prop =
  Properties.unsafe_add (Xml.dav_ns, "getlastmodified")
    ([], [ Xml.Pcdata (Ptime.to_rfc3339 (Ptime_clock.now ())) ])
    (Properties.unsafe_add key prop Properties.empty)

let user principal =
  principal,
  make_properties
    (Xml.dav_ns, "principal-URL")
    ([], [Xml.dav_node "href" [ Xml.Pcdata (Uri.to_string @@ principal_url principal) ]])

let test_cases_for_get = [
  (grant_all, user "any", true);
  (deny_all, user "any", false);
  (grant_read_write, user "read-write", true); (grant_read_write, user "invader", false);
  (grant_read, user "read", true); (grant_read, user "invader", false);
  (grant_read_acl, user "read-acl", false); (grant_read_acl, user "invader", false);
  (grant_bind, user "bind", false); (grant_bind, user "invader", false);
  (grant_unbind, user "unbind", false); (grant_unbind, user "invader", false);
  (grant_write, user "write", false); (grant_write, user "invader", false);
  (grant_write_properties, user "write-props", false); (grant_write_properties, user "invader", false);
  (grant_write_content, user "write-content", false); (grant_write_content, user "invader", false);
  (grant_write_acl, user "write-acl", false); (grant_write_acl, user "invader", false);
]

let test_cases_for_put_target_exists = [
  (grant_all, user "any", true);
  (deny_all, user "any", false);
  (grant_read_write, user "read-write", true); (grant_read_write, user "invader", false);
  (grant_read, user "read", false); (grant_read, user "invader", false);
  (grant_read_acl, user "read-acl", false); (grant_read_acl, user "invader", false);
  (grant_bind, user "bind", false); (grant_bind, user "invader", false);
  (grant_unbind, user "unbind", false); (grant_unbind, user "invader", false);
  (grant_write, user "write", true); (grant_write, user "invader", false);
  (grant_write_properties, user "write-props", false); (grant_write_properties, user "invader", false);
  (grant_write_content, user "write-content", true); (grant_write_content, user "invader", false);
  (grant_write_acl, user "write-acl", false); (grant_write_acl, user "invader", false);
]

let test_cases_for_put_not_target_exists = [
  (grant_all, user "any", true);
  (deny_all, user "any", false);
  (grant_read_write, user "read-write", true); (grant_read_write, user "invader", false);
  (grant_read, user "read", false); (grant_read, user "invader", false);
  (grant_read_acl, user "read-acl", false); (grant_read_acl, user "invader", false);
  (grant_bind, user "bind", true); (grant_bind, user "invader", false);
  (grant_unbind, user "unbind", false); (grant_unbind, user "invader", false);
  (grant_write, user "write", true); (grant_write, user "invader", false);
  (grant_write_properties, user "write-props", false); (grant_write_properties, user "invader", false);
  (grant_write_content, user "write-content", false); (grant_write_content, user "invader", false);
  (grant_write_acl, user "write-acl", false); (grant_write_acl, user "invader", false);
]

let test_cases_for_proppatch = [
  (grant_all, user "any", true);
  (deny_all, user "any", false);
  (grant_read_write, user "read-write", true); (grant_read_write, user "invader", false);
  (grant_read, user "read", false); (grant_read, user "invader", false);
  (grant_read_acl, user "read-acl", false); (grant_read_acl, user "invader", false);
  (grant_bind, user "bind", false); (grant_bind, user "invader", false);
  (grant_unbind, user "unbind", false); (grant_unbind, user "invader", false);
  (grant_write, user "write", true); (grant_write, user "invader", false);
  (grant_write_properties, user "write-props", true); (grant_write_properties, user "invader", false);
  (grant_write_content, user "write-content", false); (grant_write_content, user "invader", false);
  (grant_write_acl, user "write-acl", false); (grant_write_acl, user "invader", false);
]

let test_cases_for_acl = [
  (grant_all, user "any", true);
  (deny_all, user "any", false);
  (grant_read_write, user "read-write", true); (grant_read_write, user "invader", false);
  (grant_read, user "read", false); (grant_read, user "invader", false);
  (grant_read_acl, user "read-acl", false); (grant_read_acl, user "invader", false);
  (grant_bind, user "bind", false); (grant_bind, user "invader", false);
  (grant_unbind, user "unbind", false); (grant_unbind, user "invader", false);
  (grant_write, user "write", true); (grant_write, user "invader", false);
  (grant_write_properties, user "write-props", false); (grant_write_properties, user "invader", false);
  (grant_write_content, user "write-content", false); (grant_write_content, user "invader", false);
  (grant_write_acl, user "write-acl", true); (grant_write_acl, user "invader", false);
]

let test_cases_for_delete = [
  (grant_all, user "any", true);
  (deny_all, user "any", false);
  (grant_read_write, user "read-write", true); (grant_read_write, user "invader", false);
  (grant_read, user "read", false); (grant_read, user "invader", false);
  (grant_read_acl, user "read-acl", false); (grant_read_acl, user "invader", false);
  (grant_bind, user "bind", false); (grant_bind, user "invader", false);
  (grant_unbind, user "unbind", true); (grant_unbind, user "invader", false);
  (grant_write, user "write", true); (grant_write, user "invader", false);
  (grant_write_properties, user "write-props", false); (grant_write_properties, user "invader", false);
  (grant_write_content, user "write-content", false); (grant_write_content, user "invader", false);
  (grant_write_acl, user "write-acl", false); (grant_write_acl, user "invader", false);
]

let test_cases_for_mkcol = [
  (grant_all, user "any", true);
  (deny_all, user "any", false);
  (grant_read_write, user "read-write", true); (grant_read_write, user "invader", false);
  (grant_read, user "read", false); (grant_read, user "invader", false);
  (grant_read_acl, user "read-acl", false); (grant_read_acl, user "invader", false);
  (grant_bind, user "bind", true); (grant_bind, user "invader", false);
  (grant_unbind, user "unbind", false); (grant_unbind, user "invader", false);
  (grant_write, user "write", true); (grant_write, user "invader", false);
  (grant_write_properties, user "write-props", false); (grant_write_properties, user "invader", false);
  (grant_write_content, user "write-content", false); (grant_write_content, user "invader", false);
  (grant_write_acl, user "write-acl", false); (grant_write_acl, user "invader", false);
]

(* HTTP verb * requested_path * (access control lists * authenticated user * expected result) list *)
let acl_test_cases = [
  `GET,     "calendars", test_cases_for_get ;
  `HEAD,    "calendars", test_cases_for_get ;
  `OPTIONS, "calendars", test_cases_for_get ;
  `PUT,     "calendars", test_cases_for_put_target_exists ;
  `PUT,     "calendars/non_existing", test_cases_for_put_not_target_exists ;
  `Other "PROPPATCH", "calendars", test_cases_for_proppatch ;
  `Other "ACL", "calendars", test_cases_for_acl ;
  `Other "PROPFIND", "calendars", test_cases_for_get ;
  `DELETE, "calendars/something", test_cases_for_delete ;
  `Other "MKCOL", "calendars/something_new", test_cases_for_mkcol ;
  `Other "MKCALENDAR", "calendars/something_new", test_cases_for_mkcol ;
  `Other "REPORT", "calendars", test_cases_for_get ;
]

let request_calendars_for_acl http_verb request_path aces user user_props res () =
  let path = "calendars" in
  let fs = test_fs_with_acl path aces user user_props in
  Alcotest.(check bool __LOC__ res (Lwt_main.run @@ Dav.access_granted_for_acl fs config http_verb ~path:request_path ~user))

let webdav_acl_tests (http_verb, request_path, acls_results) =
  let http_verb_str = Sexplib.Sexp.to_string_hum @@ Cohttp.Code.sexp_of_meth http_verb in
  let test ((ace_str, aces), (user, user_props), result) =
    Printf.sprintf "%s %s %s, principal %s" http_verb_str request_path ace_str user,
    `Quick,
    request_calendars_for_acl http_verb request_path aces user user_props result
  in
  List.map test acls_results

let check_status =
  let module M = struct
    type t = Cohttp.Code.status_code
    let pp fmt status = Fmt.string fmt (Cohttp.Code.string_of_status status)
    let equal a b = compare a b = 0
  end in
  (module M : Alcotest.TESTABLE with type t = M.t)

let find_many_acl_props_empty_forbidden () =
  let userprops = Properties.empty
  and fqname = [ (Xml.dav_ns, "acl") ]
  and properties = Properties.empty
  and expected = [ (`Forbidden, [ Xml.dav_node "acl" [] ]) ] in
  Alcotest.(check (list (pair check_status (list t_tree))) __LOC__ expected (Properties.find_many userprops properties fqname))

let find_many_acl aces () =
  let userprops = snd @@ user "read-acl"
  and fqname = (Xml.dav_ns, "acl")
  and aces_xml = List.map Xml.ace_to_xml (snd aces) in
  let properties = make_properties fqname ([], aces_xml) in
  let expected = [ (`OK, [ Xml.dav_node "acl" aces_xml ]) ] in
  Alcotest.(check (list (pair check_status (list t_tree))) __LOC__ expected (Properties.find_many userprops properties [fqname]))

let find_many_acl_forbidden principal aces () =
  let userprops = snd @@ user principal
  and fqname = (Xml.dav_ns, "acl")
  and aces_xml = List.map Xml.ace_to_xml (snd aces) in
  let properties = make_properties fqname ([], aces_xml) in
  let expected = [ (`Forbidden, [ Xml.dav_node "acl" [] ]) ] in
  Alcotest.(check (list (pair check_status (list t_tree))) __LOC__ expected (Properties.find_many userprops properties [fqname]))

let find_many_current_user_privset_empty_forbidden () =
  let userprops = Properties.empty
  and fqname = [ (Xml.dav_ns, "current-user-privilege-set") ]
  and properties = Properties.empty
  and expected = [ (`Forbidden, [ Xml.dav_node "current-user-privilege-set" [] ]) ] in
  Alcotest.(check (list (pair check_status (list t_tree))) __LOC__ expected (Properties.find_many userprops properties fqname))

let find_many_current_user_privset_forbidden principal aces () =
  let userprops = snd @@ user principal
  and fqname = [ (Xml.dav_ns, "current-user-privilege-set") ]
  and aces_xml = List.map Xml.ace_to_xml (snd aces) in
  let properties = make_properties (Xml.dav_ns, "acl") ([], aces_xml) in
  let expected = [ (`Forbidden, [ Xml.dav_node "current-user-privilege-set" [] ]) ] in
  Alcotest.(check (list (pair check_status (list t_tree))) __LOC__ expected (Properties.find_many userprops properties fqname))

let find_many_current_user_privset principal aces privileges () =
  let userprops = snd @@ user principal
  and fqname = (Xml.dav_ns, "current-user-privilege-set")
  and aces_xml = List.map Xml.ace_to_xml (snd aces) in
  let properties = make_properties (Xml.dav_ns, "acl") ([], aces_xml) in
  let privilege_set = List.map (fun p -> Xml.dav_node "privilege" [Xml.priv_to_xml p]) privileges in
  let expected = [ (`OK, [ Xml.dav_node "current-user-privilege-set" privilege_set ]) ] in
  Alcotest.(check (list (pair check_status (list t_tree))) __LOC__ expected (Properties.find_many userprops properties [fqname]))


let properties_find_many_tests = [
  "Find many for acl, forbidden", `Quick, find_many_acl_props_empty_forbidden ;
  "Find many for acl", `Quick, find_many_acl grant_read_acl ;
  "Find many for acl, invader can do nothing, read-acl can read_acl", `Quick, find_many_acl_forbidden "invader" grant_read_acl;
  "Find many for acl, invader can only do current-user-privilege-set, but reads acl", `Quick, find_many_acl_forbidden "read-current-user-privilege-set" grant_read_current_user_privilege_set;
  "Find many for acl, read can read - wants to read acl", `Quick, find_many_acl_forbidden "read" grant_read ;
  "Find many for acl, grant_all", `Quick, find_many_acl grant_all ;
  "Find many for current-user-privilege-set, read can read - wants to read current user privset", `Quick, find_many_current_user_privset_forbidden "read" grant_read ;
  "Find many for current-user-privilege-set, read can read, invader can do nothing - wants to read current user privset", `Quick, find_many_current_user_privset_forbidden "invader" grant_read ;
  "Find many for current-user-privilege-set, forbidden", `Quick, find_many_current_user_privset_empty_forbidden ;
  "Find many for current-user-privilege-set", `Quick, find_many_current_user_privset "read-current-user-privilege-set" grant_read_current_user_privilege_set [`Read_current_user_privilege_set] ;
  "Find many for current-user-privilege-set, user: read-acl", `Quick, find_many_current_user_privset "read-acl" grant_read_acl [`Read_acl];
  "Find many for current-user-privilege-set, user: somebody", `Quick, find_many_current_user_privset "somebody" grant_all [
    `Read ; `Write ; `Read_current_user_privilege_set ; `Write_content ; `Write_properties ; `Bind ; `Unbind ; `All ] ;
]

let test_report_1_deny () =
  let xml = report_7_8_1
  and expected =
    Xml.dav_node "multistatus" ~a:[(("http://www.w3.org/2000/xmlns/", "D"), "DAV:")]
      [ Xml.dav_node "response"
          [ Xml.dav_node "href" [ Xml.Pcdata "/bernard/work/abcd1.ics" ] ;
            Xml.dav_node "status" [ Xml.Pcdata "HTTP/1.1 403 Forbidden" ] ]]
  in
  Alcotest.(check (result t_tree string) __LOC__
              (Ok expected)
              (report "bernard/work" xml
                 (appendix_b_1_data @@ snd deny_all)))

let test_report_7_8_1_deny () =
  let xml = report_7_8_1
  and expected = header ^ {|
<D:multistatus xmlns:D="DAV:">
     <D:response>
       <D:href>/bernard/work/abcd1.ics</D:href>
       <D:status>HTTP/1.1 403 Forbidden</D:status>
     </D:response>
     <D:response>
       <D:href>/bernard/work/abcd2.ics</D:href>
       <D:status>HTTP/1.1 403 Forbidden</D:status>
     </D:response>
     <D:response>
       <D:href>/bernard/work/abcd3.ics</D:href>
       <D:status>HTTP/1.1 403 Forbidden</D:status>
     </D:response>
     <D:response>
       <D:href>/bernard/work/abcd4.ics</D:href>
       <D:status>HTTP/1.1 403 Forbidden</D:status>
     </D:response>
     <D:response>
       <D:href>/bernard/work/abcd5.ics</D:href>
       <D:status>HTTP/1.1 403 Forbidden</D:status>
     </D:response>
     <D:response>
       <D:href>/bernard/work/abcd6.ics</D:href>
       <D:status>HTTP/1.1 403 Forbidden</D:status>
     </D:response>
     <D:response>
       <D:href>/bernard/work/abcd7.ics</D:href>
       <D:status>HTTP/1.1 403 Forbidden</D:status>
     </D:response>
     <D:response>
       <D:href>/bernard/work/abcd8.ics</D:href>
       <D:status>HTTP/1.1 403 Forbidden</D:status>
     </D:response>
   </D:multistatus>
|}
  in
  Alcotest.(check (result t_tree string) __LOC__
              (Ok (tree expected))
              (report "bernard/work" xml
                 (appendix_b_data @@ snd deny_all)))

let test_report_7_8_2_deny () =
  let xml = report_7_8_2
  and expected = header ^ {|
<D:multistatus xmlns:D="DAV:">
     <D:response>
       <D:href>/bernard/work/abcd1.ics</D:href>
       <D:status>HTTP/1.1 403 Forbidden</D:status>
     </D:response>
     <D:response>
       <D:href>/bernard/work/abcd2.ics</D:href>
       <D:status>HTTP/1.1 403 Forbidden</D:status>
     </D:response>
     <D:response>
       <D:href>/bernard/work/abcd3.ics</D:href>
       <D:status>HTTP/1.1 403 Forbidden</D:status>
     </D:response>
     <D:response>
       <D:href>/bernard/work/abcd4.ics</D:href>
       <D:status>HTTP/1.1 403 Forbidden</D:status>
     </D:response>
     <D:response>
       <D:href>/bernard/work/abcd5.ics</D:href>
       <D:status>HTTP/1.1 403 Forbidden</D:status>
     </D:response>
     <D:response>
       <D:href>/bernard/work/abcd6.ics</D:href>
       <D:status>HTTP/1.1 403 Forbidden</D:status>
     </D:response>
     <D:response>
       <D:href>/bernard/work/abcd7.ics</D:href>
       <D:status>HTTP/1.1 403 Forbidden</D:status>
     </D:response>
     <D:response>
       <D:href>/bernard/work/abcd8.ics</D:href>
       <D:status>HTTP/1.1 403 Forbidden</D:status>
     </D:response>
   </D:multistatus>
|}
  in
  Alcotest.(check (result t_tree string) __LOC__
              (Ok (tree expected))
              (report "bernard/work" xml
                 (appendix_b_data @@ snd deny_all)))

let test_multiget_7_9_1_deny () =
  let xml = multiget_7_9_1
  and expected = header ^ {|
   <D:multistatus xmlns:D="DAV:">
     <D:response>
       <D:href>/bernard/work/abcd1.ics</D:href>
       <D:status>HTTP/1.1 403 Forbidden</D:status>
     </D:response>
     <D:response>
       <D:href>/bernard/work/mtg1.ics</D:href>
       <D:status>HTTP/1.1 404 Not Found</D:status>
     </D:response>
   </D:multistatus>|}
  in
  Alcotest.(check (result t_tree string) __LOC__
              (Ok (tree expected))
              (report "bernard/work" xml
                 (appendix_b_data @@ snd deny_all)))


let report_with_acl_tests = [
  "Report 1, denied", `Quick, test_report_1_deny ;
  "Report 7_8_1, denied", `Quick, test_report_7_8_1_deny ;
  "Report 7_8_2, denied", `Quick, test_report_7_8_2_deny ;
  (* remaining reports behave the same way for acl = deny all *)
  "Report 7_9_1, denied", `Quick, test_multiget_7_9_1_deny ;
]

(*
Userprops: <?xml version="1.0" encoding="utf-8" ?>
<D:acl xmlns:D="DAV:"><D:ace><D:principal><D:href>http://127.0.0.1:8080/principals/test/</D:href></D:principal><D:grant><D:privilege><D:all></D:all></D:privilege></D:grant></D:ace><D:ace><D:principal><D:all></D:all></D:principal><D:grant><D:privilege><D:read></D:read></D:privilege></D:grant></D:ace></D:acl><?xml version="1.0" encoding="utf-8" ?>
<D:creationdate xmlns:D="DAV:">2018-09-14T09:50:19-00:00</D:creationdate><?xml version="1.0" encoding="utf-8" ?>
<D:displayname xmlns:D="DAV:">test</D:displayname><?xml version="1.0" encoding="utf-8" ?>
<D:getcontentlanguage xmlns:D="DAV:">en</D:getcontentlanguage><?xml version="1.0" encoding="utf-8" ?>
<D:getcontentlength xmlns:D="DAV:">0</D:getcontentlength><?xml version="1.0" encoding="utf-8" ?>
<D:getcontenttype xmlns:D="DAV:">text/directory</D:getcontenttype><?xml version="1.0" encoding="utf-8" ?>
<D:getetag xmlns:D="DAV:">d41d8cd98f00b204e9800998ecf8427e</D:getetag><?xml version="1.0" encoding="utf-8" ?>
<D:getlastmodified xmlns:D="DAV:">Fri, 14 Sep 2018 09:50:19 GMT</D:getlastmodified><?xml version="1.0" encoding="utf-8" ?>
<D:principal-URL xmlns:D="DAV:"><D:href>http://127.0.0.1:8080/principals/test/</D:href></D:principal-URL><?xml version="1.0" encoding="utf-8" ?>
<D:resourcetype xmlns:D="DAV:"><D:collection></D:collection><D:principal></D:principal></D:resourcetype><?xml version="1.0" encoding="utf-8" ?>
<C:calendar-home-set xmlns:D="DAV:" xmlns:C="urn:ietf:params:xml:ns:caldav"><D:href>http://127.0.0.1:8080/calendars/test/</D:href></C:calendar-home-set>

Propmap: <?xml version="1.0" encoding="utf-8" ?>
<D:acl xmlns:D="DAV:"><D:ace><D:principal><D:href>http://127.0.0.1:8080/principals/test/</D:href></D:principal><D:grant><D:privilege><D:all></D:all></D:privilege></D:grant></D:ace><D:ace><D:principal><D:all></D:all></D:principal><D:grant><D:privilege><D:read></D:read></D:privilege></D:grant></D:ace></D:acl><?xml version="1.0" encoding="utf-8" ?>
<D:creationdate xmlns:D="DAV:">2018-09-14T09:50:19-00:00</D:creationdate><?xml version="1.0" encoding="utf-8" ?>
<D:displayname xmlns:D="DAV:">calendar</D:displayname><?xml version="1.0" encoding="utf-8" ?>
<D:getcontentlanguage xmlns:D="DAV:">en</D:getcontentlanguage><?xml version="1.0" encoding="utf-8" ?>
<D:getcontentlength xmlns:D="DAV:">0</D:getcontentlength><?xml version="1.0" encoding="utf-8" ?>
<D:getcontenttype xmlns:D="DAV:">text/directory</D:getcontenttype><?xml version="1.0" encoding="utf-8" ?>
<D:getetag xmlns:D="DAV:">d41d8cd98f00b204e9800998ecf8427e</D:getetag><?xml version="1.0" encoding="utf-8" ?>
<D:getlastmodified xmlns:D="DAV:">Fri, 14 Sep 2018 09:50:19 GMT</D:getlastmodified><?xml version="1.0" encoding="utf-8" ?>
<D:resourcetype xmlns:D="DAV:" xmlns:C="urn:ietf:params:xml:ns:caldav"><D:collection></D:collection><C:calendar></C:calendar></D:resourcetype><?xml version="1.0" encoding="utf-8" ?>
<D:supported-report-set xmlns:D="DAV:" xmlns:C="urn:ietf:params:xml:ns:caldav"><D:supported-report><D:report><C:calendar-query></C:calendar-query></D:report></D:supported-report><D:supported-report><D:report><C:calendar-multiget></C:calendar-multiget></D:report></D:supported-report></D:supported-report-set><?xml version="1.0" encoding="utf-8" ?>
<C:supported-calendar-component-set xmlns:C="urn:ietf:params:xml:ns:caldav"><C:comp name="VEVENT"></C:comp><C:comp name="VTODO"></C:comp><C:comp name="VTIMEZONE"></C:comp><C:comp name="VFREEBUSY"></C:comp></C:supported-calendar-component-set>

privileges are `Read; `All

PROPPATCH: <?xml version="1.0" encoding="UTF-8"?>
<A:propertyupdate xmlns:A="DAV:"><A:set><A:prop><D:calendar-order xmlns:D="http://apple.com/ns/ical/">1</D:calendar-order></A:prop></A:set></A:propertyupdate>

writing property map calendars/test/calendar/.prop.xml: <?xml version="1.0" encoding="utf-8" ?>
<D:prop xmlns:D="DAV:" xmlns:D="http://apple.com/ns/ical/" xmlns:C="urn:ietf:params:xml:ns:caldav"><C:supported-calendar-component-set><C:comp name="VEVENT"></C:comp><C:comp name="VTODO"></C:comp><C:comp name="VTIMEZONE"></C:comp><C:comp name="VFREEBUSY"></C:comp></C:supported-calendar-component-set><D:calendar-order xmlns:D="http://apple.com/ns/ical/">1</D:calendar-order><D:supported-report-set><D:supported-report><D:report><C:calendar-query></C:calendar-query></D:report></D:supported-report><D:supported-report><D:report><C:calendar-multiget></C:calendar-multiget></D:report></D:supported-report></D:supported-report-set><D:resourcetype><D:collection></D:collection><C:calendar></C:calendar></D:resourcetype><D:getlastmodified>Fri, 14 Sep 2018 09:50:19 GMT</D:getlastmodified><D:getcontenttype>text/directory</D:getcontenttype><D:getcontentlength>0</D:getcontentlength><D:getcontentlanguage>en</D:getcontentlanguage><D:displayname>calendar</D:displayname><D:creationdate>2018-09-14T09:50:19-00:00</D:creationdate><D:acl><D:ace><D:principal><D:href>http://127.0.0.1:8080/principals/test/</D:href></D:principal><D:grant><D:privilege><D:all></D:all></D:privilege></D:grant></D:ace><D:ace><D:principal><D:all></D:all></D:principal><D:grant><D:privilege><D:read></D:read></D:privilege></D:grant></D:ace></D:acl></D:prop>
*)

let property_check = 
  let module M = struct
    type t = Properties.t
    let pp = Properties.pp
    let equal = Properties.equal
  end in
  (module M : Alcotest.TESTABLE with type t = M.t)


let property_update_conflicting_ns () =
  let propmap = {|<?xml version="1.0" encoding="utf-8" ?>
<prop>
<D:acl xmlns:D="DAV:"><D:ace><D:principal><D:href>http://127.0.0.1:8080/principals/test/</D:href></D:principal><D:grant><D:privilege><D:all></D:all></D:privilege></D:grant></D:ace><D:ace><D:principal><D:all></D:all></D:principal><D:grant><D:privilege><D:read></D:read></D:privilege></D:grant></D:ace></D:acl>
<D:creationdate xmlns:D="DAV:">2018-09-14T09:50:19-00:00</D:creationdate>
<D:displayname xmlns:D="DAV:">calendar</D:displayname>
<D:getcontentlanguage xmlns:D="DAV:">en</D:getcontentlanguage>
<D:getcontentlength xmlns:D="DAV:">0</D:getcontentlength>
<D:getcontenttype xmlns:D="DAV:">text/directory</D:getcontenttype>
<D:getetag xmlns:D="DAV:">d41d8cd98f00b204e9800998ecf8427e</D:getetag>
<D:getlastmodified xmlns:D="DAV:">Fri, 14 Sep 2018 09:50:19 GMT</D:getlastmodified>
<D:resourcetype xmlns:D="DAV:" xmlns:C="urn:ietf:params:xml:ns:caldav"><D:collection></D:collection><C:calendar></C:calendar></D:resourcetype>
<D:supported-report-set xmlns:D="DAV:" xmlns:C="urn:ietf:params:xml:ns:caldav"><D:supported-report><D:report><C:calendar-query></C:calendar-query></D:report></D:supported-report><D:supported-report><D:report><C:calendar-multiget></C:calendar-multiget></D:report></D:supported-report></D:supported-report-set>
<C:supported-calendar-component-set xmlns:C="urn:ietf:params:xml:ns:caldav"><C:comp name="VEVENT"></C:comp><C:comp name="VTODO"></C:comp><C:comp name="VTIMEZONE"></C:comp><C:comp name="VFREEBUSY"></C:comp></C:supported-calendar-component-set>
</prop>
|}

  and proppatch = {|<?xml version="1.0" encoding="UTF-8"?>
<A:propertyupdate xmlns:A="DAV:"><A:set><A:prop><D:calendar-order xmlns:D="http://apple.com/ns/ical/">1</D:calendar-order></A:prop></A:set></A:propertyupdate>
|}
  and on_disk = {|<?xml version="1.0" encoding="utf-8" ?>
<D:prop xmlns:D="DAV:" xmlns:A="http://apple.com/ns/ical/" xmlns:C="urn:ietf:params:xml:ns:caldav"><C:supported-calendar-component-set><C:comp name="VEVENT"></C:comp><C:comp name="VTODO"></C:comp><C:comp name="VTIMEZONE"></C:comp><C:comp name="VFREEBUSY"></C:comp></C:supported-calendar-component-set><A:calendar-order>1</A:calendar-order><D:supported-report-set><D:supported-report><D:report><C:calendar-query></C:calendar-query></D:report></D:supported-report><D:supported-report><D:report><C:calendar-multiget></C:calendar-multiget></D:report></D:supported-report></D:supported-report-set><D:resourcetype><D:collection></D:collection><C:calendar></C:calendar></D:resourcetype><D:getlastmodified>Fri, 14 Sep 2018 09:50:19 GMT</D:getlastmodified><D:getetag>d41d8cd98f00b204e9800998ecf8427e</D:getetag><D:getcontenttype>text/directory</D:getcontenttype><D:getcontentlength>0</D:getcontentlength><D:getcontentlanguage>en</D:getcontentlanguage><D:displayname>calendar</D:displayname><D:creationdate>2018-09-14T09:50:19-00:00</D:creationdate><D:acl><D:ace><D:principal><D:href>http://127.0.0.1:8080/principals/test/</D:href></D:principal><D:grant><D:privilege><D:all></D:all></D:privilege></D:grant></D:ace><D:ace><D:principal><D:all></D:all></D:principal><D:grant><D:privilege><D:read></D:read></D:privilege></D:grant></D:ace></D:acl></D:prop>|} in
  let expected_props = Properties.from_tree @@ match Xml.string_to_tree on_disk with None -> assert false | Some t -> t in
  let props = Properties.from_tree @@ match Xml.string_to_tree propmap with None -> assert false | Some t -> t in
  let updates = match Xml.parse_propupdate_xml @@ match Xml.string_to_tree proppatch with None -> assert false | Some t -> t with Error _ -> assert false | Ok r -> r in
  Alcotest.(check (option property_check) __LOC__ (Some expected_props) (fst @@ Properties.patch props updates))

let property_update_tests = [
  "Propertyupdate conflicting namespace", `Quick, property_update_conflicting_ns ;
]

let tests = [
  "Read propfind", parse_propfind_xml_tests ;
  "Read propertyupdate", parse_propupdate_xml_tests ;
  "Report parse tests", report_parse_tests ;
  "Report tests", report_tests ;
  "Webdav API", webdav_api_tests ;
  "ACL tests", List.flatten @@ List.map webdav_acl_tests acl_test_cases ;
  "Properties.find_many tests", properties_find_many_tests ;
  "Report with ACL tests", report_with_acl_tests ;
  "Propertyupdate tests", property_update_tests ;
]

let () =
  Logs.set_reporter (Logs_fmt.reporter ());
  Printexc.record_backtrace true;
  Alcotest.run "WebDAV tests" tests
