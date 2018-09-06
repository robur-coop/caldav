module Xml = Caldav.Webdav_xml
module Dav = Caldav.Webdav_api
module Fs = Caldav.Webdav_fs

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

let appendix_b_data =
  let open Lwt.Infix in
  Lwt_main.run (
    let now =
      let ts = Ptime.v (1, 0L) in
      Ptime.to_rfc3339 ts
    in
    Fs.connect "/tmp/caldavtest" >>= fun res_fs ->
    let props name = Xml.create_properties true now 0 name in
    Fs.mkdir res_fs (`Dir [ "bernard" ]) (props "bernard") >>= fun _ ->
    Fs.mkdir res_fs (`Dir [ "bernard" ; "work" ]) (props "bernard/work") >>= fun _ ->
    Lwt_list.iter_s (fun (fn, etag, data) ->
        let props = Xml.create_properties ~content_type:"text/calendar" ~etag false
            now (String.length data) ("bernard/work/" ^ fn)
        in
        Fs.write res_fs (`File [ "bernard" ; "work" ; fn ])
          (Cstruct.of_string data) props >|= fun _ ->
        ())
      Appendix_b.all >|= fun () ->
    res_fs)

let appendix_b_1_data =
  let open Lwt.Infix in
  Lwt_main.run (
    let now =
      let ts = Ptime.v (1, 0L) in
      Ptime.to_rfc3339 ts
    in
    Fs.connect "" >>= fun res_fs ->
    let props name = Xml.create_properties true now 0 name in
    Fs.mkdir res_fs (`Dir [ "bernard" ]) (props "bernard") >>= fun _ ->
    Fs.mkdir res_fs (`Dir [ "bernard" ; "work" ]) (props "bernard/work") >>= fun _ ->
    (match Appendix_b.all with
    | (fn, etag, data) :: _ ->
        let props = Xml.create_properties ~content_type:"text/calendar" ~etag false
            now (String.length data) ("bernard/work/" ^ fn)
        in
        Fs.write res_fs (`File [ "bernard" ; "work" ; fn ])
          (Cstruct.of_string data) props >|= fun _ ->
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

let report path request data =
  let open Lwt.Infix in
  Lwt_main.run (
    Dav.report data ~host:(Uri.of_string "http://cal.example.com") ~path request >|= function
    | Ok t -> Ok (tree (Xml.tree_to_string t))
    | Error _ -> Error "failed")

let test_report_1 () =
  let xml = report_7_8_1
  and expected = header ^ {|
<D:multistatus xmlns:D="DAV:"></D:multistatus>|}
  in
  Alcotest.(check (result t_tree string) __LOC__
              (Ok (tree expected))
              (report (`Dir [ "bernard" ; "work" ]) (tree xml) appendix_b_1_data))

let test_report_7_8_1 () =
  let xml = report_7_8_1
  and expected = header ^ {|
<D:multistatus xmlns:D="DAV:"
              xmlns:C="urn:ietf:params:xml:ns:caldav">
     <D:response>
       <D:href>http://cal.example.com/bernard/work/abcd2.ics</D:href>
       <D:propstat>
         <D:prop>
           <D:getetag>"fffff-abcd2"</D:getetag>
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
    <D:href>http://cal.example.com/bernard/work/abcd3.ics</D:href>
    <D:propstat>
      <D:prop>
        <D:getetag>"fffff-abcd3"</D:getetag>
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
              (report (`Dir [ "bernard" ; "work" ]) (tree xml) (appendix_b_data)))


let test_report_7_8_2 () =
  let xml = report_7_8_2
  and expected = header ^ {|
   <D:multistatus xmlns:D="DAV:"
              xmlns:C="urn:ietf:params:xml:ns:caldav">
     <D:response>
       <D:href>http://cal.example.com/bernard/work/abcd2.ics</D:href>
       <D:propstat>
         <D:prop>
           <D:getetag>"fffff-abcd2"</D:getetag>
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
       <D:href>http://cal.example.com/bernard/work/abcd3.ics</D:href>
       <D:propstat>
         <D:prop>
           <D:getetag>"fffff-abcd3"</D:getetag>
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
              (report (`Dir [ "bernard" ; "work" ]) (tree xml) (appendix_b_data)))

let test_report_7_8_3 () =
  let xml = report_7_8_3
  and expected = header ^ {|
   <D:multistatus xmlns:D="DAV:"
              xmlns:C="urn:ietf:params:xml:ns:caldav">
     <D:response>
       <D:href>http://cal.example.com/bernard/work/abcd2.ics</D:href>
       <D:propstat>
         <D:prop>
           <D:getetag>"fffff-abcd2"</D:getetag>
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
       <D:href>http://cal.example.com/bernard/work/abcd3.ics</D:href>
       <D:propstat>
         <D:prop>
           <D:getetag>"fffff-abcd3"</D:getetag>
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
              (report (`Dir [ "bernard" ; "work" ]) (tree xml) (appendix_b_data)))

let test_report_7_8_4 () =
  let xml = report_7_8_4
  and expected = header ^ {|
<D:multistatus xmlns:D="DAV:"
                  xmlns:C="urn:ietf:params:xml:ns:caldav">
     <D:response>
       <D:href>http://cal.example.com/bernard/work/abcd8.ics</D:href>
       <D:propstat>
         <D:prop>
           <D:getetag>"fffff-abcd8"</D:getetag>
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
              (report (`Dir [ "bernard" ; "work" ]) (tree xml) (appendix_b_data)))

let test_report_7_8_5 () =
  let xml = report_7_8_5
  and expected = header ^ {|
<D:multistatus xmlns:D="DAV:"
                  xmlns:C="urn:ietf:params:xml:ns:caldav">
     <D:response>
       <D:href>http://cal.example.com/bernard/work/abcd5.ics</D:href>
       <D:propstat>
         <D:prop>
           <D:getetag>"fffff-abcd5"</D:getetag>
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
              (report (`Dir [ "bernard" ; "work" ]) (tree xml) (appendix_b_data)))

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
       <D:href>http://cal.example.com/bernard/work/abcd1.ics</D:href>
       <D:propstat>
         <D:prop>
           <D:getetag>"fffff-abcd1"</D:getetag>
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
       <D:href>http://cal.example.com/bernard/work/mtg1.ics</D:href>
       <D:status>HTTP/1.1 404 Not Found</D:status>
     </D:response>
   </D:multistatus>|}
  in
  Alcotest.(check (result t_tree string) __LOC__
              (Ok (tree expected))
              (report (`Dir [ "bernard" ; "work" ]) (tree xml) (appendix_b_data)))

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
    let pp = Mirage_fs_mem.pp
    let equal = Mirage_fs_mem.equal
  end in
  (module M : Alcotest.TESTABLE with type t = M.t)

let err_testable =
  let module M = struct
    type t = [ `Bad_request | `Conflict | `Forbidden of Xml.tree ]
    let pp ppf = function
      | `Bad_request -> Fmt.string ppf "bad request"
      | `Conflict -> Fmt.string ppf "conflict"
      | `Forbidden _ -> Fmt.string ppf "forbidden"
    let equal a b = match a, b with
      | `Bad_request, `Bad_request -> true
      | `Conflict, `Conflict -> true
      | `Forbidden x, `Forbidden y ->
        let to_string a = Xml.tree_to_string a
        in
        String.equal (to_string x) (to_string y)
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
      Fs.connect "" >>= fun res_fs ->
      let content = {_|<?xml version="1.0" encoding="utf-8" ?>
<D:prop xmlns:D="DAV:" xmlns:A="http://example.com/ns/"><D:resourcetype><D:collection></D:collection><A:special-resource></A:special-resource></D:resourcetype><D:getlastmodified>1970-01-02T00:00:00-00:00</D:getlastmodified><D:getcontenttype>text/directory</D:getcontenttype><D:getcontentlength>0</D:getcontentlength><D:getcontentlanguage>en</D:getcontentlanguage><D:displayname>Special Resource</D:displayname><D:creationdate>1970-01-02T00:00:00-00:00</D:creationdate></D:prop>|_}
      in
      Mirage_fs_mem.mkdir res_fs "home" >>= fun _ ->
      Mirage_fs_mem.mkdir res_fs "home/special" >>= fun _ ->
      Mirage_fs_mem.write res_fs "home/special/.prop.xml" 0
        (Cstruct.of_string content) >>= fun _ ->
      Mirage_fs_mem.connect "" >>= fun fs ->
      Mirage_fs_mem.mkdir fs "home" >>= fun _ ->
      Dav.mkcol ~now fs (Fs.dir_from_string "home/special/") (Some (tree body)) >|= fun r ->
      (res_fs, r))
  in
  Alcotest.(check (result state_testable err_testable) __LOC__
              (Ok res_fs) r)

let delete_test () =
  let res_fs, r =
    Lwt_main.run (
      let open Lwt.Infix in
      Fs.connect "" >>= fun res_fs ->
      let content = {_|<?xml version="1.0" encoding="utf-8" ?>
<D:prop xmlns:D="DAV:" xmlns:A="http://example.com/ns/"><D:resourcetype><D:collection></D:collection><A:special-resource></A:special-resource></D:resourcetype><D:getlastmodified>1970-01-02T00:00:00-00:00</D:getlastmodified><D:getcontenttype>text/directory</D:getcontenttype><D:getcontentlength>0</D:getcontentlength><D:getcontentlanguage>en</D:getcontentlanguage><D:displayname>Special Resource</D:displayname><D:creationdate>1970-01-02T00:00:00-00:00</D:creationdate></D:prop>|_}
      in
      Mirage_fs_mem.write res_fs ".prop.xml" 0
        (Cstruct.of_string content) >>= fun _ ->
      Mirage_fs_mem.mkdir res_fs "home" >>= fun _ ->
      Mirage_fs_mem.write res_fs "home/.prop.xml" 0
        (Cstruct.of_string content) >>= fun _ ->
      Fs.connect "" >>= fun fs ->
      let now = Ptime.v (10, 0L) in
      let content' = {_|<?xml version="1.0" encoding="utf-8" ?>
<D:prop xmlns:D="DAV:" xmlns:A="http://example.com/ns/"><D:resourcetype><D:collection></D:collection><A:special-resource></A:special-resource></D:resourcetype><D:getlastmodified>1970-01-11T00:00:00-00:00</D:getlastmodified><D:getcontenttype>text/directory</D:getcontenttype><D:getcontentlength>0</D:getcontentlength><D:getcontentlanguage>en</D:getcontentlanguage><D:displayname>Special Resource</D:displayname><D:creationdate>1970-01-02T00:00:00-00:00</D:creationdate></D:prop>|_}
      in
      Mirage_fs_mem.write fs ".prop.xml" 0
        (Cstruct.of_string content') >>= fun _ ->
      Dav.delete ~now res_fs ~path:(`Dir [ "home" ]) >|= fun r ->
      (fs, r))
  in
  Alcotest.(check state_testable __LOC__ res_fs r)

let webdav_api_tests = [
  "successful mkcol", `Quick, mkcol_success ;
  "delete", `Quick, delete_test
]

let tests = [
  "Read propfind", parse_propfind_xml_tests ;
  "Read propertyupdate", parse_propupdate_xml_tests ;
  "Report parse tests", report_parse_tests ;
  "Report tests", report_tests ;
  "Webdav API", webdav_api_tests
]

let () =
  Printexc.record_backtrace true;
  Alcotest.run "WebDAV tests" tests
