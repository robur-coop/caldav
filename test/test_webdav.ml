module Xml = Caldav.Webdav_xml
module Fs = Caldav.Webdav_fs.Make(Mirage_fs_mem)
module Dav = Caldav.Webdav_api.Make(Fs)
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

let appendix_b_data =
  let open Lwt.Infix in
  Lwt_main.run (
    let now = Ptime.v (1, 0L) in
    Mirage_fs_mem.connect "/tmp/caldavtest" >>= fun res_fs ->
    let props name = Properties.create_dir now name in
    Fs.mkdir res_fs (`Dir [ "bernard" ]) (props "bernard") >>= fun _ ->
    Fs.mkdir res_fs (`Dir [ "bernard" ; "work" ]) (props "bernard/work") >>= fun _ ->
    Lwt_list.iter_s (fun (fn, etag, data) ->
        let props = Properties.create ~content_type:"text/calendar" ~etag 
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
    let now = Ptime.v (1, 0L) in
    Mirage_fs_mem.connect "" >>= fun res_fs ->
    let props name = Properties.create_dir now name in
    Fs.mkdir res_fs (`Dir [ "bernard" ]) (props "bernard") >>= fun _ ->
    Fs.mkdir res_fs (`Dir [ "bernard" ; "work" ]) (props "bernard/work") >>= fun _ ->
    (match Appendix_b.all with
    | (fn, etag, data) :: _ ->
        let props = Properties.create ~content_type:"text/calendar" ~etag 
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
  let user = (`Dir ["principals" ; "user"]) in
  let open Lwt.Infix in
  Lwt_main.run (
    Dav.report data ~host:(Uri.of_string "http://cal.example.com") ~path request ~user >|= function
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
                 "http://cal.example.com/bernard/work/abcd1.ics")
               ]
             ));
           (Xml.Node ("DAV:", "propstat", [],
              [(Xml.Node ("DAV:", "prop", [],
                  [(Xml.Node ("DAV:", "getetag", [],
                      [(Xml.Pcdata "\"fffff-abcd1\"")]));
                    (Xml.Node ("urn:ietf:params:xml:ns:caldav",
                       "calendar-data", [],
                       [(Xml.Pcdata
                           "BEGIN:VCALENDAR\nVERSION:2.0\nPRODID:-//Example Corp.//CalDAV Client//EN\nBEGIN:VTIMEZONE\nLAST-MODIFIED:20040110T032845Z\nTZID:US/Eastern\nBEGIN:DAYLIGHT\nDTSTART:20000404T020000\nRRULE:FREQ=YEARLY;BYDAY=1SU;BYMONTH=4\nTZNAME:EDT\nTZOFFSETFROM:-0500\nTZOFFSETTO:-0400\nEND:DAYLIGHT\nBEGIN:STANDARD\nDTSTART:20001026T020000\nRRULE:FREQ=YEARLY;BYDAY=-1SU;BYMONTH=10\nTZNAME:EST\nTZOFFSETFROM:-0400\nTZOFFSETTO:-0500\nEND:STANDARD\nEND:VTIMEZONE\nBEGIN:VEVENT\nUID:74855313FA803DA593CD579A@example.com\nDTSTAMP:20060206T001102Z\nDTSTART;TZID=US/Eastern:20060102T100000\nDURATION:PT1H\nSUMMARY:Event #1\nDescription:Go Steelers!\nEND:VEVENT\nEND:VCALENDAR\n")
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
      Mirage_fs_mem.connect "" >>= fun res_fs ->
      let props =
        let resourcetype = [ Xml.node ~ns:"http://example.com/ns/" "special-resource" [] ] in
        Properties.create_dir ~resourcetype now "Special Resource"
      in
      Mirage_fs_mem.mkdir res_fs "home" >>= fun _ ->
      Fs.mkdir res_fs (`Dir [ "home" ; "special" ]) props >>= fun _ ->
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
      Mirage_fs_mem.connect "" >>= fun res_fs ->
      let content = {_|<?xml version="1.0" encoding="utf-8" ?>
<D:prop xmlns:D="DAV:" xmlns:A="http://example.com/ns/"><D:resourcetype><D:collection></D:collection><A:special-resource></A:special-resource></D:resourcetype><D:getlastmodified>1970-01-02T00:00:00-00:00</D:getlastmodified><D:getcontenttype>text/directory</D:getcontenttype><D:getcontentlength>0</D:getcontentlength><D:getcontentlanguage>en</D:getcontentlanguage><D:displayname>Special Resource</D:displayname><D:creationdate>1970-01-02T00:00:00-00:00</D:creationdate></D:prop>|_}
      in
      Mirage_fs_mem.write res_fs ".prop.xml" 0
        (Cstruct.of_string content) >>= fun _ ->
      Mirage_fs_mem.mkdir res_fs "home" >>= fun _ ->
      Mirage_fs_mem.write res_fs "home/.prop.xml" 0
        (Cstruct.of_string content) >>= fun _ ->
      Mirage_fs_mem.connect "" >>= fun fs ->
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

let config = { principals = "principals" ; calendars = "calendars" ; user_password = [] ; host = Uri.of_string "http://example.com" }

let principal_url principal = Uri.with_path config.host (Fs.to_string (`Dir [ config.principals ; principal ]))

let test_fs_with_acl path acl = Lwt_main.run (
  let open Lwt.Infix in
  Mirage_fs_mem.connect "" >>= fun fs ->
  let props = Properties.create_dir (Ptime_clock.now ()) path in
  let props' = Properties.add (Xml.dav_ns, "acl") acl props in
  Fs.mkdir fs (`Dir [path]) props' >|= fun _ -> fs)

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

let user principal =
  principal,
  Properties.add
    (Xml.dav_ns, "principal-URL")
    ([], [Xml.dav_node "href" [ Xml.Pcdata (Uri.to_string @@ principal_url principal) ]])
    Properties.empty

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

let request_calendars_for_acl http_verb request_path aces user_props res () =
  let path = "calendars" in
  let acl = ([], List.map Xml.ace_to_xml aces ) in
  let fs = test_fs_with_acl path acl in
  Alcotest.(check bool __LOC__ res (Lwt_main.run @@ Dav.access_granted_for_acl fs request_path http_verb user_props))

let webdav_acl_tests (http_verb, request_path, acls_results) =
  let http_verb_str = Sexplib.Sexp.to_string_hum @@ Cohttp.Code.sexp_of_meth http_verb in
  let test ((ace_str, aces), (principal_str, user_props), result) =
    Printf.sprintf "%s %s %s, principal %s" http_verb_str request_path ace_str principal_str,
    `Quick,
    request_calendars_for_acl http_verb request_path aces user_props result
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
  Alcotest.(check (list (pair check_status (list t_tree))) __LOC__ expected (Properties.find_many userprops fqname properties))

let find_many_acl aces () =
  let userprops = snd @@ user "read-acl"
  and fqname = (Xml.dav_ns, "acl") 
  and aces_xml = List.map Xml.ace_to_xml (snd aces) in
  let properties = Properties.add fqname ([], aces_xml) Properties.empty in
  let expected = [ (`OK, [ Xml.dav_node "acl" aces_xml ]) ] in
  Alcotest.(check (list (pair check_status (list t_tree))) __LOC__ expected (Properties.find_many userprops [fqname] properties))

let find_many_acl_forbidden principal aces () =
  let userprops = snd @@ user principal
  and fqname = (Xml.dav_ns, "acl") 
  and aces_xml = List.map Xml.ace_to_xml (snd aces) in
  let properties = Properties.add fqname ([], aces_xml) Properties.empty in
  let expected = [ (`Forbidden, [ Xml.dav_node "acl" [] ]) ] in
  Alcotest.(check (list (pair check_status (list t_tree))) __LOC__ expected (Properties.find_many userprops [fqname] properties))

let find_many_current_user_privset_empty_forbidden () =
  let userprops = Properties.empty
  and fqname = [ (Xml.dav_ns, "current-user-privilege-set") ]
  and properties = Properties.empty
  and expected = [ (`Forbidden, [ Xml.dav_node "current-user-privilege-set" [] ]) ] in
  Alcotest.(check (list (pair check_status (list t_tree))) __LOC__ expected (Properties.find_many userprops fqname properties))

let find_many_current_user_privset_forbidden principal aces () =
  let userprops = snd @@ user principal
  and fqname = [ (Xml.dav_ns, "current-user-privilege-set") ]
  and aces_xml = List.map Xml.ace_to_xml (snd aces) in
  let properties = Properties.add (Xml.dav_ns, "acl") ([], aces_xml) Properties.empty in
  let expected = [ (`Forbidden, [ Xml.dav_node "current-user-privilege-set" [] ]) ] in
  Alcotest.(check (list (pair check_status (list t_tree))) __LOC__ expected (Properties.find_many userprops fqname properties))

let find_many_current_user_privset principal aces privilege () =
  let userprops = snd @@ user principal 
  and fqname = (Xml.dav_ns, "current-user-privilege-set") 
  and aces_xml = List.map Xml.ace_to_xml (snd aces) in
  let properties = Properties.add (Xml.dav_ns, "acl") ([], aces_xml) Properties.empty in
  let privilege_set = [ Xml.dav_node "privilege" [ Xml.priv_to_xml privilege ]] in
  let expected = [ (`OK, [ Xml.dav_node "current-user-privilege-set" privilege_set ]) ] in
  Alcotest.(check (list (pair check_status (list t_tree))) __LOC__ expected (Properties.find_many userprops [fqname] properties))


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
  "Find many for current-user-privilege-set", `Quick, find_many_current_user_privset "read-current-user-privilege-set" grant_read_current_user_privilege_set `Read_current_user_privilege_set ;
  "Find many for current-user-privilege-set, user: read-acl", `Quick, find_many_current_user_privset "read-acl" grant_read_acl `Read_acl;
  "Find many for current-user-privilege-set, user: somebody", `Quick, find_many_current_user_privset "somebody" grant_all `All ;
]

let tests = [
  "Read propfind", parse_propfind_xml_tests ;
  "Read propertyupdate", parse_propupdate_xml_tests ;
  "Report parse tests", report_parse_tests ;
  "Report tests", report_tests ;
  "Webdav API", webdav_api_tests ;
  "ACL tests", List.flatten @@ List.map webdav_acl_tests acl_test_cases ;
  "Properties.find_many tests", properties_find_many_tests ;
]

let () =
  Printexc.record_backtrace true;
  Alcotest.run "WebDAV tests" tests
