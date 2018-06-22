
let header = "<?xml version=\"1.0\" encoding=\"utf-8\" ?>"

let tree xml = match Webdav_xml.string_to_tree xml with Some t -> t | None -> Alcotest.fail "Invalid xml."

let prop =
  let module M = struct
    type t = [ `All_prop of string list | `Props of string list | `Propname ]
    let pp = Webdav_xml.pp_prop
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
  let xml = header ^ "<propfind/>" in
  Alcotest.(check (result prop str_err) "parsing <propfind/>"
              (Error "broken") (Webdav_xml.parse_propfind_xml (tree xml)))

let propname () =
  let xml = header ^ "<propfind><propname/></propfind>" in
  Alcotest.(check (result prop str_err) "parsing <propfind><propname/></propfind>"
              (Ok `Propname) (Webdav_xml.parse_propfind_xml (tree xml)))

let two_propname () =
  let xml = header ^ "<propfind><propname/><propname/></propfind>" in
  Alcotest.(check (result prop str_err) "parsing <propfind><propname/><propname/></propfind>"
              (Error "broken") (Webdav_xml.parse_propfind_xml (tree xml)))

let allprop () =
  let xml = header ^ "<propfind><allprop/></propfind>" in
  Alcotest.(check (result prop str_err) "parsing <propfind><allprop/></propfind>"
              (Ok (`All_prop [])) (Webdav_xml.parse_propfind_xml (tree xml)))

let allprop_include () =
  let xml = header ^ "<propfind><allprop/><include><foo/><bar/></include></propfind>" in
  Alcotest.(check (result prop str_err) "parsing all prop with includes"
              (Ok (`All_prop ["foo";"bar"])) (Webdav_xml.parse_propfind_xml (tree xml)))

let invalid_xml () =
  let error_tree xml = match Webdav_xml.string_to_tree xml with 
    | Some _ -> () 
    | None -> invalid_arg "Invalid xml" in
  Alcotest.(check_raises "parsing header only" (Invalid_argument "Invalid xml")
              (fun () -> error_tree header));
  let xml = header ^ "<propfind>" in
  Alcotest.(check_raises "hanging paren"
              (Invalid_argument "Invalid xml") (fun () -> error_tree xml)) ;
  let xml = header ^ "<propfind" in
  Alcotest.(check_raises "missing bracket"
              (Invalid_argument "Invalid xml") (fun () -> error_tree xml)) ;
  let xml = header ^ "<propname/>" in
  Alcotest.(check (result prop str_err) "missing propfind"
              (Error "expected propfind, but found propname") 
              (Webdav_xml.parse_propfind_xml (tree xml))) ;
  let xml = header ^ "<propfind/>" in
  Alcotest.(check (result prop str_err) "empty propfind"
              (Error "broken") 
              (Webdav_xml.parse_propfind_xml (tree xml))) ;
  let xml = header ^ "<propfind><foo/></propfind>" in
  Alcotest.(check (result prop str_err) "unexpected element foo"
              (Error "broken") 
              (Webdav_xml.parse_propfind_xml (tree xml))) ;
  let xml = header ^ "<propfind><prop><foo><bar/></foo></prop></propfind>" in
  Alcotest.(check (result prop str_err) "non-flat property list"
              (Ok (`Props [ "foo" ]))
              (Webdav_xml.parse_propfind_xml (tree xml)))

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
    type t = string
    let pp = Fmt.string
    let equal = String.equal
  end in
  (module M : Alcotest.TESTABLE with type t = M.t)

let parse_simple_report_query () =
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
   </C:calendar-query>
|_} in
  let expected = Ok "Not implemented yet" in
  Alcotest.(check (result calendar_query string) __LOC__ expected (Webdav_xml.parse_calendar_query_xml (tree xml)))

let report_tests = [
  "Parse simple report query", `Quick, parse_simple_report_query;
]

let propupdate =
  let module M = struct
    type t = [ `Remove of string | `Set of ((string * string) list * string * Webdav_xml.tree list) ] list
    let pp = Webdav_xml.pp_propupdate
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
  Alcotest.(check (result propupdate str_err) __LOC__
              (Ok [`Set (["xmlns", "http://ns.example.com/standards/z39.50/"],
                           "Authors",
                           [`Node (["xmlns", "http://ns.example.com/standards/z39.50/"],
                                   "Author", [ `Pcdata "Jim Whitehead"]) ;
                            `Node (["xmlns", "http://ns.example.com/standards/z39.50/"],
                                   "Author", [ `Pcdata "Roy Fielding" ]) ]) ;
                     `Remove "Copyright-Owner" ])
    (Webdav_xml.parse_propupdate_xml (tree xml)))

let parse_propupdate_xml_tests = [
  "propertyupdate RFC example", `Quick, proppatch
]

let state_testable =
  let module M = struct
    type t = Mirage_fs_mem.t
    let pp = Mirage_fs_mem.pp
    let equal = Mirage_fs_mem.equal
  end in
  (module M : Alcotest.TESTABLE with type t = M.t)

let err_testable =
  let module M = struct
    type t = [ `Bad_request | `Conflict | `Forbidden of Webdav_xml.tree ]
    let pp ppf = function
      | `Bad_request -> Fmt.string ppf "bad request"
      | `Conflict -> Fmt.string ppf "conflict"
      | `Forbidden _ -> Fmt.string ppf "forbidden"
    let equal a b = match a, b with
      | `Bad_request, `Bad_request -> true
      | `Conflict, `Conflict -> true
      | `Forbidden x, `Forbidden y ->
        let to_string a = Webdav_xml.tree_to_string a
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
      let content = {_|<?xml version="1.0" encoding="utf-8" ?>
<prop><resourcetype><collection></collection><special-resource xmlns="http://example.com/ns/"></special-resource></resourcetype><getlastmodified>1970-01-02T00:00:00-00:00</getlastmodified><getcontenttype>text/directory</getcontenttype><getcontentlength>0</getcontentlength><getcontentlanguage>en</getcontentlanguage><displayname>Special Resource</displayname><creationdate>1970-01-02T00:00:00-00:00</creationdate></prop>|_}
      in
      Mirage_fs_mem.write res_fs "home/special/.prop.xml" 0
        (Cstruct.of_string content) >>= fun _ ->
      Mirage_fs_mem.connect "" >>= fun fs ->
      Mirage_fs_mem.mkdir fs "home" >>= fun _ ->
      Webdav_api.mkcol ~now fs (Webdav_fs.dir_from_string "home/special/") (Some (tree body)) >|= fun r ->
      (res_fs, r))
  in
  Alcotest.(check (result state_testable err_testable) __LOC__
              (Ok res_fs) r)

let webdav_api_tests = [
  "successful mkcol", `Quick, mkcol_success
]

let tests = [
  "Read propfind", parse_propfind_xml_tests ;
  "Read propertyupdate", parse_propupdate_xml_tests ;
  "Report tests", report_tests ;
  "Webdav API", webdav_api_tests
]

let () =
  Printexc.record_backtrace true;
  Alcotest.run "WebDAV tests" tests
