
let header = "<?xml version=\"1.0\" encoding=\"utf-8\" ?>"

let prop =
  let module M = struct
    type t = [ `All_prop of string list | `Props of string list | `Propname ]
    let pp = Webdav.pp_prop
    let equal a b = compare a b = 0
  end in
  (module M : Alcotest.TESTABLE with type t = M.t)

let empty_propfind () =
  let xml = header ^ "<propfind/>" in
  Alcotest.(check (option prop) "parsing <propfind/>"
              None (Webdav.parse_propfind_xml xml))

let propname () =
  let xml = header ^ "<propfind><propname/></propfind>" in
  Alcotest.(check (option prop) "parsing <propfind><propname/></propfind>"
              (Some `Propname) (Webdav.parse_propfind_xml xml))

let two_propname () =
  let xml = header ^ "<propfind><propname/><propname/></propfind>" in
  Alcotest.(check (option prop) "parsing <propfind><propname/><propname/></propfind>"
              None (Webdav.parse_propfind_xml xml))

let allprop () =
  let xml = header ^ "<propfind><allprop/></propfind>" in
  Alcotest.(check (option prop) "parsing <propfind><allprop/></propfind>"
              (Some (`All_prop [])) (Webdav.parse_propfind_xml xml))

let allprop_include () =
  let xml = header ^ "<propfind><allprop/><include><foo/><bar/></include></propfind>" in
  Alcotest.(check (option prop) "parsing all prop with includes"
              (Some (`All_prop ["foo";"bar"])) (Webdav.parse_propfind_xml xml))

let invalid_xml () =
  Alcotest.(check (option prop) "parsing header only"
              None (Webdav.parse_propfind_xml header)) ;
  let xml = header ^ "<propfind>" in
  Alcotest.(check (option prop) "hanging paren"
              None (Webdav.parse_propfind_xml xml)) ;
  let xml = header ^ "<propfind" in
  Alcotest.(check (option prop) "missing bracket"
              None (Webdav.parse_propfind_xml xml)) ;
  let xml = header ^ "<propname/>" in
  Alcotest.(check (option prop) "missing propfind"
              None (Webdav.parse_propfind_xml xml)) ;
  let xml = header ^ "<propfind/>" in
  Alcotest.(check (option prop) "empty propfind"
              None (Webdav.parse_propfind_xml xml)) ;
  let xml = header ^ "<propfind><foo/></propfind>" in
  Alcotest.(check (option prop) "unexpected element foo"
              None (Webdav.parse_propfind_xml xml)) ;
  let xml = header ^ "<propfind><prop><foo><bar/></foo></prop></propfind>" in
  Alcotest.(check (option prop) "non-flat property list"
              (Some (`Props [ "foo" ]))
              (Webdav.parse_propfind_xml xml))

let parse_propfind_xml_tests = [
  "Empty", `Quick, empty_propfind ;
  "Propname", `Quick, propname ;
  "Two propnames", `Quick, two_propname ;
  "Allprop", `Quick, allprop ;
  "Allprop with includes", `Quick, allprop_include ;
  "Invalid XML", `Quick, invalid_xml ;
]

let propupdate =
  let module M = struct
    type t = [ `Remove of string | `Set of ((string * string) list * string * Webdav.tree list) ] list
    let pp = Webdav.pp_propupdate
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
  Alcotest.(check (option propupdate) __LOC__
              (Some [`Set (["xmlns", "http://ns.example.com/standards/z39.50/"],
                           "Authors",
                           [`Node (["xmlns", "http://ns.example.com/standards/z39.50/"],
                                   "Author", [ `Pcdata "Jim Whitehead"]) ;
                            `Node (["xmlns", "http://ns.example.com/standards/z39.50/"],
                                   "Author", [ `Pcdata "Roy Fielding" ]) ]) ;
                     `Remove "Copyright-Owner" ])
    (Webdav.parse_propupdate_xml xml))

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
    type t = [ `Bad_request | `Conflict | `Forbidden of Tyxml.Xml.elt list ]
    let pp ppf = function
      | `Bad_request -> Fmt.string ppf "bad request"
      | `Conflict -> Fmt.string ppf "conflict"
      | `Forbidden _ -> Fmt.string ppf "forbidden"
    let equal a b = match a, b with
      | `Bad_request, `Bad_request -> true
      | `Conflict, `Conflict -> true
      | `Forbidden xs, `Forbidden ys ->
        let print a =
          Tyxml.Xml.pp () Format.str_formatter a ;
          Format.flush_str_formatter ()
        in
        List.for_all2 (fun a b -> String.equal (print a) (print b)) xs ys
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
<prop><resourcetype><collection></collection><special-resource xmlns="http://example.com/ns/"></special-resource></resourcetype><getlastmodified>Fri, 02 Jan 1970 00:00:00 GMT</getlastmodified><getcontenttype>text/directory</getcontenttype><getcontentlength>0</getcontentlength><getcontentlanguage>en</getcontentlanguage><displayname>Special Resource</displayname><creationdate>Fri, 02 Jan 1970 00:00:00 GMT</creationdate></prop>|_}
      in
      Mirage_fs_mem.write res_fs "home/special/.prop.xml" 0
        (Cstruct.of_string content) >>= fun _ ->
      Mirage_fs_mem.connect "" >>= fun fs ->
      Mirage_fs_mem.mkdir fs "home" >>= fun _ ->
      Webdav_api.mkcol ~now fs ~name:"home/special/" ~body >|= fun r ->
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
  "Webdav API", webdav_api_tests
]

let () =
  Printexc.record_backtrace true;
  Alcotest.run "WebDAV tests" tests
