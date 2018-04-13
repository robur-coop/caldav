
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
              None (Webdav.read_propfind xml))

let propname () =
  let xml = header ^ "<propfind><propname/></propfind>" in
  Alcotest.(check (option prop) "parsing <propfind><propname/></propfind>"
              (Some `Propname) (Webdav.read_propfind xml))

let two_propname () =
  let xml = header ^ "<propfind><propname/><propname/></propfind>" in
  Alcotest.(check (option prop) "parsing <propfind><propname/><propname/></propfind>"
              None (Webdav.read_propfind xml))

let allprop () =
  let xml = header ^ "<propfind><allprop/></propfind>" in
  Alcotest.(check (option prop) "parsing <propfind><allprop/></propfind>"
              (Some (`All_prop [])) (Webdav.read_propfind xml))

let allprop_include () =
  let xml = header ^ "<propfind><allprop/><include><foo/><bar/></include></propfind>" in
  Alcotest.(check (option prop) "parsing all prop with includes"
              (Some (`All_prop ["foo";"bar"])) (Webdav.read_propfind xml))

let invalid_xml () =
  Alcotest.(check (option prop) "parsing header only"
              None (Webdav.read_propfind header)) ;
  let xml = header ^ "<propfind>" in
  Alcotest.(check (option prop) "hanging paren"
              None (Webdav.read_propfind xml)) ;
  let xml = header ^ "<propfind" in
  Alcotest.(check (option prop) "missing bracket"
              None (Webdav.read_propfind xml)) ;
  let xml = header ^ "<propname/>" in
  Alcotest.(check (option prop) "missing propfind"
              None (Webdav.read_propfind xml)) ;
  let xml = header ^ "<propfind/>" in
  Alcotest.(check (option prop) "empty propfind"
              None (Webdav.read_propfind xml)) ;
  let xml = header ^ "<propfind><foo/></propfind>" in
  Alcotest.(check (option prop) "unexpected element foo"
              None (Webdav.read_propfind xml)) ;
  let xml = header ^ "<propfind><prop><foo><bar/></foo></prop></propfind>" in
  Alcotest.(check (option prop) "only flat property list"
              None (Webdav.read_propfind xml))

let read_propfind_tests = [
  "Empty", `Quick, empty_propfind ;
  "Propname", `Quick, propname ;
  "Two propnames", `Quick, two_propname ;
  "Allprop", `Quick, allprop ;
  "Allprop with includes", `Quick, allprop_include ;
  "Invalid XML", `Quick, invalid_xml ;
]

let tests = [
  "Read propfind", read_propfind_tests
]

let () =
  Printexc.record_backtrace true;
  Alcotest.run "WebDAV tests" tests
