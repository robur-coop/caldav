
let read_propfind str =
  try
    let data str = assert false
    and el ((ns, name), attr) children = `N (ns, name, attr, children)
    in
    let input = Xmlm.make_input (`String (0, str)) in
    ignore (Xmlm.input input) ; (* ignore DTD *)
    let res = match Xmlm.input_tree ~el ~data input with
      | `N (_, "propfind", attr, children) ->
        begin match children with
          | [ `N (_, "propname", _, []) ] -> `Propname
          | [ `N (_, "prop", _, props) ] ->
            let children =
              List.map (function
                  | `N (_, name, _, []) -> name
                  | _ -> assert false) props
            in
            `Props children
          | [ `N (_, "allprop", _, []) ] -> `All_prop []
          | [ `N (_, "allprop", _, []) ; `N (_, "include", _, includes) ] ->
            let successors =
              List.map (function
                  | `N (_, name, _, []) -> name
                  | _ -> assert false) includes
            in
            `All_prop successors
          | _ -> assert false
        end
      | _ -> assert false
    in
    Some res
  with
  | _ -> None

let pp_prop fmt = function
  | `Propname -> Fmt.string fmt "Propname"
  | `All_prop xs -> Fmt.pf fmt "All prop %a" Fmt.(list ~sep:(unit ",@ ") string) xs
  | `Props xs -> Fmt.pf fmt "Props %a" Fmt.(list ~sep:(unit ",@ ") string) xs

let create_properties ?(content_type = "text/html") ?(language = "en") is_dir timestamp filename =
  let open Tyxml.Xml in
  let rtype = if is_dir then [ leaf "collection" ] else [] in
  node "prop" [
    node "creationdate" [ pcdata timestamp ] ;
    node "displayname" [ pcdata filename ] ;
    node "getcontentlanguage" [ pcdata language ] ;
    node "getcontenttype" [ pcdata content_type ] ;
    node "getlastmodified" [ pcdata timestamp ] ;
    (* node "lockdiscovery" *)
    node "resourcetype" rtype ;
    (* node "supportedlock" *)
  ]
