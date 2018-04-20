
let read_propfind str =
  try
    let data str = assert false
    and el ((ns, name), attr) children = `N (ns, name, attr, children)
    in
    let input = Xmlm.make_input ~strip:true (`String (0, str)) in
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

let create_properties ?(content_type = "text/html") ?(language = "en") is_dir timestamp length filename =
  let open Tyxml.Xml in
  let rtype = if is_dir then [ leaf "collection" ] else [] in
  let filename = if filename = "" then "hinz und kunz" else filename in
  node "prop" [
    node "creationdate" [ pcdata timestamp ] ;
    node "displayname" [ pcdata filename ] ;
    node "getcontentlanguage" [ pcdata language ] ;
    node "getcontenttype" [ pcdata content_type ] ;
    node "getlastmodified" [ pcdata timestamp ] ;
    node "getcontentlength" [ pcdata (string_of_int length) ] ;
    (* node "lockdiscovery" *)
    node "resourcetype" rtype ;
    (* node "supportedlock" *)
  ]

let string_to_tree str =
  let data str = `Pcdata str
  and el ((ns, name), attrs) children =
    let a =
      let namespace = match ns with
        | "" -> []
        | ns -> [ ("xmlns", ns) ]
      in
      namespace @ List.map (fun ((_ns, name), value) -> (name, value)) attrs
    in
    `Node (a, name, children)
  in
  try
    let input = Xmlm.make_input (`String (0, str)) in
    ignore (Xmlm.input input) ; (* ignore DTD *)
    Some (Xmlm.input_tree ~el ~data input)
  with _ -> None

let rec tree_fold f s forest = match forest with
  | `Node (a, name, children) :: tail ->
    let children' = tree_fold f s children
    and tail' = tree_fold f s tail in
    f (`Node (a, name, children)) children' tail'
  | (`Pcdata _ as t') :: tail ->
    let tail' = tree_fold f s tail in
    f t' s tail'
  | [] -> s

let tree_to_string t =
  let f s children tail = match s with
  | `Node (a, name, _) -> " Node: " ^ name ^ "(" ^ children ^ ")(" ^ tail ^ ")"
  | `Pcdata str -> " PCDATA: (" ^ str ^ ") " ^ tail in
  tree_fold f "" [t]

let drop_pcdata t =
  let f s children tail = match s with
  | `Node (a, n, c) -> `Node (a, n, children) :: tail
  | `Pcdata str -> tail in
  List.hd @@ tree_fold f [] [t]

(* assumption: xml is <prop><a/><b/><c/></prop> *)
let filter_in_ps ps xml =
  match xml with
  | `Node (a, "prop", children) ->
    let f acc = function
    | (`Node (a, n, c) as node) when List.mem n ps -> node :: acc
    | _ -> acc in
    let c' =
      List.fold_left f [] children in
    `Node (a, "prop", c')
  | _ -> assert false

let tree_to_tyxml t =
  let attrib_to_tyxml (name, value) =
    Tyxml.Xml.string_attrib name (Tyxml_xml.W.return value)
  in
  let f s children tail = match s with
  | `Node (a, n, c) ->
    let a' = List.map attrib_to_tyxml a in
    Tyxml.Xml.node ~a:a' n (Tyxml_xml.W.return children) :: tail
  | `Pcdata str -> Tyxml.Xml.pcdata (Tyxml_xml.W.return str) :: tail
  in List.hd @@ tree_fold f [] [t]

let tyxml_to_body t =
  Format.asprintf "<?xml version=\"1.0\" encoding=\"utf-8\" ?>\n%a"
    (Tyxml.Xml.pp ()) t
