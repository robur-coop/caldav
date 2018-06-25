module M = Map.Make(String)

type namespace = string
type name = string
type attribute = (string * string) * string

type tree =
  | Pcdata of string
  | Node of namespace * name * attribute list * tree list

let node ?(ns = "") name ?(a = []) children = Node (ns, name, a, children)

let caldav_ns = "urn:ietf:params:xml:ns:caldav"
let dav_ns = "DAV:"

let rec tree_fold_right f s forest = match forest with
  | (Node (_, _, _, children) as n) :: tail ->
    let children' = tree_fold_right f s children
    and tail' = tree_fold_right f s tail in
    f n children' tail' (* f: tree -> 'a -> 'a -> 'a *)
  | (Pcdata _ as t') :: tail ->
    let tail' = tree_fold_right f s tail in
    f t' s tail'
  | [] -> s

(*
List.fold_left : ('a -> 'b -> 'a) -> 'a -> 'b list -> 'a
List.fold_right : ('b -> 'a -> 'a) -> 'b list -> 'a -> 'a


(* f : 'a -> tree list -> 'a *)
let rec tree_fold_left f s forest = match forest with
  | (Node (_, _, _, children) as n) :: tail ->
    let s' = f n s children tail in
    let children' = tree_fold_left f s' children
    and tail' = tree_fold_left f s' tail in
    children' tail'
  | (Pcdata _ as t') :: tail ->
    let tail' = tree_fold_left f s tail in
    f t' s tail'
  | [] -> s
*)

(* apply_variables <map> <Tyxml> -> <list of trees> *)

let rec tree_unapply_namespaces ?(ns_map = M.empty) = function
  | Node (ns, n, a, c) ->
    let ns_map' = List.fold_left (fun m ((ns, key), value) ->
        (* TODO ensure uniqueness of values in map! *)
        (* TODO correct behaviour when key already in map? *)
        if ns = Xmlm.ns_xmlns then
          if key = "xmlns" then
            M.add value "" m
          else
            M.add value key m
        else
          m) ns_map a
    in
    let unapply ns name = match M.find_opt ns ns_map' with
      | None when ns = "" -> name
      | None -> ns ^ "F:F" ^ name (* TODO unclear whether this is valid XML *)
      (* <"http://foo.com":foo/> ~~> <F:foo xmlns:F="http://foo.com"/> ?? *)
      | Some "" -> name
      | Some short -> short ^ ":" ^ name
    in
    let n' = unapply ns n in
    let a' = List.map (fun ((ns, n), value) -> (("", unapply ns n), value)) a in
    let c' = List.map (tree_unapply_namespaces ~ns_map:ns_map') c in
    node ~a:a' n' c'
  | Pcdata data -> Pcdata data

let tyxml_to_body t =
  Format.asprintf "<?xml version=\"1.0\" encoding=\"utf-8\" ?>\n%a"
    (Tyxml.Xml.pp ()) t

let attrib_to_tyxml ((_, name), value) =
  Tyxml.Xml.string_attrib name (Tyxml_xml.W.return value)

let tree_to_tyxml t =
  let t' = tree_unapply_namespaces t in
  let f s children tail = match s with
    | Node (_, n, a, c) ->
      let a' = List.map attrib_to_tyxml a in
      Tyxml.Xml.node ~a:a' n (Tyxml_xml.W.return children) :: tail
    | Pcdata str -> Tyxml.Xml.pcdata (Tyxml_xml.W.return str) :: tail
  in List.hd @@ tree_fold_right f [] [t']

let tree_to_string t = tyxml_to_body @@ tree_to_tyxml t

let props_to_tree m =
  M.fold (fun k (a, v) acc -> node ~a k v :: acc) m []

let props_to_string m =
  let c = props_to_tree m in
  tree_to_string (node "prop" c)

let find_props ps m =
  let (found, not_found) =
    List.fold_left (fun (s, f) k -> match M.find_opt k m with
    | None        -> (s, node k [] :: f)
    | Some (a, v) -> (node ~a k v :: s, f)) ([], []) ps
  in
  [(`OK, found) ; (`Forbidden, not_found)]

let create_properties ?(content_type = "text/html") ?(language = "en") is_dir timestamp length filename =
  let rtype = if is_dir then [ node "collection" [] ] else [] in
  let filename = if filename = "" then "hinz und kunz" else filename in
  M.add "creationdate" ([], [ Pcdata timestamp ]) @@
  M.add "displayname" ([], [ Pcdata filename ]) @@
  M.add "getcontentlanguage" ([], [ Pcdata language ]) @@
  M.add "getcontenttype" ([], [ Pcdata content_type ]) @@
  M.add "getlastmodified" ([], [ Pcdata timestamp ]) @@
  M.add "getcontentlength" ([], [ Pcdata (string_of_int length) ]) @@
  (* M.add "lockdiscovery" *)
  M.add "resourcetype" ([], rtype) M.empty
  (* M.add "supportedlock" *)

let pp_attrib fmt attribute =
  Fmt.(pair ~sep:(unit "=") (pair ~sep:(unit ":") string string) string) fmt
    attribute

let rec pp_tree fmt = function
  | Pcdata str -> Fmt.string fmt str
  | Node (namespace, name, attributes, children) ->
    Fmt.pf fmt "(%s:%s (%a), (%a))"
      namespace name
      Fmt.(list ~sep:(unit ", ") pp_attrib) attributes
      Fmt.(list ~sep:(unit ", ") pp_tree) children

let prop_tree_to_map t =
  match t with
  | Node (_, "prop", _, children) ->
    List.fold_left (fun m c -> match c with
        | Node (_, k, a, v) -> M.add k (a, v) m
        | Pcdata _ -> assert false)
      M.empty children
  | _ -> assert false

let string_to_tree str =
  let data str = Pcdata str
  and el ((ns, name), attrs) children = node ~ns ~a:attrs name children
  in
  try
    let input = Xmlm.make_input ~strip:true (`String (0, str)) in
    ignore (Xmlm.input input) ; (* ignore DTD *)
    Some (Xmlm.input_tree ~el ~data input)
  with _ -> None

let rec pp_prop fmt = function
  | `Propname -> Fmt.string fmt "Propname"
  | `All_prop xs -> Fmt.pf fmt "All prop %a" Fmt.(list ~sep:(unit ",@ ") string) xs
  | `Props xs -> Fmt.pf fmt "Props %a" Fmt.(list ~sep:(unit ",@ ") string) xs

let rec filter_map f = function
  | []    -> []
  | x::xs ->
    match f x with
    | Error e ->       filter_map f xs
    | Ok x'   -> x' :: filter_map f xs

let tree_lift f node_p children_p =
  (fun tree ->
     match node_p tree with
     | Error e -> Error e
     | Ok node ->
       let ch' = match tree with
         | Node (_, _, _, ch) -> filter_map children_p ch
         | Pcdata _           -> []
       in
       f node ch')

let name str = function
  | (Node (_, name, _, _) as node) ->
    if String.equal name str then
      Ok node
    else
      Error ("expected " ^ str ^ ", but found " ^ name)
  | _ ->
    Error ("expected " ^ str ^ ", but got pcdata")

let name_ns name namespace = function
  | (Node (ns, n, _, _) as node) ->
    if String.equal name n && String.equal ns namespace then
      Ok node
    else
      Error ("expected " ^ name ^ ", but found " ^ n)
  | _ -> Error ("expected " ^ name ^ ", but got pcdata")

let any tree = Ok tree

let alternative a b =
  (fun tree ->
     match a tree with
     | Error _ -> b tree
     | Ok x    -> Ok x)

let (>>=) p f =
  (fun tree -> match p tree with
     | Ok x    -> f x
     | Error e -> Error e)

let extract_name = function
  | Node (_, n, _, _) -> Ok n
  | Pcdata _          -> Error "couldn't extract name from pcdata"

(* TODO care about namespace (but more complex, needs refactoring of property map) *)
let extract_name_value = function
  | Node (ns, n, a, c) -> Ok (a, n, c)
  | Pcdata _           -> Error "couldn't extract name and value from pcdata"

let leaf_node = function
  | Node (_, _, _, []) as n -> Ok n
  | _                       -> Error "not a leaf"

let (|||) = alternative
let (>>|) = Rresult.R.Infix.(>>|)

let is_empty = function
  | [] -> Ok ()
  | _ -> Error "expected no children, but got some"

let non_empty = function
  | [] -> Error "expected non-empty, got empty"
  | _  -> Ok ()

let run p tree = p tree

type res = [
  | `All_prop of string list
  | `Propname
  | `Props of string list
]

let propfind_prop_parser : tree -> ([> res | `Include of string list ], string) result =
  ((tree_lift
      (fun _ c -> is_empty c >>| fun () -> `Propname)
      (name "propname") any)
   ||| (tree_lift
          (fun _ c -> non_empty c >>| fun () -> `Props c)
          (name "prop") (any >>= extract_name))
   ||| (tree_lift
          (fun _ c -> is_empty c >>| fun () -> `All_prop [])
          (name "allprop") any)
   ||| (tree_lift
          (fun _ c -> non_empty c >>| fun () -> `Include c)
          (name "include") (any >>= extract_name)))

let parse_propfind_xml tree =
  let tree_grammar =
    tree_lift
      (fun _ c -> match c with
         | [ #res as r ] -> Ok r
         | [ `Include _ ] -> Error "lonely include"
         | [ `All_prop _ ; `Include is ] -> Ok (`All_prop is)
         | _ -> Error "broken")
      (name "propfind")
      propfind_prop_parser
  in
  run tree_grammar tree

(* TODO this actually belongs to CalDAV! this is Webdav_xml module! *)
type comp = [ `Comp of string * comp list | `Allcomp | `Allprop | `Prop of string * bool ]

let rec pp_comp ppf = function
  | `Prop (name, value) -> Fmt.pf ppf "prop %s, value %b" name value
  | `Allprop -> Fmt.string ppf "all properties"
  | `Allcomp -> Fmt.string ppf "all components"
  | `Comp (name, comps) -> Fmt.pf ppf "component %s, %a" name Fmt.(list ~sep:(unit ", ") pp_comp) comps

let find_attribute name attrs =
  match List.filter (fun ((_, n'), _) -> String.equal n' name) attrs with
  | [ (_, v) ] -> Some v
  | _ -> None

let rec comp_parser tree : (comp, string) result =
  tree_lift
    (fun (Node (_, _, a, _)) c ->
         match find_attribute "name" a with
         | None -> Error "Expected name in comp"
         | Some name -> Ok (`Comp (name, c)))
    (name_ns "comp" caldav_ns)
    ((tree_lift (fun _ c -> is_empty c >>| fun () -> `Allprop) (name_ns "allprop" caldav_ns) any)
     ||| (tree_lift
            (fun (Node (_, _, a, _)) c ->
               let novalue = match find_attribute "novalue" a with
                 | Some "yes" -> true
                 | _ -> false
               in
               match find_attribute "name" a with
               | None -> Error "No name in prop"
               | Some name' -> is_empty c >>| fun () -> (`Prop (name', novalue)))
            (name_ns "prop" caldav_ns) any)
     ||| (tree_lift (fun _ c -> is_empty c >>| fun () -> `Allcomp) (name_ns "allcomp" caldav_ns) any)
     ||| comp_parser)
    tree

let calendar_data_parser : tree -> ([> `Calendar_data of comp list ], string) result =
  tree_lift (fun _ c -> Ok (`Calendar_data c))
    (name_ns "calendar-data" caldav_ns)
    comp_parser

let any_in_ns ns = function
  | Node (ns', _, _, _) as n ->
    if String.equal ns ns' then
      Ok n
    else
      Error ("expected namespace " ^ ns)
  | Pcdata _ -> Error "expected a node"

let extract_and_tag_prop = function
  | Node (_, name, _, _) -> Ok (`Prop name)
  | Pcdata _ -> Error "expected node"

type calendar_data = [
  | `All_props
  | `Proplist of [ `Calendar_data of comp list | `Prop of string ] list
  | `Propname
]

let pp_proplist_element ppf = function
  | `Calendar_data xs -> Fmt.pf ppf "calendar data %a" Fmt.(list ~sep:(unit ", ") pp_comp) xs
  | `Prop s -> Fmt.pf ppf "property %s" s

let pp_calendar_data ppf = function
  | `All_props -> Fmt.string ppf "all properties"
  | `Propname -> Fmt.string ppf "property name"
  | `Proplist xs -> Fmt.pf ppf "proplist (%a)" Fmt.(list ~sep:(unit ", ") pp_proplist_element) xs

let report_prop_parser : tree -> (calendar_data, string) result =
  (tree_lift
     (fun _ c -> is_empty c >>| fun () -> `Propname)
     (name_ns "propname" dav_ns) any)
  ||| (tree_lift
         (fun _ c -> is_empty c >>| fun () -> `All_props)
         (name_ns "allprop" dav_ns) any)
  ||| (tree_lift
         (fun _ c -> non_empty c >>| fun () -> `Proplist c)
         (name_ns "prop" dav_ns)
         ((any_in_ns dav_ns >>= extract_and_tag_prop) ||| calendar_data_parser))

let parse_calendar_query_xml tree =
  let tree_grammar =
    tree_lift
      (fun _ c -> match c with
         | [ `Propname ] -> Ok `Propname
         | [ `Proplist x ] -> Ok (`Proplist x)
         | [ `All_props ] -> Ok `All_props
         | xs ->
           Format.printf "got calendar data (%a)@." Fmt.(list ~sep:(unit ", ") pp_calendar_data) xs ;
           Error "broken1")
      (name_ns "calendar-query" caldav_ns)
      report_prop_parser
  in
  Format.printf "input tree: (%a)@." pp_tree tree ;
  run tree_grammar tree

let pp_propupdate fmt update =
  List.iter (function
      | `Set (a, k, v) -> Fmt.pf fmt "Set %a %s %a" Fmt.(list ~sep:(unit ",") pp_attrib) a k (Fmt.list pp_tree) v
      | `Remove k   -> Fmt.pf fmt "Remove %s" k
    ) update

let exactly_one = function
  | [ x ] -> Ok x
  | _     -> Error "expected exactly one child"

let proppatch_prop_parser f =
  tree_lift (fun _ c -> Ok c) (name "prop") (any >>= f)

let set_parser : tree -> ([>`Set of attribute list * string * tree list] list, string) result =
  tree_lift (* exactly one prop tag, but a list of property trees below that tag *)
    (fun _ c -> exactly_one c >>| List.map (fun k -> `Set k))
    (name "set")
    (proppatch_prop_parser extract_name_value)

let remove_parser : tree -> ([>`Remove of string] list, string) result =
  tree_lift (* exactly one prop tag, but a list of property trees below that tag *)
    (fun _ c -> exactly_one c >>| List.map (fun k -> `Remove k))
    (name "remove")
    (proppatch_prop_parser extract_name)

let parse_propupdate_xml tree =
  let propupdate =
    tree_lift
      (fun _ lol -> Ok (List.flatten lol))
      (name "propertyupdate")
      (set_parser ||| remove_parser)
  in
  run propupdate tree

let parse_mkcol_xml tree =
  let mkcol =
    tree_lift
      (fun _ lol -> Ok (List.flatten lol))
      (name "mkcol")
      set_parser
  in
  run mkcol tree

let get_prop p map = M.find_opt p map

let ptime_to_http_date ptime =
  let (y, m, d), ((hh, mm, ss), _)  = Ptime.to_date_time ptime
  and weekday = match Ptime.weekday ptime with
  | `Mon -> "Mon"
  | `Tue -> "Tue"
  | `Wed -> "Wed"
  | `Thu -> "Thu"
  | `Fri -> "Fri"
  | `Sat -> "Sat"
  | `Sun -> "Sun"
  and month = [|"Jan"; "Feb"; "Mar"; "Apr"; "May"; "Jun"; "Jul"; "Aug"; "Sep"; "Oct"; "Nov"; "Dec"|]
  in
  Printf.sprintf "%s, %02d %s %04d %02d:%02d:%02d GMT" weekday d (Array.get month (m-1)) y hh mm ss 
