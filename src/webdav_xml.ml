module M = Map.Make(String)

type tree = [
  | `Pcdata of string
  | `Node of (string * string) list * string * tree list
]

let rec tree_fold f s forest = match forest with
  | `Node (a, name, children) :: tail ->
    let children' = tree_fold f s children
    and tail' = tree_fold f s tail in
    f (`Node (a, name, children)) children' tail'
  | (`Pcdata _ as t') :: tail ->
    let tail' = tree_fold f s tail in
    f t' s tail'
  | [] -> s

let tyxml_to_body t =
  Format.asprintf "<?xml version=\"1.0\" encoding=\"utf-8\" ?>\n%a"
    (Tyxml.Xml.pp ()) t

let attrib_to_tyxml (name, value) =
  Tyxml.Xml.string_attrib name (Tyxml_xml.W.return value)

let tree_to_tyxml t =
  let f s children tail = match s with
  | `Node (a, n, c) ->
    let a' = List.map attrib_to_tyxml a in
    Tyxml.Xml.node ~a:a' n (Tyxml_xml.W.return children) :: tail
  | `Pcdata str -> Tyxml.Xml.pcdata (Tyxml_xml.W.return str) :: tail
  in List.hd @@ tree_fold f [] [t]

let tree_to_string t = tyxml_to_body @@ tree_to_tyxml t

let props_to_tree m = M.fold (fun k (a, v) acc -> `Node (a, k, v) :: acc) m []

let props_to_string m =
  let c = props_to_tree m in
  tree_to_string (`Node ([], "prop", c))

let find_props ps m =
  let (found, not_found) = 
    List.fold_left (fun (s, f) k -> match M.find_opt k m with
    | None        -> (s, `Node ([], k, []) :: f)
    | Some (a, v) -> (`Node (a, k, v) :: s, f)) ([], []) ps
  in
  [(`OK, found) ; (`Forbidden, not_found)]

let create_properties ?(content_type = "text/html") ?(language = "en") is_dir timestamp length filename =
  let rtype = if is_dir then [ `Node ([], "collection", []) ] else [] in
  let filename = if filename = "" then "hinz und kunz" else filename in
  M.add "creationdate" ([], [ `Pcdata timestamp ]) @@
  M.add "displayname" ([], [ `Pcdata filename ]) @@
  M.add "getcontentlanguage" ([], [ `Pcdata language ]) @@
  M.add "getcontenttype" ([], [ `Pcdata content_type ]) @@
  M.add "getlastmodified" ([], [ `Pcdata timestamp ]) @@
  M.add "getcontentlength" ([], [ `Pcdata (string_of_int length) ]) @@
  (* M.add "lockdiscovery" *)
  M.add "resourcetype" ([], rtype) M.empty
  (* M.add "supportedlock" *)

let rec pp_tree fmt = function
  | `Pcdata str -> Fmt.string fmt str
  | `Node (attrib, name, children) ->
    Fmt.pf fmt "(%s: %a, %a)"
      name
      Fmt.(list (pair string string)) attrib
      Fmt.(list pp_tree) children

let prop_tree_to_map t =
  match t with
  | `Node (_, "prop", children) -> 
    List.fold_left (fun m c -> match c with
    | `Node (a, k, v) -> M.add k (a, v) m
    | `Pcdata _ -> assert false)
    M.empty children
  | _ -> assert false

let string_to_tree str =
  let data str = `Pcdata str
  and el ((ns, name), attrs) children =
    let a =
      let namespace = match ns with
        | "" -> []
        | ns when ns = "DAV:" -> []
        | ns -> [ ("xmlns", ns) ]
      in
      namespace @ List.map (fun ((ns, name), value) -> 
      let ns' = if ns = "" then "" else if ns = Xmlm.ns_xml then "xml:" else ns ^ ":" in
      (ns' ^ name, value)) attrs
    in
    `Node (a, name, children)
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
         | `Node (_, _, ch) -> filter_map children_p ch
         | `Pcdata _        -> []
       in
       f node ch')

let name str = function
  | (`Node (_, name, _) as node) ->
    if String.equal name str then
      Ok node
    else
      Error ("expected " ^ str ^ ", but found " ^ name)
  | _ ->
    Error ("expected " ^ str ^ ", but got pcdata")

let name_ns name namespace = function
  | (`Node (attr, n, _) as node) ->
    if String.equal name n then
      match List.assoc_opt "xmlns" attr with
      | Some ns when String.equal ns namespace -> Ok node
      | _ -> Error ("expected namespace " ^ namespace)
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
  | `Node (_, n, _) -> Ok n
  | `Pcdata _       -> Error "couldn't extract name from pcdata"

let extract_name_value = function
  | `Node (a, n, c) -> Ok (a, n, c)
  | `Pcdata _       -> Error "couldn't extract name and value from pcdata"

let leaf_node = function
  | `Node (_, _, []) as n -> Ok n
  | _                     -> Error "not a leaf"

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

let prop_parser : tree -> ([> res | `Include of string list ], string) result =
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
      prop_parser
  in
  run tree_grammar tree

let caldav_ns = "urn:ietf:params:xml:ns:caldav"

(* TODO this actually belongs to CalDAV! this is Webdav_xml module! *)
type comp = [ `Comp of string * comp list | `Allcomp | `Allprop | `Prop of string * bool ]

let rec pp_comp ppf = function
  | `Prop (name, value) -> Fmt.pf ppf "prop %s, value %b" name value
  | `Allprop -> Fmt.string ppf "all properties"
  | `Allcomp -> Fmt.string ppf "all components"
  | `Comp (name, comps) -> Fmt.pf ppf "component %s, %a" name Fmt.(list ~sep:(unit ", ") pp_comp) comps

let rec comp_parser tree : (comp, string) result =
  tree_lift
    (fun (`Node (a, _, _)) c ->
       match List.assoc_opt "name" a with
       | None -> Error "Expected name in comp"
       | Some name -> Ok (`Comp (name, c)))
    (name_ns "comp" caldav_ns)
    ((tree_lift (fun _ c -> is_empty c >>| fun () -> `Allprop) (name_ns "allprop" caldav_ns) any)
     ||| (tree_lift
            (fun (`Node (a, _, _)) c ->
               let novalue = match List.assoc_opt "novalue" a with
                 | Some "yes" -> true
                 | _ -> false
               in
               match List.assoc_opt "name" a with
               | None -> Error "No name in prop"
               | Some name' -> is_empty c >>| fun () -> (`Prop (name', novalue)))
            (name_ns "prop" caldav_ns) any)
     ||| (tree_lift (fun _ c -> is_empty c >>| fun () -> `Allcomp) (name_ns "allcomp" caldav_ns) any)
     ||| comp_parser)
    tree

let calendar_data_parser =
  tree_lift (fun _ c -> Ok (`Calendar_data c))
    (name_ns "calendar-data" caldav_ns)
    comp_parser

type calendar_data = [
  | `All_prop of string list
  | `Calendar_data of comp list
  | `Propname
  | `Props of string list
]

let pp_calendar_data ppf = function
  | #res as r -> pp_prop ppf r
  | `Calendar_data xs -> Fmt.pf ppf "calendar data %a" Fmt.(list ~sep:(unit ", ") pp_comp) xs

let parse_calendar_query_xml tree =
  let tree_grammar =
    tree_lift
      (fun _ c -> match c with
         | [ #res as r ] -> Ok r
         | [ `Calendar_data x ] -> Ok (`Calendar_data x)
         | [ `Include _ ] -> Error "lonely include"
         | [ `All_prop _ ; `Include is ] -> Ok (`All_prop is)
         | _ -> Error "broken")
      (name_ns "calendar-query" caldav_ns)
      (prop_parser ||| calendar_data_parser)
  in
  run tree_grammar tree

let pp_propupdate fmt update =
  List.iter (function
      | `Set (a, k, v) -> Fmt.pf fmt "Set %a %s %a" Fmt.(list (pair string string)) a k (Fmt.list pp_tree) v
      | `Remove k   -> Fmt.pf fmt "Remove %s" k
    ) update

let exactly_one = function
  | [ x ] -> Ok x
  | _     -> Error "expected exactly one child"

let prop_parser f =
  tree_lift (fun _ c -> Ok c) (name "prop") (any >>= f)

let set_parser : tree -> ([>`Set of (string * string) list * string * tree list] list, string) result =
  tree_lift (* exactly one prop tag, but a list of property trees below that tag *)
    (fun _ c -> exactly_one c >>| List.map (fun k -> `Set k))
    (name "set")
    (prop_parser extract_name_value)

let remove_parser : tree -> ([>`Remove of string] list, string) result =
  tree_lift (* exactly one prop tag, but a list of property trees below that tag *)
    (fun _ c -> exactly_one c >>| List.map (fun k -> `Remove k))
    (name "remove")
    (prop_parser extract_name)

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
