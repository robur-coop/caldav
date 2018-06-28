module M = Map.Make(String)

type namespace = string
type name = string
type fqname = namespace * name
type attribute = fqname * string

type tree =
  | Pcdata of string
  | Node of namespace * name * attribute list * tree list

(* TODO this actually belongs to CalDAV! this is Webdav_xml module! *)
type comp = [ `Comp of string * comp list | `Allcomp | `Allprop | `Prop of string * bool ]

type calendar_data = [
  | `All_props
  | `Proplist of [ `Calendar_data of comp list | `Prop of fqname ] list
  | `Propname
]

type param_filter = [ `Param_filter of string * [ `Is_not_defined | `Text_match of string list * string * bool ] list ]

type prop_filter =
  [ `Prop_filter of
      string *
      [ `Exists
      | `Is_not_defined
      | `Range of (string * string) * param_filter list
      | `Text of (string list * string * bool) * param_filter list ]
  ]

type comp_filter =
  [ `Comp_filter of
      string *
      [ `Exists
      | `Is_not_defined
      | `Prop_or_comp of prop_filter list * comp_filter list
      | `Prop_or_comp_with_range of (string * string) * prop_filter list * comp_filter list ]
  ]

(*
type calendar_query = calendar_data option * filter
*)

let pp_fqname = Fmt.(pair ~sep:(unit ":") string string)

let pp_attrib = Fmt.(pair ~sep:(unit "=") pp_fqname string) 

let rec pp_tree fmt = function
  | Pcdata str -> Fmt.string fmt str
  | Node (namespace, name, attributes, children) ->
    Fmt.pf fmt "(%s:%s (%a), (%a))"
      namespace name
      Fmt.(list ~sep:(unit ", ") pp_attrib) attributes
      Fmt.(list ~sep:(unit ", ") pp_tree) children

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


let new_identifier map =
  let taken s = M.exists (fun _ v -> String.equal v s) map in
  let rec gen i = 
    let id = String.make 1 (char_of_int i) in
    if taken id then gen (succ i) else id in
  gen 0x41 

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
    let unapply ns name m = match M.find_opt ns m with
      | None when ns = "" -> (m, name, []) (* node has no namespace *)
      | None when ns = Xmlm.ns_xml -> (m, "xml:" ^ name, [])
      | None ->
        let id = new_identifier m in
        (M.add ns id m, id ^ ":" ^ name, [(id, ns)])
      | Some "" -> (m, name, [])
      | Some short -> (m, short ^ ":" ^ name, [])
    in
    let (ns_map'', n', new_ns) = unapply ns n ns_map' in
    let (ns_map''', a', new_ns') = List.fold_left (fun (m, attributes, new_ns) ((ns, n), value) -> 
      let (m', name, new_ns') = unapply ns n m in
      m', (("", name), value) :: attributes, new_ns' @ new_ns) (ns_map'', [], new_ns) a in
    let c' = List.map (tree_unapply_namespaces ~ns_map:ns_map''') c in
    let a'' = List.map (fun (id, ns) -> (("", "xmlns:" ^ id), ns)) new_ns' in
    node ~a:(a' @ a'') n' c'
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

module PairMap = Map.Make (struct
  type t = string * string
  let compare (a1, a2) (b1, b2) = match String.compare a1 b1 with
    | 0 -> String.compare a2 b2
    | x -> x
  end)
 
let props_to_tree m =
  PairMap.fold (fun (ns, k) (a, v) acc -> 
    node ~ns ~a k v :: acc) m []

let props_to_string m =
  let c = props_to_tree m in
  tree_to_string (node ~ns:dav_ns "prop" c)

let find_props ps m =
  let (found, not_found) =
    List.fold_left (fun (s, f) (ns, k) -> match PairMap.find_opt (ns, k) m with
    | None        -> (s, node ~ns k [] :: f)
    | Some (a, v) -> (node ~ns ~a k v :: s, f)) ([], []) ps
  in
  [(`OK, found) ; (`Forbidden, not_found)]

let create_properties ?(content_type = "text/html") ?(language = "en") is_dir timestamp length filename =
  let rtype = if is_dir then [ node ~ns:dav_ns "collection" [] ] else [] in
  let filename = if filename = "" then "hinz und kunz" else filename in
  PairMap.add (dav_ns, "creationdate") ([], [ Pcdata timestamp ]) @@
  PairMap.add (dav_ns, "displayname") ([], [ Pcdata filename ]) @@
  PairMap.add (dav_ns, "getcontentlanguage") ([], [ Pcdata language ]) @@
  PairMap.add (dav_ns, "getcontenttype") ([], [ Pcdata content_type ]) @@
  PairMap.add (dav_ns, "getlastmodified") ([], [ Pcdata timestamp ]) @@
  PairMap.add (dav_ns, "getcontentlength") ([], [ Pcdata (string_of_int length) ]) @@
  (* PairMap.add "lockdiscovery" *)
  PairMap.add (dav_ns, "resourcetype") ([], rtype) PairMap.empty
  (* PairMap.add "supportedlock" *)

let prop_tree_to_map t =
  match t with
  | Node (_, "prop", _, children) ->
    List.fold_left (fun m c -> match c with
        | Node (ns, k, a, v) -> PairMap.add (ns, k) (a, v) m
        | Pcdata _ -> assert false)
      PairMap.empty children
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
  | `Props xs -> Fmt.pf fmt "Props %a" Fmt.(list ~sep:(unit ",@ ") pp_fqname) xs

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

let (>>~) p f =
  (fun tree -> match p tree with
     | Ok x    -> f x
     | Error e -> Error e)

let extract_name = function
  | Node (_, n, _, _) -> Ok n
  | Pcdata _          -> Error "couldn't extract name from pcdata"

let extract_ns_name = function
  | Node (ns, n, _, _) -> Ok (ns, n)
  | Pcdata _          -> Error "couldn't extract name from pcdata"

let extract_name_value = function
  | Node (ns, n, a, c) -> Ok (a, (ns, n), c)
  | Pcdata _           -> Error "couldn't extract name and value from pcdata"

let extract_pcdata = function
  | Pcdata str -> Ok str
  | Node _ -> Error "Expected PCDATA"

let extract_attributes = function
  | Node (_, _, a, _) -> Ok a
  | Pcdata _ -> Error "Expected Node with attributes, got PCDATA"

let leaf_node = function
  | Node (_, _, _, []) as n -> Ok n
  | _                       -> Error "not a leaf"

let (|||) = alternative
open Rresult.R.Infix

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
  | `Props of fqname list
]

let propfind_prop_parser : tree -> ([> res | `Include of string list ], string) result =
  ((tree_lift
      (fun _ c -> is_empty c >>| fun () -> `Propname)
      (name "propname") any)
   ||| (tree_lift
          (fun _ c -> non_empty c >>| fun () -> `Props c)
          (name "prop") (any >>~ extract_ns_name))
   ||| (tree_lift
          (fun _ c -> is_empty c >>| fun () -> `All_prop [])
          (name "allprop") any)
   ||| (tree_lift
          (fun _ c -> non_empty c >>| fun () -> `Include c)
          (name "include") (any >>~ extract_name)))

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
  | Node (ns, name, _, _) -> Ok (`Prop (ns, name))
  | Pcdata _ -> Error "expected node"

let pp_proplist_element ppf = function
  | `Calendar_data xs -> Fmt.pf ppf "calendar data %a" Fmt.(list ~sep:(unit ", ") pp_comp) xs
  | `Prop n -> Fmt.pf ppf "property %a" pp_fqname n

let pp_calendar_data ppf = function
  | `All_props -> Fmt.string ppf "all properties"
  | `Propname -> Fmt.string ppf "property name"
  | `Proplist xs -> Fmt.pf ppf "proplist (%a)" Fmt.(list ~sep:(unit ", ") pp_proplist_element) xs

let report_prop_parser : tree -> ([> calendar_data], string) result =
  (tree_lift
     (fun _ c -> is_empty c >>| fun () -> `Propname)
     (name_ns "propname" dav_ns) any)
  ||| (tree_lift
         (fun _ c -> is_empty c >>| fun () -> `All_props)
         (name_ns "allprop" dav_ns) any)
  ||| (tree_lift
         (fun _ c -> non_empty c >>| fun () -> `Proplist c)
         (name_ns "prop" dav_ns)
         (calendar_data_parser ||| (any >>~ extract_and_tag_prop)))

let is_not_defined_parser =
  tree_lift
    (fun _ c -> is_empty c >>| fun () -> `Is_not_defined)
    (name_ns "is-not-defined" caldav_ns)
    any

let text_match_parser =
  tree_lift
    (fun a c -> 
      let collation = 
      match find_attribute "collation" a with
      | None -> "i;ascii-casemap"
      | Some v -> v 
      in
      let negate =
      match find_attribute "negate-condition" a with
      | Some "yes" -> true
      | _ -> false
      in 
      Ok (`Text_match (c, collation, negate))
    )
    (name_ns "text-match" caldav_ns >>~ extract_attributes)
    extract_pcdata

let param_filter_parser : tree -> ([> param_filter ], string) result =
  tree_lift
    (fun a c -> match find_attribute "name" a with
       | None -> Error "Attribute \"name\" required"
       | Some n -> Ok (`Param_filter (n, c)))
    (name_ns "param-filter" caldav_ns >>~ extract_attributes)
    (is_not_defined_parser ||| text_match_parser)

let time_range_parser: tree -> ([> `Timerange of string * string ], string) result =
  tree_lift
    (fun a c -> is_empty c >>= fun () -> match find_attribute "start" a, find_attribute "end" a with
     | None, _ | _, None -> Error "Missing attribute \"start\" or \"end\""
     | Some s, Some e -> Ok (`Timerange (s, e)))
    (name_ns "time-range" caldav_ns >>~ extract_attributes)
    (any)

let all_param_filters lst : (param_filter list, string) result = 
  List.fold_left (fun acc elem -> match acc, elem with
   | Ok items, `Param_filter f -> Ok (`Param_filter f :: items)
   | _ -> Error "Only param-filters allowed.") (Ok []) lst

let prop_filter_parser : tree -> ([> prop_filter ], string) result =
  tree_lift
    (fun a c -> match find_attribute "name" a with 
      | None -> Error "Attribute \"name\" required."
      | Some n -> match c with
        | [] -> Ok (`Prop_filter (n, `Exists))
        | [ `Is_not_defined ] -> Ok (`Prop_filter (n, `Is_not_defined))
        | `Timerange t :: pfs -> all_param_filters pfs >>| fun pfs' -> `Prop_filter (n, `Range (t, pfs'))
        | `Text_match t :: pfs -> all_param_filters pfs >>| fun pfs' -> `Prop_filter (n, `Text (t, pfs'))
        | _ -> Error "Invalid prop-filter.")
    (name_ns "prop-filter" caldav_ns >>~ extract_attributes)
    (is_not_defined_parser ||| time_range_parser ||| text_match_parser ||| param_filter_parser)

let split_filters lst =
  List.fold_left (fun acc elem -> match acc, elem with
   | Ok (pflst, []), `Prop_filter f -> Ok (`Prop_filter f :: pflst, [])
   | Ok (pflst, cflst), `Comp_filter f -> Ok (pflst, `Comp_filter f :: cflst)
   | _ -> Error "Only prop- or comp-filters allowed.")
    (Ok ([], [])) lst >>| fun (pflst, cflst) ->
  (List.rev pflst, List.rev cflst)

let rec comp_filter_parser tree : ([> comp_filter ], string) result =
  tree_lift
    (fun a c -> match find_attribute "name" a with
       | None -> Error "Attribute \"name\" required."
       | Some n -> match c with
         | [] -> Ok (`Comp_filter (n, `Exists))
         | [`Is_not_defined ] -> Ok (`Comp_filter (n, `Is_not_defined))
         | `Timerange t :: pfs -> split_filters pfs >>| fun (pflst, cflst) -> `Comp_filter (n, `Prop_or_comp_with_range (t, pflst, cflst))
         | pfs -> split_filters pfs >>| fun (pflst, cflst) -> `Comp_filter (n, `Prop_or_comp (pflst, cflst)))
    (name_ns "comp-filter" caldav_ns >>~ extract_attributes)
    (is_not_defined_parser ||| time_range_parser ||| prop_filter_parser ||| comp_filter_parser)
    tree

let filter_parser = 
  tree_lift 
    (fun _ c -> Ok (`Filter c))
    (name_ns "filter" caldav_ns)
    (comp_filter_parser)

let parse_calendar_query_xml tree =
  let tree_grammar =
    tree_lift
      (fun _ c -> let prop, rest = match c with
         | `Propname :: xs -> Some `Propname, xs
         | `Proplist x :: xs -> Some (`Proplist x), xs
         | `All_props :: xs -> Some `All_props, xs
         | xs -> None, xs
       in
       match rest with
         | `Filter f :: xs -> Ok (prop, f)
         | xs -> Error "broken1")
      (name_ns "calendar-query" caldav_ns)
      (report_prop_parser ||| filter_parser)
  in
  Format.printf "input tree: (%a)@." pp_tree tree ;
  run tree_grammar tree

let pp_propupdate fmt update =
  List.iter (function
      | `Set (a, k, v) -> Fmt.pf fmt "Set %a %a %a" Fmt.(list ~sep:(unit ",") pp_attrib) a pp_fqname k (Fmt.list pp_tree) v
      | `Remove k   -> Fmt.pf fmt "Remove %a" pp_fqname k
    ) update

let exactly_one = function
  | [ x ] -> Ok x
  | _     -> Error "expected exactly one child"

let proppatch_prop_parser f =
  tree_lift (fun _ c -> Ok c) (name "prop") (any >>~ f)

let set_parser : tree -> ([>`Set of attribute list * fqname * tree list] list, string) result =
  tree_lift (* exactly one prop tag, but a list of property trees below that tag *)
    (fun _ c -> exactly_one c >>| List.map (fun k -> `Set k))
    (name "set")
    (proppatch_prop_parser extract_name_value)

let remove_parser : tree -> ([>`Remove of fqname] list, string) result =
  tree_lift (* exactly one prop tag, but a list of property trees below that tag *)
    (fun _ c -> exactly_one c >>| List.map (fun k -> `Remove k))
    (name "remove")
    (proppatch_prop_parser extract_ns_name)

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

let get_prop p map = PairMap.find_opt p map

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
