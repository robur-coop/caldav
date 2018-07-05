open Rresult.R.Infix

module M = Map.Make(String)

type namespace = string [@@deriving show, eq]
type name = string [@@deriving show, eq]
type fqname = namespace * name [@@deriving show, eq]
type attribute = fqname * string [@@deriving show, eq]

type tree =
  | Pcdata of string
  | Node of namespace * name * attribute list * tree list
  [@@deriving show, eq]

type propfind = [
  | `All_prop of string list
  | `Propname
  | `Props of fqname list
] [@@deriving show, eq]

type propupdate = [
  | `Set of attribute list * fqname * tree list
  | `Remove of fqname
] [@@deriving show, eq] 

(* TODO this actually belongs to CalDAV! this is Webdav_xml module! *)
type comp = [ `Allcomp | `Comp of component list ]
and prop = [ `Allprop | `Prop of (string * bool) list ]
and component = string * prop * comp [@@deriving show, eq]

type timerange = string * string [@@deriving show, eq]

type calendar_data =
  component option *
  [ `Expand of timerange | `Limit_recurrence_set of timerange ] option *
  [ `Limit_freebusy_set of timerange ] option [@@deriving show, eq]

type report_prop = [
  | `All_props
  | `Proplist of [ `Calendar_data of calendar_data | `Prop of fqname ] list
  | `Propname
] [@@deriving show, eq]

type param_filter = [
    `Param_filter of
      string *
      [ `Is_defined (* represents empty filter in RFC *)
      | `Is_not_defined
      | `Text_match of string * string * bool ]
] [@@deriving show, eq]

type prop_filter =
  string *
  [ `Is_defined (* represents empty filter in RFC *)
  | `Is_not_defined
  | `Range of (string * string) * param_filter list
  | `Text of (string * string * bool) * param_filter list ] [@@deriving show, eq]

(* TODO maybe add tag, make filter section more clear in the parse tree *)
type comp_filter = [
  | `Is_defined (* represents empty filter in RFC *)
  | `Is_not_defined
  | `Comp_filter of timerange option * prop_filter list * component_filter list
]
and component_filter = string * comp_filter [@@deriving show, eq]

type calendar_query = report_prop option * component_filter [@@deriving show, eq]

let caldav_ns = "urn:ietf:params:xml:ns:caldav"
let dav_ns = "DAV:"

let node ?(ns = "") name ?(a = []) children = Node (ns, name, a, children)
let dav_node = node ~ns:dav_ns
let pcdata str = Pcdata str

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
      | None when ns = Xmlm.ns_xmlns -> (m, "xmlns:" ^ name, [])
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
  Format.printf "map is %a\n" Fmt.(list ~sep:(unit "\n") pp_tree) c ;
  tree_to_string (node ~ns:dav_ns "prop" c)

let find_props ps m =
  let (found, not_found) =
    List.fold_left (fun (s, f) (ns, k) -> match PairMap.find_opt (ns, k) m with
        | None        -> (s, node ~ns k [] :: f)
        | Some (a, v) -> (node ~ns ~a k v :: s, f))
      ([], []) ps
  in
  match found, not_found with
  | [], [] -> []
  | [], nf -> [ (`Not_found, nf) ]
  | f, [] -> [ (`OK, f) ]
  | f, nf -> [ (`OK, f) ; (`Not_found, nf) ]

let create_properties ?(content_type = "text/html") ?(language = "en") is_dir ?etag timestamp length filename =
  let rtype = if is_dir then [ node ~ns:dav_ns "collection" [] ] else [] in
  let filename = if filename = "" then "hinz und kunz" else filename in
  let etag' m = match etag with None -> m | Some e -> PairMap.add (dav_ns, "getetag") ([], [ Pcdata e ]) m in
  etag' @@
  PairMap.add (dav_ns, "creationdate") ([], [ Pcdata timestamp ]) @@
  PairMap.add (dav_ns, "displayname") ([], [ Pcdata filename ]) @@
  PairMap.add (dav_ns, "getcontentlanguage") ([], [ Pcdata language ]) @@
  PairMap.add (dav_ns, "getcontenttype") ([], [ Pcdata content_type ]) @@
  PairMap.add (dav_ns, "getcontentlength") ([], [ Pcdata (string_of_int length) ]) @@
  PairMap.add (dav_ns, "getlastmodified") ([], [ Pcdata timestamp ]) @@
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

let rec filter_map f = function
  | []    -> []
  | x::xs ->
    match f x with
    | Error e ->       filter_map f xs
    | Ok x'   -> x' :: filter_map f xs

let tree_lift f node_p children_p =
  (fun tree ->
     node_p tree >>= fun node ->
     let ch' = match tree with
       | Node (_, _, _, ch) -> filter_map children_p ch
       | Pcdata _           -> []
     in
     f node ch')

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

let is_empty = function
  | [] -> Ok ()
  | _ -> Error "expected no children, but got some"

let non_empty = function
  | [] -> Error "expected non-empty, got empty"
  | _  -> Ok ()

let run p tree = p tree

let propfind_prop_parser : tree -> ([ propfind | `Include of string list ], string) result =
  ((tree_lift
      (fun _ c -> is_empty c >>| fun () -> `Propname)
      (name_ns "propname" dav_ns) any)
   ||| (tree_lift
          (fun _ c -> non_empty c >>| fun () -> `Props c)
          (name_ns "prop" dav_ns) (any >>~ extract_ns_name))
   ||| (tree_lift
          (fun _ c -> is_empty c >>| fun () -> `All_prop [])
          (name_ns "allprop" dav_ns) any)
   ||| (tree_lift
          (fun _ c -> non_empty c >>| fun () -> `Include c)
          (name_ns "include" dav_ns) (any >>~ extract_name)))

let parse_propfind_xml tree =
  let tree_grammar =
    tree_lift
      (fun _ c -> match c with
         | [ #propfind as r ] -> Ok r
         | [ `Include _ ] -> Error "lonely include"
         | [ `All_prop _ ; `Include is ] -> Ok (`All_prop is)
         | _ -> Error "broken")
      (name_ns "propfind" dav_ns)
      propfind_prop_parser
  in
  run tree_grammar tree

let find_attribute name attrs =
  match List.filter (fun ((_, n'), _) -> String.equal n' name) attrs with
  | [ (_, v) ] -> Some v
  | _ -> None

let take_drop_while p xs =
  let a, b =
    List.fold_left (fun (is_p, not_is_p) e ->
        if not_is_p = [] then
          match p e with
          | None -> (is_p, [ e ])
          | Some a -> (a :: is_p, [])
        else
          (is_p, e :: not_is_p))
      ([], []) xs
  in
  (List.rev a, List.rev b)

let rec comp_parser tree : (component, string) result =
  tree_lift
    (fun a c ->
       match find_attribute "name" a with
       | None -> Error "Expected name in comp"
       | Some name ->
         let prop, rest = match c with
           | `Allprop :: xs -> (`Allprop, xs)
           | xs ->
             let props, rest' = take_drop_while (function `Prop a -> Some a | _ -> None) xs in
             `Prop props, rest'
         in
         let comp, rest' = match rest with
           | `Allcomp :: xs -> (`Allcomp, xs)
           | xs ->
             let comps, rest'' = take_drop_while (function `Comp a -> Some a | _ -> None) xs in
             `Comp comps, rest''
         in
         if rest' = [] then
           Ok (name, prop, comp)
         else
           Error "broken")
    (name_ns "comp" caldav_ns >>~ extract_attributes)
    ((tree_lift (fun _ c -> is_empty c >>| fun () -> `Allprop) (name_ns "allprop" caldav_ns) any)
     ||| (tree_lift
            (fun a c ->
               let novalue = match find_attribute "novalue" a with
                 | Some "yes" -> true
                 | _ -> false
               in
               match find_attribute "name" a with
               | None -> Error "No name in prop"
               | Some name' -> is_empty c >>| fun () -> (`Prop (name', novalue)))
            (name_ns "prop" caldav_ns >>~ extract_attributes) any)
     ||| (tree_lift (fun _ c -> is_empty c >>| fun () -> `Allcomp) (name_ns "allcomp" caldav_ns) any)
     ||| (comp_parser >>~ fun (s, p, c) -> Ok (`Comp (s, p, c))))
    tree

let range_parser name : tree -> (timerange, string) result =
  tree_lift
    (fun a c -> is_empty c >>= fun () -> match find_attribute "start" a, find_attribute "end" a with
       | None, _ | _, None -> Error "Missing attribute \"start\" or \"end\""
       | Some s, Some e -> Ok (s, e))
    (name_ns name caldav_ns >>~ extract_attributes)
    (any)

let limit_recurrence_set_parser =
  range_parser "limit-recurrence-set" >>~ fun d -> Ok (`Limit_recurrence_set d)
let expand_parser =
  range_parser "expand" >>~ fun d -> Ok (`Expand d)
let limit_freebusy_set_parser =
  range_parser "limit-freebusy-set" >>~ fun d -> Ok (`Limit_freebusy_set d)

let calendar_data_parser : tree -> (calendar_data, string) result =
  tree_lift (fun _ c ->
      let component, rest = match c with
        | `Comp c :: xs -> Some c, xs
        | xs -> None, xs
      in
      let exp_rec, rest' = match rest with
        | `Expand x :: xs -> Some (`Expand x), xs
        | `Limit_recurrence_set x :: xs -> Some (`Limit_recurrence_set x), xs
        | xs -> None, xs
      in
      match rest' with
      | [] -> Ok (component, exp_rec, None)
      | `Limit_freebusy_set x :: [] -> Ok (component, exp_rec, Some (`Limit_freebusy_set x))
      | _ -> Error "couldn't parse calendar-data")
    (name_ns "calendar-data" caldav_ns)
    ((comp_parser >>~ fun c -> Ok (`Comp c))
     ||| expand_parser ||| limit_recurrence_set_parser ||| limit_freebusy_set_parser)

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

let exactly_one = function
  | [ x ] -> Ok x
  | _     -> Error "expected exactly one child"

let report_prop_parser : tree -> (report_prop, string) result =
  (tree_lift
     (fun _ c -> is_empty c >>| fun () -> `Propname)
     (name_ns "propname" dav_ns) any)
  ||| (tree_lift
         (fun _ c -> is_empty c >>| fun () -> `All_props)
         (name_ns "allprop" dav_ns) any)
  ||| (tree_lift
         (fun _ c -> non_empty c >>| fun () -> `Proplist c)
         (name_ns "prop" dav_ns)
         ((calendar_data_parser >>~ fun c -> Ok (`Calendar_data c))
          ||| (any >>~ extract_and_tag_prop)))

let is_not_defined_parser =
  tree_lift
    (fun _ c -> is_empty c >>| fun () -> `Is_not_defined)
    (name_ns "is-not-defined" caldav_ns)
    any

let text_match_parser =
  tree_lift
    (fun a c ->
      exactly_one c >>| fun str ->
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
      `Text_match (str, collation, negate)
    )
    (name_ns "text-match" caldav_ns >>~ extract_attributes)
    extract_pcdata

let param_filter_parser : tree -> ([> param_filter ], string) result =
  tree_lift
    (fun a c -> match find_attribute "name" a with
       | None -> Error "Attribute \"name\" required"
       | Some n -> match c with
         | [] -> Ok (`Param_filter (n, `Is_defined))
         | [ `Is_not_defined ] -> Ok (`Param_filter (n, `Is_not_defined))
         | [ `Text_match t ] -> Ok (`Param_filter (n, `Text_match t))
         | _ -> Error "expected zero or one elements in param-filter")
    (name_ns "param-filter" caldav_ns >>~ extract_attributes)
    (is_not_defined_parser ||| text_match_parser)

let time_range_parser : tree -> ([> `Timerange of timerange ], string) result =
  range_parser "time-range" >>~ fun d -> Ok (`Timerange d)

let all_param_filters lst : (param_filter list, string) result =
  List.fold_left (fun acc elem -> match acc, elem with
   | Ok items, `Param_filter f -> Ok (`Param_filter f :: items)
   | _ -> Error "Only param-filters allowed.") (Ok []) lst

let prop_filter_parser : tree -> (prop_filter, string) result =
  tree_lift
    (fun a c -> match find_attribute "name" a with
      | None -> Error "Attribute \"name\" required."
      | Some n -> match c with
        | [] -> Ok (n, `Is_defined)
        | [ `Is_not_defined ] -> Ok (n, `Is_not_defined)
        | `Timerange t :: pfs -> all_param_filters pfs >>| fun pfs' -> (n, `Range (t, pfs'))
        | `Text_match t :: pfs -> all_param_filters pfs >>| fun pfs' -> (n, `Text (t, pfs'))
        | _ -> Error "Invalid prop-filter.")
    (name_ns "prop-filter" caldav_ns >>~ extract_attributes)
    (is_not_defined_parser ||| time_range_parser ||| text_match_parser ||| param_filter_parser)

let rec comp_filter_parser tree : (component_filter, string) result =
  tree_lift
    (fun a c ->
       match find_attribute "name" a with
       | None -> Error "Attribute \"name\" required."
       | Some n ->
         let f xs =
           let prop_filters, rest =
             take_drop_while (function `Prop_filter a -> Some a | _ -> None) xs
           in
           let comp_filters, rest' =
             take_drop_while (function `Filter a -> Some a | _ -> None) rest
           in
           if rest' = [] then Ok (prop_filters, comp_filters) else Error "malformed comp-filter "
         in
         match c with
         | [] -> Ok (n, `Is_defined)
         | [ `Is_not_defined ] -> Ok (n, `Is_not_defined)
         | `Timerange (a, b)::xs ->
           f xs >>= fun (props, comps) ->
           Ok (n, `Comp_filter (Some (a, b), props, comps))
         | xs ->
           f xs >>= fun (props, comps) ->
           Ok (n, `Comp_filter (None, props, comps)))
    (name_ns "comp-filter" caldav_ns >>~ extract_attributes)
    (is_not_defined_parser ||| time_range_parser
     ||| (prop_filter_parser >>~ fun p -> Ok (`Prop_filter p))
     ||| (comp_filter_parser >>~ fun c -> Ok (`Filter c)))
    tree

let filter_parser : tree -> (component_filter, string) result =
  tree_lift
    (fun _ c -> exactly_one c)
    (name_ns "filter" caldav_ns)
    (comp_filter_parser)

let parse_calendar_query_xml tree : (calendar_query, string) result =
  let tree_grammar =
    tree_lift
      (fun _ c ->
         let prop, rest = match c with
           | `Report r :: xs -> Some r, xs
           | xs -> None, xs
         in
         match rest with
         | `Filter f :: xs -> Ok (prop, f)
         | xs -> Error "wrong input in calendar-query")
      (name_ns "calendar-query" caldav_ns)
      ((report_prop_parser >>~ fun p -> Ok (`Report p))
       ||| (filter_parser >>~ fun f -> Ok (`Filter f)))
  in
  Format.printf "input tree: (%a)@." pp_tree tree ;
  run tree_grammar tree

let proppatch_prop_parser f =
  tree_lift (fun _ c -> Ok c) (name_ns "prop" dav_ns) (any >>~ f)

let set_parser : tree -> ([>`Set of attribute list * fqname * tree list] list, string) result =
  tree_lift (* exactly one prop tag, but a list of property trees below that tag *)
    (fun _ c -> exactly_one c >>| List.map (fun k -> `Set k))
    (name_ns "set" dav_ns)
    (proppatch_prop_parser extract_name_value)

let remove_parser : tree -> ([>`Remove of fqname] list, string) result =
  tree_lift (* exactly one prop tag, but a list of property trees below that tag *)
    (fun _ c -> exactly_one c >>| List.map (fun k -> `Remove k))
    (name_ns "remove" dav_ns)
    (proppatch_prop_parser extract_ns_name)

let parse_propupdate_xml tree =
  let propupdate =
    tree_lift
      (fun _ lol -> Ok (List.flatten lol))
      (name_ns "propertyupdate" dav_ns)
      (set_parser ||| remove_parser)
  in
  run propupdate tree

let parse_mkcol_xml tree =
  let mkcol =
    tree_lift
      (fun _ lol -> Ok (List.flatten lol))
      (name_ns "mkcol" dav_ns)
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
