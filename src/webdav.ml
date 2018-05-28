
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

type tree = [
  | `Pcdata of string
  | `Node of (string * string) list * string * tree list
]

let rec pp_tree fmt = function
  | `Pcdata str -> Fmt.string fmt str
  | `Node (attrib, name, children) ->
    Fmt.pf fmt "%s: %a %a"
      name
      Fmt.(list (pair string string)) attrib
      Fmt.(list pp_tree) children

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
    let input = Xmlm.make_input ~strip:true (`String (0, str)) in
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

(*
  val select : ???

<a>
  <b/>
<a/>

name_equals : string -> string parser
next_segment : 'a parser -> 'a parser -> 'a parser
alternative : parser -> parser -> parser


optional_segment : default -> parser -> parser
*)

let rec filter_map f = function
  | []    -> []
  | x::xs ->
    match f x with
    | None    ->       filter_map f xs
    | Some x' -> x' :: filter_map f xs

type x = [
  | `Segment of string
  | `Alternative of x * x
  | `Optional_segment of string
]

let rec pp_x fmt = function
  | `Segment s -> Fmt.string fmt s
  | `Alternative (a, b) -> Fmt.pf fmt "(%a|%a)" pp_x a pp_x b
  | `Optional_segment s -> Fmt.pf fmt "opt %s" s

let rec select_path path tree =
  let alternative a b tl t =
    match select_path (a::tl) t with
    | Some t' -> Some t'
    | None -> select_path (b::tl) t
  and segment s tl = function
    | `Node (attr, n, ch) when s = n ->
      let ch' = filter_map (select_path tl) ch in
      if ch' = [] && tl <> [] then None else Some (`Node (attr, n, ch'))
    | _ -> None
  and optional_segment s tl = function
    | `Node (attr, n, ch) when s = n ->
      let ch' = filter_map (select_path tl) ch in
      if ch' = [] && tl <> [] then None else Some (`Node (attr, n, ch'))
    | tree' -> select_path tl tree'
  in
  match path with
  | [] -> Some tree
  | `Alternative (a, b)::tl -> alternative a b tl tree
  | `Segment x::zs -> segment x zs tree
  | `Optional_segment p::tl -> optional_segment p tl tree
  | _ -> None


(* let rec select_path path tree =
match path with
| [] -> Some tree
| f::fs ->
   begin match f tree with
     | None -> None
     | Some (`Node (attr, name, ch)) ->
        let ch' = filter_map (select_path fs) ch in
        if ch' = [] then None else Some (`Node (attr, name, ch'))
   end

tree =
<a>
  <b></b>
  <c></c>
  <b><d></d><b/>
</a>

--> <a><b></b><b><d></d></b></a>

path = (string "a") --> [ a ; b|c ]
path = (string "a" ; string "b") --> [ a ; b ]
 *)



let parse_propfind_xml str =
  try
    let r = match string_to_tree str with
      | None -> assert false
      | Some tree ->
        (*        select [ "propfind" ; ("propname" | "prop" | "allprop" ["include"]) ] *)

        match tree with
        | `Node (attr, "propfind", children) ->
          begin match children with
            | [ `Node (_, "propname", []) ] -> `Propname
            | [ `Node (_, "prop", props) ] ->
              let children =
                List.map (function
                  | `Node (_, name, []) -> name
                  | _ -> assert false) props
              in
              `Props children
            | [ `Node (_, "allprop", []) ] -> `All_prop []
            | [ `Node (_, "allprop", []) ; `Node (_, "include", includes) ] ->
              let successors =
              List.map (function
                    | `Node (_, name, []) -> name
                    | _ -> assert false) includes
              in
              `All_prop successors
            | _ -> assert false
          end
        | _ -> assert false
    in
    Some r
  with
  | _ -> None

let pp_prop fmt = function
  | `Propname -> Fmt.string fmt "Propname"
  | `All_prop xs -> Fmt.pf fmt "All prop %a" Fmt.(list ~sep:(unit ",@ ") string) xs
  | `Props xs -> Fmt.pf fmt "Props %a" Fmt.(list ~sep:(unit ",@ ") string) xs

let pp_propupdate fmt update =
  List.iter (function
      | `Set xs ->
        Fmt.pf fmt "Set %a" Fmt.(list ~sep:(unit ",@ ") (pair string (list pp_tree))) xs
      | `Remove xs ->
        Fmt.pf fmt "Remove %a" Fmt.(list ~sep:(unit ",@ ") string) xs
    ) update

let parse_propupdate_xml str =
  let process_children children =
    let process_kid = function
      | `Node (_, "set", [ `Node (_, "prop", children) ]) ->
        let children =
          List.map (function
              | `Node (_, name, v) -> (name, v)
              | _ -> assert false)
            children
        in
        `Set children
      | `Node (_, "remove", [ `Node (_, "prop", children) ]) ->
        let children =
          List.map (function
              | `Node (_, name, []) -> name
              | _ -> assert false)
            children
        in
        `Remove children
      | `Node (_, name, children) ->
        Printf.eprintf "name is %s, %d children\n%!" name (List.length children) ;
        assert false
      | `Pcdata str ->
        Printf.eprintf "got pcdata FF%sFF\n%!" str ;
        assert false
    in
    List.map process_kid children
  in
  match string_to_tree str with
  | None -> assert false
  | Some tree -> match tree with
    | `Node (attr, "propertyupdate", children) -> process_children children
    | _ -> assert false

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
