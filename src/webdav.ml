
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
    Fmt.pf fmt "(%s: %a, %a)"
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

let rec tree_filter_map keep_leaves f = function (* : ('a -> 'b option) -> 'a forest -> 'b forest *)
  | [] -> []
  | (`Pcdata str as t)::ts ->
    let ts' = tree_filter_map keep_leaves f ts in
    if keep_leaves then
      t :: ts'
    else
      ts'
  | (`Node (a, name, children) as t)::ts ->
    let ts' = tree_filter_map keep_leaves f ts in
    match f t with
    | None -> ts'
    | Some (a', name') ->
      `Node (a', name', tree_filter_map keep_leaves f children) :: ts'

let rec filter_map f = function
  | []    -> []
  | x::xs ->
    match f x with
    | None    ->       filter_map f xs
    | Some x' -> x' :: filter_map f xs

let segment s f = (fun tree ->
  match tree with
  | (`Node (a, n, c) as nd) when String.equal s n -> Some (f, nd)
  | _ -> None)

let alternative a b = (fun tree ->
  match a tree with 
  | None -> b tree 
  | Some x -> Some x)

let (|||) = alternative

type 'a liftf = (string * string) list -> string -> tree list -> 'a list -> 'a

let rec select (path: (tree -> ('a liftf * tree) option) list ) (tree: tree) : 'a option = 
  match path with
  | [] -> assert false
  | [f] -> begin match f tree with None -> None | Some (lift, `Node (a, n, c)) -> Some (lift a n c []) | Some (lift, `Pcdata _) -> None end
  | f::g::hs ->
     begin match f tree with
       | None -> None
       | Some (lift, `Pcdata _) -> None
       | Some (lift, `Node (a, n, c)) ->
          let c' = filter_map (select (g::hs)) c in
          if c' = [] then None else Some (lift a n [] c')
     end
(*
- verifiziere shape vom request-xml baum (strukturelle rekursion auf path)
- selecte teilbaum(e) von request-xml baum den/die wir zur verarbeitung brauchen (z.b. eine liste von set und remove) (strukturelle rekursion auf path + "lift")
=> rueckgabe: liste von transformationen fuer den property xml baum
- wende die aenderungen auf den property-xml-baum an (strukturelle rekursion auf property xml baum?)
*)

let rec pp_transform fmt = function
  | `Pcdata str -> Fmt.string fmt str
  | `Node (attrib, name, children) ->
    Fmt.pf fmt "Node (%s: %a, %a)"
      name
      Fmt.(list (pair string string)) attrib
      Fmt.(list pp_transform) children
  | `Transformation (attrib, name, children) ->
    Fmt.pf fmt "Transformation (%s: %a, %a)"
      name
      Fmt.(list (pair string string)) attrib
      Fmt.(list pp_transform) children

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

let name string = function
  | (`Node (_, name, _) as node) ->
    if String.equal name string then
      Ok node
    else
      Error ("expected " ^ string ^ ", but found " ^ name)
  | _ ->
    Error ("expected " ^ string ^ ", but got pcdata")

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
  | `Node (_, n, c) -> Ok (n, c)
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

let apply x a b = x a b

let parse_propfind_xml str =
  match string_to_tree str with
  | None -> None
  | Some tree ->
    let tree_grammar =
      tree_lift
        (fun _ c -> match c with
           | [ #res as r ] -> Ok r
           | [ `Include _ ] -> Error "lonely include"
           | [ `All_prop _ ; `Include is ] -> Ok (`All_prop is)
           | _ -> Error "broken")
        (name "propfind")
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
    in
    match run tree_grammar tree with
    | Error e ->
      Format.printf "error %s while parsing tree\n" e ; None
    | Ok tree ->
      Format.printf "Parsed tree with tree_grammar: %a\n" pp_prop tree;
      Some tree

let pp_propupdate fmt update =
  List.iter (function
      | `Set (k, v) -> Fmt.pf fmt "Set %s %a" k (Fmt.list pp_tree) v
      | `Remove k   -> Fmt.pf fmt "Remove %s" k
    ) update

let exactly_one = function
  | [ x ] -> Ok x
  | _     -> Error "expected exactly one child"

let parse_propupdate_xml str =
  let propupdate =
    let prop f =
      tree_lift (fun _ c -> Ok c) (name "prop") (any >>= f)
    in
    tree_lift
      (fun _ lol -> Ok (List.flatten lol))
      (name "propertyupdate")
      ((tree_lift (* exactly one prop tag, but a list of property trees below that tag *)
          (fun _ c -> exactly_one c >>| List.map (fun k -> `Set k))
          (name "set")
          (prop extract_name_value))
       ||| (tree_lift (* exactly one prop tag, but a list of property trees below that tag *)
              (fun _ c -> exactly_one c >>| List.map (fun k -> `Remove k))
              (name "remove")
              (prop extract_name)))
  in
  match string_to_tree str with
  | None -> None
  | Some tree ->
    match run propupdate tree with
    | Ok x -> Some x
    | Error e -> None

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

let get_prop p xml = match xml with
  | `Node (a, "prop", children) ->
      List.find_opt
        (function `Node (_, p, _) -> true | _ -> false)
        children
  | _ -> None

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
