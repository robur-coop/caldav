module Xml = Webdav_xml

module PairMap = Map.Make (struct
    type t = string * string
    let compare (a1, a2) (b1, b2) = match String.compare a1 b1 with
      | 0 -> String.compare a2 b2
      | x -> x
end)

type t = (Webdav_xml.attribute list * Webdav_xml.tree list) PairMap.t

let find = PairMap.find_opt

let add = PairMap.add

let remove = PairMap.remove

let empty = PairMap.empty

let keys m = List.map fst (PairMap.bindings m)

let not_returned_by_allprop = [
  (Xml.dav_ns, "owner");
  (Xml.dav_ns, "group");
  (Xml.dav_ns, "supported-privilege-set");
  (Xml.dav_ns, "current-user-privilege-set");
  (Xml.dav_ns, "acl");
  (Xml.dav_ns, "acl-restrictions");
  (Xml.dav_ns, "inherited-acl-set");
  (Xml.dav_ns, "principal-collection-set");
  (Xml.caldav_ns, "calendar-description");
  (Xml.caldav_ns, "calendar-timezone");
  (Xml.caldav_ns, "supported-calendar-component-set");
  (Xml.caldav_ns, "supported-calendar-data");
  (Xml.caldav_ns, "max-resource-size");
  (Xml.caldav_ns, "min-date-time");
  (Xml.caldav_ns, "max-date-time");
  (Xml.caldav_ns, "max-instances");
  (Xml.caldav_ns, "max-attendees-per-instance");
  (Xml.caldav_ns, "calendar-home-set");
  (Xml.caldav_ns, "supported-collation-set");
]

let protected = [
  (Xml.dav_ns, "resourcetype");
]

let to_trees m =
  PairMap.fold (fun (ns, k) (a, v) acc ->
    Xml.node ~ns ~a k v :: acc) m []

let allprop m = to_trees (List.fold_right remove not_returned_by_allprop m)

let propname m =
  List.map (fun (ns, k) -> Xml.node ~ns k []) @@
  (Xml.dav_ns, "current-user-privilege-set") :: keys m

let to_string m =
  let c = to_trees m in
  Xml.tree_to_string (Xml.dav_node "prop" c)

let create ?(content_type = "text/html") ?(language = "en") ?etag ?(resourcetype = []) acl timestamp length filename =
  let filename = if filename = "" then "hinz und kunz" else filename in
  let etag' m = match etag with None -> m | Some e -> PairMap.add (Xml.dav_ns, "getetag") ([], [ Xml.Pcdata e ]) m in
  let timestamp' = Ptime.to_rfc3339 timestamp in
  etag' @@
  PairMap.add (Xml.dav_ns, "acl") ([], List.map Xml.ace_to_xml acl) @@
  PairMap.add (Xml.dav_ns, "creationdate") ([], [ Xml.Pcdata timestamp' ]) @@
  PairMap.add (Xml.dav_ns, "displayname") ([], [ Xml.Pcdata filename ]) @@
  PairMap.add (Xml.dav_ns, "getcontentlanguage") ([], [ Xml.Pcdata language ]) @@
  PairMap.add (Xml.dav_ns, "getcontenttype") ([], [ Xml.Pcdata content_type ]) @@
  PairMap.add (Xml.dav_ns, "getcontentlength") ([], [ Xml.Pcdata (string_of_int length) ]) @@
  PairMap.add (Xml.dav_ns, "getlastmodified") ([], [ Xml.Pcdata timestamp' ]) @@
  (* PairMap.add "lockdiscovery" *)
  PairMap.add (Xml.dav_ns, "resourcetype") ([], resourcetype) PairMap.empty
  (* PairMap.add "supportedlock" *)

let create_dir ?(resourcetype = []) acl timestamp dirname =
  create ~content_type:"text/directory"
    ~resourcetype:(Xml.dav_node "collection" [] :: resourcetype)
    acl timestamp 0 dirname

let from_tree = function
  | Xml.Node (_, "prop", _, children) ->
    List.fold_left (fun m c -> match c with
        | Xml.Node (ns, k, a, v) -> PairMap.add (ns, k) (a, v) m
        | Xml.Pcdata _ -> assert false)
      PairMap.empty children
  | _ -> assert false

(* TODO groups only one level deep right now *)
let identities props =
  let url = function
    | Xml.Node (_, "href", _, [ Xml.Pcdata url ]) -> [ Uri.of_string url ]
    | _ -> []
  in
  let urls n = List.flatten (List.map url n) in
  match
    find (Xml.dav_ns, "principal-URL") props,
    find (Xml.dav_ns, "group-membership") props
  with
  | None, _ -> []
  | Some (_, principal), Some (_, groups) -> urls principal @ urls groups
  | Some (_, principal), None -> urls principal

let privileges ~userprops map =
  let aces = match find (Xml.dav_ns, "acl") map with
    | None -> []
    | Some (_, aces) -> aces
  in
  let aces' = List.map Xml.xml_to_ace aces in
  let aces'' = List.fold_left (fun acc -> function Ok ace -> ace :: acc | Error _ -> acc) [] aces' in (* TODO malformed ace? *)
  let aces''' = List.filter (function
      | `All, _ -> true
      | `Href principal, _ -> List.exists (Uri.equal principal) (identities userprops)
      | _ -> assert false) aces''
  in
  List.flatten @@ List.map (function `Grant ps -> ps | `Deny _ -> []) (List.map snd aces''')

let current_user_privilege_set ~userprops map =
  let make_node p = Xml.dav_node "privilege" [ Xml.priv_to_xml p ] in
  Some ([], (List.map make_node (privileges ~userprops map)))

let privilege_met ~requirement privileges =
  List.exists (fun privilege -> match requirement, privilege with
  | _, `All -> true
  | `Read, `Read -> true
  | `Read_acl, `Read_acl -> true
  | `Read_current_user_privilege_set, `Read_current_user_privilege_set -> true
  | `Read_current_user_privilege_set, `Read_acl -> true
  | `Write, `Write -> true
  | `Write_content, `Write -> true
  | `Write_properties, `Write -> true
  | `Write_acl, `Write -> true
  | `Bind, `Write -> true
  | `Unbind, `Write -> true
  | `Write_content, `Write_content -> true
  | `Write_properties, `Write_properties -> true
  | `Write_acl, `Write_acl -> true
  | `Bind, `Bind -> true
  | `Unbind, `Unbind -> true
  | _ -> false ) privileges

let can_read_prop fqname privileges = 
  let requirement = match fqname with
    | ns, "current-user-privilege-set" when ns = Xml.dav_ns -> Some `Read_current_user_privilege_set
    | ns, "acl" when ns = Xml.dav_ns -> Some `Read_acl
    | _ -> None
  in
  match requirement with 
  | Some requirement -> privilege_met ~requirement privileges 
  | None -> true

let get_prop userprops m = function 
| ns, "current-user-privilege-set" when ns = Xml.dav_ns -> current_user_privilege_set ~userprops m 
| fqname -> find fqname m

let find_many ~userprops property_names m =
  let privileges = privileges ~userprops m in
  let props = List.map (fun fqname ->
    if can_read_prop fqname privileges
    then match get_prop userprops m fqname with 
      | None -> `Not_found
      | Some v -> `Found v
    else `Forbidden
  ) property_names in
  let results = List.map2 (fun (ns, name) p -> p, match p with
  | `Found (a, c) -> Xml.node ~ns ~a name c
  | `Forbidden  
  | `Not_found    -> Xml.node ~ns name []) property_names props
  in
  (* group by return code *)
  let found, rest = List.partition (function | `Found _, _ -> true | _ -> false) results in
  let not_found, forbidden = List.partition (function | `Not_found, _ -> true | `Forbidden, _ -> false | `Found _, _ -> assert false) rest in
  let apply_tag tag l = if l = [] then [] else [ tag, List.map snd l ] in
  apply_tag `OK found @ apply_tag `Not_found not_found @ apply_tag `Forbidden forbidden
