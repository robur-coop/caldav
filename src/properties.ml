module Xml = Webdav_xml

module PairMap = Map.Make (struct
    type t = string * string
    let compare (a1, a2) (b1, b2) = match String.compare a1 b1 with
      | 0 -> String.compare a2 b2
      | x -> x
end)

type property = Xml.attribute list * Xml.tree list

type t = property PairMap.t

(* not safe *)
let unsafe_find = PairMap.find_opt

(* not safe *)
let unsafe_add = PairMap.add

(* not safe, not public *)
let remove = PairMap.remove

let prepare_for_disk map =
  let map' = remove (Xml.dav_ns, "getetag") map in
  remove (Xml.dav_ns, "getlastmodified") map'

(* public and ok *)
let empty = PairMap.empty

(* internal *)
let keys m = List.map fst (PairMap.bindings m)

(* public and ok *)
let count = PairMap.cardinal

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

let write_protected = [
  (Xml.dav_ns, "principal-URL");
  (Xml.dav_ns, "group-membership");
  (Xml.dav_ns, "resourcetype");
  (Xml.dav_ns, "current-user-principal");
  (Xml.dav_ns, "current-user-privilege-set");
  (Xml.dav_ns, "content-length");
  (Xml.dav_ns, "etag");
]

let computed_properties = [
  (Xml.dav_ns, "current-user-privilege-set") ;
  (Xml.dav_ns, "current-user-principal")
]

(* assume that it is safe, should call can_write_prop *)
(* TODO check `Write_acl if writing an ACL property *)
(* TODO remove doesn't check write_protected list here *)
let patch ?(is_mkcol = false) props_for_resource updates =
  let set_prop k v props_for_resource =
    if List.mem k write_protected && not (is_mkcol && k = (Xml.dav_ns, "resourcetype"))
    then None, `Forbidden
    else
      (* set needs to be more expressive: forbidden, conflict, insufficient storage needs to be added *)
      let map = unsafe_add k v props_for_resource in
      Some map, `OK
  in
  (* if an update did not apply, m will be None! *)
  let xml (ns, n) = [ Xml.node ~ns n [] ] in
  let apply (props_for_resource, propstats) update = match props_for_resource, update with
    | None, `Set (_, k, _) -> None, (`Failed_dependency, xml k) :: propstats
    | None, `Remove k   -> None, (`Failed_dependency, xml k) :: propstats
    | Some props_for_resource', `Set (a, k, v) ->
      let props_for_resource'', p = set_prop k (a, v) props_for_resource' in
      (props_for_resource'', (p, xml k) :: propstats)
    | Some props_for_resource', `Remove k ->
      let props_for_resource'' = remove k props_for_resource' in
      Some props_for_resource'', (`OK, xml k) :: propstats
  in
  match List.fold_left apply (Some props_for_resource, []) updates with
  | Some props_for_resource', xs -> Some props_for_resource', xs
  | None, xs ->
    (* some update did not apply -> tree: None *)
    let ok_to_failed (s, k) =
      ((match s with
          | `OK -> `Failed_dependency
          | x -> x), k)
    in
    None, List.map ok_to_failed xs

(* housekeeping *)
let to_trees m =
  PairMap.fold (fun (ns, k) (a, v) acc ->
    Xml.node ~ns ~a k v :: acc) m []

(* housekeeping *)
let to_string m =
  let c = to_trees m in
  Xml.tree_to_string (Xml.dav_node "prop" c)

(* housekeeping *)
let pp ppf t = Fmt.string ppf @@ to_string t

(* housekeeping *)
let equal a b = String.equal (to_string a) (to_string b)

(* creates property map for file, only needs to check `Bind in parent, done by webmachine *)
let create ?(initial_props = []) ?(content_type = "text/html") ?(language = "en") ?etag ?(resourcetype = []) acl timestamp length filename =
  let filename = if filename = "" then "hinz und kunz" else filename in
  let etag' m = match etag with None -> m | Some e -> unsafe_add (Xml.dav_ns, "getetag") ([], [ Xml.Pcdata e ]) m in
  let timestamp' = Ptime.to_rfc3339 timestamp in
  let propmap = etag' @@
    unsafe_add (Xml.dav_ns, "acl") ([], List.map Xml.ace_to_xml acl) @@
    unsafe_add (Xml.dav_ns, "creationdate") ([], [ Xml.Pcdata timestamp' ]) @@
    unsafe_add (Xml.dav_ns, "displayname") ([], [ Xml.Pcdata filename ]) @@
    unsafe_add (Xml.dav_ns, "getcontentlanguage") ([], [ Xml.Pcdata language ]) @@
    unsafe_add (Xml.dav_ns, "getcontenttype") ([], [ Xml.Pcdata content_type ]) @@
    unsafe_add (Xml.dav_ns, "getcontentlength") ([], [ Xml.Pcdata (string_of_int length) ]) @@
    unsafe_add (Xml.dav_ns, "getlastmodified") ([], [ Xml.Pcdata timestamp' ]) @@
    (* unsafe_add "lockdiscovery" *)
    unsafe_add (Xml.dav_ns, "resourcetype") ([], resourcetype) empty
    (* unsafe_add "supportedlock" *)
  in
  List.fold_left (fun p (k, v) -> unsafe_add k v p) propmap initial_props

(* creates property map for directory *)
let create_dir ?initial_props ?(resourcetype = []) acl timestamp dirname =
  create ?initial_props ~content_type:"text/directory"
    ~resourcetype:(Xml.dav_node "collection" [] :: resourcetype)
    acl timestamp 0 dirname

(* housekeeping *)
let from_tree = function
  | Xml.Node (_, "prop", _, children) ->
    List.fold_left (fun m c -> match c with
        | Xml.Node (ns, k, a, v) -> unsafe_add (ns, k) (a, v) m
        | Xml.Pcdata _ -> assert false)
      empty children
  | _ -> assert false

(* TODO groups only one level deep right now *)
(* TODO belongs elsewhere? *)
(* outputs identities for a single user *)
let identities userprops =
  let url = function
    | Xml.Node (_, "href", _, [ Xml.Pcdata url ]) -> [ Uri.of_string url ]
    | _ -> []
  in
  let urls n = List.flatten (List.map url n) in
  match
    unsafe_find (Xml.dav_ns, "principal-URL") userprops,
    unsafe_find (Xml.dav_ns, "group-membership") userprops
  with
  | None, _ -> []
  | Some (_, principal), Some (_, groups) -> urls principal @ urls groups
  | Some (_, principal), None -> urls principal

(* user_privileges_for_resource: user properties and resource properties as input, output is the list of granted privileges *)
let privileges ~auth_user_props props_for_resource =
  let aces = match unsafe_find (Xml.dav_ns, "acl") props_for_resource with
    | None -> []
    | Some (_, aces) -> aces
  in
  let aces' = List.map Xml.xml_to_ace aces in
  let aces'' = List.fold_left (fun acc -> function Ok ace -> ace :: acc | Error _ -> acc) [] aces' in (* TODO malformed ace? *)
  let aces''' = List.filter (function
      | `All, _ -> true
      | `Href principal, _ -> List.exists (Uri.equal principal) (identities auth_user_props)
      | _ -> assert false) aces''
  in
  List.flatten @@ List.map (function `Grant ps -> ps | `Deny _ -> []) (List.map snd aces''')

(* helper computing "current-user-privilege-set", not public *)
let current_user_privilege_set ~auth_user_props map =
  let make_node p = Xml.dav_node "privilege" [ Xml.priv_to_xml p ] in
  Some ([], (List.map make_node (privileges ~auth_user_props map)))

(* TODO maybe move to own module *)
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

(* checks privileges for "current-user-privilege-set" (`Read_current_user_privilege_set) and "acl" (`Read_acl) *)
let can_read_prop fqname privileges =
  let requirement = match fqname with
    | ns, "current-user-privilege-set" when ns = Xml.dav_ns -> Some `Read_current_user_privilege_set
    | ns, "acl" when ns = Xml.dav_ns -> Some `Read_acl
    | _ -> None
  in
  match requirement with
  | Some requirement -> privilege_met ~requirement privileges
  | None -> true

(* checks nothing, computes current-user-principal, helper function *)
let current_user_principal props =
  match unsafe_find (Xml.dav_ns, "principal-URL") props with
  | None -> Some ([], [ Xml.dav_node "unauthenticated" [] ])
  | Some url -> Some url

(* checks nothing, computes properties, should be visible? but requires auth_user_props *)
let get_prop auth_user_props m = function
  | ns, "current-user-privilege-set" when ns = Xml.dav_ns -> current_user_privilege_set ~auth_user_props m
  | ns, "current-user-principal" when ns = Xml.dav_ns -> current_user_principal auth_user_props
  | fqname -> unsafe_find fqname m

let authorized_properties_for_resource ~auth_user_props requested_props propmap_for_resource =
  let privileges = privileges ~auth_user_props propmap_for_resource in
  let requested_allowed, requested_forbidden =
    List.partition (fun prop -> can_read_prop prop privileges) requested_props
  in
  (requested_allowed, requested_forbidden)

(* checks sufficient privileges for "current-user-privilege-set" and "read-acl" via can_read_prop *)
let find_many ~auth_user_props property_names m =
  let privileges = privileges ~auth_user_props m in
  let props = List.map (fun fqname ->
    if can_read_prop fqname privileges
    then match get_prop auth_user_props m fqname with
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

(* not safe, exposed, returns property names *)
let names m =
  List.map (fun (ns, k) -> Xml.node ~ns k []) @@
  computed_properties @ keys m

(* not really safe, but excludes from the not-returned-by-allprop list *)
let all m = to_trees (List.fold_right remove not_returned_by_allprop m)
