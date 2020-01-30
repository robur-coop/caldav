module Xml = Webdav_xml

let prop_version = [ Xml.pcdata "1" ]

module PairMap = Map.Make (struct
    type t = string * string
    let compare (a1, a2) (b1, b2) = match String.compare a1 b1 with
      | 0 -> String.compare a2 b2
      | x -> x
end)

open Sexplib.Conv
type property = Xml.attribute list * Xml.tree list [@@deriving sexp]

type t = property PairMap.t

type property_list = ((string * string) * property) list [@@deriving sexp]

let to_sexp t =
  let bindings = PairMap.bindings t in
  sexp_of_property_list bindings

let of_sexp now s =
  let bindings = property_list_of_sexp s in
  let map =
    List.fold_left (fun map (k, v) -> PairMap.add k v map) PairMap.empty bindings
  in
  match PairMap.find_opt (Xml.robur_ns, "prop_version") map with
  | Some ([], [ Xml.Pcdata n ]) ->
    begin match int_of_string n with
      | exception Failure _ ->
        Logs.warn (fun m -> m "couldn't parse version"); map
      | 0 ->
        let current = [], [ Xml.Pcdata (Ptime.to_rfc3339 now) ] in
        (* version 0 didn't write the lastmodified *)
        (* for directories, we use the current timestamp,
           for files the creationdate (which is always updated) *)
        let ts = match PairMap.find_opt (Xml.dav_ns, "getcontenttype") map with
          | Some ([], [ Xml.Pcdata ct ]) when ct = "text/directory" -> current
          | _ ->
            begin match PairMap.find_opt (Xml.dav_ns, "creationdate") map with
              | None -> Logs.warn (fun m -> m "map without creationdate"); current
              | Some v -> v
            end
        in
        PairMap.add (Xml.robur_ns, "prop_version") ([], prop_version)
          (PairMap.add (Xml.dav_ns, "getlastmodified") ts map)
      | _ -> map
    end
  | _ -> (* shouldn't happen *)
    Logs.warn (fun m -> m "property map without version"); map

(* not safe *)
let unsafe_find = PairMap.find_opt

let unsafe_add = PairMap.add

let unsafe_remove = PairMap.remove

(* public and ok *)
let empty = PairMap.empty

(* internal *)
let keys m = List.map fst (PairMap.bindings m)

(* public and ok *)
let count = PairMap.cardinal

let not_returned_by_allprop = [
  (Xml.robur_ns, "prop_version");
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
  (Xml.robur_ns, "password");
  (Xml.robur_ns, "salt");
]

let write_protected = [
  (Xml.robur_ns, "prop_version");
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
let patch ?(is_mkcol = false) props_for_resource updates =
  (* if an update did not apply, m will be None! *)
  let xml (ns, n) = [ Xml.node ~ns n [] ] in
  let apply (props_for_resource, propstats) update = match props_for_resource, update with
    | None, `Set (_, k, _) -> None, (`Failed_dependency, xml k) :: propstats
    | None, `Remove k   -> None, (`Failed_dependency, xml k) :: propstats
    | Some props_for_resource', `Set (a, k, v) ->
      if List.mem k write_protected && not (is_mkcol && k = (Xml.dav_ns, "resourcetype"))
      then None, (`Forbidden, xml k) :: propstats
      else
        let props_for_resource'' = unsafe_add k (a, v) props_for_resource' in
        (Some props_for_resource'', (`OK, xml k) :: propstats)
    | Some props_for_resource', `Remove k ->
      if List.mem k write_protected
      then None, (`Forbidden, xml k) :: propstats
      else
        let props_for_resource'' = unsafe_remove k props_for_resource' in
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
let create ?(initial_props = []) ?(content_type = "text/html") ?(language = "en") ?(resourcetype = []) acl timestamp length filename =
  let filename = if filename = "" then "hinz und kunz" else filename in
  let timestamp' = Ptime.to_rfc3339 timestamp in
  let propmap =
    unsafe_add (Xml.robur_ns, "prop_version") ([], prop_version) @@
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

let privileges ~auth_user_props resource_props =
  let aces = match unsafe_find (Xml.dav_ns, "acl") resource_props with
    | None -> []
    | Some (_, aces) -> aces
  in
  Privileges.list ~identities:(identities auth_user_props) aces

let inherited_acls ~auth_user_props resource_props =
  let aces = match unsafe_find (Xml.dav_ns, "acl") resource_props with
    | None -> []
    | Some (_, aces) -> aces
  in
  Logs.debug (fun m -> m "inherited aces size %d" (List.length aces));
  let inherited = Privileges.inherited_acls ~identities:(identities auth_user_props) aces in
  Logs.debug (fun m -> m "inherited size %d" (List.length inherited));
  inherited

(* helper computing "current-user-privilege-set", not public *)
let current_user_privilege_set ~auth_user_props map =
  let make_node p = Xml.dav_node "privilege" [ Xml.priv_to_xml p ] in
  let privileges = privileges ~auth_user_props map in
  let uniq =
    (* workaround for Firefox OS which doesn't understand <privilege><all/></privilege> *)
    if List.mem `All privileges
    then [ `Read ; `Write ; `Read_current_user_privilege_set ; `Write_content ; `Write_properties ; `Bind ; `Unbind ; `All ]
    else List.sort_uniq compare privileges
  in
  Some ([], (List.map make_node uniq))

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
    List.partition (fun prop -> Privileges.can_read_prop prop privileges) requested_props
  in
  (requested_allowed, requested_forbidden)

let find ~auth_user_props ~resource_props property_fqname =
  let privileges = privileges ~auth_user_props resource_props in
  if Privileges.can_read_prop property_fqname privileges
  then match get_prop auth_user_props resource_props property_fqname with
    | None -> Error `Not_found
    | Some v -> Ok v
  else Error `Forbidden

let transform_lastmodified = function
  | None -> None
  | Some (attrs, [ Xml.Pcdata str ]) ->
    Some (attrs, [ Xml.Pcdata (Xml.rfc3339_date_to_http_date str) ])
  | Some _ -> assert false

(* checks sufficient privileges for "current-user-privilege-set" and "read-acl" via can_read_prop *)
let find_many ~auth_user_props ~resource_props property_names =
  let resource_props = PairMap.update (Xml.dav_ns, "getlastmodified") transform_lastmodified resource_props in
  let props = List.map (find ~auth_user_props ~resource_props) property_names in
  let results = List.map2 (fun (ns, name) p -> p, match p with
    | Ok (a, c) -> Xml.node ~ns ~a name c
    | Error _   -> Xml.node ~ns name []) property_names props
  in
  (* group by return code *)
  let found, rest = List.partition (function | Ok _, _ -> true | _ -> false) results in
  let not_found, forbidden = List.partition (function | Error `Not_found, _ -> true | Error `Forbidden, _ -> false | Ok _, _ -> assert false) rest in
  let apply_tag tag l = if l = [] then [] else [ tag, List.map snd l ] in
  apply_tag `OK found @ apply_tag `Not_found not_found @ apply_tag `Forbidden forbidden

(* not safe, exposed, returns property names *)
let names m =
  List.map (fun (ns, k) -> Xml.node ~ns k []) @@
  computed_properties @ keys m

(* not really safe, but excludes from the not-returned-by-allprop list *)
let all m =
  let m' = PairMap.update (Xml.dav_ns, "getlastmodified") transform_lastmodified m in
  to_trees (List.fold_right unsafe_remove not_returned_by_allprop m')
