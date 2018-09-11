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

let create ?(content_type = "text/html") ?(language = "en") ?etag ?(resourcetype = []) timestamp length filename =
  let filename = if filename = "" then "hinz und kunz" else filename in
  let etag' m = match etag with None -> m | Some e -> PairMap.add (Xml.dav_ns, "getetag") ([], [ Xml.Pcdata e ]) m in
  etag' @@
  PairMap.add (Xml.dav_ns, "creationdate") ([], [ Xml.Pcdata timestamp ]) @@
  PairMap.add (Xml.dav_ns, "displayname") ([], [ Xml.Pcdata filename ]) @@
  PairMap.add (Xml.dav_ns, "getcontentlanguage") ([], [ Xml.Pcdata language ]) @@
  PairMap.add (Xml.dav_ns, "getcontenttype") ([], [ Xml.Pcdata content_type ]) @@
  PairMap.add (Xml.dav_ns, "getcontentlength") ([], [ Xml.Pcdata (string_of_int length) ]) @@
  PairMap.add (Xml.dav_ns, "getlastmodified") ([], [ Xml.Pcdata timestamp ]) @@
  (* PairMap.add "lockdiscovery" *)
  PairMap.add (Xml.dav_ns, "resourcetype") ([], resourcetype) PairMap.empty
  (* PairMap.add "supportedlock" *)

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

let current_user_privilege_set ~userprops map =
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
  let privs =
    List.flatten @@
    List.map (function `Grant ps -> ps | `Deny _ -> [])
      (List.map snd aces''')
  in
  Some ([], (List.map (fun p -> Xml.dav_node "privilege" [ Xml.priv_to_xml p ]) privs))
