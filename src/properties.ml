module Xml = Webdav_xml

module PairMap = Map.Make (struct
    type t = string * string
    let compare (a1, a2) (b1, b2) = match String.compare a1 b1 with
      | 0 -> String.compare a2 b2
      | x -> x
end)

type t = (Webdav_xml.attribute list * Webdav_xml.tree list) PairMap.t

let to_trees m =
  PairMap.fold (fun (ns, k) (a, v) acc ->
    Xml.node ~ns ~a k v :: acc) m []

let to_string m =
  let c = to_trees m in
  Xml.tree_to_string (Xml.dav_node "prop" c)

let find = PairMap.find_opt

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

let add = PairMap.add

let remove = PairMap.remove

let empty = PairMap.empty

let keys m = List.map fst (PairMap.bindings m)
