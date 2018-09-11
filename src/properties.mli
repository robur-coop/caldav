module Xml = Webdav_xml

type t

val empty : t

val create : ?content_type:string -> ?language:string -> ?etag:string ->
  ?resourcetype:Xml.tree list -> string -> int -> string -> t

val add : Xml.fqname -> (Xml.attribute list * Xml.tree list) -> t -> t

val remove : Xml.fqname -> t -> t

val find : Xml.fqname -> t -> (Xml.attribute list * Xml.tree list) option

val to_string : t -> string

val from_tree : Xml.tree -> t

val allprop : t -> Xml.tree list

val propname : t -> Xml.tree list

val current_user_privilege_set : userprops:t -> t -> (Xml.attribute list * Xml.tree list) option

val privileges : userprops:t -> t -> Xml.privilege list
