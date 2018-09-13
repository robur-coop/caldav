module Xml = Webdav_xml

type t

val empty : t

val create : ?content_type:string -> ?language:string -> ?etag:string ->
  ?resourcetype:Xml.tree list -> Xml.ace list -> Ptime.t -> int -> string -> t

val create_dir : ?resourcetype:Xml.tree list -> Xml.ace list -> Ptime.t ->
  string -> t

val add : Xml.fqname -> (Xml.attribute list * Xml.tree list) -> t -> t

val remove : Xml.fqname -> t -> t

val find : Xml.fqname -> t -> (Xml.attribute list * Xml.tree list) option

val to_string : t -> string

val from_tree : Xml.tree -> t

val privileges : userprops:t -> t -> Xml.privilege list

val privilege_met : requirement:Xml.privilege -> Xml.privilege list -> bool

val find_many : userprops:t -> Xml.fqname list -> t -> (Cohttp.Code.status_code * Xml.tree list) list

val allprop : t -> Xml.tree list

val propname : t -> Xml.tree list

val protected : Xml.fqname list
