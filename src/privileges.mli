module Xml = Webdav_xml

val list : identities:Uri.t list -> Xml.tree list -> Xml.privilege list

val is_met : requirement:Xml.privilege -> Xml.privilege list -> bool

val can_read_prop : Xml.fqname -> Xml.privilege list -> bool

val required : Cohttp.Code.meth -> target_exists:bool -> Xml.privilege * [ `Parent | `Target ]
