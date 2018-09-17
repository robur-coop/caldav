module Xml = Webdav_xml

(* web machine already verified the _resource_ ACL *)

type t

val pp : t Fmt.t

val equal : t -> t -> bool

val empty : t

val count : t -> int

val to_string : t -> string

val from_tree : Xml.tree -> t

val privileges : auth_user_props:t -> t -> Xml.privilege list

val privilege_met : requirement:Xml.privilege -> Xml.privilege list -> bool

(* unsafe methods *)
val add : Xml.fqname -> (Xml.attribute list * Xml.tree list) -> t -> t

val unsafe_find : Xml.fqname -> t -> (Xml.attribute list * Xml.tree list) option

val prepare_for_disk : t -> t

(* safe methods: ACL is verified for property, property is checked to be not in any exclusion list *)
val create : ?content_type:string -> ?language:string -> ?etag:string ->
  ?resourcetype:Xml.tree list -> Xml.ace list -> Ptime.t -> int -> string -> t

val create_dir : ?resourcetype:Xml.tree list -> Xml.ace list -> Ptime.t ->
  string -> t

val find_many : auth_user_props:t -> Xml.fqname list -> t ->
  (Cohttp.Code.status_code * Xml.tree list) list

val all : t -> Xml.tree list

val names : t -> Xml.tree list

val patch : ?is_mkcol:bool -> t -> Xml.propupdate list ->
  t option * (Cohttp.Code.status_code * Xml.tree list) list