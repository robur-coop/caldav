module Xml = Webdav_xml

(* web machine already verified the _resource_ ACL *)

type t

type property = Xml.attribute list * Xml.tree list

val to_sexp : t -> Sexplib.Sexp.t
val of_sexp : Ptime.t -> Sexplib.Sexp.t -> t

val pp : t Fmt.t

val equal : t -> t -> bool

val empty : t

val count : t -> int

val to_string : t -> string

val from_tree : Xml.tree -> t

val privileges : auth_user_props:t -> t -> Xml.privilege list
val inherited_acls : auth_user_props:t -> t -> Uri.t list

(* unsafe methods *)
val unsafe_find : Xml.fqname -> t -> property option

val unsafe_add : Xml.fqname -> property -> t -> t

val unsafe_remove : Xml.fqname -> t -> t

(* safe methods: ACL is verified for property, property is checked to be not in any exclusion list *)
val create : ?initial_props:(Xml.fqname * property) list ->
  ?content_type:string -> ?language:string ->
  ?resourcetype:Xml.tree list -> Xml.ace list -> Ptime.t -> int -> string -> t

val create_dir : ?initial_props:(Xml.fqname * property) list ->
  ?resourcetype:Xml.tree list -> Xml.ace list -> Ptime.t -> string -> t

val find : auth_user_props:t -> resource_props:t -> Xml.fqname -> 
  (property, [> `Forbidden | `Not_found]) result

val find_many : auth_user_props:t -> resource_props:t -> Xml.fqname list -> 
  (Cohttp.Code.status_code * Xml.tree list) list

val all : t -> Xml.tree list

val names : t -> Xml.tree list

val patch : ?is_mkcol:bool -> t -> Xml.propupdate list ->
  t option * (Cohttp.Code.status_code * Xml.tree list) list
