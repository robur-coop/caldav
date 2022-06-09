type tree = Webdav_xml.tree
type content_type = string
open Webdav_config

module type S =
sig
  type state

  val mkcol : state -> config -> path:string -> user:string -> Cohttp.Code.meth -> Ptime.t -> data:string ->
    (unit, [ `Bad_request | `Conflict | `Forbidden of string ]) result Lwt.t

  val propfind : state -> config -> path:string -> user:string -> depth:string option -> data:string ->
    (string, [> `Bad_request | `Forbidden of string | `Property_not_found ]) result Lwt.t

  val proppatch : state -> config -> path:string -> user:string -> data:string ->
    (string, [> `Bad_request ]) result Lwt.t

  val report : state -> config -> path:string -> user:string -> data:string ->
    (string, [> `Bad_request ]) result Lwt.t

  val write_component : state -> config -> path:string -> user:string -> Ptime.t -> content_type:content_type -> data:string ->
    (string, [> `Bad_request | `Conflict | `Forbidden | `Internal_server_error ]) result Lwt.t

  val delete : state -> path:string -> Ptime.t -> bool Lwt.t

  val read : state -> path:string -> is_mozilla:bool -> (string * content_type, [> `Not_found ]) result Lwt.t

  val access_granted_for_acl : state -> config -> Cohttp.Code.meth -> path:string -> user:string -> bool Lwt.t

  val last_modified : state -> path:string -> string option Lwt.t

  val compute_etag : state -> path:string -> string option Lwt.t

  val verify_auth_header : state -> config -> string -> (string,  [> `Msg of string | `Unknown_user of string * string ]) result Lwt.t

  val make_user : ?props:(Webdav_xml.fqname * Properties.property) list -> state -> Ptime.t -> config -> name:string -> password:string -> salt:Cstruct.t ->
    (Uri.t, [> `Conflict ]) result Lwt.t
  val change_user_password : state -> config -> name:string -> password:string -> salt:Cstruct.t -> (unit, [> `Internal_server_error ]) result Lwt.t
  val delete_user : state -> config -> string -> (unit, [> `Internal_server_error | `Not_found ]) result Lwt.t

  val make_group : state -> Ptime.t -> config -> string -> string list -> (Uri.t, [> `Conflict ]) result Lwt.t
  val enroll : state -> config -> member:string -> group:string -> unit Lwt.t
  val resign : state -> config -> member:string -> group:string -> unit Lwt.t
  val replace_group_members : state -> config -> string -> string list -> unit Lwt.t
  val delete_group : state -> config -> string -> (unit, [> `Internal_server_error | `Not_found ]) result Lwt.t

  val initialize_fs : state -> Ptime.t -> config -> unit Lwt.t
  val initialize_fs_for_apple_testsuite : state -> Ptime.t -> config -> unit Lwt.t

  val generate_salt : unit -> Cstruct.t

  val connect : state -> config -> string option -> state Lwt.t
end

module Make(R : Mirage_random.S)(Clock : Mirage_clock.PCLOCK)(Fs: Webdav_fs.S) : S with type state = Fs.t
