type tree = Webdav_xml.tree
type content_type = string
open Webdav_config

module type S =
sig
  type state

  val mkcol : state -> path:Webdav_fs.dir -> Webdav_xml.ace list -> Ptime.t -> is_calendar: bool -> tree option ->
    (state, [ `Bad_request | `Conflict | `Forbidden of tree ])
      result Lwt.t

  val propfind : state -> config -> path:string -> user:string -> depth:string option -> data:string -> 
    (string, [> `Bad_request | `Forbidden of string | `Property_not_found ]) result Lwt.t

  val proppatch : state -> config -> path:string -> user:string -> data:string -> 
    (string, [> `Bad_request ]) result Lwt.t

  val report : state -> config -> path:string -> user:string -> data:string -> 
    (string, [> `Bad_request ]) result Lwt.t

  val write_component : state -> config -> path:string -> Ptime.t -> content_type:content_type -> user:string -> data:string ->
    (string, [> `Bad_request | `Conflict | `Forbidden | `Internal_server_error ]) result Lwt.t

  val delete : state -> path:Webdav_fs.file_or_dir -> Ptime.t -> state Lwt.t

  val read : state -> path:string -> is_mozilla:bool -> (string * content_type, [> `Not_found ]) result Lwt.t

  val access_granted_for_acl : state -> config -> Cohttp.Code.meth -> path:string -> user:string -> bool Lwt.t

  val compute_etag : string -> string

  val parent_acl : state -> config -> string -> Webdav_fs.file_or_dir -> (Webdav_xml.ace list, [> `Forbidden ]) result Lwt.t
  val verify_auth_header : state -> config -> string -> (string, string) result Lwt.t
  val properties_for_current_user : state -> config -> string -> Properties.t Lwt.t
  val calendar_to_collection : string -> (string, [ `Bad_request ]) result
  val parent_is_calendar : state -> Webdav_fs.file_or_dir -> bool Lwt.t

  val make_user : ?props:(Webdav_xml.fqname * Properties.property) list -> state -> config -> string -> string -> unit Lwt.t
  val make_group : state -> config -> string -> string -> string list -> unit Lwt.t
  val initialize_fs : state -> config -> unit Lwt.t
end

module Make (Fs: Webdav_fs.S) : S with type state = Fs.t
