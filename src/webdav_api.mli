module type S =
sig
  type state
  type tree = Webdav_xml.tree

  val mkcol : state -> path:Webdav_fs.dir -> Webdav_xml.ace list -> Ptime.t -> is_calendar: bool -> tree option ->
    (state, [ `Bad_request | `Conflict | `Forbidden of tree ])
      result Lwt.t

  val propfind : state -> host:Uri.t -> path:Webdav_fs.file_or_dir -> tree -> auth_user_props:Properties.t -> depth:string option ->
    (tree, [ `Bad_request | `Forbidden of tree | `Property_not_found ]) result Lwt.t

  val proppatch : state -> host:Uri.t -> path:Webdav_fs.file_or_dir -> tree -> auth_user_props:Properties.t ->
    (state * tree, [ `Bad_request ]) result Lwt.t

  val report : state -> host:Uri.t -> path:Webdav_fs.file_or_dir -> tree -> auth_user_props:Properties.t ->
    (tree, [`Bad_request]) result Lwt.t

  val write_component : state -> path:Webdav_fs.file -> Webdav_xml.ace list -> Ptime.t -> ?etag:string -> content_type:string ->
    string ->
    (state, [ `Conflict | `Internal_server_error | `Method_not_allowed ]) result Lwt.t

  val delete : state -> path:Webdav_fs.file_or_dir -> Ptime.t -> state Lwt.t

  (*
  val get : state -> string ->

  val head : state -> string ->

  val post : state -> string -> ?? -> state

  val put : state -> string ->

  val read : state -> string ->
  *)

  val access_granted_for_acl : state -> string -> Cohttp.Code.meth -> Properties.t -> bool Lwt.t

  val compute_etag : string -> string

  val parent_acl : state -> Webdav_config.config -> Cohttp.Header.t -> Webdav_fs.file_or_dir -> (Webdav_xml.ace list, [> `Forbidden ]) result Lwt.t
  val directory_as_html : state -> Webdav_fs.dir -> string Lwt.t
  val directory_as_ics : state -> Webdav_fs.dir -> string Lwt.t
  val verify_auth_header : state -> Webdav_config.config -> string -> (string, string) result Lwt.t
  val properties_for_current_user : state -> Webdav_config.config -> Cohttp.Header.t -> Properties.t Lwt.t
  val calendar_to_collection : string -> (string, [ `Bad_request ]) result
  val parent_is_calendar : state -> Webdav_fs.file_or_dir -> bool Lwt.t

  val make_user : ?props:(Webdav_xml.fqname * Properties.property) list -> state -> Webdav_config.config -> string -> string -> unit Lwt.t
  val make_group : state -> Webdav_config.config -> string -> string -> string list -> unit Lwt.t
  val initialize_fs : state -> Webdav_config.config -> unit Lwt.t
end

module Make (Fs: Webdav_fs.S) : S with type state = Fs.t
