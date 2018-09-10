module type S = 
sig
  type state
  type tree = Webdav_xml.tree
  
  val mkcol : ?now:Ptime.t -> state -> Webdav_fs.dir -> tree option ->
    (state, [ `Bad_request | `Conflict | `Forbidden of tree ])
      result Lwt.t
  
  val propfind : state -> host:Uri.t -> path:Webdav_fs.file_or_dir -> tree -> depth:string option ->
    (tree, [ `Bad_request | `Forbidden of tree | `Property_not_found ]) result Lwt.t
  
  val proppatch : state -> host:Uri.t -> path:Webdav_fs.file_or_dir -> tree ->
    (state * tree, [ `Bad_request ]) result Lwt.t
  
  val report : state -> host:Uri.t -> path:Webdav_fs.file_or_dir -> tree ->
    (tree, [`Bad_request]) result Lwt.t
  
  val write : state -> path:Webdav_fs.file -> ?etag:string -> content_type:string -> string ->
    (state, [ `Conflict | `Internal_server_error | `Method_not_allowed ]) result Lwt.t
  
  val delete : ?now:Ptime.t -> state -> path:Webdav_fs.file_or_dir -> state Lwt.t
  
  (*
  val get : state -> string ->
  
  val head : state -> string ->
  
  val post : state -> string -> ?? -> state
  
  val put : state -> string ->
  
  val read : state -> string ->
  *)
  
  val access_granted_for_acl : state -> string -> Cohttp.Code.meth -> Webdav_fs.propmap -> bool Lwt.t
end

module Make (Fs: Webdav_fs.S) : S with type state = Fs.t
