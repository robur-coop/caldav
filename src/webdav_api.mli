
type state = Webdav_fs.Fs.t
type tree = Webdav_xml.tree

val dav_ns : Tyxml.Xml.attrib

val mkcol : ?now:Ptime.t -> state -> string -> tree option ->
  (state, [ `Bad_request | `Conflict | `Forbidden of tree ])
    result Lwt.t

val propfind : state -> prefix:string -> name:string -> tree -> depth:string option ->
  (state * tree, [ `Bad_request | `Forbidden of tree | `Property_not_found ]) result Lwt.t

val proppatch : state -> prefix:string -> name:string -> body:string ->
  (state * string, [ `Bad_request ]) result Lwt.t

(*
val delete : state -> string -> state

val get : state -> string ->

val head : state -> string ->

val post : state -> string -> ?? -> state

val put : state -> string ->

val read : state -> string ->

val write : state -> string ->
*)
