
type state = Webdav_fs.Fs.t


val mkcol : state -> name:string -> body:string ->
  (state, [ `Bad_request | `Conflict | `Forbidden of Tyxml.Xml.elt list ])
    result Lwt.t

val propfind : state -> prefix:string -> name:string -> body:string -> depth:string option ->
  (state * string, [ `Bad_request | `Forbidden of string | `Property_not_found ]) result Lwt.t

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
