
type state = Webdav_fs.Fs.t

(*
val mkcol : state -> string ->
  (state, [ `Forbidden | `Method_not_allowed | `Conflict
          | `Unsupported_media_type | `Insufficient_storage ]) result
*)

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
