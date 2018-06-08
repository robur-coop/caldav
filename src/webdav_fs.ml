open Lwt.Infix
module Fs = Mirage_fs_mem

let (>>==) a f = a >>= function
| Error e -> Lwt.return (Error e) 
| Ok res  -> f res

let (>>|=) a f = a >|= function
| Error e -> Error e
| Ok res  -> f res

let file_to_propertyfile name =
  name ^ ".prop.xml"

let get_properties fs name =
  let propfile = file_to_propertyfile name in
  Fs.size fs propfile >>== fun size ->
  Fs.read fs propfile 0 (Int64.to_int size)

let get_property_map fs name =
  get_properties fs name >|= function
  | Error _ -> None
  | Ok data -> match Webdav.string_to_tree Cstruct.(to_string @@ concat data) with
               | None -> None
               | Some t -> Some (Webdav.prop_tree_to_map t)

(*
GET /foo
GET /foo/

foo.prop.xml
foo/.prop.xml
directory "foo", propfile "foo/.prop.xml"

file "bar.xml", propfile "bar.xml.prop.xml"
file "foo/boo.ics", propfile "foo/boo.ics.prop.xml"
 *)

let write_property_map fs name map =
  let data = Webdav.props_to_string map in
  let name' = if Astring.String.is_suffix ~affix:"/" name then name else
    match Webdav.get_prop "resourcetype" map with
    | Some (_, c) when List.exists (function `Node (_, "collection", _) -> true | _ -> false) c -> name ^ "/"
    | _ -> name in
  let filename = file_to_propertyfile name' in
  Printf.printf "writing %s, content: %s\n" filename data ;
  Fs.write fs filename 0 (Cstruct.of_string data)

(* exclude property files *)
let filter_properties files =
  let ends_in_prop x = not @@ Astring.String.is_suffix ~affix:"prop.xml" x in
  List.filter ends_in_prop files

let size fs name = Fs.size fs name
let read fs name = 
  Fs.size fs name >>== fun length ->
  Fs.read fs name 0 (Int64.to_int length) >>== fun data ->
  get_property_map fs name >|= function 
  | None -> assert false
  | Some props -> Ok (Cstruct.concat data, props)

let stat fs name = Fs.stat fs name
let listdir fs name = 
  Fs.listdir fs name >>|= fun files ->
  Ok (filter_properties files)

let isdir fs name =
  Fs.stat fs name >>|= fun stat ->
  Ok stat.Mirage_fs.directory

let mkdir fs name = Fs.mkdir fs name

let write fs name data property = 
  Fs.write fs name 0 data >>== fun () ->
  write_property_map fs name property

let destroy fs name = Fs.destroy fs name
let connect () = Fs.connect ""

let pp_error = Fs.pp_error
let pp_write_error = Fs.pp_write_error
