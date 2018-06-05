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

let get_property_tree fs name =
  get_properties fs name >|= function
  | Error _ -> None
  | Ok data -> Webdav.string_to_tree Cstruct.(to_string @@ concat data)

let write_property_tree fs name is_dir tree =
  let propfile = Webdav.tyxml_to_body tree in
  let trailing_slash = if is_dir then "/" else "" in
  let filename = file_to_propertyfile (name ^ trailing_slash) in
  Fs.write fs filename 0 (Cstruct.of_string propfile)

(*
let create_dir fs name props =
  Fs.mkdir fs name >>= function
  | Error 
  | Ok 
  write_property_tree fs filename true props
*)

(*
(* mkdir -p with properties *)
let create_dir_rec fs name =
  let segments = Astring.String.cuts ~sep:"/" name in
  let rec prefixes = function
    | [] -> []
    | h :: t -> [] :: (List.map (fun a -> h :: a) (prefixes t)) in
  let directories = prefixes segments in
  let create_dir path =
    let filename = String.concat "/" path in 
    Fs.mkdir fs filename >>= fun _ ->
    let props = create_properties filename "text/directory" true 0 in
    write_property_tree fs filename true props
    >|= fun _ ->
    Printf.printf "creating properties %s\n" filename;
    () in 
  Lwt_list.iter_s create_dir directories
*)

(* exclude property files *)
let filter_properties files =
  let ends_in_prop x = not @@ Astring.String.is_suffix ~affix:"prop.xml" x in
  List.filter ends_in_prop files

let size fs name = Fs.size fs name
let read fs name offset length = Fs.read fs name offset length
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
  write_property_tree fs name false property

let destroy fs name = Fs.destroy fs name
let connect () = Fs.connect ""

let pp_error = Fs.pp_error
let pp_write_error = Fs.pp_write_error
