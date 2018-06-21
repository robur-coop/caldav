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

let has_trailing_slash s = Astring.String.is_suffix ~affix:"/" s

let get_properties fs name =
  Fs.stat fs name >>== fun stat ->
  let name' =
    if stat.directory && not (has_trailing_slash name)
    then name ^ "/"
    else name
  in
  let propfile = file_to_propertyfile name' in
  Fs.size fs propfile >>== fun size ->
  Fs.read fs propfile 0 (Int64.to_int size)

let get_property_map fs name =
  get_properties fs name >|= function
  | Error _ -> None
  | Ok data -> match Webdav_xml.string_to_tree Cstruct.(to_string @@ concat data) with
               | None -> None
               | Some t -> Some (Webdav_xml.prop_tree_to_map t)

let write_property_map fs name map =
  let data = Webdav_xml.props_to_string map in
  let name' =
    if Astring.String.is_suffix ~affix:"/" name
    then name
    else match Webdav_xml.get_prop "resourcetype" map with
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

(* property getlastmodified does not exist for directories *)
let last_modified_as_ptime fs file =
  Printf.printf "last_modified_as_ptime for %s\n" file ;
  get_property_map fs file >|= function
  | None -> Error `Invalid_xml
  | Some map ->
    match Webdav_xml.get_prop "getlastmodified" map with
    | Some (_, [ `Pcdata last_modified ]) ->
      begin match Ptime.of_rfc3339 last_modified with
        | Error _ -> Error `Invalid_date
        | Ok (ts, _, _) -> Ok ts
      end
    | _ -> Error `Unknown_prop


let last_modified_of_dir fs dir =
  Printf.printf "last_modified_of_dir %s\n" dir ;
  listdir fs dir >>== fun files ->
  Printf.printf "last_modified_of_dir found %d files\n" (List.length files) ;
  Lwt_list.map_p (fun filename ->
      last_modified_as_ptime fs (dir ^ "/" ^ filename))
    files >|= fun last_modifieds ->
  let oks = List.filter (function Ok _ -> true | _ -> false) last_modifieds in
  Printf.printf "last_modified_of_dir: found %d last_modified\n" (List.length oks) ;
  let max_mtime a (Ok b) =
    Printf.printf "max_mtime a %s b %s, is_later %b\n"
      (Ptime.to_rfc3339 a) (Ptime.to_rfc3339 b)
      (Ptime.is_later ~than:a b) ;
    if Ptime.is_later ~than:a b then b else a in
  Ok (List.fold_left max_mtime Ptime.epoch oks)

let open_fs_error x =
  (x : ('a, Fs.error) result Lwt.t :> ('a, [> Fs.error ]) result Lwt.t)

let last_modified fs name =
  Printf.printf "last-modified for %s\n" name ;
  open_fs_error (isdir fs name) >>== fun is_dir ->
  Printf.printf "is_dir %b\n" is_dir ;
  begin
    if is_dir
    then open_fs_error @@ last_modified_of_dir fs name
    else last_modified_as_ptime fs name
  end >>|= fun ptime ->
  Ok (Webdav_xml.ptime_to_http_date ptime)

let mkdir fs name = Fs.mkdir fs name

let write fs name data property =
  Fs.write fs name 0 data >>== fun () ->
  write_property_map fs name property

let destroy fs name = Fs.destroy fs name
let connect () = Fs.connect ""

let pp_error = Fs.pp_error
let pp_write_error = Fs.pp_write_error
