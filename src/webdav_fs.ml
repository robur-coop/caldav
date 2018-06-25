open Lwt.Infix
module Fs = Mirage_fs_mem

type t = Fs.t

let (>>==) a f = a >>= function
  | Error e -> Lwt.return (Error e)
  | Ok res  -> f res

let (>>|=) a f = a >|= function
  | Error e -> Error e
  | Ok res  -> f res

let isdir fs name =
  Fs.stat fs name >>|= fun stat ->
  Ok stat.Mirage_fs.directory

type file = [ `File of string list ]
type dir = [ `Dir of string list ]

type file_or_dir = [ file | dir ]

let create_file (`Dir data) name =
  `File (data @ [name])

(* TODO: no handling of .. done here yet *)
let data str = Astring.String.cuts ~empty:false ~sep:"/" str

let dir_from_string str = `Dir (data str)

let file_from_string str = `File (data str)

let from_string fs str =
  isdir fs str >>|= fun dir ->
  Ok (if dir then `Dir (data str) else `File (data str))

let to_string =
  let a = Astring.String.concat ~sep:"/" in
  function
  | `File data -> a data
  | `Dir data -> a data ^ "/"

let propfilename =
  let ext = ".prop.xml" in
  function
  | `Dir data -> `File (data @ [ ext ])
  | `File data -> match List.rev data with
    | filename :: path -> `File (List.rev path @ [ filename ^ ext ])
    | [] -> assert false (* no file without a name *)

type propmap = (Webdav_xml.attribute list * Webdav_xml.tree list) Webdav_xml.M.t

let get_properties fs f_or_d =
  let propfile = to_string (propfilename f_or_d) in
  Fs.size fs propfile >>== fun size ->
  Fs.read fs propfile 0 (Int64.to_int size)

let get_property_map fs f_or_d =
  get_properties fs f_or_d >|= function
  | Error _ -> None
  | Ok data ->
    match Webdav_xml.string_to_tree Cstruct.(to_string @@ concat data) with
    | None -> None
    | Some t -> Some (Webdav_xml.prop_tree_to_map t)

(* TODO: check call sites, used to do:
    else match Webdav_xml.get_prop "resourcetype" map with
      | Some (_, c) when List.exists (function `Node (_, "collection", _) -> true | _ -> false) c -> name ^ "/"
      | _ -> name in
*)
let write_property_map fs f_or_d map =
  let data = Webdav_xml.props_to_string map in
  let filename = to_string (propfilename f_or_d) in
  Printf.printf "writing %s, content: %s\n" filename data ;
  Fs.write fs filename 0 (Cstruct.of_string data) >|= fun r ->
  Format.printf "file system %a" Mirage_fs_mem.pp fs ;
  r

let size fs (`File file) =
  let name = to_string (`File file) in
  Fs.size fs name

let read fs (`File file) =
  let name = to_string (`File file) in
  Fs.size fs name >>== fun length ->
  Fs.read fs name 0 (Int64.to_int length) >>== fun data ->
  get_property_map fs (`File file) >|= function
  | None -> assert false
  | Some props -> Ok (Cstruct.concat data, props)

let stat fs f_or_d = Fs.stat fs (to_string f_or_d)

let exists fs str =
  Fs.stat fs str >|= function
  | Ok _ -> true
  | Error _ -> false

let listdir fs (`Dir dir) =
  let dir_string = to_string (`Dir dir) in
  Fs.listdir fs dir_string >>== fun files ->
  Lwt_list.fold_left_s (fun acc fn ->
      if Astring.String.is_suffix ~affix:".prop.xml" fn then
        Lwt.return acc
      else
        let str = dir_string ^ fn in
        isdir fs str >|= function
        | Error _ -> acc
        | Ok is_dir ->
          let f_or_d =
            if is_dir
            then dir_from_string str
            else file_from_string str
          in
          f_or_d :: acc)
    [] files >|= fun files ->
  Ok files

(* property getlastmodified does not exist for directories *)
let last_modified_as_ptime fs f_or_d =
  Printf.printf "last_modified_as_ptime for %s\n" (to_string f_or_d) ;
  get_property_map fs f_or_d >|= function
  | None ->
    Printf.printf "invalid XML!\n" ;
    Error `Invalid_xml
  | Some map ->
    match Webdav_xml.get_prop "getlastmodified" map with
    | Some (_, [ Webdav_xml.Pcdata last_modified ]) ->
      begin match Ptime.of_rfc3339 last_modified with
        | Error _ ->
          Printf.printf "invalid data!\n" ;
          Error `Invalid_date
        | Ok (ts, _, _) -> Ok ts
      end
    | _ ->
      Printf.printf "unknown prop!\n" ;
      Error `Unknown_prop


let last_modified_of_dir fs (`Dir dir) =
  Printf.printf "last_modified_of_dir %s\n" (to_string (`Dir dir)) ;
  listdir fs (`Dir dir) >>== fun files ->
  Printf.printf "last_modified_of_dir found %d files\n" (List.length files) ;
  Lwt_list.map_p (last_modified_as_ptime fs) files >>= fun last_modifieds ->
  let oks = List.filter (function Ok _ -> true | _ -> false) last_modifieds in
  Printf.printf "last_modified_of_dir: found %d last_modified\n" (List.length oks) ;
  let max_mtime a (Ok b) =
    Printf.printf "max_mtime a %s b %s, is_later %b\n"
      (Ptime.to_rfc3339 a) (Ptime.to_rfc3339 b)
      (Ptime.is_later ~than:a b) ;
    if Ptime.is_later ~than:a b then b else a
  in
  last_modified_as_ptime fs (`Dir dir) >|= function
  | Ok start -> Ok (List.fold_left max_mtime start oks)
  | Error _ -> assert false (* getlastmodified property is ensured on creation *)

let open_fs_error x =
  (x : ('a, Fs.error) result Lwt.t :> ('a, [> Fs.error ]) result Lwt.t)

let last_modified fs f_or_d =
  Printf.printf "last-modified for %s\n" (to_string f_or_d) ;
  begin match f_or_d with
  | `File _ -> last_modified_as_ptime fs f_or_d
  | `Dir data -> open_fs_error @@ last_modified_of_dir fs (`Dir data)
  end >>|= fun ptime ->
  Ok (Webdav_xml.ptime_to_http_date ptime)

let mkdir fs (`Dir dir) propmap =
  Fs.mkdir fs (to_string (`Dir dir)) >>== fun () ->
  write_property_map fs (`Dir dir) propmap

let write fs (`File file) data propmap =
  Fs.write fs (to_string (`File file)) 0 data >>== fun () ->
  write_property_map fs (`File file) propmap

let destroy fs f_or_d =
  let propfile = propfilename f_or_d in
  Fs.destroy fs (to_string propfile) >>== fun () ->
  Fs.destroy fs (to_string f_or_d)

let connect () = Fs.connect ""

let pp_error = Fs.pp_error
let pp_write_error = Fs.pp_write_error
