type file = [ `File of string list ]

type dir = [ `Dir of string list ]

type file_or_dir = [ file | dir ]

module type S =

sig

  val (>>==) : ('a, 'b) result Lwt.t -> ('a -> ('c, 'b) result Lwt.t) -> ('c, 'b) result Lwt.t

  type t

  type error

  type write_error

  val basename : file_or_dir -> string

  val create_file : dir -> string -> file

  val dir_from_string : string -> dir

  val file_from_string : string -> file

  val from_string : t -> string -> (file_or_dir, error) result Lwt.t

  val to_string : file_or_dir -> string

  val parent : file_or_dir -> dir

  val get_property_map : t -> file_or_dir -> Properties.t Lwt.t

  val write_property_map : t -> file_or_dir -> Properties.t ->
    (unit, write_error) result Lwt.t

  val size : t -> file -> (int64, error) result Lwt.t

  val read : t -> file -> (string * Properties.t, error) result Lwt.t

  val exists : t -> string -> bool Lwt.t

  val dir_exists : t -> dir -> bool Lwt.t

  val listdir : t -> dir -> (file_or_dir list, error) result Lwt.t

  val mkdir : t -> dir -> Properties.t -> (unit, write_error) result Lwt.t

  val write : t -> file -> string -> Properties.t -> (unit, write_error) result Lwt.t

  val destroy : t -> file_or_dir -> (unit, write_error) result Lwt.t

  val pp_error : error Fmt.t

  val pp_write_error : write_error Fmt.t

  val valid : t -> Webdav_config.config -> (unit, [> `Msg of string ]) result Lwt.t

  val last_modified : t -> file_or_dir -> (Ptime.t, error) result Lwt.t

  val etag : t -> file_or_dir -> (string, error) result Lwt.t
end

let src = Logs.Src.create "webdav.fs" ~doc:"webdav fs logs"
module Log = (val Logs.src_log src : Logs.LOG)

let propfile_ext = ".prop" 

module Make (Pclock : Mirage_clock.PCLOCK) (Fs:Mirage_kv.RW) = struct

  open Lwt.Infix

  module Xml = Webdav_xml

  type t = Fs.t
  type error = Fs.error
  type write_error = Fs.write_error
  let pp_error = Fs.pp_error
  let pp_write_error = Fs.pp_write_error

  let (>>==) a f = a >>= function
    | Error e -> Lwt.return (Error e)
    | Ok res  -> f res

  let (>>|=) a f = a >|= function
    | Error e -> Error e
    | Ok res  -> f res

  let basename = function
    | `File path | `Dir path ->
      match List.rev path with
      | base::_ -> base
      | [] -> invalid_arg "basename of root directory not allowed"

  let create_file (`Dir data) name =
    `File (data @ [name])

  (* TODO: no handling of .. done here yet *)
  let data_to_list str = Astring.String.cuts ~empty:false ~sep:"/" str
  let data str = Mirage_kv.Key.v str

  let dir_from_string str = `Dir (data_to_list str)

  let file_from_string str = `File (data_to_list str)

  let to_string =
    let a = Astring.String.concat ~sep:"/" in
    function
    | `File data -> "/" ^ a data
    | `Dir data -> "/" ^ a data ^ "/"

  let isdir fs name =
    (* TODO `File is wrong here, we're here to figure out whether it is a file or directory *)
    let key = data @@ to_string (`File name) in
    Fs.exists fs key >|= function
    | Ok None -> Error (`Not_found key)
    | Ok (Some `Value) -> Ok false
    | Ok (Some `Dictionary) -> Ok true
    | Error e -> Error e

  let from_string fs str =
    let key = data_to_list str in
    isdir fs key >>|= fun dir ->
    Ok (if dir then `Dir key else `File key)

  let parent f_or_d =
    let parent p =
      match List.rev p with
      | _ :: tl -> `Dir (List.rev tl)
      | [] -> `Dir []
    in
    match f_or_d with
    | `Dir d -> parent d
    | `File f -> parent f

  let propfilename f_or_d =
    let segments = match f_or_d with
    | `Dir data -> data @ [ propfile_ext ]
    | `File data -> match List.rev data with
      | filename :: path -> List.rev path @ [ filename ^ propfile_ext ]
      | [] -> assert false (* no file without a name *) in
    Mirage_kv.Key.v (Astring.String.concat ~sep:"/" segments)

  let get_properties fs f_or_d =
    let propfile = propfilename f_or_d in
    Fs.get fs propfile

  (* TODO: check call sites, used to do:
      else match Xml.get_prop "resourcetype" map with
        | Some (_, c) when List.exists (function `Node (_, "collection", _) -> true | _ -> false) c -> name ^ "/"
        | _ -> name in
  *)
  let write_property_map fs f_or_d map =
    let map' = Properties.unsafe_remove (Xml.dav_ns, "getetag") map in
    let data = Sexplib.Sexp.to_string_hum (Properties.to_sexp map') in
    let filename = propfilename f_or_d in
    Fs.set fs filename data

  let size fs (`File file) =
    let key = data @@ to_string (`File file) in
    Fs.get fs key >|= function
    | Error e -> Error e
    | Ok data -> Ok (Int64.of_int @@ String.length data)

  let exists fs str =
    let file = data str in
    Fs.exists fs file >|= function
    | Error e -> (* Error e *) false
    | Ok None -> false
    | Ok (Some _) -> true
    (*Fs.mem fs file*)

  let dir_exists fs (`Dir dir) =
    let key = data @@ to_string (`Dir dir) in
    Fs.exists fs key >|= function
    | Error e -> (* Error e *) false
    | Ok None -> false
    | Ok (Some `Value) -> false
    | Ok (Some `Dictionary) -> true

  let listdir fs (`Dir dir) =
    let kv_dir = data @@ to_string (`Dir dir) in
    Fs.list fs kv_dir >|= function
    | Error e -> Error e
    | Ok files ->
      let files = List.fold_left (fun acc (step, kind) ->
          if Astring.String.is_suffix ~affix:propfile_ext step then
            acc
          else
            (* TODO check whether step is the entire path, or dir needs to be included *)
            let file = dir @ [step] in
            match kind with
            | `Value -> `File file :: acc
            | `Dictionary -> `Dir file :: acc)
          [] files in
      Ok files

  let get_raw_property_map fs f_or_d =
    get_properties fs f_or_d >|= function
    | Error e ->
      Log.err (fun m -> m "error while getting properties for %s %a" (to_string f_or_d) pp_error e) ;
      None
    | Ok str ->
      Some (Properties.of_sexp (Ptime.v (Pclock.now_d_ps ())) (Sexplib.Sexp.of_string str))

  let etag fs f_or_d =
    let key = data @@ to_string f_or_d in
    Fs.digest fs key >|= function
    | Error e -> Error e
    | Ok d ->
      let `Hex hex = Hex.of_string d in
      Ok hex

  (* careful: unsafe_find, unsafe_add *)
  let get_property_map fs f_or_d =
    get_raw_property_map fs f_or_d >>= function
    | None -> Lwt.return Properties.empty
    | Some map ->
      (* insert etag (from Fs.digest) into the propertymap *)
      etag fs f_or_d >|= function
      | Error e ->
        Log.err (fun m -> m "error %a while computing etag for %s"
                    Fs.pp_error e (to_string f_or_d));
        map
      | Ok etag ->
        let etag = ([], [ Xml.Pcdata etag ]) in
        Properties.unsafe_add (Xml.dav_ns, "getetag") etag map

  let last_modified fs f_or_d =
    get_property_map fs f_or_d >|= fun map ->
    let ts =
      match Properties.unsafe_find (Xml.dav_ns, "getlastmodified") map with
      | Some (_, [ Xml.Pcdata ts ]) ->
        begin match Ptime.of_rfc3339 ts with
          | Ok (ts, _, _) -> ts
          | Error (`RFC3339 (_, err)) ->
            Log.err (fun m -> m "error %a parsing %s as RFC3339 time, using current time"
                        Ptime.pp_rfc3339_error err ts);
            Ptime.v (Pclock.now_d_ps ())
        end
      | _ ->
        Log.err (fun m -> m "error while retrieving getlastmodified, not present or wrong XML data, using current time");
        Ptime.v (Pclock.now_d_ps ())
    in
    Ok ts

  let read fs (`File file) =
    let kv_file = data @@ to_string (`File file) in
    Fs.get fs kv_file >>= function
    | Error e -> Lwt.return (Error e)
    | Ok data ->
      get_property_map fs (`File file) >|= fun props ->
      Ok (data, props)

  let mkdir fs (`Dir dir) propmap =
    write_property_map fs (`Dir dir) propmap

  let write fs (`File file) value propmap =
    Fs.batch fs (fun batch ->
        let kv_file = data @@ to_string (`File file) in
        Fs.set batch kv_file value >>= function
        | Error e -> Lwt.return (Error e)
        | Ok () -> write_property_map batch (`File file) propmap)

  let destroy_file_or_empty_dir fs f_or_d =
    Fs.batch fs (fun batch ->
        let propfile = propfilename f_or_d in
        Fs.remove batch propfile >>= function
        | Error e -> Lwt.return (Error e)
        | Ok () ->
          let file = data @@ to_string f_or_d in
          Fs.remove batch file)

  let destroy fs f_or_d =
    destroy_file_or_empty_dir fs f_or_d

  (* TODO check the following invariants:
      - every resource has a .prop.xml file
      - there are no references to non-existing principals (e.g. in <acl><ace>)
      - all principals (apart from groups) have a password and salt (of type Pcdata)
      - all local URLs use the correct hostname *)
  let valid fs config =
    get_property_map fs (`Dir [config.Webdav_config.principals ; "root"]) >|= fun root_map ->
    match
      Properties.unsafe_find (Xml.robur_ns, "password") root_map,
      Properties.unsafe_find (Xml.robur_ns, "salt") root_map
    with
    | Some _, Some _ -> Ok ()
    | _ -> Error (`Msg "root user does not have password and salt")
end
