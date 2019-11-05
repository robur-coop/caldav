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
end

let src = Logs.Src.create "webdav.fs" ~doc:"webdav fs logs"
module Log = (val Logs.src_log src : Logs.LOG)

let propfile_ext = ".prop" 

module Make (Fs:Mirage_kv.RW) = struct

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
    match Properties.unsafe_find (Xml.dav_ns, "getlastmodified") map with
    | None ->
      Log.err (fun m -> m "map %s without getlastmodified" (to_string f_or_d)) ;
      assert false
    | Some (_, [Xml.Pcdata str]) ->
      Log.debug (fun m -> m "found %s" str) ;
      begin match Ptime.of_rfc3339 str with
        | Error (`RFC3339 (_, e)) ->
          Printf.printf "expected RFC3339, got %s\n%!" str ;
          Log.err (fun m -> m "expected RFC3339 timestamp in map of %s, got %s (%a)"
                      (to_string f_or_d) str Ptime.pp_rfc3339_error e) ;
          assert false
        | Ok _ ->
          (*    let data = Properties.to_string map in *)
          let data = Sexplib.Sexp.to_string_hum (Properties.to_sexp map) in
          let filename = propfilename f_or_d in
          (* Log.debug (fun m -> m "writing property map %s: %s" filename data) ; *)
          Fs.set fs filename data
      end
    | Some _ ->
      Log.err (fun m -> m "map %s with non-singleton pcdata for getlastmodified" (to_string f_or_d)) ;
      assert false

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
      Some (Properties.of_sexp (Sexplib.Sexp.of_string str))

      (* match Xml.string_to_tree str with
         | None ->
           Log.err (fun m -> m "couldn't convert %s to xml tree" str) ;
           None
         | Some t -> Some (Properties.from_tree t) *)

  (* property getlastmodified does not exist for directories *)
  (* careful: unsafe_find *)
  let last_modified_as_ptime fs f_or_d =
    get_raw_property_map fs f_or_d >|= function
    | None ->
      Printf.printf "invalid XML!\n" ;
      None
    | Some map ->
      match Properties.unsafe_find (Xml.dav_ns, "getlastmodified") map with
      | Some (_, [ Xml.Pcdata last_modified ]) ->
        begin match Ptime.of_rfc3339 last_modified with
          | Error _ ->
            Printf.printf "invalid data!\n" ;
            None
          | Ok (ts, _, _) -> Some ts
        end
      | _ -> None

  (* we only take depth 1 into account when computing the overall last modified *)
  (* careful: unsafe_find *)
  let last_modified_of_dir map fs (`Dir dir) =
    let start = match Properties.unsafe_find (Xml.dav_ns, "creationdate") map with
      | Some (_, [ Xml.Pcdata date ]) ->
        begin match Ptime.of_rfc3339 date with
          | Error _ -> assert false
          | Ok (ts, _, _) -> ts
        end
      | _ -> Ptime.epoch
    in
    listdir fs (`Dir dir) >>= function
    | Error _ -> Lwt.return (Xml.ptime_to_http_date start)
    | Ok files ->
      Lwt_list.map_p (last_modified_as_ptime fs) files >>= fun last_modifieds ->
      let lms = List.fold_left (fun acc -> function None -> acc | Some lm -> lm :: acc) [] last_modifieds in
      let max_mtime a b = if Ptime.is_later ~than:a b then b else a
      in
      Lwt.return (Xml.ptime_to_http_date @@ List.fold_left max_mtime start lms)

  (* careful: unsafe_find *)
  let get_etag fs f_or_d =
    get_raw_property_map fs f_or_d >|= function
    | None -> None
    | Some map -> match Properties.unsafe_find (Xml.dav_ns, "getetag") map with
      | Some (_, [ Xml.Pcdata etag ]) -> Some etag
      | _ -> Some (to_string f_or_d)

  (* careful: unsafe_find (when calling get_etag) *)
  let etag_of_dir fs (`Dir dir) =
    listdir fs (`Dir dir) >>= function
    | Error _ -> Lwt.return ""
    | Ok files ->
      Lwt_list.map_p (get_etag fs) files >>= fun etags ->
      let some_etags = List.fold_left (fun acc -> function None -> acc | Some x -> x :: acc) [] etags in
      let data = String.concat ":" some_etags in
      Lwt.return (Digest.to_hex @@ Digest.string data)

  (* let open_fs_error x =
       (x : ('a, Fs.error) result Lwt.t :> ('a, [> Fs.error ]) result Lwt.t) *)

  (* careful: unsafe_find, unsafe_add *)
  let get_property_map fs f_or_d =
    get_raw_property_map fs f_or_d >|= function
    | None -> Properties.empty
    | Some map -> match f_or_d with
      | `File _ -> map
      | `Dir d ->
        match Properties.unsafe_find (Xml.dav_ns, "getlastmodified") map with
        | Some _ -> map
        | None ->
          let initial_date =
            match Properties.unsafe_find (Xml.dav_ns, "creationdate") map with
            | None -> ([], [ Xml.Pcdata (Ptime.to_rfc3339 Ptime.epoch) ])
            | Some x -> x
          in
          Properties.unsafe_add (Xml.dav_ns, "getlastmodified") initial_date map

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
    let kv_file = data @@ to_string (`File file) in
    Fs.set fs kv_file value >>= function
    | Error e -> Lwt.return (Error e)
    | Ok () -> write_property_map fs (`File file) propmap

  let destroy_file_or_empty_dir fs f_or_d =
    (* TODO could a propfile influence the right to deletion if it gets deleted first? *)
    (* TODO use Mirage_kv.RW.batch! *)
    let propfile = propfilename f_or_d in
    Fs.remove fs propfile >>= function
    | Error e -> Lwt.return (Error e)
    | Ok () ->
      let file = data @@ to_string f_or_d in
      Fs.remove fs file

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
