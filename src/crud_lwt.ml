(** An example CRUD API for id-based operations on JSON objects. The objects are
    stored in-memory and therefore will not persist across runs of the database.
    The application does not perform any JSON validation at this time.

    Build by running `jbuilder`:

      [jbuilder build _build/default/examples/crud_lwt.exe]

    Run using the following command, which will display the path that each
    request takes through the decision diagram:

      [DEBUG_PATH= ./crud_lwt.exe]

    Here are some sample CURL commands to test on a running server:

      - Get a complete list of items:
        [curl -i -w "\n" -X GET http://localhost:8080/items]

      - Get the item with id 1:
        [curl -i -w "\n" -X GET http://localhost:8080/item/1]

      - Create a new item:
        [curl -i -w "\n" -X POST -d '{"name":"new item"}' http://localhost:8080/items]

      - Modify the item with id 1:
        [curl -i -w "\n" -X PUT -H 'Content-Type: application/json'\
          -d '{"name":"modified item"}' http://localhost:8080/item/1]

      - Delete the item with id 1:
        [curl -i -w "\n" -X DELETE http://localhost:8080/item/1] *)

open Cohttp_lwt_unix
open Lwt.Infix

module Fs = Mirage_fs_mem

(* Apply the [Webmachine.Make] functor to the Lwt_unix-based IO module
 * exported by cohttp. For added convenience, include the [Rd] module
 * as well so you don't have to go reaching into multiple modules to
 * access request-related information. *)
module Wm = struct
  module Rd = Webmachine.Rd
  include Webmachine.Make(Cohttp_lwt_unix__Io)
end

(** A resource for querying all the items in the database via GET and creating
    a new item via POST. Check the [Location] header of a successful POST
    response for the URI of the item. *)
class items fs = object(self)
  inherit [Cohttp_lwt.Body.t] Wm.resource

  method private to_json rd =
    (* TODO use path from request uri *)
    Fs.listdir fs "" >>= function
    | Error e -> assert false
    | Ok items ->
      let json = Printf.sprintf "[%s]" (String.concat ", " items) in
      Wm.continue (`String json) rd

  method allowed_methods rd =
    Wm.continue [`GET; `HEAD; `POST] rd

  method content_types_provided rd =
    Wm.continue [
      "application/json", self#to_json
    ] rd

  method content_types_accepted rd =
    Wm.continue [] rd

  method process_post rd =
(*    Cohttp_lwt.Body.to_string rd.Wm.Rd.req_body >>= fun body ->
    Db.add db body >>= fun new_id ->
      let rd' = Wm.Rd.redirect ("/item/" ^ (string_of_int new_id)) rd in *)
    Wm.continue true rd
end

(** A resource for querying an individual item in the database by id via GET,
    modifying an item via PUT, and deleting an item via DELETE. *)
class item fs = object(self)
  inherit [Cohttp_lwt.Body.t] Wm.resource

  method private of_json rd =
    Cohttp_lwt.Body.to_string rd.Wm.Rd.req_body >>= fun body ->
    Fs.write fs (string_of_int (self#id rd)) 0 (Cstruct.of_string body) >>= fun modified ->
    let modified', resp_body =
      match modified with
      | Error _ -> false, `String "{\"status\":\"not found\"}"
      | Ok () -> true, `String "{\"status\":\"ok\"}"
    in
    Wm.continue modified' { rd with Wm.Rd.resp_body }

  method private to_json rd =
    Fs.read fs (string_of_int (self#id rd)) 0 100 >>= function
    | Error _ -> assert false
    | Ok data ->
      let value = String.concat "" @@ List.map Cstruct.to_string data in
      Wm.continue (`String value) rd

  method allowed_methods rd =
    Wm.continue [`GET; `HEAD; `PUT; `DELETE; `Other "PROPFIND"; `Other "PROPPATCH"; `Other "COPY" ; `Other "MOVE"] rd

  method known_methods rd =
    Wm.continue [`GET; `HEAD; `PUT; `DELETE; `Other "PROPFIND"; `Other "PROPPATCH"; `Other "COPY" ; `Other "MOVE"] rd

  method resource_exists rd =
    Fs.stat fs (string_of_int (self#id rd)) >>= function
    | Error _ -> Wm.continue false rd
    | Ok _ -> Wm.continue true rd

  method content_types_provided rd =
    Wm.continue [
      "application/json", self#to_json
    ] rd

  method content_types_accepted rd =
    Wm.continue [
      "application/json", self#of_json
    ] rd

  method process_property rd =
    let is_well_formed_xml ic =
      let buf = Buffer.create 100 in
      let i = Xmlm.make_input (`String (0, ic)) in
      let o = Xmlm.make_output (`Buffer buf) in
      try
        let rec pull i o depth =
          Xmlm.output o (Xmlm.peek i);
          match Xmlm.input i with
          | `El_start _ -> pull i o (depth + 1)
          | `El_end -> if depth = 1 then () else pull i o (depth - 1)
          | `Data _ -> pull i o depth
          | `Dtd _ -> assert false
        in
        Xmlm.output o (Xmlm.input i); (* `Dtd *)
        pull i o 0;
        Xmlm.eoi i
      with e ->
        Printf.printf "XML threw exception: %s\n" (Printexc.to_string e) ;
        false
    in
    Cohttp_lwt.Body.to_string rd.Wm.Rd.req_body >>= fun body ->
    let res = is_well_formed_xml body in
    Wm.continue res rd

  method delete_resource rd =
    Fs.destroy fs (string_of_int (self#id rd)) >>= fun res ->
    let deleted, resp_body =
      match res with
      | Ok () -> true, `String "{\"status\":\"ok\"}"
      | Error _ -> false, `String "{\"status\":\"not found\"}"
    in
    Wm.continue deleted { rd with Wm.Rd.resp_body }

  method generate_etag rd =
    Wm.continue (Some "foo") rd

  method private id rd =
    int_of_string (Wm.Rd.lookup_path_info_exn "id" rd)
end

let main () =
  (* listen on port 8080 *)
  let port = 8080 in
  (* create the file system *)
  Fs.connect "" >>= fun fs ->
  (* the route table *)
  let routes = [
    ("/items", fun () -> new items fs) ;
    ("/item/:id", fun () -> new item fs) ;
  ] in
  let callback (ch, conn) request body =
    let open Cohttp in
    (* Perform route dispatch. If [None] is returned, then the URI path did not
     * match any of the route patterns. In this case the server should return a
     * 404 [`Not_found]. *)
    Wm.dispatch' routes ~body ~request
    >|= begin function
      | None        -> (`Not_found, Header.init (), `String "Not found", [])
      | Some result -> result
    end
    >>= fun (status, headers, body, path) ->
      (* If you'd like to see the path that the request took through the
       * decision diagram, then run this example with the [DEBUG_PATH]
       * environment variable set. This should suffice:
       *
       *  [$ DEBUG_PATH= ./crud_lwt.native]
       *
       *)
      let path =
        match Sys.getenv "DEBUG_PATH" with
        | _ -> Printf.sprintf " - %s" (String.concat ", " path)
        | exception Not_found   -> ""
      in
      Printf.eprintf "%d - %s %s%s"
        (Code.code_of_status status)
        (Code.string_of_method (Request.meth request))
        (Uri.path (Request.uri request))
        path;
      (* Finally, send the response to the client *)
      Server.respond ~headers ~body ~status ()
  in
  (* create the server and handle requests with the function defined above *)
  let conn_closed (ch, conn) =
    Printf.printf "connection %s closed\n%!"
      (Sexplib.Sexp.to_string_hum (Conduit_lwt_unix.sexp_of_flow ch))
  in
  (* init the database with two items *)
  Fs.write fs "1" 0 (Cstruct.of_string "{\"name\":\"item 1\"}") >>= fun _ ->
  Fs.write fs "2" 0 (Cstruct.of_string "{\"name\":\"item 2\"}") >>= fun _ ->
  let config = Server.make ~callback ~conn_closed () in
  Server.create  ~mode:(`TCP(`Port port)) config
  >>= (fun () -> Printf.eprintf "hello_lwt: listening on 0.0.0.0:%d%!" port;
      Lwt.return_unit)

let () =  Lwt_main.run (main ())
