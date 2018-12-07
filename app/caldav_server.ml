(*
open Lwt.Infix
open Caldav.Webdav_config

module Clock = struct
  let now = fun () -> int_of_float (Unix.gettimeofday ())
end

module Fs = Caldav.Webdav_fs.Make(FS_unix)
module Webdav_server = Caldav.Webdav_server.Make(Clock)(Fs)
module Dav = Caldav.Webdav_api.Make(Fs)

module Http_server = Cohttp_lwt_unix.Server

let now = Ptime_clock.now

let generate_salt () = Nocrypto.Rng.generate 15

let init_users fs now config user_password =
  Lwt_list.iter_p (fun (name, password) ->
      let salt = generate_salt () in
      Dav.make_user fs now config ~name ~password ~salt >|= fun _ -> ())
    user_password >>= fun () ->
  Dav.make_group fs now config "group" [ "root" ; "test" ]

let main () =
  (* avoids ECONNRESET, see https://github.com/mirage/ocaml-cohttp/issues/511 *)
  Lwt.async_exception_hook := (function
      | Unix.Unix_error (error, func, arg) ->
        Logs.warn (fun m -> m  "Client connection error %s: %s(%S)"
                      (Unix.error_message error) func arg)
      | exn -> Logs.err (fun m -> m "Unhandled exception: %a" Fmt.exn exn)
    );
  Logs.set_reporter (Logs_fmt.reporter ());
  Logs.set_level (Some Logs.Debug);
  let host = host () in
  let do_trust_on_first_use = match Sys.getenv "CALDAV_TOFU" with
    | exception Not_found -> false
    | "yes" | "YES" | "1" | "true" | "TRUE" -> true
    | _ -> false
  in
  let config = config ~do_trust_on_first_use host in
  (* create the file system *)
  FS_unix.connect "/tmp/calendar" >>= fun fs ->
  (* only for apple test suite *)
  (* initialize_fs_for_apple_testsuite fs now config >>= fun () -> *)
  Dav.initialize_fs fs (now ()) config >>= fun () ->
  let user_password = [
    ("test", "password") ;
    ("root", "toor") ;
    ("nobody", "1")
  ] in
  init_users fs (now ()) config user_password >>= fun _ ->
  let callback (ch, conn) request body =
    let open Cohttp in
    (* Perform route dispatch. If [None] is returned, then the URI path did not
     * match any of the route patterns. In this case the server should return a
     * 404 [`Not_found]. *)
    Logs.info (fun m -> m "REQUEST %s %s headers %s"
      (Code.string_of_method (Request.meth request))
      (Request.resource request)
      (Header.to_string (Request.headers request)) );
    Webdav_server.Wm.dispatch' (Webdav_server.routes config fs now generate_salt) ~body ~request
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
      Logs.info (fun m -> m "\nRESPONSE %d - %s %s%s, body: %s"
        (Code.code_of_status status)
        (Code.string_of_method (Request.meth request))
        (Uri.path (Request.uri request))
        path
        (match body with `String s -> s | `Empty -> "empty" | _ -> "unknown") ) ;
      (* Finally, send the response to the client *)
      Http_server.respond ~headers ~body ~status ()
  in
  (* create the server and handle requests with the function defined above *)
  let conn_closed (ch, conn) =
    Logs.info (fun m -> m "connection %s closed"
      (Sexplib.Sexp.to_string_hum (Conduit_lwt_unix.sexp_of_flow ch)))
  in
  let port = match Uri.port config.host with None -> 80 | Some x -> x in
  let http_config = Http_server.make ~callback ~conn_closed () in
  Http_server.create ~mode:(`TCP(`Port port)) http_config
  >>= (fun () -> Logs.app (fun m -> m "caldav_server.exe: listening on 0.0.0.0:%d%!" port);
      Lwt.return_unit)

let () =  Lwt_main.run (main ())
          *)


open Lwt.Infix
open OUnit
open Cohttp
open Cohttp_lwt_unix
open Cohttp_lwt_unix_test

module Body = Cohttp_lwt.Body

let message = "Hello sanity!"

let server =
  List.map const [ (* t *)
    Server.respond_string ~status:`OK ~body:message ();
  ] |> response_sequence

let ts =
  Cohttp_lwt_unix_test.test_server_s server begin fun uri ->
    let t () =
      Client.get uri >>= fun (_, body) ->
      body |> Body.to_string >|= fun body ->
      assert_equal body message in
    [ "sanity test", t ]
  end

let () = ignore (Lwt_main.run (run_async_tests ts))
