open Lwt.Infix

(** Common signature for http and https. *)
module type HTTP = Cohttp_lwt.S.Server

let http_src = Logs.Src.create "http" ~doc:"HTTP server"
module Http_log = (val Logs.src_log http_src : Logs.LOG)

module Main (R : Mirage_random.C) (Clock: Mirage_clock.PCLOCK) (S: HTTP) = struct
  module Fs = Caldav.Webdav_fs.Make(FS_unix)
  module Dav = Caldav.Webdav_api.Make(Fs)

  let serve clock generate_salt config fs =
    let module WmClock = struct
      let now () =
        let ts = Clock.now_d_ps clock in
        let span = Ptime.Span.v ts in
        match Ptime.Span.to_int_s span with
        | None -> 0
        | Some seconds -> seconds
    end in
    let module Webdav_server = Caldav.Webdav_server.Make(WmClock)(Fs) in
    let now () = Ptime.v (Clock.now_d_ps clock) in
    let callback (_, cid) request body =
      (* Perform route dispatch. If [None] is returned, then the URI path did not
       * match any of the route patterns. In this case the server should return a
       * 404 [`Not_found]. *)
      Http_log.info (fun m -> m "REQUEST %s %s headers %s"
        (Cohttp.Code.string_of_method (Cohttp.Request.meth request))
        (Cohttp.Request.resource request)
        (Cohttp.Header.to_string (Cohttp.Request.headers request)) );
      Webdav_server.Wm.dispatch' (Webdav_server.routes config fs now generate_salt) ~body ~request
      >|= begin function
        | None        -> (`Not_found, Cohttp.Header.init (), `String "Not found", [])
        | Some result -> result
      end
      >>= fun (status, headers, body, _path) ->
      Http_log.info (fun m -> m "\nRESPONSE %d - %s %s, body: %s"
        (Cohttp.Code.code_of_status status)
        (Cohttp.Code.string_of_method (Cohttp.Request.meth request))
        (Uri.path (Cohttp.Request.uri request))
        (match body with `String s -> s | `Empty -> "empty" | _ -> "unknown") ) ;
      (* Finally, send the response to the client *)
      S.respond ~headers ~body ~status ()
    in
    let conn_closed (_,cid) =
      let cid = Cohttp.Connection.to_string cid in
      Http_log.info (fun f -> f "[%s] closing" cid);
    in
    S.make ~conn_closed ~callback ()

  let init_users fs now config user_password =
    Lwt_list.iter_p (fun (name, password) ->
        let salt = R.generate 15 in
        Dav.make_user fs now config ~name ~password ~salt >|= fun _ -> ())
      user_password >>= fun () ->
    Dav.make_group fs now config "group" [ "root" ; "test" ]

  let start _random clock http =
    let http_port = Key_gen.http_port () in
    let tcp = `TCP http_port in
    FS_unix.connect "/tmp/calendar" >>= fun fs ->
    let now = Ptime.v (Clock.now_d_ps clock) in
    let config = Caldav.Webdav_config.(config (host ())) in
    Dav.initialize_fs fs now config >>= fun () ->
    let user_password = [
      ("test", "password") ;
      ("root", "toor") ;
      ("nobody", "1")
    ] in
    init_users fs now config user_password >>= fun _ ->
    let generate_salt () = R.generate 15 in
    let http =
      Http_log.info (fun f -> f "listening on %d/TCP" http_port);
      http tcp @@ serve clock generate_salt config fs
    in
    http
end
