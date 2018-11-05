open Lwt.Infix

(** Common signature for http and https. *)
module type HTTP = Cohttp_lwt.S.Server

let http_src = Logs.Src.create "http" ~doc:"HTTP server"
module Http_log = (val Logs.src_log http_src : Logs.LOG)

module Main (R : Mirage_random.C) (Clock: Mirage_clock.PCLOCK) (KEYS: Mirage_types_lwt.KV_RO) (S: HTTP) = struct
  module Fs = Caldav.Webdav_fs.Make(FS_unix)
  module Dav = Caldav.Webdav_api.Make(Fs)
  module X509 = Tls_mirage.X509(KEYS)(Clock)

  let init_users fs now config user_password =
    Lwt_list.iter_p (fun (name, password) ->
        let salt = R.generate 15 in
        Dav.make_user fs now config ~name ~password ~salt >|= fun _ -> ())
      user_password >>= fun () ->
    Dav.make_group fs now config "group" [ "root" ; "test" ]

  let initialize_fs clock host () =
    FS_unix.connect "/tmp/calendar" >>= fun fs ->
    let now = Ptime.v (Clock.now_d_ps clock) in
    let config = Caldav.Webdav_config.config host in
    Dav.initialize_fs fs now config >>= fun () ->
    let user_password = [
      ("test", "password") ;
      ("root", "toor") ;
      ("nobody", "1")
    ] in
    init_users fs now config user_password >|= fun _ ->
    (config, fs)

  let tls_init kv =
    Lwt.catch (fun () ->
        X509.certificate kv `Default >|= fun cert ->
        Tls.Config.server ~certificates:(`Single cert) ())
      (function
        | Failure _ -> Lwt.fail_with "Could not find server.pem and server.key in the <working directory>/tls."
        | e -> Lwt.fail e)

  let dispatch clock generate_salt config fs request body =
    let module WmClock = struct
      let now () =
        let ts = Clock.now_d_ps clock in
        let span = Ptime.Span.v ts in
        match Ptime.Span.to_int_s span with
        | None -> 0
        | Some seconds -> seconds
    end in
    let module Webdav_server = Caldav.Webdav_server.Make(WmClock)(Fs) in
    (* Perform route dispatch. If [None] is returned, then the URI path did not
     * match any of the route patterns. In this case the server should return a
     * 404 [`Not_found]. *)
    let now () = Ptime.v (Clock.now_d_ps clock) in
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

  (* Redirect to the same address, but in https. *)
  let redirect port request _body =
    let redirect_port = match port with 443 -> None | x -> Some x in
    let uri = Cohttp.Request.uri request in
    let new_uri = Uri.with_scheme uri (Some "https") in
    let new_uri = Uri.with_port new_uri redirect_port in
    Http_log.info (fun f -> f "[%s] -> [%s]"
                      (Uri.to_string uri) (Uri.to_string new_uri));
    let headers = Cohttp.Header.init_with "location" (Uri.to_string new_uri) in
    S.respond ~headers ~status:`Moved_permanently ~body:`Empty ()

  let serve callback =
    let callback (_, cid) request body =
      let cid = Cohttp.Connection.to_string cid in
      let uri = Cohttp.Request.uri request in
      Http_log.info (fun f -> f "[%s] serving %s." cid (Uri.to_string uri));
      callback request body
    and conn_closed (_,cid) =
      let cid = Cohttp.Connection.to_string cid in
      Http_log.info (fun f -> f "[%s] closing" cid);
    in
    S.make ~conn_closed ~callback ()

  let start _random clock keys http =
    let generate_salt () = R.generate 15 in
    let init_http port (config, fs) =
      Http_log.info (fun f -> f "listening on %d/HTTP" port);
      http (`TCP port) @@ serve (dispatch clock generate_salt config fs)
    in
    let init_https port (config, fs) =
      tls_init keys >>= fun tls_config ->
      Http_log.info (fun f -> f "listening on %d/HTTPS" port);
      let tls = `TLS (tls_config, `TCP port) in
      http tls @@ serve (dispatch clock generate_salt config fs)
    in
    match Key_gen.http_port (), Key_gen.https_port () with
    | None, None ->
      Logs.err (fun m -> m "no port provided for neither HTTP nor HTTPS, exiting") ;
      Lwt.return_unit
    | Some port, None ->
      let host = Caldav.Webdav_config.host ~port () in
      initialize_fs clock host () >>=
      init_http port
    | None, Some port ->
      let host = Caldav.Webdav_config.host ~scheme:"https" ~port () in
      initialize_fs clock host () >>=
      init_https port
    | Some http_port, Some https_port ->
      let host = Caldav.Webdav_config.host ~scheme:"https" ~port:https_port () in
      initialize_fs clock host () >>= fun (config, fs) ->
      Lwt.pick [
        http (`TCP http_port) @@ serve (redirect https_port) ;
        init_https https_port (config, fs)
      ]
end
