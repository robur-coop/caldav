open Lwt.Infix

(** Common signature for http and https. *)
module type HTTP = Cohttp_lwt.S.Server

let server_src = Logs.Src.create "http.server" ~doc:"HTTP server"
module Server_log = (val Logs.src_log server_src : Logs.LOG)

let access_src = Logs.Src.create "http.access" ~doc:"HTTP server access log"
module Access_log = (val Logs.src_log access_src : Logs.LOG)

module Main (R : Mirage_random.C) (Clock: Mirage_clock.PCLOCK) (Mclock: Mirage_clock.MCLOCK) (KEYS: Mirage_types_lwt.KV_RO) (S: HTTP) (Resolver : Resolver_lwt.S) (Conduit : Conduit_mirage.S) (Store : Mirage_kv_lwt.RW) = struct
  module X509 = Tls_mirage.X509(KEYS)(Clock)
  module Dav_fs = Caldav.Webdav_fs.Make(Store)
  module Dav = Caldav.Webdav_api.Make(R)(Clock)(Dav_fs)
  module Webdav_server = Caldav.Webdav_server.Make(R)(Clock)(Dav_fs)(S)
(*
  module Metrics_reporter = Metrics_mirage.Influx(Mclock)(STACK)
*)

  let tls_init kv =
    Lwt.catch (fun () ->
        X509.certificate kv `Default >|= fun cert ->
        Tls.Config.server ~certificates:(`Single cert) ())
      (function
        | Failure _ -> Lwt.fail_with "Could not find server.pem and server.key in the <working directory>/tls."
        | e -> Lwt.fail e)

  (* Redirect to the same address, but in https. *)
  let redirect port request _body =
    let redirect_port = match port with 443 -> None | x -> Some x in
    let uri = Cohttp.Request.uri request in
    let new_uri = Uri.with_scheme uri (Some "https") in
    let new_uri = Uri.with_port new_uri redirect_port in
    Access_log.debug (fun f -> f "[%s] -> [%s]"
                         (Uri.to_string uri) (Uri.to_string new_uri));
    let headers = Cohttp.Header.init_with "location" (Uri.to_string new_uri) in
    S.respond ~headers ~status:`Moved_permanently ~body:`Empty ()

  let serve callback =
    let callback (_, cid) request body =
      let cid = Cohttp.Connection.to_string cid in
      let uri = Cohttp.Request.uri request in
      Access_log.debug (fun f -> f "[%s] serving %s." cid (Uri.to_string uri));
      callback request body
    and conn_closed (_,cid) =
      let cid = Cohttp.Connection.to_string cid in
      Access_log.debug (fun f -> f "[%s] closing" cid);
    in
    S.make ~conn_closed ~callback ()

(*
  let gc_quick_stat = Metrics.gc_quick_stat ~tags:Metrics.Tags.[]
  let gc_stat = Metrics.gc_stat ~tags:Metrics.Tags.[]

  let monitor_gc ?(quick = true) delay =
  let id x = x in
  let f () =
    if quick then Metrics.add gc_quick_stat id (fun d -> d ())
    else Metrics.add gc_stat id (fun d -> d ())
  in
  let rec loop () =
    f ();
    OS.Time.sleep_ns (Duration.of_f delay) >>= fun () -> loop ()
  in
  Lwt.async loop
*)

  let start _random clock mclock tls_keys http resolver conduit store =
(*
    (match Key_gen.monitor () with
    | None -> Lwt.return_unit 
    | Some monitor -> 
      Metrics.enable_all ();
      monitor_gc 0.1;
      let hostname = Key_gen.hostname () in
      Metrics_reporter.create mclock net ~hostname (Ipaddr.V4.of_string_exn monitor) () 
      >|= function
      | Error () -> assert false
      | Ok reporter -> Metrics.set_reporter reporter) >>= fun () ->
*)
    (* TODO naming *)
    let init_http port config store =
      Server_log.info (fun f -> f "listening on %d/HTTP" port);
      http (`TCP port) @@ serve @@ Webdav_server.dispatch config store
    in
    let init_https port config store =
      tls_init tls_keys >>= fun tls_config ->
      Server_log.info (fun f -> f "listening on %d/HTTPS" port);
      let tls = `TLS (tls_config, `TCP port) in
      http tls @@ serve @@ Webdav_server.dispatch config store
    in
    let config host =
      let do_trust_on_first_use = Key_gen.tofu () in
      Caldav.Webdav_config.config ~do_trust_on_first_use host
    in
    let init_store_for_runtime config =
      let admin_pass = Key_gen.admin_password ()
      and apple_testable = Key_gen.apple_testable ()
      in
      if not apple_testable 
      then Dav.connect store config admin_pass 
      else
        let now = Ptime.v (Clock.now_d_ps clock) in
        Dav.initialize_fs_for_apple_testsuite store now config >|= fun () -> 
        store
    in
    let hostname = Key_gen.hostname () in
    match Key_gen.http_port (), Key_gen.https_port () with
    | None, None ->
      Logs.err (fun m -> m "no port provided for neither HTTP nor HTTPS, exiting") ;
      Lwt.return_unit
    | Some port, None ->
      let config = config @@ Caldav.Webdav_config.host ~port ~hostname () in
      init_store_for_runtime config >>=
      init_http port config
    | None, Some port ->
      let config = config @@ Caldav.Webdav_config.host ~scheme:"https" ~port ~hostname () in
      init_store_for_runtime config >>=
      init_https port config
    | Some http_port, Some https_port ->
      Server_log.info (fun f -> f "redirecting on %d/HTTP to %d/HTTPS" http_port https_port);
      let config = config @@ Caldav.Webdav_config.host ~scheme:"https" ~port:https_port ~hostname () in
      init_store_for_runtime config >>= fun store ->
      Lwt.pick [
        http (`TCP http_port) @@ serve (redirect https_port) ;
        init_https https_port config store
      ]
end
