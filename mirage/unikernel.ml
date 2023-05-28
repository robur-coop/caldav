open Lwt.Infix

let server_src = Logs.Src.create "http.server" ~doc:"HTTP server"
module Server_log = (val Logs.src_log server_src : Logs.LOG)

let access_src = Logs.Src.create "http.access" ~doc:"HTTP server access log"
module Access_log = (val Logs.src_log access_src : Logs.LOG)

module Main (R : Mirage_random.S) (T : Mirage_time.S) (Clock: Mirage_clock.PCLOCK) (Stack : Tcpip.Stack.V4V6) (_ : sig end) (S: Cohttp_mirage.Server.S) (Zap : Mirage_kv.RO) = struct

  let author = Lwt.new_key ()
  and user_agent = Lwt.new_key ()
  and http_req = Lwt.new_key ()

  module Store = struct
    include Git_kv.Make(Clock)
    let batch t f =
      let author =
        Fmt.str "%a (via caldav unikernel)"
          Fmt.(option ~none:(any "no author") string) (Lwt.get author)
      and message =
        Fmt.str "during processing HTTP request %a@.by user-agent %a"
          Fmt.(option ~none:(any "no HTTP request") string) (Lwt.get http_req)
          Fmt.(option ~none:(any "none") string) (Lwt.get user_agent)
      in
      change_and_push t ~author ~message f
  end
  module Dav_fs = Caldav.Webdav_fs.Make(Clock)(Store)
  module Dav = Caldav.Webdav_api.Make(R)(Clock)(Dav_fs)
  module Webdav_server = Caldav.Webdav_server.Make(R)(Clock)(Dav_fs)(S)
  module Dns = Dns_certify_mirage.Make(R)(Clock)(T)(Stack)

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

  let opt_static_file zap_data next request body =
    let uri = Cohttp.Request.uri request in
    let path = match Uri.path uri with
      | "/" -> "/index.html"
      | p -> p
    in
    Zap.get zap_data (Mirage_kv.Key.v path) >>= function
    | Ok data ->
      let mime_type = Magic_mime.lookup path in
      let headers = Cohttp.Header.init_with "content-type" mime_type in
      S.respond ~headers ~status:`OK ~body:(`String data) ()
    | _ -> next request body

  let get_user_from_auth = function
    | None -> None
    | Some v ->
      let basic = "Basic " in
      let blen = String.length basic
      and vlen = String.length v
      in
      if vlen >= blen && String.(equal (sub v 0 blen) basic) then
        let b64 = String.sub v blen (vlen - blen) in
        match Base64.decode b64 with
        | Error _ -> Some "bad b64 encoding"
        | Ok data -> Some (List.hd (String.split_on_char ':' data))
      else
        Some "not basic"

  let serve (author_k, user_agent_k, request_k) callback =
    let callback (_, cid) request body =
      let open Cohttp in
      let cid = Connection.to_string cid in
      let uri = Request.uri request in
      Access_log.debug (fun f -> f "[%s] serving %s." cid (Uri.to_string uri));
      let hdr = request.Request.headers in
      Lwt.with_value author_k (get_user_from_auth (Header.get hdr "Authorization")) @@ fun () ->
      Lwt.with_value user_agent_k (Header.get hdr "User-Agent") @@ fun () ->
      let req = Fmt.str "%s %s" (Code.string_of_method request.Request.meth) (Uri.path uri) in
      Lwt.with_value request_k (Some req) @@ fun () ->
      callback request body
    and conn_closed (_,cid) =
      let cid = Cohttp.Connection.to_string cid in
      Access_log.debug (fun f -> f "[%s] closing" cid);
    in
    S.make ~conn_closed ~callback ()

  let start _random _time _clock stack ctx http zap =
    let dynamic = author, user_agent, http_req in
    let init_http port config store =
      Server_log.info (fun f -> f "listening on %d/HTTP" port);
      http (`TCP port) @@ serve dynamic @@ opt_static_file zap @@ Webdav_server.dispatch config store
    in
    let hostname = Key_gen.hostname () in
    let init_https port config store =
      let hostname = Domain_name.(host_exn (of_string_exn hostname)) in
      let rec get_cert () =
        Dns.retrieve_certificate
          stack ~dns_key:(Key_gen.dns_key ())
          ~hostname ~key_seed:(Key_gen.key_seed ())
          (Key_gen.dns_server ()) (Key_gen.dns_port ()) >>= function
        | Error (`Msg m) -> Lwt.fail_with m
        | Ok certificates ->
          let valid_until =
            match fst certificates with
            | cert :: _ -> snd (X509.Certificate.validity cert)
            | _ -> failwith "empty certificate chain retrieved"
          in
          let certificates = `Single certificates in
          let expiry =
            fst (Ptime.Span.to_d_ps (Ptime.diff valid_until (Ptime.v (Clock.now_d_ps ()))))
          in
          let tls_config = Tls.Config.server ~certificates () in
          Server_log.info (fun f -> f "listening on %d/HTTPS" port);
          let tls = `TLS (tls_config, `TCP port) in
          Lwt.pick [
            T.sleep_ns (max (Duration.of_hour 1) (Duration.of_day (max 0 (expiry - 7))));
            http tls @@ serve dynamic @@ opt_static_file zap @@  Webdav_server.dispatch config store
          ] >>= get_cert
      in
      get_cert ()
    in
    let config host =
      let do_trust_on_first_use = Key_gen.tofu () in
      Caldav.Webdav_config.config ~do_trust_on_first_use host
    in
    let init_store_for_runtime config =
      Git_kv.connect ctx (Key_gen.remote ()) >>= fun store ->
      Dav.connect store config (Key_gen.admin_password ())
    in
    match Key_gen.http_port (), Key_gen.https_port () with
    | None, None ->
      Logs.err (fun m -> m "no port provided for neither HTTP nor HTTPS, exiting") ;
      exit Functoria_runtime.argument_error
    | Some port, None ->
      let scheme, base_port =
        if Key_gen.tls_proxy () then "https", 443 else "http", port
      in
      let config = config @@ Caldav.Webdav_config.host ~scheme ~port:base_port ~hostname () in
      init_store_for_runtime config >>=
      init_http port config
    | None, Some port ->
      (if Key_gen.tls_proxy () then begin
          Logs.err (fun m -> m "Both https port and TLS proxy chosen, please choose only one");
          exit Functoria_runtime.argument_error
        end);
      let config = config @@ Caldav.Webdav_config.host ~scheme:"https" ~port ~hostname () in
      init_store_for_runtime config >>=
      init_https port config
    | Some http_port, Some https_port ->
      (if Key_gen.tls_proxy () then begin
          Logs.err (fun m -> m "Both https port and TLS proxy chosen, please choose only one");
          exit Functoria_runtime.argument_error
        end);
      Server_log.info (fun f -> f "redirecting on %d/HTTP to %d/HTTPS" http_port https_port);
      let config = config @@ Caldav.Webdav_config.host ~scheme:"https" ~port:https_port ~hostname () in
      init_store_for_runtime config >>= fun store ->
      Lwt.pick [
        http (`TCP http_port) @@ serve dynamic @@ redirect https_port ;
        init_https https_port config store
      ]
end
