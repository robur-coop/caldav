open Lwt.Infix

let server_src = Logs.Src.create "http.server" ~doc:"HTTP server"
module Server_log = (val Logs.src_log server_src : Logs.LOG)

let access_src = Logs.Src.create "http.access" ~doc:"HTTP server access log"
module Access_log = (val Logs.src_log access_src : Logs.LOG)

module Main (C : Mirage_console.S) (R : Mirage_random.S) (T : Mirage_time.S) (Clock: Mirage_clock.PCLOCK) (_ : sig end) (S: Cohttp_mirage.Server.S) (Zap : Mirage_kv.RO) (Management : Tcpip.Stack.V4V6) = struct

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

  module Monitoring = Mirage_monitoring.Make(T)(Clock)(Management)
  module Syslog = Logs_syslog_mirage.Udp(C)(Clock)(Management)

  let start c _random _time _clock ctx http zap management =
    let hostname = Key_gen.name ()
    and syslog = Key_gen.syslog ()
    and monitor = Key_gen.monitor ()
    in
    (match syslog with
     | None -> Logs.warn (fun m -> m "no syslog specified, dumping on stdout")
     | Some ip -> Logs.set_reporter (Syslog.create c management ip ~hostname ()));
    (match monitor with
     | None -> Logs.warn (fun m -> m "no monitor specified, not outputting statistics")
     | Some ip -> Monitoring.create ~hostname ip management);
    let dynamic = author, user_agent, http_req in
    let init_http port config store =
      Server_log.info (fun f -> f "listening on %d/HTTP" port);
      http (`TCP port) @@ serve dynamic @@ opt_static_file zap @@ Webdav_server.dispatch config store
    in
    let config host =
      let do_trust_on_first_use = Key_gen.tofu () in
      Caldav.Webdav_config.config ~do_trust_on_first_use host
    in
    let init_store_for_runtime config =
      Git_kv.connect ctx (Key_gen.remote ()) >>= fun store ->
      Dav.connect store config (Key_gen.admin_password ())
    in
    let port = Key_gen.http_port () in
    (* we assume a TLS reverse proxy in front *)
    let config = config (Uri.of_string ("https://" ^ hostname)) in
    init_store_for_runtime config >>=
    init_http port config
end
