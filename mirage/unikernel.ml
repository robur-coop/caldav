open Lwt.Infix

let server_src = Logs.Src.create "http.server" ~doc:"HTTP server"
module Server_log = (val Logs.src_log server_src : Logs.LOG)

let access_src = Logs.Src.create "http.access" ~doc:"HTTP server access log"
module Access_log = (val Logs.src_log access_src : Logs.LOG)

(* Command line arguments *)

open Cmdliner
open Mirage_runtime_network.Arg

let ssh_key =
  let doc = Arg.info ~doc:"Private ssh key (rsa:<seed> or ed25519:<b64-key>)." ["ssh-key"] in
  Arg.(value & (opt (some string) None doc))

let ssh_password =
  let doc = Arg.info ~doc:"The private SSH password." [ "ssh-password" ] in
  Arg.(value & (opt (some string) None doc))

let authenticator =
  let doc = Arg.info ~doc:"Authenticator for SSH." ["authenticator"] in
  Arg.(value & (opt (some string) None doc))

let tls_authenticator =
  (* this will not look the same in the help printout *)
  let doc = "TLS host authenticator. See git_http in lib/mirage/mirage.mli for a description of the format."
  in
  let doc = Arg.info ~doc ["tls-authenticator"] in
  Arg.(value & (opt (some string) None doc))

(* TODO: make it possible to enable and disable schemes without providing a port *)
let http_port =
  let doc = Arg.info ~doc:"Listening HTTP port." ["http"] ~docv:"PORT" in
  Arg.(value & (opt (some int) None doc))

let https_port =
  let doc = Arg.info ~doc:"Listening HTTPS port." ["https"] ~docv:"PORT" in
  Arg.(value & (opt (some int) None doc))

let tls_proxy =
  let doc = "TLS proxy in front (use https://<hostname> as base url)." in
  let doc = Arg.info ~doc ["tls-proxy"] in
  Arg.(value & (opt bool false doc))

let admin_password =
  let doc = Arg.info ~doc:"Password for the administrator." ["admin-password"] ~docv:"STRING" in
  Arg.(value & (opt (some string) None doc))

let remote =
  let doc = Arg.info ~doc:"Location of calendar data. Use suffix #foo to specify branch 'foo'." [ "remote" ] ~docv:"REMOTE" in
  Arg.(required & (opt (some string) None doc))

let tofu =
  let doc = Arg.info ~doc:"If a user does not exist, create them and give them a new calendar." [ "tofu" ] in
  Arg.(value & (flag doc))

let hostname =
  let doc = Arg.info ~doc:"Hostname to use." [ "host"; "name" ] ~docv:"STRING" in
  Arg.(required & (opt (some string) None doc))

let apple_testable =
  let doc = Arg.info ~doc:"Configure the server to use with Apple CCS CalDAVtester." [ "apple-testable" ] in
  Arg.(value & (flag doc))

let monitor =
  let doc = Arg.info ~doc:"monitor host IP" ["monitor"] in
  Arg.(value & (opt (some ip_address) None doc))

let syslog =
  let doc = Arg.info ~doc:"syslog host IP" ["syslog"] in
  Arg.(value & (opt (some ip_address) None doc))

module Main (R : Mirage_random.S) (Clock: Mirage_clock.PCLOCK) (_ : sig end) (KEYS: Mirage_kv.RO) (S: Cohttp_mirage.Server.S) (Zap : Mirage_kv.RO) = struct

  let author = Lwt.new_key ()
  and user_agent = Lwt.new_key ()
  and http_req = Lwt.new_key ()

  module X509 = Tls_mirage.X509(KEYS)(Clock)
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

  let tls_init kv =
    Lwt.catch (fun () ->
        X509.certificate kv `Default >|= fun cert ->
        Tls.Config.server ~certificates:(`Single cert) ())
      (fun e ->
         (match e with
          | Failure f ->
            Logs.err (fun m -> m "Could not find server.pem and server.key in the <working directory>/tls. %s" f)
          | e -> Logs.err (fun m -> m "Exception %s while reading certificates" (Printexc.to_string e)));
         exit Functoria_runtime.argument_error)

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

  let start _random _clock ctx keys http zap
      http_port https_port tls_proxy admin_password remote tofu hostname _apple_testable =
    let dynamic = author, user_agent, http_req in
    let init_http port config store =
      Server_log.info (fun f -> f "listening on %d/HTTP" port);
      http (`TCP port) @@ serve dynamic @@ opt_static_file zap @@ Webdav_server.dispatch config store
    in
    let init_https port config store =
      tls_init keys >>= fun tls_config ->
      Server_log.info (fun f -> f "listening on %d/HTTPS" port);
      let tls = `TLS (tls_config, `TCP port) in
      http tls @@ serve dynamic @@ opt_static_file zap @@ Webdav_server.dispatch config store
    in
    let config host =
      Caldav.Webdav_config.config ~do_trust_on_first_use:tofu host
    in
    let init_store_for_runtime config =
      Git_kv.connect ctx remote >>= fun store ->
      Dav.connect store config admin_password
    in
    match http_port, https_port with
    | None, None ->
      Logs.err (fun m -> m "no port provided for neither HTTP nor HTTPS, exiting") ;
      exit Functoria_runtime.argument_error
    | Some port, None ->
      let scheme, base_port =
        if tls_proxy then "https", 443 else "http", port
      in
      let config = config @@ Caldav.Webdav_config.host ~scheme ~port:base_port ~hostname () in
      init_store_for_runtime config >>=
      init_http port config
    | None, Some port ->
      (if tls_proxy then begin
          Logs.err (fun m -> m "Both https port and TLS proxy chosen, please choose only one");
          exit Functoria_runtime.argument_error
        end);
      let config = config @@ Caldav.Webdav_config.host ~scheme:"https" ~port ~hostname () in
      init_store_for_runtime config >>=
      init_https port config
    | Some http_port, Some https_port ->
      (if tls_proxy then begin
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
