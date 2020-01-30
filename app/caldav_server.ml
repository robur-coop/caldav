(*
open Lwt.Infix
open Caldav.Webdav_config

module Fs = Caldav.Webdav_fs.Make(Pclock)(FS_unix)
module Webdav_server = Caldav.Webdav_server.Make(Clock)(Fs)

module G = Irmin_git.Mem
module Store = Irmin_mirage.Git.KV_RW(G)(Pclock)
module Fs = Caldav.Webdav_fs.Make(Pclock)(Store)

module Xml = Caldav.Webdav_xml
module Dav = Caldav.Webdav_api.Make(Fs)

module Http_server = Cohttp_lwt_unix.Server
module Conduit_mirage_tcp = Conduit_mirage.With_tcp(Tcpip_stack_socket)

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
  G.v (Fpath.v "bla") >>= function
  | Error _ -> assert false
  | Ok git ->
    Udpv4_socket.connect None >>= fun udp ->
    Tcpv4_socket.connect None >>= fun tcp ->
    Tcpip_stack_socket.connect [] udp tcp >>= fun stack ->
    Conduit_mirage_tcp.connect stack Conduit_mirage.empty >>= fun conduit' ->
    Conduit_mirage.with_tls conduit' >>= fun conduit ->
    let resolver = Resolver_lwt_unix.system in
    Store.connect git ~conduit ~author:"caldav" ~resolver ~msg:(fun _ -> "a calendar change") ()
      "https://github.com/roburio/testcalendar.git" >>= fun fs ->
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
open Cohttp_lwt_unix_test

module Http_server = Cohttp_lwt_unix.Server
module Body = Cohttp_lwt.Body

module KV_mem = Mirage_kv_mem.Make(Pclock)
module Dav_fs = Caldav.Webdav_fs.Make(Pclock)(KV_mem)

module Webdav_server = Caldav.Webdav_server.Make(Mirage_random_test)(Pclock)(Dav_fs)(Http_server)

module Api = Caldav.Webdav_api.Make(Mirage_random_test)(Pclock)(Dav_fs)

let header, content, footer =
{|BEGIN:VCALENDAR
VERSION:2.0
PRODID:-//Example Corp.//CalDAV Client//EN
|},
{|BEGIN:VTIMEZONE
LAST-MODIFIED:20040110T032845Z
TZID:US/Eastern
BEGIN:DAYLIGHT
DTSTART:20000404T020000
RRULE:FREQ=YEARLY;BYDAY=1SU;BYMONTH=4
TZNAME:EDT
TZOFFSETFROM:-0500
TZOFFSETTO:-0400
END:DAYLIGHT
BEGIN:STANDARD
DTSTART:20001026T020000
RRULE:FREQ=YEARLY;BYDAY=-1SU;BYMONTH=10
TZNAME:EST
TZOFFSETFROM:-0400
TZOFFSETTO:-0500
END:STANDARD
END:VTIMEZONE
BEGIN:VEVENT
UID:74855313FA803DA593CD579A@example.com
DTSTAMP:20060206T001102Z
DTSTART;TZID=US/Eastern:20060102T100000
DURATION:PT1H
SUMMARY:Event #1
Description:Go Steelers!
END:VEVENT
|},
{|END:VCALENDAR
|}

let data = header ^ content ^ footer

let rec repeat n s =
  if n = 0 then "" else s ^ repeat (pred n) s

let convert_nl_to_cr_nl str =
  let re = Re.compile ( Re.Perl.re "\n" ) in
  Re.replace_string ~all:true re ~by:"\r\n" str

let expected =
  convert_nl_to_cr_nl
    ({|BEGIN:VCALENDAR
PRODID:-//ROBUR.IO//EN
VERSION:2.0
X-WR-CALNAME:root
|} ^ repeat 1000 content ^ footer)

let config = Caldav.Webdav_config.config ~do_trust_on_first_use:true (Uri.of_string "localhost")

let server fs =
  let headers =
    let auth_header = "Basic " ^ Base64.encode_string "root:foo" in
    Cohttp.Header.add (Cohttp.Header.init ()) "Authorization" auth_header
  in
  let request, body = Request.make ~headers ~meth:`GET (Uri.of_string "/calendars/root"), `Empty in
  List.map const [ (* t *)
    Webdav_server.dispatch config fs request body
    (* Http_server.respond ~status:`OK ~body:(`String message) (); *)
  ] |> response_sequence

let ts fs =
  Cohttp_lwt_unix_test.test_server_s (server fs) begin fun uri ->
    let t () =
      Cohttp_lwt_unix.Client.get uri >>= fun (_, body) ->
      body |> Body.to_string >|= fun body ->
      Logs.debug (fun m -> m "found %s" body) ;
      assert_equal body expected in
    [ "sanity test", t ]
  end

let () =
  ignore (Lwt_main.run (
      Logs.set_reporter (Logs_fmt.reporter ~dst:Format.std_formatter ()) ;
      Logs.set_level (Some Logs.Info);
      KV_mem.connect () >>= fun fs ->
      let now = Ptime.epoch in
      Api.connect fs config (Some "foo") >>= fun _fs ->
      let rec go = function
        | 0 -> Lwt.return_unit
        | n ->
          let name = string_of_int n ^ ".ics" in
          let filename = Dav_fs.create_file (`Dir ["calendars" ; "root"]) name in
          let props = Caldav.Properties.create
              ~content_type:"text/calendar"
              [(`All, `Grant [ `All ])] now (String.length data) name
          in
          Dav_fs.write fs filename data props >>= fun _ ->
          go (pred n)
      in
      go 1000 >>= fun () ->
      run_async_tests (ts fs)))
