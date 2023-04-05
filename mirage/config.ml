open Mirage

let ssh_key =
  let doc = Key.Arg.info ~doc:"Private ssh key (rsa:<seed> or ed25519:<b64-key>)." ["ssh-key"] in
  Key.(create "ssh-key" Arg.(opt (some string) None doc))

let ssh_password =
  let doc = Key.Arg.info ~doc:"The private SSH password." [ "ssh-password" ] in
  Key.(create "ssh-password" Arg.(opt (some string) None doc))

let authenticator =
  let doc = Key.Arg.info ~doc:"Authenticator for SSH." ["authenticator"] in
  Key.(create "authenticator" Arg.(opt (some string) None doc))

let tls_authenticator =
  (* this will not look the same in the help printout *)
  let doc = "TLS host authenticator. See git_http in lib/mirage/mirage.mli for a description of the format."
  in
  let doc = Key.Arg.info ~doc ["tls-authenticator"] in
  Key.(create "tls-authenticator" Arg.(opt (some string) None doc))

let net = generic_stackv4v6 default_network

(* set ~tls to false to get a plain-http server *)
let http_srv = cohttp_server @@ conduit_direct ~tls:true net

(* TODO: make it possible to enable and disable schemes without providing a port *)
let http_port =
  let doc = Key.Arg.info ~doc:"Listening HTTP port." ["http"] ~docv:"PORT" in
  Key.(create "http_port" Arg.(opt (some int) None doc))

let https_port =
  let doc = Key.Arg.info ~doc:"Listening HTTPS port." ["https"] ~docv:"PORT" in
  Key.(create "https_port" Arg.(opt (some int) None doc))

let tls_proxy =
  let doc = "TLS proxy in front (use https://<hostname> as base url)." in
  let doc = Key.Arg.info ~doc ["tls-proxy"] in
  Key.(create "tls-proxy" Arg.(opt bool false doc))

let certs = generic_kv_ro ~key:Key.(value @@ kv_ro ()) "tls"
let zap = generic_kv_ro ~key:Key.(value @@ kv_ro ()) "caldavzap"

let admin_password =
  let doc = Key.Arg.info ~doc:"Password for the administrator." ["admin-password"] ~docv:"STRING" in
  Key.(create "admin_password" Arg.(opt (some string) None doc))

let remote =
  let doc = Key.Arg.info ~doc:"Location of calendar data. Use suffix #foo to specify branch 'foo'." [ "remote" ] ~docv:"REMOTE" in
  Key.(create "remote" Arg.(required string doc))

let tofu =
  let doc = Key.Arg.info ~doc:"If a user does not exist, create them and give them a new calendar." [ "tofu" ] in
  Key.(create "tofu" Arg.(flag doc))

let hostname =
  let doc = Key.Arg.info ~doc:"Hostname to use." [ "host"; "name" ] ~docv:"STRING" in
  Key.(create "hostname" Arg.(required string doc))

let apple_testable =
  let doc = Key.Arg.info ~doc:"Configure the server to use with Apple CCS CalDAVtester." [ "apple-testable" ] in
  Key.(create "apple_testable" Arg.(flag doc))

let enable_monitoring =
  let doc = Key.Arg.info
      ~doc:"Enable monitoring (only available for solo5 targets)"
      [ "enable-monitoring" ]
  in
  Key.(create "enable-monitoring" Arg.(flag ~stage:`Configure doc))

let management_stack =
  if_impl
    (Key.value enable_monitoring)
    (generic_stackv4v6 ~group:"management" (netif ~group:"management" "management"))
    net

let name = Key.v hostname

let monitoring =
  let monitor =
    let doc = Key.Arg.info ~doc:"monitor host IP" ["monitor"] in
    Key.(v (create "monitor" Arg.(opt (some ip_address) None doc)))
  in
  let connect _ modname = function
    | [ _ ; _ ; stack ] ->
      Fmt.str "Lwt.return (match %a with\
               | None -> Logs.warn (fun m -> m \"no monitor specified, not outputting statistics\")\
               | Some ip -> %s.create ip ~hostname:%a %s)"
        Key.serialize_call monitor modname
        Key.serialize_call name stack
    | _ -> assert false
  in
  impl
    ~packages:[ package "mirage-monitoring" ]
    ~keys:[ name ; monitor ]
    ~connect "Mirage_monitoring.Make"
    (time @-> pclock @-> stackv4v6 @-> job)

let syslog =
  let syslog =
    let doc = Key.Arg.info ~doc:"syslog host IP" ["syslog"] in
    Key.(v (create "syslog" Arg.(opt (some ip_address) None doc)))
  in
  let connect _ modname = function
    | [ console ; _ ; stack ] ->
      Fmt.str "Lwt.return (match %a with\
               | None -> Logs.warn (fun m -> m \"no syslog specified, dumping on stdout\")\
               | Some ip -> Logs.set_reporter (%s.create %s %s ip ~hostname:%a ()))"
        Key.serialize_call syslog modname console stack
        Key.serialize_call name
    | _ -> assert false
  in
  impl
    ~packages:[ package ~sublibs:["mirage"] ~min:"0.3.0" "logs-syslog" ]
    ~keys:[ name ; syslog ]
    ~connect "Logs_syslog_mirage.Udp"
    (console @-> pclock @-> stackv4v6 @-> job)

let optional_monitoring time pclock stack =
  if_impl (Key.value enable_monitoring)
    (monitoring $ time $ pclock $ stack)
    noop

let optional_syslog console pclock stack =
  if_impl (Key.value enable_monitoring)
    (syslog $ console $ pclock $ stack)
    noop

let main =
  let direct_dependencies = [
    package "uri" ;
    package "caldav" ;
    package ~min:"0.1.3" "icalendar" ;
    package ~min:"0.8.7" "fmt";
    package ~min:"0.0.3" "git-kv"
  ] in
  let keys =
    [ Key.v http_port ; Key.v https_port ; Key.v tls_proxy ;
      Key.v admin_password ; Key.v remote ;
      Key.v tofu ; Key.v hostname ;
      Key.v apple_testable ]
  in
  foreign
    ~packages:direct_dependencies ~keys
    "Unikernel.Main" (random @-> pclock @-> git_client @-> kv_ro @-> http @-> kv_ro @-> job)

let git_client =
  let dns = generic_dns_client net in
  let git = mimic_happy_eyeballs net dns (generic_happy_eyeballs net dns) in
  let tcp = tcpv4v6_of_stackv4v6 net in
  merge_git_clients (git_tcp tcp git)
    (merge_git_clients (git_ssh ~key:ssh_key ~password:ssh_password ~authenticator tcp git)
       (git_http ~authenticator:tls_authenticator tcp git))

let () =
  register "caldav" [
    optional_syslog default_console default_posix_clock management_stack ;
    optional_monitoring default_time default_posix_clock management_stack ;
    main $ default_random $ default_posix_clock $ git_client $ certs $ http_srv $ zap
  ]
