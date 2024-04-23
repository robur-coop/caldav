(* mirage >= 4.5.0 *)
open Mirage

let net = generic_stackv4v6 default_network

(* set ~tls to false to get a plain-http server *)
let http_srv = cohttp_server @@ conduit_direct ~tls:true net

let certs = generic_kv_ro ~key:Key.(value @@ kv_ro ()) "tls"
let zap = generic_kv_ro ~key:Key.(value @@ kv_ro ()) "caldavzap"

let enable_monitoring =
  let doc = Key.Arg.info
      ~doc:"Enable monitoring (only available for solo5 targets)"
      [ "enable-monitoring" ]
  in
  Key.(create "enable-monitoring" Arg.(flag doc))

let management_stack =
  if_impl
    (Key.value enable_monitoring)
    (generic_stackv4v6 ~group:"management" (netif ~group:"management" "management"))
    net

let name = runtime_arg ~pos:__POS__ "Unikernel.hostname"
let monitor = runtime_arg ~pos:__POS__ "Unikernel.monitor"
let syslog = runtime_arg ~pos:__POS__ "Unikernel.syslog"

let monitoring =
  let connect _ modname = function
    | [ _; _; stack; _; _ ] ->
        code ~pos:__POS__
          "Lwt.return (match %a with| None -> Logs.warn (fun m -> m \"no \
           monitor specified, not outputting statistics\")| Some ip -> \
           %s.create ip ~hostname:(%a) %s)"
          Runtime_arg.call monitor modname Runtime_arg.call name stack
    | _ -> assert false
  in
  impl
    ~packages:[ package "mirage-monitoring" ]
    ~runtime_args:[ name; runtime_arg ~pos:__POS__ "Unikernel.monitor" ]
    ~connect "Mirage_monitoring.Make"
    (time @-> pclock @-> stackv4v6 @-> job)

let syslog =
  let connect _ modname = function
    | [ _; stack; _; _ ] ->
        code ~pos:__POS__
          "Lwt.return (match %a with| None -> Logs.warn (fun m -> m \"no \
           syslog specified, dumping on stdout\")| Some ip -> \
           Logs.set_reporter (%s.create %s ip ~hostname:(%a) ()))"
          Runtime_arg.call syslog modname stack Runtime_arg.call name
    | _ -> assert false
  in
  impl
    ~packages:[ package ~sublibs:[ "mirage" ] ~min:"0.4.0" "logs-syslog" ]
    ~runtime_args:[ name; runtime_arg ~pos:__POS__ "Unikernel.syslog" ]
    ~connect "Logs_syslog_mirage.Udp"
    (pclock @-> stackv4v6 @-> job)

let optional_monitoring time pclock stack =
  if_impl (Key.value enable_monitoring)
    (monitoring $ time $ pclock $ stack)
    noop

let optional_syslog pclock stack =
  if_impl (Key.value enable_monitoring)
    (syslog $ pclock $ stack)
    noop

let main =
  let direct_dependencies =
    [
      package "uri";
      package "caldav";
      package ~min:"0.1.3" "icalendar";
      package ~min:"0.8.7" "fmt";
      package ~min:"0.0.3" "git-kv";
    ]
  in
  let runtime_args =
    [
      runtime_arg ~pos:__POS__ "Unikernel.http_port";
      runtime_arg ~pos:__POS__ "Unikernel.https_port";
      runtime_arg ~pos:__POS__ "Unikernel.tls_proxy";
      runtime_arg ~pos:__POS__ "Unikernel.admin_password";
      runtime_arg ~pos:__POS__ "Unikernel.remote";
      runtime_arg ~pos:__POS__ "Unikernel.tofu";
      runtime_arg ~pos:__POS__ "Unikernel.hostname";
      runtime_arg ~pos:__POS__ "Unikernel.apple_testable";
    ]
  in
  main ~packages:direct_dependencies ~runtime_args "Unikernel.Main"
    (random @-> pclock @-> git_client @-> kv_ro @-> http @-> kv_ro @-> job)

let git_client =
  let dns = generic_dns_client net in
  let git = mimic_happy_eyeballs net dns (generic_happy_eyeballs net dns) in
  let tcp = tcpv4v6_of_stackv4v6 net in
  let key = Runtime_arg.create ~pos:__POS__ "Unikernel.ssh_key"
  and password = Runtime_arg.create ~pos:__POS__ "Unikernel.ssh_password"
  and authenticator = Runtime_arg.create ~pos:__POS__ "Unikernel.authenticator"
  and tls_authenticator =
    Runtime_arg.create ~pos:__POS__ "Unikernel.tls_authenticator"
  in
  merge_git_clients (git_tcp tcp git)
    (merge_git_clients
       (git_ssh ~key ~password ~authenticator tcp git)
       (git_http ~authenticator:tls_authenticator tcp git))

let () =
  register "caldav" [
    optional_syslog default_posix_clock management_stack ;
    optional_monitoring default_time default_posix_clock management_stack ;
    main $ default_random $ default_posix_clock $ git_client $ certs $ http_srv $ zap
  ]
