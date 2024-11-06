(* mirage >= 4.8.0 & < 4.9.0 *)
open Mirage

let net = generic_stackv4v6 default_network

(* set ~tls to false to get a plain-http server *)
let http_srv = cohttp_server @@ conduit_direct ~tls:true net

let certs = generic_kv_ro ~key:Key.(value @@ kv_ro ()) "tls"
let zap = generic_kv_ro ~key:Key.(value @@ kv_ro ()) "caldavzap"

let enable_monitoring =
  let doc = Key.Arg.info
      ~doc:"Enable monitoring (syslog, metrics to influx, log level, statmemprof tracing)"
      [ "enable-monitoring" ]
  in
  Key.(create "enable-monitoring" Arg.(flag doc))

let management_stack =
  if_impl
    (Key.value enable_monitoring)
    (generic_stackv4v6 ~group:"management" (netif ~group:"management" "management"))
    net

let name = runtime_arg ~pos:__POS__ "Unikernel.K.hostname"

let monitoring =
  let monitor = Runtime_arg.(v (monitor None)) in
  let connect _ modname = function
    | [ _ ; _ ; stack ; name ; monitor ] ->
      code ~pos:__POS__
        "Lwt.return (match %s with\
         | None -> Logs.warn (fun m -> m \"no monitor specified, not outputting statistics\")\
         | Some ip -> %s.create ip ~hostname:%s %s)"
        monitor modname name stack
    | _ -> assert false
  in
  impl
    ~packages:[ package "mirage-monitoring" ]
    ~runtime_args:[ name; monitor ]
    ~connect "Mirage_monitoring.Make"
    (time @-> pclock @-> stackv4v6 @-> job)

let syslog =
  let syslog = Runtime_arg.(v (syslog None)) in
  let connect _ modname = function
    | [ _ ; stack ; name ; syslog ] ->
      code ~pos:__POS__
        "Lwt.return (match %s with\
         | None -> Logs.warn (fun m -> m \"no syslog specified, dumping on stdout\")\
         | Some ip -> Logs.set_reporter (%s.create %s ip ~hostname:%s ()))"
        syslog modname stack name
    | _ -> assert false
  in
  impl
    ~packages:[ package ~sublibs:[ "mirage" ] ~min:"0.4.0" "logs-syslog" ]
    ~runtime_args:[ name; syslog ]
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
      package ~min:"0.1.0" "git-kv";
    ]
  in
  main ~packages:direct_dependencies "Unikernel.Main"
    (random @-> pclock @-> git_client @-> kv_ro @-> http @-> kv_ro @-> job)

let he = generic_happy_eyeballs net
let dns = generic_dns_client net he

let git_client =
  let git = mimic_happy_eyeballs net he dns in
  let tcp = tcpv4v6_of_stackv4v6 net in
  merge_git_clients (git_tcp tcp git)
    (merge_git_clients (git_ssh tcp git) (git_http tcp git))

let () =
  register "caldav" [
    optional_syslog default_posix_clock management_stack ;
    optional_monitoring default_time default_posix_clock management_stack ;
    main $ default_random $ default_posix_clock $ git_client $ certs $ http_srv $ zap
  ]
