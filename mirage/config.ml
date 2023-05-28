(* mirage >= 4.10.0 & < 4.12.0 *)
open Mirage

let net = generic_stackv4v6 default_network

(* set ~tls to false to get a plain-http server *)
let http_srv = cohttp_server @@ conduit_direct ~tls:true net

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

let monitoring =
  let monitor = Runtime_arg.(v (monitor None)) in
  let connect _ modname = function
    | [ stack ; monitor ] ->
      code ~pos:__POS__
        "Lwt.return (match %s with\
         | None -> Logs.warn (fun m -> m \"no monitor specified, not outputting statistics\")\
         | Some ip -> %s.create ip ~hostname:(Mirage_runtime.name ()) %s)"
        monitor modname stack
    | _ -> assert false
  in
  impl
    ~packages:[ package ~min:"0.0.6" "mirage-monitoring" ]
    ~runtime_args:[ monitor ]
    ~connect "Mirage_monitoring.Make"
    (stackv4v6 @-> job)

let syslog =
  let syslog = Runtime_arg.(v (syslog None)) in
  let connect _ modname = function
    | [ stack ; syslog ] ->
      code ~pos:__POS__
        "Lwt.return (match %s with\
         | None -> Logs.warn (fun m -> m \"no syslog specified, dumping on stdout\")\
         | Some ip -> Logs.set_reporter (%s.create %s ip ~hostname:(Mirage_runtime.name ()) ()))"
        syslog modname stack
    | _ -> assert false
  in
  impl
    ~packages:[ package ~sublibs:[ "mirage" ] ~min:"0.5.0" "logs-syslog" ]
    ~runtime_args:[ syslog ]
    ~connect "Logs_syslog_mirage.Udp"
    (stackv4v6 @-> job)

let optional_monitoring stack =
  if_impl (Key.value enable_monitoring)
    (monitoring $ stack)
    noop

let optional_syslog stack =
  if_impl (Key.value enable_monitoring)
    (syslog $ stack)
    noop

let main =
  let direct_dependencies =
    [
      package "uri";
      package "caldav";
      package ~min:"0.1.3" "icalendar";
      package ~min:"0.8.7" "fmt";
      package ~min:"0.2.0" "git-kv";
      package ~min:"9.1.0" ~sublibs:["mirage"] "dns-certify";
    ]
  in
  main ~packages:direct_dependencies "Unikernel.Main"
    (stackv4v6 @-> git_client @-> http @-> kv_ro @-> job)

let he = generic_happy_eyeballs net
let dns = generic_dns_client net he

let git_client =
  let git = mimic_happy_eyeballs net he dns in
  let tcp = tcpv4v6_of_stackv4v6 net in
  merge_git_clients (git_tcp tcp git)
    (merge_git_clients (git_ssh tcp git) (git_http tcp git))

let () =
  register "caldav" [
    optional_syslog management_stack ;
    optional_monitoring management_stack ;
    main $ net $ git_client $ http_srv $ zap
  ]
