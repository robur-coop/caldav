open Mirage

let ssh_key =
  let doc = Key.Arg.info ~doc:"Private ssh key (rsa:<seed> or ed25519:<b64-key>)." ["ssh-key"] in
  Key.(create "ssh-key" Arg.(opt (some string) None doc))

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
  Key.(create "http_port" Arg.(opt int 8080 doc))

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

let monitor =
  let doc = Key.Arg.info ~doc:"monitor host IP" ["monitor"] in
  Key.(create "monitor" Arg.(opt (some ip_address) None doc))

let syslog =
  let doc = Key.Arg.info ~doc:"syslog host IP" ["syslog"] in
  Key.(create "syslog" Arg.(opt (some ip_address) None doc))

let name =
  let doc = Key.Arg.info ~doc:"Name of the unikernel" ["name"] in
  Key.(create "name" Arg.(opt string "calendar.robur.coop" doc))

let apple_testable =
  let doc = Key.Arg.info ~doc:"Configure the server to use with Apple CCS CalDAVtester." [ "apple-testable" ] in
  Key.(create "apple_testable" Arg.(flag doc))

let management_stack = generic_stackv4v6 ~group:"management" (netif ~group:"management" "management")

let main =
  let direct_dependencies = [
    package "uri" ;
    package "caldav" ;
    package ~min:"0.1.3" "icalendar" ;
    package ~min:"0.8.7" "fmt";
    package ~min:"0.0.3" "git-kv";
    package ~min:"0.3.0" ~sublibs:["mirage"] "logs-syslog";
    package "mirage-monitoring";
  ] in
  let keys =
    [ Key.v http_port ;
      Key.v admin_password ; Key.v remote ;
      Key.v tofu ;
      Key.v name ; Key.v syslog ; Key.v monitor ;
      Key.v apple_testable ]
  in
  foreign
    ~packages:direct_dependencies ~keys
    "Unikernel.Main" (console @-> random @-> time @-> pclock @-> git_client @-> http @-> kv_ro @-> stackv4v6 @-> job)

let git_client =
  let dns = generic_dns_client net in
  let git = mimic_happy_eyeballs net dns (generic_happy_eyeballs net dns) in
  let tcp = tcpv4v6_of_stackv4v6 net in
  merge_git_clients (git_tcp tcp git)
    (merge_git_clients (git_ssh ~key:ssh_key ~authenticator tcp git)
       (git_http ~authenticator:tls_authenticator tcp git))

let () =
  register "caldav" [main $ default_console $ default_random $ default_time $ default_posix_clock $ git_client $ http_srv $ zap $ management_stack ]
