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
  let doc = Key.Arg.info ~doc:"Hostname to use." [ "host" ] ~docv:"STRING" in
  Key.(create "hostname" Arg.(required string doc))

let apple_testable =
  let doc = Key.Arg.info ~doc:"Configure the server to use with Apple CCS CalDAVtester." [ "apple-testable" ] in
  Key.(create "apple_testable" Arg.(flag doc))

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
  register "caldav" [main $ default_random $ default_posix_clock $ git_client $ certs $ http_srv $ zap ]
