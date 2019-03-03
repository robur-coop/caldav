open Mirage

let net = generic_stackv4 default_network

(* set ~tls to false to get a plain-http server *)
let http_srv = http_server @@ conduit_direct ~tls:true net

(* TODO: make it possible to enable and disable schemes without providing a port *)
let http_port =
  let doc = Key.Arg.info ~doc:"Listening HTTP port." ["http"] ~docv:"PORT" in
  Key.(create "http_port" Arg.(opt (some int) None doc))

let https_port =
  let doc = Key.Arg.info ~doc:"Listening HTTPS port." ["https"] ~docv:"PORT" in
  Key.(create "https_port" Arg.(opt (some int) None doc))

let certs = generic_kv_ro ~key:Key.(value @@ kv_ro ()) "tls"

let admin_password =
  let doc = Key.Arg.info ~doc:"Password for the administrator." ["admin-password"] ~docv:"STRING" in
  Key.(create "admin_password" Arg.(opt (some string) None doc))

let remote =
  let doc = Key.Arg.info ~doc:"Location of calendar data." [ "remote" ] ~docv:"REMOTE" in
  Key.(create "remote" Arg.(opt string "" doc))

let tofu =
  let doc = Key.Arg.info ~doc:"If a user does not exist, create them and give them a new calendar." [ "tofu" ] in
  Key.(create "tofu" Arg.(flag doc))

let hostname =
  let doc = Key.Arg.info ~doc:"Hostname to use." [ "host" ] ~docv:"STRING" in
  Key.(create "hostname" Arg.(required string doc))

let monitor =
  let doc = Key.Arg.info ~doc:"Hostname to use for monitoring." [ "monitor" ] ~docv:"STRING" in
  Key.(create "monitor" Arg.(opt (some string) None doc))

let apple_testable =
  let doc = Key.Arg.info ~doc:"Configure the server to use with Apple CCS CalDAVtester." [ "apple-testable" ] in
  Key.(create "apple_testable" Arg.(flag doc))

let main =
  let direct_dependencies = [
    package "uri" ;
    package ~pin:"git+https://github.com/roburio/caldav.git#retreat" "caldav" ;
    package ~pin:"git+https://github.com/hannesm/irmin.git#next" "irmin" ;
    package ~pin:"git+https://github.com/hannesm/irmin.git#next" "irmin-mirage" ;
    package ~pin:"git+https://github.com/hannesm/irmin.git#next" "irmin-git" ;
    package ~pin:"git+https://github.com/hannesm/irmin.git#next" "irmin-mem" ;
    package ~pin:"git+https://github.com/linse/ocaml-git.git" "git-http" ;
    package ~pin:"git+https://github.com/linse/ocaml-git.git" "git-mirage" ;
  ] in
  let keys =
    [ Key.abstract http_port ; Key.abstract https_port ;
      Key.abstract admin_password ; Key.abstract remote ;
      Key.abstract tofu ; Key.abstract hostname ;
      Key.abstract monitor ; Key.abstract apple_testable ]
  in
  foreign
    ~packages:direct_dependencies ~keys
    "Unikernel.Main" (random @-> pclock @-> mclock @-> kv_ro @-> http @-> resolver @-> conduit @-> job)

let () =
  register "caldav" [main $ default_random $ default_posix_clock $ default_monotonic_clock $ certs $ http_srv $ resolver_dns net $ conduit_direct ~tls:true net ]
