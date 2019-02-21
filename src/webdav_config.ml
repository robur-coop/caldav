type config = {
  principals : string ;
  calendars : string ;
  host : Uri.t ;
  do_trust_on_first_use : bool ;
}

let admin_acl config = [
  (`Href (Uri.of_string @@ "/" ^ config.principals ^ "/root/"), `Grant [ `All ]) ;
  (`All, `Grant [ `Read ])
]

let calendars_acl config = [
  (`Href (Uri.of_string @@ "/" ^ config.principals ^ "/root/"), `Grant [ `All ]) ;
  (`All, `Grant [ `Read ; `Bind ; `Unbind ])
]

let host ?(scheme = "http") ?(port = 8080) ?(hostname = "127.0.0.1") () =
  (* strip port if default for scheme, this should be handled by the Uri library *)
  let port = match scheme, port with
    | "http", 80 -> None
    | "https", 443 -> None
    | _ -> Some port
  in
  Uri.make ?port ~scheme ~host:hostname ()

let config
    ?(principals = "principals")
    ?(calendars = "calendars")
    ?(do_trust_on_first_use = false)
    host =
  { principals ;
    calendars ;
    do_trust_on_first_use ;
    host ;
  }
