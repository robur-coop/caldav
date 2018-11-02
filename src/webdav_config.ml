type config = {
  principals : string ;
  calendars : string ;
  host : Uri.t ;
  trust_on_first_use_mode : bool ;
}

let admin_acl config = [
  (`Href (Uri.with_path config.host @@ "/" ^ config.principals ^ "/root/"), `Grant [ `All ]) ;
  (`All, `Grant [ `Read ])
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
    ?(trust_on_first_use_mode = false)
    host =
  { principals ;
    calendars ;
    trust_on_first_use_mode ;
    host ;
  }
