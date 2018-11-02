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
