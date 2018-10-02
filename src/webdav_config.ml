type config = {
  principals : string ;
  calendars : string ;
  host : Uri.t ;
  admin_only_acl : Webdav_xml.ace list ;
  trust_on_first_use_mode : bool ;
}
