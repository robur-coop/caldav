type config = {
  principals : string ;
  calendars : string ;
  host : Uri.t ;
  user_password : (string * string) list ;
  default_acl : Webdav_xml.ace list ;
}
