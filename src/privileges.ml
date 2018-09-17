module Xml = Webdav_xml

(* user_privileges_for_resource: user properties and resource properties as input, output is the list of granted privileges *)
let list ~identities aces =
  let aces' = List.map Xml.xml_to_ace aces in
  let aces'' = List.fold_left (fun acc -> function Ok ace -> ace :: acc | Error _ -> acc) [] aces' in (* TODO malformed ace? *)
  let aces''' = List.filter (function
      | `All, _ -> true
      | `Href principal, _ -> List.exists (Uri.equal principal) identities
      | _ -> assert false) aces''
  in
  List.flatten @@ List.map (function `Grant ps -> ps | `Deny _ -> []) (List.map snd aces''')

(* TODO maybe move to own module *)
let is_met ~requirement privileges =
  List.exists (fun privilege -> match requirement, privilege with
  | _, `All -> true
  | `Read, `Read -> true
  | `Read_acl, `Read_acl -> true
  | `Read_current_user_privilege_set, `Read_current_user_privilege_set -> true
  | `Read_current_user_privilege_set, `Read_acl -> true
  | `Write, `Write -> true
  | `Write_content, `Write -> true
  | `Write_properties, `Write -> true
  | `Write_acl, `Write -> true
  | `Bind, `Write -> true
  | `Unbind, `Write -> true
  | `Write_content, `Write_content -> true
  | `Write_properties, `Write_properties -> true
  | `Write_acl, `Write_acl -> true
  | `Bind, `Bind -> true
  | `Unbind, `Unbind -> true
  | _ -> false ) privileges

(* checks privileges for "current-user-privilege-set" (`Read_current_user_privilege_set) and "acl" (`Read_acl) *)
let can_read_prop fqname privileges =
  let requirement = match fqname with
    | ns, "current-user-privilege-set" when ns = Xml.dav_ns -> Some `Read_current_user_privilege_set
    | ns, "acl" when ns = Xml.dav_ns -> Some `Read_acl
    | _ -> None
  in
  match requirement with
  | Some requirement -> is_met ~requirement privileges
  | None -> true

let required verb ~target_exists = match verb with
  | `GET -> `Read, `Target
  | `HEAD -> `Read, `Target
  | `OPTIONS -> `Read, `Target
  | `PUT when target_exists     -> `Write_content, `Target
  | `PUT (* no target exists *) -> `Bind, `Parent
  | `Other "PROPPATCH" -> `Write_properties, `Target
  | `Other "ACL" -> `Write_acl, `Target
  | `Other "PROPFIND" -> `Read, `Target (* plus <D:read-acl> and <D:read-current-user-privilege-set> as needed, see check in Properties.find_many *)
  | `DELETE -> `Unbind, `Parent
  | `Other "MKCOL" -> `Bind, `Parent
  | `Other "MKCALENDAR" -> `Bind, `Parent
  | `Other "REPORT" -> `Read, `Target (* referenced_resources body *)
  | _ -> assert false
  (* | COPY (target exists)            | <D:read>, <D:write-content> and |
     |                                 | <D:write-properties> on target  |
     |                                 | resource                        |
     | COPY (no target exists)         | <D:read>, <D:bind> on target    |
     |                                 | collection                      |
     | MOVE (no target exists)         | <D:unbind> on source collection |
     |                                 | and <D:bind> on target          |
     |                                 | collection                      |
     | MOVE (target exists)            | As above, plus <D:unbind> on    |
     |                                 | the target collection           |
     | LOCK (target exists)            | <D:write-content>               |
     | LOCK (no target exists)         | <D:bind> on parent collection   |
     | UNLOCK                          | <D:unlock>                      |
     | CHECKOUT                        | <D:write-properties>            |
     | CHECKIN                         | <D:write-properties>            |
     | VERSION-CONTROL                 | <D:write-properties>            |
     | MERGE                           | <D:write-content>               |
     | MKWORKSPACE                     | <D:write-content> on parent     |
     |                                 | collection                      |
     | BASELINE-CONTROL                | <D:write-properties> and        |
     |                                 | <D:write-content>               |
     | MKACTIVITY                      | <D:write-content> on parent     |
     |                                 | collection                      | *)

