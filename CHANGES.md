## v0.2.1 (2022-12-16)

* Use Mirage-kv 6.0.0 API (#32 @hannesm)
* Log more write errors (#32 @hannesm)
* Refine Webdav_fs.batch to return a ('a, [> `Msg of string ]) result io, so
  it can fail (#32 @hannesm)
* unikernel: use git-kv instead of irmin-mirage-git (#32 @hannesm)

## v0.2.0 (2022-10-28)

* unikernel: upgrade to mirage 4.3 (#31 @hannesm)
* bugfix: href_to_principal always returned "/principals" (since v0.1.1)
  (#24 @hannesm)
* error when principal or calendar already exists when adding a new principal
  (#24 @hannesm)
* when modifying an ACL, check that all referenced principals exist
  (#24 @hannesm)
* check for groups and principals on operations thereof (i.e. disallow deleting
  a group when the principal is a user, etc.) and test cases (#25 @hannesm)
* use batch in Webdav_api instead of Webdav_fs to reduce commits
  (#26, fixes in #27 @hannesm)
* bugfix: adding and removing groups and users, only edit prop file once
  (#27 @hannesm)
* bugfix: compute etag after batch (#27 @hannesm)

## v0.1.1 (2021-11-02)

* Drop rresult and astring dependency (#23 @hannesm).

## v0.1.0 (2021-05-06)

* Initial release, with webmachine vendored, and provided as a sublibrary.
