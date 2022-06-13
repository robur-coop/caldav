## unreleased

* bugfix: href_to_principal always returned "/principals" (since v0.1.1)
  (#24 @hannesm)
* error when principal or calendar already exists when adding a new principal
  (#24 @hannesm)
* when modifying an ACL, check that all referenced principals exist
  (#24 @hannesm)

## v0.1.1 (2021-11-02)

* Drop rresult and astring dependency (#23 @hannesm).

## v0.1.0 (2021-05-06)

* Initial release, with webmachine vendored, and provided as a sublibrary.
