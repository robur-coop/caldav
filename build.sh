#!/bin/sh

ocamlbuild -use-ocamlfind -I src -pkg alcotest -pkg xmlm -pkg tyxml -pkg rresult test/test_webdav.native

./test_webdav.native
