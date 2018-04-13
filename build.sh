#!/bin/sh

ocamlbuild -use-ocamlfind -I src -pkg alcotest -pkg xmlm -pkg tyxml test/test_webdav.native

./test_webdav.native
