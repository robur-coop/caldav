#!/bin/sh

ocamlbuild -use-ocamlfind -I src -pkg alcotest -pkg xmlm -pkg tyxml -pkg rresult -pkg ptime -pkg lwt -pkg mirage-fs-mem -pkg lwt.unix -pkg cohttp -pkg ptime.clock.os test/test_webdav.native

./test_webdav.native
