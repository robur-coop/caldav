#!/bin/sh

ocamlbuild -use-ocamlfind -pkg webmachine -pkg lwt -pkg cohttp-lwt-unix -pkg mirage-fs-mem -pkg xmlm -pkg tyxml crud_lwt.native
