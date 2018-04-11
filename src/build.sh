#!/bin/sh

ocamlbuild -use-ocamlfind -pkg webmachine -pkg lwt -pkg cohttp-lwt-unix -pkg mirage-fs-mem -pkg xmlm crud_lwt.native
