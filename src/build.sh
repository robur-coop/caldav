#!/bin/sh

ocamlbuild -use-ocamlfind -pkg webmachine -pkg lwt -pkg ptime -pkg ptime.clock.os -pkg cohttp-lwt-unix -pkg mirage-fs-mem -pkg xmlm -pkg tyxml -pkg icalendar -pkg rresult webdav_lwt.native
