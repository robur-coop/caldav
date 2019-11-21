#!/bin/sh

eval `opam config env`

opam update
opam upgrade -y
opam install -y mirage

cd mirage
mirage configure && make depend && mirage build && mirage clean &&
mirage configure -t hvt && make depend && mirage build && mirage clean
