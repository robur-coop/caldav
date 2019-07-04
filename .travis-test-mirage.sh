#!/bin/sh

eval `opam config env`

opam install -y mirage

cd mirage
mirage configure && make depend && mirage build && mirage clean &&
mirage configure -t hvt && make depend && mirage build && mirage clean
