#!/bin/sh

eval `opam config env`

opam repo add -y future git+https://github.com/roburio/git-ssh-dns-mirage3-repo.git
opam update
opam upgrade -y
opam install -y mirage

cd mirage
mirage configure && make depend && mirage build && mirage clean &&
mirage configure -t hvt && make depend && mirage build && mirage clean
