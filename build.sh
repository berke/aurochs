#!/bin/sh

set -e

ocamlbuild -tags debug -cflag -unsafe -cflags -w,P aurochs.native $@

cp aurochs.native bin/aurochs
