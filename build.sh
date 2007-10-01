#!/bin/sh

set -e

ocamlbuild -cflag -unsafe -cflags -w,P aurochs_tool.native $@

cp aurochs_tool.native bin/aurochs
