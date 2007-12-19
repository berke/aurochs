#!/bin/sh

set -e

ocamlbuild -cflags -w,P aurochs_tool.native $@
