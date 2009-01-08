.PHONY: all aurochs clean test_grammar test_arith install lib distrib tgz

VERSION=$(shell sed -n -e 's:^let version = (\([0-9]*\),\([0-9]*\),\([0-9]*\))*$$:\1.\2.\3:p' aurochs/version.ml)
PREFIX?=/usr/local
OCAML_DIR=$(shell ocamlc -where)
TARGET=$(OCAML_DIR)/aurochs_lib
DISTRIB=_build/aurochs-$(VERSION)

all: targets

targets:
	@./build.sh aurochs_lib.cma aurochs_lib.cmxa aurochs_tool.native aurochs/test_aurochs.native cnog/check

clean:
	rm -rf _build/

grammar.ml: aurochs grammar.peg
	./aurochs_tool.native -bootstrap -target ml -generate grammar.peg

install: targets
	install -m 0755 aurochs_tool.native $(PREFIX)/bin/aurochs
	mkdir -p $(TARGET)
	install -m 0644 \
	  _build/aurochs_pack.cmi \
	  _build/aurochs_lib.a \
	  _build/aurochs_lib.cma \
	  _build/aurochs_lib.cmxa \
	  _build/libaurochs.a \
	  _build/dllaurochs.so \
	  $(TARGET)

tgz:
	hg archive -t tgz ~/pub/aurochs.tar.gz
