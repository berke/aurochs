.PHONY: all aurochs clean test_grammar test_arith install lib

OCAML_DIR=$(shell ocamlc -where)
TARGET=$(OCAML_DIR)/aurochs_lib


all: aurochs

aurochs:
	@./build.sh

clean:
	rm __build/*

grammar.ml: aurochs grammar.peg
	./aurochs.native -bootstrap -target ml -generate grammar.peg

test_grammar:
	ocamlbuild.native test_grammar.native

test_arith: aurochs
	./aurochs.native -target ml -generate arith.peg
	ocamlbuild.native test_arith.native

lib:
	./build.sh aurochs_lib.cmxa aurochs_lib.cma

install: aurochs lib
	cp aurochs.native bin/aurochs
	cp aurochs.native ~/bin/aurochs
	cp __build/*.{cmxa,cma,a,cmi} $(TARGET)
	rm $(TARGET)/process.cmi
