.PHONY: all aurochs clean test_grammar test_arith install lib

OCAML_DIR=$(shell ocamlc -where)
TARGET=$(OCAML_DIR)/aurochs_lib

all: targets

targets:
	@./build.sh aurochs_lib.cma aurochs_lib.cmxa aurochs_tool.native aurochs/test_aurochs.native cnog/check

clean:
	rm -rf _build/

grammar.ml: aurochs grammar.peg
	./aurochs_tool.native -bootstrap -target ml -generate grammar.peg

install: targets
	cp aurochs_tool.native bin/aurochs
	cp aurochs_tool.native ~/bin/aurochs
	mkdir -p $(TARGET)
	cp _build/aurochs_pack.cmi $(TARGET)
	cp _build/{aurochs_lib.{a,cma,cmxa},libaurochs.a,dllaurochs.so} $(TARGET)
