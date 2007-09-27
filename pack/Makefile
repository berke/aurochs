.PHONY: all lib install

OCAML_DIR=$(shell ocamlc -where)
TARGET=$(OCAML_DIR)/pack_lib
BUILD=_build

all: lib

lib:
	ocamlbuild.native pack_lib.cma pack_lib.cmxa

install: lib
	mkdir -p $(TARGET)
	cp $(BUILD)/{pack_lib.cma,pack_lib.cmxa,pack.cmi} $(TARGET)
