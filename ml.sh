#!/bin/sh

exec ledit \
  -h .history \
  -x \
  ocaml -I __build -rectypes \
  boolean.cmo \
  canonify.cmo \
  peg.cmo \
  convert_grammar.cmo \
  driver.cmo \
  grammar.cmo \
  pretty.cmo \
  seq.cmo \
  opt.cmo \
  machine.cmo \
  nog.cmo \
  ritchie.cmo \
  process.cmo
