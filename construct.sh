#!/bin/sh

set -e
mkdir _build
cd _build
cp ../*.{ml,mli} .

# Target: aurochs.ml.depends, tags: { extension:ml, file:aurochs.ml, ocaml, ocamldep, quiet, traverse }
ocamldep.opt -modules aurochs.ml > aurochs.ml.depends
# Target: process.ml.depends, tags: { extension:ml, file:process.ml, ocaml, ocamldep, quiet, traverse }
ocamldep.opt -modules process.ml > process.ml.depends
# Target: ritchie.ml.depends, tags: { extension:ml, file:ritchie.ml, ocaml, ocamldep, quiet, traverse }
ocamldep.opt -modules ritchie.ml > ritchie.ml.depends
# Target: seq.ml.depends, tags: { extension:ml, file:seq.ml, ocaml, ocamldep, quiet, traverse }
ocamldep.opt -modules seq.ml > seq.ml.depends
# Target: peg.mli.depends, tags: { extension:mli, file:peg.mli, ocaml, ocamldep, quiet, traverse }
ocamldep.opt -modules peg.mli > peg.mli.depends
# Target: boolean.mli.depends, tags: { extension:mli, file:boolean.mli, ocaml, ocamldep, quiet, traverse }
ocamldep.opt -modules boolean.mli > boolean.mli.depends
# Target: boolean.cmi, tags: { byte, compile, extension:mli, file:boolean.mli, interf, ocaml, quiet, traverse }
ocamlc.opt -c -rectypes -o boolean.cmi boolean.mli
# Target: opt.ml.depends, tags: { extension:ml, file:opt.ml, ocaml, ocamldep, quiet, traverse }
ocamldep.opt -modules opt.ml > opt.ml.depends
# Target: machine.ml.depends, tags: { extension:ml, file:machine.ml, ocaml, ocamldep, quiet, traverse }
ocamldep.opt -modules machine.ml > machine.ml.depends
# Target: seq.cmo, tags: { byte, compile, extension:ml, file:seq.ml, implem, ocaml, quiet, traverse }
ocamlc.opt -c -rectypes -o seq.cmo seq.ml
# Target: canonify.ml.depends, tags: { extension:ml, file:canonify.ml, ocaml, ocamldep, quiet, traverse }
ocamldep.opt -modules canonify.ml > canonify.ml.depends
# Target: peg.cmi, tags: { byte, compile, extension:mli, file:peg.mli, interf, ocaml, quiet, traverse }
ocamlc.opt -c -rectypes -o peg.cmi peg.mli
# Target: opt.cmo, tags: { byte, compile, extension:ml, file:opt.ml, implem, ocaml, quiet, traverse }
ocamlc.opt -c -rectypes -o opt.cmo opt.ml
# Target: machine.cmo, tags: { byte, compile, extension:ml, file:machine.ml, implem, ocaml, quiet, traverse }
ocamlc.opt -c -rectypes -o machine.cmo machine.ml
# Target: canonify.cmo, tags: { byte, compile, extension:ml, file:canonify.ml, implem, ocaml, quiet, traverse }
ocamlc.opt -c -rectypes -o canonify.cmo canonify.ml
# Target: pretty.ml.depends, tags: { extension:ml, file:pretty.ml, ocaml, ocamldep, quiet, traverse }
ocamldep.opt -modules pretty.ml > pretty.ml.depends
# Target: noggie.ml.depends, tags: { extension:ml, file:noggie.ml, ocaml, ocamldep, quiet, traverse }
ocamldep.opt -modules noggie.ml > noggie.ml.depends
# Target: nog.ml.depends, tags: { extension:ml, file:nog.ml, ocaml, ocamldep, quiet, traverse }
ocamldep.opt -modules nog.ml > nog.ml.depends
# Target: util.ml.depends, tags: { extension:ml, file:util.ml, ocaml, ocamldep, quiet, traverse }
ocamldep.opt -modules util.ml > util.ml.depends
# Target: util.cmo, tags: { byte, compile, extension:ml, file:util.ml, implem, ocaml, quiet, traverse }
ocamlc.opt -c -rectypes -o util.cmo util.ml
# Target: nog.cmo, tags: { byte, compile, extension:ml, file:nog.ml, implem, ocaml, quiet, traverse }
ocamlc.opt -c -rectypes -o nog.cmo nog.ml
# Target: grammar.ml.depends, tags: { extension:ml, file:grammar.ml, ocaml, ocamldep, quiet, traverse }
ocamldep.opt -modules grammar.ml > grammar.ml.depends
# Target: exa.ml.depends, tags: { extension:ml, file:exa.ml, ocaml, ocamldep, quiet, traverse }
ocamldep.opt -modules exa.ml > exa.ml.depends
# Target: driver.ml.depends, tags: { extension:ml, file:driver.ml, ocaml, ocamldep, quiet, traverse }
ocamldep.opt -modules driver.ml > driver.ml.depends
# Target: convert_grammar.ml.depends, tags: { extension:ml, file:convert_grammar.ml, ocaml, ocamldep, quiet, traverse }
ocamldep.opt -modules convert_grammar.ml > convert_grammar.ml.depends
# Target: amd64.ml.depends, tags: { extension:ml, file:amd64.ml, ocaml, ocamldep, quiet, traverse }
ocamldep.opt -modules amd64.ml > amd64.ml.depends
# Target: ritchie.cmo, tags: { byte, compile, extension:ml, file:ritchie.ml, implem, ocaml, quiet, traverse }
ocamlc.opt -c -rectypes -o ritchie.cmo ritchie.ml
# Target: pretty.cmo, tags: { byte, compile, extension:ml, file:pretty.ml, implem, ocaml, quiet, traverse }
ocamlc.opt -c -rectypes -o pretty.cmo pretty.ml
# Target: noggie.cmo, tags: { byte, compile, extension:ml, file:noggie.ml, implem, ocaml, quiet, traverse }
ocamlc.opt -c -rectypes -o noggie.cmo noggie.ml
# Target: grammar.cmo, tags: { byte, compile, extension:ml, file:grammar.ml, implem, ocaml, quiet, traverse }
ocamlc.opt -c -rectypes -o grammar.cmo grammar.ml
# Target: exa.cmo, tags: { byte, compile, extension:ml, file:exa.ml, implem, ocaml, quiet, traverse }
ocamlc.opt -c -rectypes -o exa.cmo exa.ml
# Target: driver.cmo, tags: { byte, compile, extension:ml, file:driver.ml, implem, ocaml, quiet, traverse }
ocamlc.opt -c -rectypes -o driver.cmo driver.ml
# Target: convert_grammar.cmo, tags: { byte, compile, extension:ml, file:convert_grammar.ml, implem, ocaml, quiet, traverse }
ocamlc.opt -c -rectypes -o convert_grammar.cmo convert_grammar.ml
# Target: amd64.cmo, tags: { byte, compile, extension:ml, file:amd64.ml, implem, ocaml, quiet, traverse }
ocamlc.opt -c -rectypes -o amd64.cmo amd64.ml
# Target: process.cmo, tags: { byte, compile, extension:ml, file:process.ml, implem, ocaml, quiet, traverse }
ocamlc.opt -c -rectypes -o process.cmo process.ml
# Target: aurochs.cmo, tags: { byte, compile, extension:ml, file:aurochs.ml, implem, ocaml, quiet, traverse }
ocamlc.opt -c -rectypes -o aurochs.cmo aurochs.ml
# Target: peg.ml.depends, tags: { extension:ml, file:peg.ml, ocaml, ocamldep, quiet, traverse }
ocamldep.opt -modules peg.ml > peg.ml.depends
# Target: boolean.ml.depends, tags: { extension:ml, file:boolean.ml, ocaml, ocamldep, quiet, traverse }
ocamldep.opt -modules boolean.ml > boolean.ml.depends
# Target: boolean.cmx, tags: { compile, extension:ml, file:boolean.ml, implem, native, ocaml, quiet, traverse }
ocamlopt.opt -c -rectypes -o boolean.cmx boolean.ml
# Target: seq.cmx, tags: { compile, extension:ml, file:seq.ml, implem, native, ocaml, quiet, traverse }
ocamlopt.opt -c -rectypes -o seq.cmx seq.ml
# Target: peg.cmx, tags: { compile, extension:ml, file:peg.ml, implem, native, ocaml, quiet, traverse }
ocamlopt.opt -c -rectypes -o peg.cmx peg.ml
# Target: opt.cmx, tags: { compile, extension:ml, file:opt.ml, implem, native, ocaml, quiet, traverse }
ocamlopt.opt -c -rectypes -o opt.cmx opt.ml
# Target: machine.cmx, tags: { compile, extension:ml, file:machine.ml, implem, native, ocaml, quiet, traverse }
ocamlopt.opt -c -rectypes -o machine.cmx machine.ml
# Target: canonify.cmx, tags: { compile, extension:ml, file:canonify.ml, implem, native, ocaml, quiet, traverse }
ocamlopt.opt -c -rectypes -o canonify.cmx canonify.ml
# Target: util.cmx, tags: { compile, extension:ml, file:util.ml, implem, native, ocaml, quiet, traverse }
ocamlopt.opt -c -rectypes -o util.cmx util.ml
# Target: nog.cmx, tags: { compile, extension:ml, file:nog.ml, implem, native, ocaml, quiet, traverse }
ocamlopt.opt -c -rectypes -o nog.cmx nog.ml
# Target: ritchie.cmx, tags: { compile, extension:ml, file:ritchie.ml, implem, native, ocaml, quiet, traverse }
ocamlopt.opt -c -rectypes -o ritchie.cmx ritchie.ml
# Target: pretty.cmx, tags: { compile, extension:ml, file:pretty.ml, implem, native, ocaml, quiet, traverse }
ocamlopt.opt -c -rectypes -o pretty.cmx pretty.ml
# Target: noggie.cmx, tags: { compile, extension:ml, file:noggie.ml, implem, native, ocaml, quiet, traverse }
ocamlopt.opt -c -rectypes -o noggie.cmx noggie.ml
# Target: grammar.cmx, tags: { compile, extension:ml, file:grammar.ml, implem, native, ocaml, quiet, traverse }
ocamlopt.opt -c -rectypes -o grammar.cmx grammar.ml
# Target: exa.cmx, tags: { compile, extension:ml, file:exa.ml, implem, native, ocaml, quiet, traverse }
ocamlopt.opt -c -rectypes -o exa.cmx exa.ml
# Target: driver.cmx, tags: { compile, extension:ml, file:driver.ml, implem, native, ocaml, quiet, traverse }
ocamlopt.opt -c -rectypes -o driver.cmx driver.ml
# Target: convert_grammar.cmx, tags: { compile, extension:ml, file:convert_grammar.ml, implem, native, ocaml, quiet, traverse }
ocamlopt.opt -c -rectypes -o convert_grammar.cmx convert_grammar.ml
# Target: amd64.cmx, tags: { compile, extension:ml, file:amd64.ml, implem, native, ocaml, quiet, traverse }
ocamlopt.opt -c -rectypes -o amd64.cmx amd64.ml
# Target: process.cmx, tags: { compile, extension:ml, file:process.ml, implem, native, ocaml, quiet, traverse }
ocamlopt.opt -c -rectypes -o process.cmx process.ml
# Target: aurochs.cmx, tags: { compile, extension:ml, file:aurochs.ml, implem, native, ocaml, quiet, traverse }
ocamlopt.opt -c -rectypes -o aurochs.cmx aurochs.ml
# Target: aurochs.native, tags: { extension:native, file:aurochs.native, link, native, ocaml, program, quiet, traverse }
ocamlopt.opt seq.cmx machine.cmx util.cmx nog.cmx amd64.cmx boolean.cmx peg.cmx canonify.cmx convert_grammar.cmx driver.cmx opt.cmx exa.cmx grammar.cmx noggie.cmx pretty.cmx ritchie.cmx process.cmx aurochs.cmx -o aurochs.native
# Compilation successful.
