#!/bin/sh

set -e

BASE=$1

rm -f pegfile.h && ln -s $BASE.h pegfile.h
CFLAGS=${CFLAGS:--O3}

echo ">>> Generating bytecode"
./aurochs.native -generate -target nog $BASE.peg

echo ">>> Generating C code"
./aurochs.native -target c -build-only -generate $BASE.peg
indent $BASE.c

echo ">>> Generating X86-64 code"
./aurochs.native -generate -target amd64 $BASE.peg
mv $BASE.s ${BASE}_asm.s

echo ">>> Compiling and linking"
gcc -Wall -DPEGFILE="$BASE.h" $CFLAGS ${BASE}_asm.s $BASE.c peg_lib.c check.c -o ckasm
