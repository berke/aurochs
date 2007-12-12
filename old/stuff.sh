#!/bin/sh

set -e

BASE=$1

rm -f pegfile.h && ln -s $BASE.h pegfile.h
CFLAGS=${CFLAGS:--O3}
./aurochs.native -function-prefix foobar_ -target c -generate -dump-canonified $BASE.cano $BASE.peg </dev/null
indent $BASE.c
gcc -Wall -DPEGFILE="$BASE.h" $CFLAGS $BASE.c peg_lib.c check.c -o ck
