#!/bin/sh

set -x
set -e

VERSION=$(sed -n -e 's/^.*version *= *\([0-9]\+\) *;.*$/\1/p' aurochs/version.ml)

echo "Version is $VERSION"

./build.sh

OLD=$PWD
TARGET=${TARGET:-aurochs-sdk-v$VERSION}

cd _build

mkdir -p $TARGET
mkdir -p $TARGET/bin
mkdir -p $TARGET/obj
mkdir -p $TARGET/src
mkdir -p $TARGET/lib
mkdir -p $TARGET/include

cp ../Makefile.distrib $TARGET/Makefile

cp aurochs/aurochs_tool.native $TARGET/bin/aurochs
cp libaurochs.a $TARGET/lib
cp cnog/*.h cpack/*.h cutil/*.h include/*.h $TARGET/include
cp cnog/*.c cpack/*.c cutil/*.c $TARGET/src
cp ../cnog/check.c $TARGET/src

tar czvf $TARGET.tar.gz $TARGET

cd $OLD
cp _build/$TARGET.tar.gz .
