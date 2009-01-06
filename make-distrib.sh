#!/bin/sh

set -e

VERSION=$(sed -n -e 's/^.*version *= *(\([0-9]\+,[0-9]\+,[0-9]\+\)) *$/\1/' -e 's/,/./gp' aurochs/version.ml)

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
mkdir -p $TARGET/java
mkdir -p $TARGET/java/fr/aurochs

cp ../Makefile.distrib $TARGET/Makefile
cp ../Makefile.java $TARGET/Makefile.java

cp aurochs/aurochs_tool.native $TARGET/bin/aurochs
cp libaurochs.a $TARGET/lib
cp include/*.h $TARGET/include
cp ../cnog/check.c $TARGET/src
cp aurochs/*.c cnog/*.c cpack/*.c cutil/*.c $TARGET/src
cp ../java/*.c ../java/Makefile $TARGET/java
cp ../java/fr/aurochs/*.java $TARGET/java/fr/aurochs
cp ../java/arith.peg $TARGET/java
cp ../java/arith.txt $TARGET/java

tar czvf $TARGET.tar.gz $TARGET

cd $OLD
cp _build/$TARGET.tar.gz .
