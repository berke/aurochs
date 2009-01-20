#VERSION=1.5.0.11
VERSION=1.6.0

LIBNAME=aurochsjava
JAVASRC=fr/aurochs/Attribute.java fr/aurochs/Node.java fr/aurochs/Parser.java fr/aurochs/Token.java fr/aurochs/Tree.java fr/aurochs/Test.java
JAVAJNI=fr.aurochs.Parser
JNIHEADERS=fr_aurochs_Parser.h
OBJS=aurochs_stub.o $(addprefix ../_build/, cnog/cnog.o cpack/pack.o cnog/parse_tree.o cnog/cnog_unpack.o cutil/alloc.o cutil/staloc.o cutil/pushdown.o)

JAVA=/opt/sun-jdk-$(VERSION)/bin/java
JAVAFLAGS=-cp .

JAVAC=/opt/sun-jdk-$(VERSION)/bin/javac
JAVAH=/opt/sun-jdk-$(VERSION)/bin/javah

INCDIRS=/opt/sun-jdk-$(VERSION)/include /opt/sun-jdk-$(VERSION)/include/linux ../include ../cnog ../cutil ../cpack
INCLUDES=$(addprefix -I, $(INCDIRS))

CC=gcc
CFLAGS=$(INCLUDES) -Wall -fPIC -c -std=c99

CXX=g++
CXXFLAGS=$(CFLAGS)

.PHONY: all build clean cac check lib test

all: build java lib

test: all arith.nog
	LD_LIBRARY_PATH=. $(JAVA) -classpath . fr.aurochs.Test arith.nog arith.txt

%.nog: %.peg
	../aurochs_tool.native -root-node calcRoot -target nog -generate $<

build:
	@cd .. && ./build.sh
	
lib: lib$(LIBNAME).so

java: $(JAVASRC)
	$(JAVAC) $(JAVASRC)

$(JNIHEADERS): java
	$(JAVAH) -classpath . -jni $(JAVAJNI)

lib$(LIBNAME).so: $(OBJS)
	$(CC) -shared $(OBJS) -o $@

aurochs_stub.o: aurochs_stub.c fr_aurochs_Parser.h

%.o: %.c
	$(CC) $(CFLAGS) $<

clean:
	rm -f *.class
	rm -f *.o
	rm -f *.so
	rm -f *.h
	rm -f *.nog
	rm -f `find . -name '*.class'`
