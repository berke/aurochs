#VERSION=1.5.0.11

JAVAHOME=$(shell java-config -O)
LIBNAME=aurochsjava
JAVASRC=fr/aurochs/Attribute.java fr/aurochs/Node.java fr/aurochs/Parser.java fr/aurochs/Token.java fr/aurochs/Tree.java fr/aurochs/Test.java
JAVAJNI=fr.aurochs.Parser
JNIHEADERS=fr_aurochs_Parser.h
OBJS=aurochs_stub.o $(addprefix ../_build/, c/alloc.o c/nog.o c/pack.o c/nog_unpack.o c/parse_tree.o c/peg.o c/pushdown.o c/staloc.o)

JAVA=$(shell java-config -J)
JAVAFLAGS=-cp .

JAVAC=$(shell java-config -c)
JAVAH=$(JAVAHOME)/bin/javah

INCDIRS=$(JAVAHOME)/include $(JAVAHOME)/include/linux ../include
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
	../aurochs_tool.native -root-node calcRoot -target nog $<

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
