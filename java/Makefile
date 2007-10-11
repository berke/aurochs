LIBNAME=aurochsjava
JAVASRC=fr/aurochs/Aurochs.java fr/aurochs/Test.java
JAVAJNI=fr.aurochs.Parser
JNIHEADERS=fr_aurochs_Parser.h
OBJS=aurochs_stub.o $(addprefix ../_build/, cnog/cnog.o cpack/pack.o cnog/parse_tree.o cnog/peg_lib.o cnog/cnog_unpack.o cutil/alloc.o cutil/stack.o)

#VERSION=1.5.0.11
VERSION=1.6.0

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
	../aurochs_tool.native -target nog -generate $<

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

check:
	LD_LIBRARY_PATH=. CLASSPATH=. $(JAVA) $(JAVAFLAGS) $(TARGET)

clean:
	rm -f *.class
	rm -f *.o
	rm -f *.so
	rm -f *.h
	rm -f `find . -name '*.class'`

cac: clean all check
