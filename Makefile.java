DYNOBJ=dyn
BIN=bin
OBJ=obj
LIB=lib
LIBNAME=aurochs
DIR=java
SRC=src

JAVASRC=$(addprefix $(DIR)/, fr/aurochs/Aurochs.java fr/aurochs/Test.java)
JAVAJNI=fr.aurochs.Parser
JNIHEADER=$(DIR)/fr_aurochs_Parser.h
OBJS=$(addprefix $(DYNOBJ)/, aurochs_stub.o cnog.o pack.o parse_tree.o cnog_unpack.o alloc.o staloc.o pushdown.o)

#VERSION=1.5.0.11
VERSION=1.6.0

JAVA=/opt/sun-jdk-$(VERSION)/bin/java
JAVAFLAGS=-cp $(DIR)

JAVAC=/opt/sun-jdk-$(VERSION)/bin/javac
JAVAH=/opt/sun-jdk-$(VERSION)/bin/javah -classpath $(DIR)

INCDIRS=/opt/sun-jdk-$(VERSION)/include /opt/sun-jdk-$(VERSION)/include/linux include $(DIR)
INCLUDES=$(addprefix -I, $(INCDIRS))

CC=gcc
CFLAGS=$(INCLUDES) -Wall -fPIC -c -std=c99

CXX=g++
CXXFLAGS=$(CFLAGS)

.PHONY: all build clean cac lib test java

all: build java lib

test: all $(OBJ)/arith.nog
	LD_LIBRARY_PATH=lib $(JAVA) $(JAVAFLAGS) fr.aurochs.Test obj/arith.nog java/arith.txt

$(OBJ)/%.nog: $(DIR)/%.peg
	$(BIN)/aurochs -target nog -base $(OBJ)/$(basename $(notdir $@)) -generate $<

lib: $(LIB)/lib$(LIBNAME).so

java:
	$(JAVAC) $(JAVASRC)

$(JNIHEADER): java
	$(JAVAH) -jni -o $(JNIHEADER) $(JAVAJNI)

$(LIB)/lib$(LIBNAME).so: $(OBJS)   
	$(CC) -shared $(OBJS) -o $@

$(DYNOBJ)/aurochs_stub.o: $(DIR)/aurochs_stub.c $(DIR)/fr_aurochs_Parser.h
	$(CC) $(CFLAGS) $< -o $@

$(DYNOBJ)/%.o: $(SRC)/%.c
	$(CC) $(CFLAGS) $< -o $@

clean:
	rm -f $(OBJ)/*.nog
	rm -f $(DYNOBJ)/*.o
	rm -f $(LIB)/*.so
	rm -f $(DIR)/fr_aurochs_Parser.h
	rm -f `find $(DIR) -name '*.class'`

cac: clean all check
