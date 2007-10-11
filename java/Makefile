LIBNAME=aurochsjava
JAVASRC=fr/aurochs/Aurochs.java
JAVAJNI=fr.aurochs.Parser
JNIHEADERS=fr_aurochs_Parser.h
OBJS=aurochs_stub.o

#VERSION=1.5.0.11
VERSION=1.6.0

JAVA=/opt/sun-jdk-$(VERSION)/bin/java
JAVAFLAGS=-cp .

JAVAC=/opt/sun-jdk-$(VERSION)/bin/javac
JAVAH=/opt/sun-jdk-$(VERSION)/bin/javah

INCDIRS=/opt/sun-jdk-$(VERSION)/include /opt/sun-jdk-$(VERSION)/include/linux ../include ../cnog ../cutil ../cpack
INCLUDES=$(addprefix -I, $(INCDIRS))

CC=gcc
CFLAGS=$(INCLUDES) -fPIC -c -std=c99

CXX=g++
CXXFLAGS=$(CFLAGS)

.PHONY: all clean cac check lib

all: lib
	
lib: lib$(LIBNAME).so

$(JNIHEADERS):
	$(JAVAC) $(JAVASRC)
	$(JAVAH) -classpath . -jni $(JAVAJNI)

lib$(LIBNAME).so: $(OBJS)
	$(CC) -shared $(OBJS)

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
