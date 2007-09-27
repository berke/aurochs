# Makefile

CC=gcc
CFLAGS=-Wall -fPIC -g -I . -I ..
#CFLAGS=-Wall -fPIC -g -I . -I .. -O3
HEADERS=pack.h

.PHONY: all clean

all:	libpack.so

clean:
	rm -f test_pack *.o *.so

test_pack:
	$(CC) $(CFLAGS) -I. -L. -lpack test_pack.c -o test_pack

libpack.so: pack.o
	$(CC) -shared $(CFLAGS) $^ -o $@

%.o: $(HEADERS)
