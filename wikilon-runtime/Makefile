# Makefile for Wikilon's Awelon Bytecode Runtime

W = -W -Wall -Wno-unused-parameter -Wbad-function-cast -Wuninitialized
THREADS = -pthread
OPT = -O2 -flto
CFLAGS = $(THREADS) $(OPT) $(W) -fPIC -g 
CC = gcc $(CFLAGS)
OBJECTS = lmdb/mdb.o lmdb/midl.o murmur3/murmur3.o wikrt.o

.PHONY: clean lmdb murmur3 wikrt all lib

all: lib
clean:
	rm -f $(OBJECTS)
	rm -f libwikrt.so

lib: libwikrt.so
lmdb: lmdb/mdb.o lmdb/midl.o
murmur3: murmur3/murmur3.o
wikrt: wikrt.o

libwikrt.so: $(OBJECTS) wikrt.lds
	$(CC) -shared -Wl,--version-script=wikrt.lds,--strip-all -o $@ $(OBJECTS)

wikrt.o: wikrt.c wikrt.h lmdb/lmdb.h murmur3/murmur3.h 
	$(CC) -o $@ -c wikrt.c

lmdb/mdb.o: lmdb/mdb.c lmdb/lmdb.h lmdb/midl.h
	$(CC) -Ilmdb -o $@ -c lmdb/mdb.c 

lmdb/midl.o: lmdb/midl.c lmdb/midl.h
	$(CC) -Ilmdb -o $@ -c lmdb/midl.c

murmur3/murmur3.o: murmur3/murmur3.c murmur3/murmur3.h
	$(CC) -Imurmur3 -o $@ -c murmur3/murmur3.c

