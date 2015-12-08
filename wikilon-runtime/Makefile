# Makefile for Wikilon's Awelon Bytecode Runtime

W = -W -Wall -Wextra -Wno-unused-parameter -Wbad-function-cast -Wuninitialized
THREADS = -pthread
OPT = -O2 -flto
CFLAGS = $(THREADS) $(OPT) $(W) -fPIC -g 
CC = gcc $(CFLAGS)
OBJECTS = lmdb/mdb.o lmdb/midl.o murmur3/murmur3.o wikrt.o

.PHONY: all lib clean 

all: lib
lib: libwikilon-runtime.so.1
clean:
	rm -f $(OBJECTS)
	rm -f libwikilon-runtime.so.1

libwikilon-runtime.so.1: $(OBJECTS) wikrt.lds
	$(CC) -shared -Wl,--version-script=wikrt.lds,--strip-all,-soname=$@ -o $@ $(OBJECTS)

# I'll add more files as I need them. Likely:
#  one file for stowage
#  one file for utilities
#  one file for GC
wikrt.o: wikrt.c wikilon-runtime.h lmdb/lmdb.h murmur3/murmur3.h 
	$(CC) -o $@ -c wikrt.c

# Directly include copy of LMDB key-value database
lmdb/mdb.o: lmdb/mdb.c lmdb/lmdb.h lmdb/midl.h
	$(CC) -Ilmdb -o $@ -c lmdb/mdb.c 

lmdb/midl.o: lmdb/midl.c lmdb/midl.h
	$(CC) -Ilmdb -o $@ -c lmdb/midl.c

# Directly include copy of Murmur3 hash
murmur3/murmur3.o: murmur3/murmur3.c murmur3/murmur3.h
	$(CC) -Imurmur3 -o $@ -c murmur3/murmur3.c

