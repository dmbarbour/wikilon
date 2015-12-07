# Makefile for Wikilon's Awelon Bytecode Runtime

CC = gcc
W = -W -Wall -Wno-unused-parameter -Wbad-function-cast -Wuninitialized
THREADS = -pthread
OPT = -O2 -g
CFLAGS = $(THREADS) $(OPT) $(W) $(XCFLAGS)

.PHONY: clean lmdb murmur3

clean:
	cd lmdb && rm *.o
	cd murmur3 && rm *.o

lmdb: lmdb/mdb.o lmdb/midl.o
murmur3: murmur3/murmur3.o

lmdb/mdb.o: lmdb/mdb.c lmdb/lmdb.h lmdb/midl.h
	$(CC) $(CFLAGS) -Ilmdb -o $@ -c lmdb/mdb.c 

lmdb/midl.o: lmdb/midl.c lmdb/midl.h
	$(CC) $(CFLAGS) -Ilmdb -o $@ -c lmdb/midl.c

murmur3/murmur3.o: murmur3/murmur3.c murmur3/murmur3.h
	$(CC) $(CFLAGS) -Imurmur3 -o $@ -c murmur3/murmur3.c


