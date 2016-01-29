# Makefile for Wikilon's Awelon Bytecode Runtime

W := -Wall -Wextra -pedantic -Wno-unused-parameter
THREADS := -pthread
OPT := -O2 -flto
CFLAGS := $(THREADS) $(OPT) $(W) -fPIC -g -std=gnu11
CC := gcc $(CFLAGS)
OBJECTS := lmdb/mdb.o lmdb/midl.o murmur3/murmur3.o futil.o wikrt.o
INSTALL_PREFIX := /usr/local

.PHONY: all lib clean install uninstall runtests

all: lib runtests

lib: libwikilon-runtime.so

clean:
	rm -f $(OBJECTS)
	rm -f libwikilon-runtime.so
	rm -f test
	rm -rf testdir

install: libwikilon-runtime.so
	mkdir -p $(INSTALL_PREFIX) && test -w $(INSTALL_PREFIX)
	mkdir -p $(INSTALL_PREFIX)/lib
	mkdir -p $(INSTALL_PREFIX)/include
	cp ./libwikilon-runtime.so $(INSTALL_PREFIX)/lib
	cp ./wikilon-runtime.h $(INSTALL_PREFIX)/include

uninstall:
	test -w $(INSTALL_PREFIX)
	rm -f $(INSTALL_PREFIX)/lib/libwikilon-runtime.so
	rm -f $(INSTALL_PREFIX)/include/wikilon-runtime.h

runtests: testSuite
	LD_LIBRARY_PATH=. ./testSuite

testSuite: wikilon-runtime.h libwikilon-runtime.so testSuite.c
	$(CC) -o $@ testSuite.c -L. -lwikilon-runtime

libwikilon-runtime.so: $(OBJECTS) wikrt.lds
	$(CC) -shared -Wl,--version-script=wikrt.lds,--strip-all,-soname=$@ -o $@ $(OBJECTS)

# I'll add more files as I need them.
wikrt.o: wikrt.c wikrt.h wikilon-runtime.h lmdb/lmdb.h murmur3/murmur3.h 
	$(CC) -o $@ -c wikrt.c

# filesystem utilities
futil.o: futil.c futil.h
	$(CC) -o $@ -c futil.c

# Directly include copy of LMDB key-value database
lmdb/mdb.o: lmdb/mdb.c lmdb/lmdb.h lmdb/midl.h
	$(CC) -Ilmdb -o $@ -c lmdb/mdb.c 

lmdb/midl.o: lmdb/midl.c lmdb/midl.h
	$(CC) -Ilmdb -o $@ -c lmdb/midl.c

# Directly include copy of Murmur3 hash
murmur3/murmur3.o: murmur3/murmur3.c murmur3/murmur3.h
	$(CC) -Imurmur3 -o $@ -c murmur3/murmur3.c

# Maybe introduce Keccak or Blake2? E.g. for HMAC on stowage?
# Maybe introduce zstd to compress stowed data and streams

