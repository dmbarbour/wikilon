# Makefile for Wikilon's Awelon Bytecode Runtime

W := -Wall -Wextra -Wno-unused-parameter
THREADS := -pthread
OPT := -O2 -flto
CFLAGS := $(THREADS) $(OPT) $(W) -fPIC -g -std=gnu11
CC := gcc $(CFLAGS)
RUNTIME_OBJECTS := wikrt.o
INSTALL_PREFIX := /usr/local

.PHONY: all lib clean install install_lib uninstall

all: lib runtests runABC linkAO

lib: libwikrt.so

clean:
	rm -f $(RUNTIME_OBJECTS) $(LINKAO_OBJECTS)
	rm -f libwikrt.so
	rm -f testSuite
	rm -rf testdir

install: install_lib
	#mkdir -p $(INSTALL_PREFIX)/bin     && test -w $(INSTALL_PREFIX)/bin
	#cp ./linkAO $(INSTALL_PREFIX)/bin
	#cp ./runABC $(INSTALL_PREFIX)/bin

install_lib: libwikrt.so wikrt.h
	mkdir -p $(INSTALL_PREFIX)         && test -w $(INSTALL_PREFIX)
	mkdir -p $(INSTALL_PREFIX)/lib     && test -w $(INSTALL_PREFIX)/lib
	mkdir -p $(INSTALL_PREFIX)/include && test -w $(INSTALL_PREFIX)/include
	cp ./libwikrt.so $(INSTALL_PREFIX)/lib
	cp ./wikrt.h $(INSTALL_PREFIX)/include

uninstall:
	test -w $(INSTALL_PREFIX)
	rm -f $(INSTALL_PREFIX)/lib/libwikrt.so
	rm -f $(INSTALL_PREFIX)/include/wikrt.h
	#rm -f $(INSTALL_PREFIX)/bin/linkAO
	#rm -f $(INSTALL_PREFIX)/bin/runABC

runtests: testSuite
	LD_LIBRARY_PATH=. ./testSuite

testSuite: testSuite.c testAO.h wikilon-runtime.h libwikilon-runtime.so 
	$(CC) -o $@ $< -L. -lwikilon-runtime

runABC: runABC.c wikilon-runtime.h libwikilon-runtime.so 
	$(CC) -o $@ $< -L. -lwikilon-runtime

linkAO: $(LINKAO_OBJECTS)
	$(CC) -o $@ $(LINKAO_OBJECTS)

libwikilon-runtime.so: $(RUNTIME_OBJECTS) wikrt.lds
	$(CC) -shared -Wl,--version-script=wikrt.lds,--strip-all,-soname=$@ -o $@ $(RUNTIME_OBJECTS)

# I'll add more files as I need them.
wikrt.o: wikrt.c wikrt.h utf8.h lmdb/lmdb.h wikilon-runtime.h 
	$(CC) -o $@ -c $<

wikrt_db.o: wikrt_db.c wikrt.h futil.h lmdb/lmdb.h
	$(CC) -o $@ -c $<

wikrt_parse.o: wikrt_parse.c wikrt.h wikilon-runtime.h utf8.h
	$(CC) -o $@ -c $<

wikrt_write.o: wikrt_write.c wikrt.h wikilon-runtime.h utf8.h
	$(CC) -o $@ -c $<

wikrt_eval.o: wikrt_eval.c wikrt.h wikilon-runtime.h
	$(CC) -o $@ -c $<

# unicode utilities
utf8.o: utf8.c utf8.h
	$(CC) -o $@ -c $<

# filesystem utilities
futil.o: futil.c futil.h
	$(CC) -o $@ -c $<

# AO utilities
AO/aofile.o: AO/aofile.c AO/aofile.h
	$(CC) -o $@ -c $<

linkAO.o: linkAO.c AO/aofile.h
	$(CC) -o $@ -c $<

# Directly include copy of LMDB key-value database
lmdb/mdb.o: lmdb/mdb.c lmdb/lmdb.h lmdb/midl.h
	$(CC) -Ilmdb -o $@ -c $<

lmdb/midl.o: lmdb/midl.c lmdb/midl.h
	$(CC) -Ilmdb -o $@ -c $<

# Directly include copy of Murmur3 hash
murmur3/murmur3.o: murmur3/murmur3.c murmur3/murmur3.h
	$(CC) -Imurmur3 -o $@ -c $<

# Maybe introduce Keccak or Blake2? E.g. for HMAC on stowage?
# Maybe introduce zstd to compress stowed data and streams

