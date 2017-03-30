# Makefile for Wikilon's Awelon Bytecode Runtime

W := -Wall -Wextra -Wno-unused-parameter
OPT := -O2 -flto
CFLAGS := $(W) $(OPT) -fPIC -g -std=gnu11
CC := gcc $(CFLAGS)
RUNTIME_OBJECTS := \
  wikrt_reg.o wikrt_parse_check.o         \
  wikrt_db.o wikrt_eph.o wikrt_prelude.o  \
  wikrt.o  \
  futil.o b64.o utf8.o
INSTALL_PREFIX := /usr/local

.PHONY: all lib utils clean install uninstall test

all: lib test utils prelude.ao 

lib: libwikrt.so

clean:
	rm -f $(RUNTIME_OBJECTS)
	rm -f libwikrt.so
	rm -f testSuite
	rm -f wikrt_hash
	rm -rf testdir
	rm -f *~

#todo: command line utilities to manage database, compile code, etc.
utils: wikrt_hash

install: lib utils wikrt.h
	mkdir -p $(INSTALL_PREFIX)         && test -w $(INSTALL_PREFIX)
	mkdir -p $(INSTALL_PREFIX)/lib     && test -w $(INSTALL_PREFIX)/lib
	mkdir -p $(INSTALL_PREFIX)/include && test -w $(INSTALL_PREFIX)/include
	mkdir -p $(INSTALL_PREFIX)/bin     && test -w $(INSTALL_PREFIX)/bin
	cp ./libwikrt.so $(INSTALL_PREFIX)/lib
	cp ./wikrt.h $(INSTALL_PREFIX)/include
	cp ./wikrt_hash $(INSTALL_PREFIX)/bin

uninstall:
	test -w $(INSTALL_PREFIX)
	rm -f $(INSTALL_PREFIX)/lib/libwikrt.so
	rm -f $(INSTALL_PREFIX)/include/wikrt.h
	rm -f $(INSTALL_PREFIX)/bin/wikrt_hash
	#rm -f $(INSTALL_PREFIX)/bin/linkAO
	#rm -f $(INSTALL_PREFIX)/bin/runABC

test: testSuite 
	LD_LIBRARY_PATH=. ./testSuite

prelude.ao: printPrelude
	LD_LIBRARY_PATH=. ./printPrelude > prelude.ao

prelude: printPrelude
	LD_LIBRARY_PATH=. ./printPrelude

printPrelude: printPrelude.c wikrt.h libwikrt.so
	$(CC) -o $@ $< -L. -lwikrt 

testSuite: testSuite.c wikrt.h libwikrt.so 
	$(CC) -o $@ $< -L. -lwikrt 

wikrt_hash: wikrt_hash.c b64.o b64.h 
	$(CC) -o $@ $< b64.o -lb2 

libwikrt.so: $(RUNTIME_OBJECTS) wikrt.lds
	$(CC) -o $@ $(RUNTIME_OBJECTS) -shared \
          -Wl,--version-script=wikrt.lds,--strip-all,-soname=$@,--as-needed \
          -llmdb -lb2 -lrt -pthread 

# I'll add more files as I need them.
wikrt.o: wikrt.c wikrt.h wikrt_private.h b64.h utf8.h
	$(CC) -o $@ -c $<

wikrt_prelude.o: wikrt_prelude.c wikrt.h
	$(CC) -o $@ -c $<

wikrt_reg.o: wikrt_reg.c wikrt_private.h
	$(CC) -o $@ -c $<

wikrt_db.o: wikrt_db.c wikrt_private.h wikrt_eph.h futil.h
	$(CC) -o $@ -c $<

wikrt_eph.o: wikrt_eph.c wikrt_eph.h futil.h
	$(CC) -o $@ -c $<

wikrt_parse_check.o: wikrt_parse_check.c wikrt_private.h wikrt.h 

# filesystem utilities
futil.o: futil.c futil.h
	$(CC) -o $@ -c $<

# base 64 conversions
b64.o: b64.c b64.h
	$(CC) -o $@ -c $<

utf8.o: utf8.c utf8.h
	$(CC) -o $@ -c $<

