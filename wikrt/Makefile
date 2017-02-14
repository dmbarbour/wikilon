# Makefile for Wikilon's Awelon Bytecode Runtime

W := -Wall -Wextra -Wno-unused-parameter
THREADS := -pthread
OPT := -O2 -flto
CFLAGS := $(THREADS) $(OPT) $(W) -fPIC -g -std=gnu11
CC := gcc $(CFLAGS)
RUNTIME_OBJECTS := wikrt.o futil.o b64.o 
INSTALL_PREFIX := /usr/local

.PHONY: all lib utils clean install uninstall

all: lib utils test

lib: libwikrt.so

clean:
	rm -f $(RUNTIME_OBJECTS) $(LINKAO_OBJECTS)
	rm -f libwikrt.so
	rm -f testSuite
	rm -rf testdir

#todo: command line utilities to manage database, compile code, etc.
utils:

install: libwikrt.so wikrt.h
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

test: testSuite
	LD_LIBRARY_PATH=. ./testSuite

testSuite: testSuite.c wikrt.h libwikrt.so 
	$(CC) -o $@ $< -L. -lwikrt

libwikrt.so: $(RUNTIME_OBJECTS) wikrt.lds
	$(CC) -o $@ -shared -llmdb -lb2 -pthread \
          -Wl,--version-script=wikrt.lds,--strip-all,-soname=$@ \
          $(RUNTIME_OBJECTS)

# I'll add more files as I need them.
wikrt.o: wikrt.c wikrt.h wikrt_private.h b64.h
	$(CC) -o $@ -c $<

# filesystem utilities
futil.o: futil.c futil.h
	$(CC) -o $@ -c $<

# base 64 conversions
b64.o: b64.c b64.h
	$(CC) -o $@ -c $<


