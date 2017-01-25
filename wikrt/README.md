
# Wikilon Runtime

This directory contains the C runtime for Wikilon. I might add some command line utilities.

## Installation

External dependencies:

* gcc
* libpthread
* libc
* make
* liblmdb (liblmdb-dev on my Ubuntu 16.04 install)

We could probably make this work with Clang, but it isn't a priority for me at the moment.

## Outputs

Primary Objects:

* `wikrt.h` - header
* `libwikrt.so` - primary source code

I might later add some command line utilities to:

* access and update the codebase
* import and export resources 
* evaluate code in dictionary
* persistent REPL sessions

But this isn't a critical area, and could be constructed separately.

## Instructions

Something like the following

        sudo apt-get install liblmdb-dev
        make && sudo make install

Content is installed under `/usr/local`. And `sudo make uninstall` will remove the installed objects.

External dependencies include:

* gcc
* libpthread
* libc
* make

Version constraints aren't known. The gcc used during development is 5.4.0


 
