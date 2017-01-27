
# Wikilon Runtime

This directory contains the C runtime for Wikilon. I might add some command line utilities.

## Installation

Development is currently on Ubuntu 16.04. However, I'm aiming to keep things reasonably portable. Here are the current dependencies and packages used during development where relevant:

* gcc (developed with 5.4.0)
* libpthread
* libc
* make
* Lightning MDB - memory mapped database
 * liblmdb (liblmdb-dev 0.9.17-3)
* BLAKE2b Secure Hash
 * libb2 (libb2-dev 0.97-2)

Actual version constraints aren't known. 

## Outputs

Primary Objects:

* `wikrt.h` - header
* `libwikrt.so` - primary source code

I intend to eventually provide some command-line utilities, too.

* access and update the codebase
* import and export resources 
* evaluate code in dictionary
* persistent REPL sessions

But this isn't a critical area, and could be constructed separately.

## Instructions

Something like the following

        sudo apt-get install liblmdb-dev libb2-dev
        make && sudo make install

Content is installed under `/usr/local`. 

Use of `sudo make uninstall` will remove the installed objects.


