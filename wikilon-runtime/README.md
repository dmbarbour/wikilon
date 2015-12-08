
# Wikilon Runtime

This directory provides header `wikilon-runtime.h` and constructs shared object `libwikilon-runtime.so.1`, both of which are necessary for the `wikilon-model` haskell package. This shared object enables ad-hoc construction of multiple runtime contexts, including large value stowage and persistence. Relevant documentation is included in the header.

External dependencies include:

* gcc
* libpthread
* libc
* make

Version constraints aren't known. The gcc used during development is 4.8.4. 
