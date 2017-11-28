# Stowage.Data

This module aims to provide useful data structures above Stowage. It also includes Stowage concepts:

* `Stowage` - remote value storage by secure hash
* `RscHash` - concrete secure hash function 
* `Codec` - interpret binary data as values
* `DB` - durable software transactional memory
* `VRef` - remote value reference
* `LVRef` - VRef with caching, delayed write
* `CVRef` - LVRef but uses memory for small values
* `IntMap` - sparse associative array, indexed by uint64
* `Trie` - tree with binary keys, prefix sharing
* `LSMTrie` - trie with write buffering

TODO:

* finger-tree sequences for vectors, deques
* SeqHash or          

I'm also interested in developing the SeqHash structure, which is roughly a history-independent finger-tree sequence. But that's low priority. I don't fully grok the details yet.

