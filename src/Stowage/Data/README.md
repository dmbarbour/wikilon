# Stowage.Data

This module aims to provide useful data structures above Stowage.

Initial goals include:

* `VRef` and `LVRef` - smart pointers for Stowage data
* `IntMap` - a natural number indexed sparse associative array
* `Trie` - bit-level patricia trie 
* `LSMTrie` - trie enhanced with update buffers?
* finger-tree sequences for vectors or dequeues
* generic finger-trees (user-provided monoid)

Mapping a function across larger-than-memory structures seems problematic, but I could try to model 'compacting map' operations that also receive the codec for the new value type.

I'm somewhat interested in the SeqHash structure, which is a bit like a history-independent finger-tree sequence. But it seems more complicated than I'm willing to deal with at this time.

I'd like to perhaps develop a variation of Stowage `TX` that is generic, e.g. via use of `Codec` and delayed writes, and reads via Codec. But this isn't critical for any use case right now. 

