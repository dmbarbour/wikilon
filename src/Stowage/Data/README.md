# Stowage.Data

This module aims to provide useful data structures above Stowage.

Initial goals include:

* `VRef` and `LVRef` - smart pointers for Stowage data
* `IntMap` - a natural number indexed sparse associative array
* `Trie` - bit-level patricia trie 
* `LSMTrie` - trie enhanced with update buffers?
* finger-tree sequences for vectors or dequeues
* generic finger-trees (user-provided monoid)

I should probably have for each collection type, a "compacting map" and "compacting filter" operation to avoid loading the entire collection into memory.

I'm interested in developing the SeqHash structure, which is effectively a history-independent finger-tree sequence. Eventually. It's low priority. I don't fully grok the details yet.

I'd also like to abstract the key-value store and transactional updates, leveraging the Codec and caching parsed data for lightweight comparisons. It would be convenient if we can model some sort of hierarchical transactions and buffer multiple commits as a single, efficient operation at the .Net layer before anything reaches the underlying database. The "durability" of a commit could be optional, too.

