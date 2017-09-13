# Stowage.Data

This module aims to provide useful data structures above Stowage.

Initial goals include:

* smart pointers for Stowage data
 * `VRef` and `LVRef`
* critbit based key-value lookup tree 
 * `CBTree`
* LSM-trees for write-heavy updates
 * `LSMTree` - albeit, buffers only insertions
* finger-tree sequences for vectors or dequeues
* generic finger-trees (user-provided monoid)

I'm somewhat interested in the SeqHash structure, which is a bit like a history-independent finger-tree sequence. But it seems more complicated than I'm willing to deal with at this time.

