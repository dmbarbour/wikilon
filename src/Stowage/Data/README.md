# Stowage.Data

This module aims to provide useful data structures above Stowage.

Initial goals include:

* critbit based key-value lookup tree 
* LSM-trees for write-heavy updates
* finger-tree sequences for vectors or dequeues
* generic finger-trees (user-provided monoid)
* 

I'm also interested in the tradeoff between structure sharing vs. efficient insert/delete. Thus, implementing a key-value tree variant such as the LSM-tree (at least the two-level variant) seems wise. 

Thus, a variant of the key-value lookup tree based on LSM-trees would be good, and a history-independent sequence such as SeqHash could prove useful. But these are much lower priority.

Additionally, the simple `Rsc` and `Binary` types represent structured binaries within normal memory.

