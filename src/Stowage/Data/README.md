# Stowage.Data

This module aims to provide useful data structures above Stowage.

Initial goals include:

* key-value lookup tree (critbit tree) 
* finger-trees as vectors or dequeues

I'm also interested in optimizing for structure sharing vs. efficient insert/delete. Thus, a variant of the key-value lookup based on LSM-trees would be good, and a history-independent sequence such as SeqHash could prove useful. But these are much lower priority.

Additionally, the simple `Rsc` and `Binary` types represent structured binaries within normal memory.

