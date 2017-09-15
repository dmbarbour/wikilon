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

I'd like to perhaps develop a variation of Stowage `TX` that is generic, e.g. via use of `Codec` and delayed writes, and reads via Codec. But this isn't critical for any use case right now. 

Another interesting possibility is stowage-sensitive compression, e.g. we could compress a binary without compressing the recognizable secure hashes. But this isn't a critical feature at this time, and isn't all that useful unless our stowage objects are much larger than they probably should be. (It would probably be better to compress the whole database or underlying filesystem.)

