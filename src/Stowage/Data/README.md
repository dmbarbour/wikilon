# Stowage.Data

This module aims to provide several useful data structures above Stowage. These structures are generally tree-based, persistent structures that permit a high level of structure sharing. Due to use of secure hashes as references, this structure sharing and serializability can work efficiently even over a network. And Stowage enables these data structures to be larger than memory, full multi-gigabyte databases.

Which data structures am I interested in supporting?

* Critbit Tree (or Trie) - a history-independent key-value mapping structure with logarithmic update and efficient union. 

* finger trees - these are excellent for large scale lists, vectors, stacks, queues, ropes. Finger-trees have O(1) access and update to either end, O(lg(K)) access to the Kth (or N-Kth) element, and O(lg(N+M)) concatenation. 

* log-structured merge tree - a history-dependent key-value lookup structure, potentially more efficient for insert/delete when compared to a critbit tree because it doesn't require deep rewrites. 

* SeqHash - a history-independent, rope-like sequence. I don't fully grasp the details of SeqHash yet, but the fact that it's history-independent is useful for structure sharing when compared to a finger-tree, though undoubtedly has some performance overhead.

Of these, the most critical are the critbit tree and the finger tree structures. And possibly a variant finger-tree binary structure for modeling a very large binary value.

## Deep Structured Data

To support structured references to Stowage from Memory, I'll simply use untyped VRefs. I've experimented with development of a VRef<'T> type with a codec. But it seems simpler to separate the interpretation. The VRef type supports both singular RscHash values and potential inlining of structured data.



