

Design Consideration: 

* Keep a clean Wikilon dictionary, limit error creep. 
* I'll eventually need responsive recompiles
* Renaming should be efficient, if feasible.
* I want to compact memory requirements wherever I can.


## Validation

For the moment, I'm seeking a weak validation criteria:

1. parse
2. acyclic
3. deeply defined 

This is the bare minimum to ensure every AO word compiles into something (even if it's just nonsense). Later, I will validate features like: tests still pass, words still typecheck, etc.

Validating parse is trivial. Validating acyclic is easy: a simplified compilation recompile against the dictionary will do the job. To validate that I don't delete a word that someone else is using, I'll need either a reverse lookup or a reference count.

Validating against missing words is more difficult: I need either a reverse lookup or a reference count. 

Reactively running relevant tests will also benefit from a reverse lookup. And typechecking 

Typechecking the words will essentially require a full recompile.

requires partially compiling a word. Later, 




1. to 
2. Testing whether a word is part of a cycle is not difficult: seek dependencies of the word.


: you can compile the word, for example, to test if it has any dependencies. 

Testing for cycles is not especially difficult. If a word is part of a cycle, this can always be discovered by compiling that word.


that there are no undefined words, seems much more difficult



If we're going to simply validate words as we add them, this should be easy.





One option is to simply compile every word after it is u


Whenever we update a word, we have possibilities of:

* using undefined word
* introducing a cycle

When we delete a word, we have possibility of:

* rendering other words undefined
* 





-- Design Thoughts:
--
--
--   To ensure validity, I need to make sure that each update does
--   not create a cycle, and that deletions do not break code that
--   otherwise works fine.

## Fast Renaming

The common approach to optimizing rename is to use a layer of indirection, e.g. map each word to an integer, and maintain a reverse lookup index. Then renaming a word is easy: simply map the integer to a different string. This technique is certainly feasible, but it adds a fair amount of complexity to parsing and showing code.

Without indirection, the only option is direct manipulation - to directly modify every use of a word. Though, this can be performed lazily. This has advantage of simplicity and disadvantage of performance.

However...

If we have a good reverse lookup index, the cost to modify every use of a word will be greatly mitigated. If it's a very widely used word, the cost will still be high, of course, but I think that will also be mitigated by social pressures and policies.

Presumably, we'll have a good reverse lookup. We need it for other features, such as reactive update or validating that we don't delete a word that's in use by another word. With that assumption, I think I'll go ahead and favor the brute force technique for renaming. 

## Compacting Memory Requirements

There are many possible ways to compact memory requirements. Quick list:

* intern words, replacing with another copy of themselves (DONE)
* favor gzipped, binary representations of transactions in memory...
* use secure-hash key-value store for transactions

Interning can save memory by combining references to common words. I'm not sure how much this will save, but I suspect it's pretty large across a long history and multiple forks. I went ahead and did this via Edward Kmett's `intern` package.

Compressing transactions in-memory could work pretty well. We don't often access the transactions except to build the initial dictionary upon loading, or a historical dictionary, or when performing a compaction. So we could possibly save a lot of memory and reduce burden on GC if we serialize and compress transactions that aren't necessary at any given moment.

We can go a step further, and store transactions externally, and only load them into memory when they're needed. This would have a few advantages, especially when working with 'forks' of wikis. OTOH, it would require explicit handling of garbage-collection to remove old transactions when they're no longer necessary. It's worth thinking about later.

Alternatively, it might be useful to compress a full list of transactions. And the extension of this would be to name lists of transactions via secure hash, though that would still result in GC issues.




