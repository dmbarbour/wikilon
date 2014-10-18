

# Design Criteria

* Keep a clean Wikilon dictionary, limit error creep. 
* I'll eventually need responsive recompiles
* Renaming should be efficient, if feasible.
* I want to minimize memory requirements.

From a little analysis (below), I'll certainly want both a primary index (word → code) and a reverse lookup index (word → words). The first is trivial. The second is also trivial... but what are the space requirements? Well, conceptually, a word may be used by every other word in the dictionary, but we're already paying for those entries once at each use. So the worst case is O(N) with the dictionary size. Not bad.

So, a straightforward implementation of the indices may be sufficient in this case.

I'll also, eventually need to support fuzzy word search. However, since I went ahead with interning words, I'll certainly want to push fuzzy search into a different data structure so I don't end up interning a bunch of meaningless words. An interesting possibility is to concatenate all words into one large bytestring, then perform a suffix sort on it.

## Validation

For the moment, I'm seeking a weak validation criteria:

1. parse
2. acyclic
3. deeply defined 

This is the bare minimum to ensure every AO word compiles into something (even if it's just nonsense). Later, I will validate features like: tests still pass, words still typecheck, etc.

Validating parse is trivial. Validating acyclic is easy: a simplified compilation recompile against the dictionary will do the job. To validate that I don't delete a word that someone else is using, I'll need either a reverse lookup or a reference count. Later, to determine which tests must be rerun and which programs must be re-typechecked, I'll certainly need a reverse lookup.

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

Interning can save memory by combining references to common words. I'm not sure how much this will save, but I suspect it's pretty large after accounting for a long history, multiple forks, and inverted indices. I went ahead and did this via Edward Kmett's `intern` package.

Compressing transactions in-memory could work pretty well. We don't often access the transactions except to build the initial dictionary upon loading, or a historical dictionary, or when performing a compaction. So we could possibly save a lot of memory and reduce burden on GC if we serialize and compress transactions that aren't necessary at any given moment.

We can go a step further, and store transactions externally, and only load them into memory when they're needed. This would have a few advantages, especially when working with 'forks' of wikis. OTOH, it would require explicit handling of garbage-collection to remove old transactions when they're no longer necessary. It's worth thinking about later.

Alternatively, it might be useful to compress a full list of transactions. And the extension of this would be to name lists of transactions via secure hash, though that would still result in GC issues.




