
# Word-Level Caching

When we update a word's definition, this implicitly updates the meaning for all clients of that word in the dictionary. For example, the type of those clients may change (and might become invalid). Staged computations, partial evaluations, optimizations, tests, etc. may need to be recomputed.

Options:

1. Keep track of downstream computations. Somehow mark them invalid when updating the upstream sources. This option adds a lot of complexity, and doesn't work nicely between dictionaries or with historical views of dictionaries or branching dictionaries.

2. Avoid invalidation of old cache entries; instead, shift new content to new cache entries. This could be done with a secure hash, e.g. at the level of definitions. The old content could remain cached, e.g. when browsing historical dictionaries or overlapping with a branching dictionary.

Between these, the second option seems simpler, more robust, more widely usable, and seems *obviously* better. So I'll run with it for now. 

For a secure hash, I have at least two options:

1. alpha-independent secure hash, i.e. names don't matter.
2. alpha-dependent secure hash, i.e. names do matter

An alpha-independent secure hash is possible by removing names, transitively replace each `{%foo}` token with the `{#secureHashOfFoo}`. An alpha-dependent secure hash can be achieved by almost the same but additionally prefixing each word foo's definition with `{&@foo}` before taking the secure hash.

The motivation for alpha-independence is greater reuse of our cache, e.g. across rename operations. The cost of alpha-independence is that the hash cannot be used to cache any name-dependent results, e.g. error reports that might use human-layer words.

I estimate the level of effective reuse for alpha-independence would be marginal. Also, the *worst* case for reuse of a cache based on secure hash (even for alpha-dependent names) seems to be superior to the *best* case for reuse with invalidation-based caching, due to effective reuse with older versions of the dictionary and between dictionaries. 

So I'm going to favor an alpha-dependent secure hash function.

# Dictionary-Level Caching

In addition to caching per-word, I might need to maintain some per-dictionary cached results, especially related to anything that involves a *list* of words - e.g. if I wish to list type errors and so on without recomputing them every time the dictionary is viewed. 

I can probably leverage the VCache address as a value for distinguishing caches at the dictionary level. 

