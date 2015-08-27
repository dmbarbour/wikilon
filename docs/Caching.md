
# Hashing for Caching

We can use a secure hash as an index for pure computations. Compared to tracking updates on dependencies, use of secure hashes is a lot simpler: no cache invalidation, only need to expire old cached content, potential reuse of hashes across multiple versions and branches of a dictionary, etc.. 

Effectively the secure hash may be assumed unique and collision-free. Thus we don't need to provide a complete description of the computation, and may therefore hash rather arbitrary computations. 

## Word Level Secure Hash

At the moment I maintain a secure hash for each word in the dictionary, such that the secure hash is updated on any change to its definition, name, or its dependencies. Including the name allows the hash to be used broadly, e.g. allowing error reports. But a word-level hash is limited to that word, e.g. I cannot cache information about the larger dictionary.

It seems useful that a lot of resources be associated directly with words, e.g. css could be loaded based on a word in the current dictionary (perhaps selected via cookie). 

# Cache Expiration

If I have a cache, I need to expire old information from it. I'll probably want a basic exponential decay model. OTOH, I don't want to do a lot of mutation on a normal access. If a computation is not in the cache, only then should I take action. 

One option might be to maintain two layers of cache: a primary layer that requires no mutation to access, and a secondary layer that can preserve information expired from the primary and may be updated on access. For example, the first layer might be a simple percentage kill (when over capacity), while the second layer selects some random subset for consideration then kills a fraction of that subset (e.g. pick 3, kill 1 with the worst heuristic score for reuse). 

With the two-layer option, access from the second layer results in the object being restored to the first layer, with some metadata maintenance. The first layer is then accessed without mutations. And the second layer can be expired separately from the first. Regularly used objects will bounce between layers.

Both layers could be stored in the same trie, e.g. simply restricting the conditions in which we perform mutations. 

Fair random selection seems difficult to express, but a simple rotation might work easily enough. I might need to add an interface to vcache-trie to ask for all keys before or after a given key. (Also, I could probably optimize the `keys` function.)

# Cached Data Types

While I could attempt to create a 'generic' cache storage model, I expect the API would be difficult to model. That is, I'd somehow need to deal with selecting a cache (a PVar or similar) based on type descriptors, and I'd need to deal with versioning. It seems simpler and more immediately useful to focus on a few specific data types in the cache, e.g. binaries and ABC values.

Binaries seem a useful specialization mostly because ABC doesn't directly support binaries, yet I need binary or text representations for a lot of interactions at the web layer (e.g. for images, icons). 

For ABC values, I would like the ability to model large ad-hoc values, e.g. large game worlds and spreadsheets and similar, without loading them all into memory at once. I could leverage VCache for this purpose, though I still don't want to expose VCache to the WAI layer. For REPL and abstract interpretation and elsewhere, it might be more useful to model and cache *types*, e.g. abstract ABC values with associated constraints. Can I find a simple constraint model sufficient for ABC operation? Maybe, but it seems like research work. 

It might be best to focus on binaries first, as a simple case and one immediately useful for a lot of purposes (e.g. including optimized bytecode). Then I could gradually introduce a few additional types as needed.

# Caching API

I want something simple that works well, at least to start. 

* simple ByteString key, which may use secure hashes
* simple ByteString result for the binary cache
* ability to set-or-compute

After the binary cache is implemented, others will probably follow the same format, though I'll probably need to figure out how to represent values and such in an abstract and machine-dependent way.

# Dictionary-Level Resources

If I want a list of words with type errors or similar, that doesn't seem a very good fit for the above cache model. I'll need to think about how to automatically maintain such lists, either on a per-branch basis (i.e. so the lists aren't held for all historical versions of a dictionary, though they could be recomputed after forking an old version). 

