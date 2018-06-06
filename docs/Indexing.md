# Indexing Dictionaries

I need to index dictionaries for effective use. 

Which indices are most useful?

* word (or resource) → version
* version → evaluated definition
 * indirectly? version → evaluated version + version → definition
* version → link optimized or compiled versions
* version → inferred type
* Reverse Lookup: symbol → client words 
 * symbols include annotations
 * symbols include secure-hash resources
 * find obviously erroneous (undefined, cyclic, etc.) words
 * potentially find full-text fragments

A challenge for indexing is that Wikilon will have many dictionaries, both for multiple projects/users and histories and hierarchical structure. I assume that dictionaries will be 99% similar, but the differences might be private to a project or user. So we both need to somehow share indices and ensure secure separation thereof. Association of definitions with deep versions based on secure hash should help, since we can securely index from version to many kinds of computed results (like evaluation or type). This may also be treated similarly to memoization! 

For an interactive development environment, I also want the ability to find type-appropriate words within a dictionary. Maintaining a reverse lookup index for type information is non-trivial because reverse-lookup is always specific to each dictionary. But presumably we could just ask developers or software agents to augment the dictionary with some metadata that can be used in the reverse lookup. E.g. for `foo` we could have `foo-meta-kw` that includes ad-hoc key-words or tags to swiftly discover the word `foo`. 

So, I think the primary indices to maintain are for deep-version and a simple reverse-lookup. Fortunately, these two indices can be maintained incrementally. A relevant concern is that a version update for a 'deep' word (e.g. the initial state in a command pattern) could result in a long, cascading chains of version invalidations with high cost. And if we have a lot of updates, we shouldn't recompute all versions after each update. 

This could be ameliorated by making the deep-version index lazier in nature (so we only invalidate once for multiple updates), or otherwise batching our updates.

In most cases, we can assume that the more a word is used, the more stable its definition will be. So eager versioning could also work. Or perhaps we should treat version IDs as a memory cached index, recomputed as needed, rather than a fully durable index.



