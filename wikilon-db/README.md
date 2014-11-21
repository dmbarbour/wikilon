Wikilon, if successful, will host multitudes of large data objects (music, images, meshes, game worlds, fanfiction, formal arguments, etc.), embedded in an accessible computational context based on the AO language. Wikilon also serves as a software platform, and must keep state for various services. Deep histories of all this data shall remain available for analysis, recovery, retroactive testing, etc..

Since I can't squeeze all of those features into **acid-state** without saturating my poor RAM and thrashing (and because RAM is expensive compared to disk on VPS systems) I'm instead building a database above **Berkeley DB**. This allows memory to be a cache for whatever data is 'hot', while still supporting transactional updates.

Berkeley DB, in this case, is used primarily as a persistent, mem-cached value store. 

This store is *weakly* content-addressed: I allow that a value might have more than one key such that I can use smaller (64 bit) keys. But collisions will be rare, and duplicate values will typically be collapsed. How much this helps is an open question; it shouldn't help much for the dictionary, but it might help a lot for values replicated in Wikilon's 'filesystem'.

The key-value database is modeled as a value above Berkeley DB. Sadly, this is something of an abstraction inversion. But it enables multiple historical snapshots of the database to be tracked and manipulated.

Developers will mostly control the *numbers* of historical samples, both a window to keep some precise history and a logarithmic history. E.g. if we want to keep 2000 samples, we might keep the last 400 precisely, and also keep another 1600 that we collect periodically. Half-life of the logarithmic history then depends on count and update frequency.
