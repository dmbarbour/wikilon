
When we update a word's definition, this implicitly updates the meaning for all clients of that word in the dictionary. For example, the type of those clients may change (and might become invalid). Staged computations, partial evaluations, optimizations, tests, etc. may need to be recomputed.

One difficulty is keeping track of all this. How do know what to update? What to recompute? Especially across multiple versions of a dictionary that are mostly the same?

One viable option is to model workspaces, something similar to a working directory in a filesystem, where we keep a lot of cached versions. When we update the dictionary in the workspace, we can compute which other objects must be invalidated. However, this introduces a lot of complexity because we need to carefully track what was updated.

Another viable option is to keep some small version information, e.g. a modified time or secure hash, for every word in a dictionary. This version information becomes a handle for indexing a cache. This allows us to separate the concern of cache invalidation, but it does require a more sophisticated dictionary model.

The secure hash option, in particular, seems especially valuable: it would allow reuse of a shared cache across multiple branches and versions of a dictionary. In the case where a lot of dictionary structure is shared, we'll gain a lot of benefits here compared to separate workspaces. And in cases where dictionaries diverge significantly, we really won't lose much compared to separate workspaces.

How should we go about computing a secure hash?



Naively, I could directly convert AO definitions to ABC resources, transitively converting every `{%bar}` to a `{#secureHash}`. An update to any word will propagate to update client hashes). This would have the property of being alpha independent, i.e. any definition with the same structure would return the same hash. However, an alpha-independent hash might hinder caching for views that depend on words, e.g. claw-code views.

To make the hash more widely usable, we could implicitly prefix the definiton of bar with an annotation `{&@bar}`. This sort of implicit annotation might be useful more generally, e.g. naming a 'hole' in context of an incomplete definition, and for debugging and presentation purposes. 

