
When we update a word's definition, this implicitly updates the meaning for all clients of that word in the dictionary. For example, the type of those clients may change (and might become invalid). Staged computations, partial evaluations, optimizations, tests, etc. may need to be recomputed.

Options:

1. Keep track of downstream computations. Somehow mark them invalid when updating the upstream sources. This option adds a lot of complexity, and doesn't work nicely between dictionaries or with historical views of dictionaries or branching dictionaries.

2. Avoid invalidation of old cache entries; instead, shift new content to new cache entries. This could be done with a secure hash, e.g. at the level of definitions. The old content could remain cached, e.g. when browsing historical dictionaries or overlapping with a branching dictionary.

Between these, the second option seems simpler, more robust, more widely usable, and seems *obviously* better. So I'll run with it for now. 
