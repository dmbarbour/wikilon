# Caching in Wikilon Runtime

For my [application model](ApplicationModel.md), I'm going to need effective caching. 

For a lot of interesting use cases (evolutionary programming, lightweight forking and merging of codebases, basic refactoring), it's also valuable if the cache isn't strongly tied to any particular codebase, but rather can be shared by many.

Ideally, I can get the same hash for every word in a dictionary regardless of how those words are factored into smaller words. Minimally, this requires an *associative* hashing model.

I should also strip trivial 'comments' from code, i.e. anything of form `"message\n~%` or `[foobar]%`, as part of linking. Though obviously clients could use annotations to preserve comments (something like `"message\n~{&rem}%`). It would also be convenient if hashes were preserved across basic simplifications and optimizations, but it certainly isn't critical.

The work on [SeqHash and VerSum](https://people.csail.mit.edu/nickolai/papers/vandenhooff-versum.pdf) by MIT might be a good starting place. And there has been some similar work on [SplitHash](http://lambda-the-ultimate.org/node/5302). I'll need to look into both.

SeqHash has a O(lg(N)) *join* algorithm. SplitHash has that plus a O(lg(N)) *split* algorithm. The latter might be convenient for recomputing a hash, though is probably not essential for anything I'm interested in doing (since I'm happy to just recompute a join). 

How does it work?




In any case, assuming these hashes are associative, via SeqHash or something like it I can presumably compute hashes for every 'word' in the dictionary, e.g. based on its total expansion.



