The idea of merging, synchronizing, and cherry-picking wikis has been around for some time, but is typically hindered by entanglement of pages and collisions of page names. AO helps address the entanglement issue by requiring acyclic definitions. Collisions might be addressed by adding a unique suffix to words from a separate origin, then indirecting a few cherry-picked words to hide the suffix.

        @define foo foo@remote

Now, it would be pretty trivial to just grab the current value of a word. But I think we can do better if we grab the whole history of the remote wiki, perhaps filtered for whatever words you've cherry-picked and the dependencies for those words.

Merging histories has the advantage of being a streaming model with respect to synchronization - e.g. we can continue to receive transactional updates to the remote wiki, and checkpoint/regenerate the merged dictionary accordingly. Further, keeping histories can help us recover information about words with the same origin, and hide origins for non-conflicting words.

I haven't yet hammered out the details of these ideas. But I think the way forward with forking or inheriting from other wikis will be based on such merges.

