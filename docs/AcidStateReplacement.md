The **acid-state** package won't scale to my needs.

Usually, I'm not very confident about such performance assertions. I don't doubt acid-state will scale pretty well up to a few hundred megabytes, though GC costs would begin to scale non-linearly (which is a problem). We can do a lot with a few hundred megabytes. But...

AO isn't like most other languages. It's intended for use in a fashion similar to Wolfram language, i.e. having tons of data readily available, and building interesting data-things into the dictionary for reuse elsewhere. Further, we'll have a great many versions of our code and data, multiplying storage costs by a few orders of magnitude (modulo excellent, multi-layered compression tactics). I can easily afford big storage costs, so long as they're outside my process memory and outside purview of the normal garbage collector. One hundred gigabytes? No problem, just get an SSD or buy a little space in the cloud. 

Speaking of cloud, I plan to eventually run Wikilon in a cloud. That means designing for it.

It will be very important to treat memory as a limited resource, a cache.

Acid-state also has a bottleneck. All writes are serialized. But I feel this is much less an issue. It might become an issue if acid-state is processing lots of high-rate, real-time input streams (web cams, etc.), but even then it is likely to be an RDP signal in which case we can easily batch all available updates and process them together.

While not strictly necessary, I would like to support asynchronous computations via annotation. This suggests support for hierarchical transactions and MonadFork may be necessary.

So, the main issue is pushing 'cold' data onto the disk. And doing so without abandoning ACID properties of acid-state. ACID is awesome.

What is available to me on this end? To be investigated:

* **Perdure** (needs work)
 * Roughly, IORefs persisted within a file. 
 * Replication + Hash verification for writes.
 * Many unnecessary internal-use-only typeclasses.
 * No transactions. Create variation with TVars?
 * Naive allocation strategy; limit 100MB. Fixable?
* **TCache** (hmm.)
 * STM transactions with external, abstract storage.
 * DBRefs can reference other DBRefs (nice!)
 * Includes adapter to AWS. User programmable backends.
 * Seems to integrate with local STM, too.
 * Doubts about robustness. Needs code review.
* **TX** (needs work)
 * Lacks checkpointing. 
 * Doesn't do anything to keep cold data on disk.
 * Nice interface. Might be adaptable.
* existing databases and key-value stores
 * **berkeley db** - key-value store; ACID; manages its own cache
* write my own if necessary

Of the native Haskell solutions, **TCache** seems okay. Its 'IResource' model seems pretty bad, since it doesn't offer a clean separation of resource type from storage strategy. It isn't very clear to me that it will be fast or scalable, and I have doubts about its quality. OTOH, it does offer a backend for Amazon S3 storage. It does offer a backend for Amazon S3 storage, which seems a sweet feature useful for my long term goals of running on a cloud.

The **berkeley db** option is undoubtedly robust, fast, and scalable. Has scaled to hundreds of terabytes. Has a `DB_TXN_WRITE_NOSYNC` option that might be useful for statistics or perma-cache. It would require a persistent host (VPS? rackspace, atlantic, lots of offers for $20/month or so) instead of a cloud instance. Has two different bindings in Haskell.

At the moment, I'm leaning in favor of BDB. 

(Thought: Might be useful to combine BDB+STM.)

# History and Cold Storage

Assuming I have transparently memcached, transactional, persistent variables...

How should I represent history? 

The idea with history is that we have some timeline of updates to our resources, such that we may later examine historical states for our data. Potentially, we may support branching time, i.e. the ability to begin operating on a past state (or, generally, to model branches). That's a useful idea, e.g. for debugging at historical times. In any case, we should certainly be able to study the past states. I also want to use logarithmic history, which is mostly a strategy for forgetting. This might be implemented a number of ways: ring buffers, probabilistic merge, or periodic decimation.

My goal, of course, is to avoid loading multiple versions for large objects into memory (i.e. temporal index) and to avoid loading lots of unnecessary values into memory (i.e. spatial index). It might also be nice to automatically intern frequently used structures (i.e. content addressing), but that would require I track shared usage or occasionally perform a GC.


TODO: Design it.

I'll start with the dictionary. 

* I shouldn't need to load the entire dictionary into memory. 
* I shouldn't need to load entire history for a word into memory.
* I should support multiple 'views' of dictionary on temporal axis.
* I should support metadata (types, compiled forms, etc.) from past.
* I would like to preserve transactional view of dictionary updates.
* I would like to preserve clean 'slices' in time for whole dictionary.
* It is okay to load compact metadata associated with a word's history.
* It is okay to load compact metadata associated with a dictionary's history.

Ideas:

* give transactions small IDs, e.g. 2 bytes each.


Store transactio metadata


A dictionary will eventually contain lots of binary data, ELOs, and so on, and will grow very large, both in number of words and content for a small subset of words. Similarly, I shouldn't need to load the entire history for each word. I also want to make it easy for different users to have different 'views' of the dictionary on the temporal axis.

I do want to preserve the notion of a dictionary as a simple list of transactions.







One concern is that developers will frequently have historical views of the dictionary. In addition, I might want to preserve certain amounts of metadata associated with these historical views, e.g. compiled and optimized code, type information, and so on. 





I shouldn't need to load the entire history of the dictionary into memory. I should still support transactions-list as a way to share histories.

To avoid loading the entire dictionary, words must be loaded either individually or in small groups.











