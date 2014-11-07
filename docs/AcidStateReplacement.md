Acid-state won't scale to my needs.

Usually, I'm not very confident about such performance assertions. I don't doubt acid-state will scale pretty well up to a few hundred megabytes, though GC costs start to scale non-linearly, which is a problem. We can do a lot with a few hundred megabytes. But...

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

The **berkeley db** option is undoubtedly robust, fast, and scalable. It would require a persistent host (VPS?) instead of a cloud instance. This might eventually be mitigated by developing a separate application that (via the replication interface) will back up BDB, or at least the log files. 

I could combine these two, using **TCache** backed by **berkeley db**. Not sure it would do me any good, though, with each having its own transaction manager.

# History and Cold Storage

Assuming I have transparently memcached, transactional, persistent variables.

How should I represent history? 

The idea with history is that we have some timeline of updates to our resources, such that we may later examine historical states for our data. Potentially, we may support branching time, i.e. the ability to begin operating on a past state (or, generally, to model branches). That's a useful idea, e.g. for debugging at historical times. In any case, we should certainly be able to study the past states. I also want to use logarithmic history, which is mostly a strategy for forgetting. This might be implemented a number of ways: ring buffers, probabilistic merge, or periodic decimation.

My goal, of course, is to avoid loading multiple versions for large objects into memory (i.e. temporal index) and to avoid loading lots of unnecessary values into memory (i.e. spatial index). It might also be nice to automatically intern frequently used structures.

TODO: Design it.




