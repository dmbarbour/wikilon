Acid-state won't scale to my needs.

Usually, I'm not very confident about such performance assertions. I don't doubt acid-state will scale pretty well up to a few hundred megabytes, though GC costs start to scale non-linearly, which is a problem. We can do a lot with a few hundred megabytes. But...

AO isn't like most other languages. It's intended for use in a fashion similar to Wolfram language, i.e. having tons of data readily available, and building interesting data-things into the dictionary for reuse elsewhere. Further, we'll have a great many versions of our code and data, multiplying storage costs by a few orders of magnitude (modulo excellent, multi-layered compression tactics). I can easily afford big storage costs, so long as they're outside my process memory and outside purview of the normal garbage collector. One hundred gigabytes? No problem, just get an SSD or buy a little space in the cloud. 

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
* **TCache** (very promising)
 * STM transactions with external, abstract storage.
 * DBRefs can reference other DBRefs (nice!)
 * Includes adapter to AWS. User programmable backends.
 * Seems to integrate with local STM, too.
* **TX** (needs work)
 * Lacks checkpointing. 
 * Doesn't do anything to keep cold data on disk.
 * Nice interface. Might be adaptable.
* existing databases and key-value stores
* write my own if necessary


Of these, **TCache** is by far the closest to what I need. 


