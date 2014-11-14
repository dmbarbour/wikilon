The **acid-state** package won't scale to my needs. 

Keeping everything in memory is too much. It scales poorly for GC. And I plan to scale upwards of a hundred gigabytes, including history and rich literal types and Wikilon as a software platform. I expect that a very small fraction of wiki will be 'hot' at any given time. I would like to treat memory as a cache, loading and unloading data as needed.

I also want to push Wikilon onto external servers, either VPS or cloud. Fortunately, it looks like I can get a good deal on servers. And I'll probably want a dedicated server (note: ovh.com & contabo.com look very promising), but I could get started with VPS at 1/10th the cost.

If I can, I'd also like to avoid the serialization bottleneck of **acid-state**. But I do want to keep the 

# Design

I would like to consider two basic designs.

1. model a time-series of full databases (snapshots)
2. model a time-series of database deltas (transactions)

The first option has many advantages. It greatly simplifies seek and view for any historical snapshot of the database. Merges can be avoided entirely, i.e. we just delete whole snapshots. Branching would be easy, which could be advantageous for debugging.

The second option also has a few advantages. The transactions near the head are relatively small. And the load costs might be a bit smaller, too, since we can directly use database facilities near the head as opposed to indirectly modeling a database-as-a-value above another database or system.

With respect to performance, the snapshot approach might fall behind for operations on the head. But it will likely have much better performance for operations like viewing histories.


# Utilities

After studying a few options:

* **Perdure** - persistent IORefs, no transactions, unsuitable.
* **TCache** - persistent STM refs, backed by file or AWS
* **TX** - persistent STM ref, no checkpoint... unsuitable
* **berkeley db** - key-value store, ACID, memcache

Both **TCache** and **berkeley db** seem to be viable options. Of these, I think **berkeley db** may be the better choice, having a highly proven robustness, performance, and scalability. My impression (from the multitude of spelling errors) is that TCache code isn't very robust, and the IResource model should offer a better separation between data type and storage. 

Potentially, I could use TCache backed above BDB. OTOH, it isn't clear to me that this would offer any significant advantages. It might be worth emulating the cache from TCache, however, so I can reuse parsings? I'll need to think about it.

Nested transactions are also a useful feature. They can allow me to `{try}` a behavior without fully committing to it, i.e. failure as undo, which could greatly simplify reasoning about partial failure and system consistency. Both BDB and STM can support this. 

# Streaming Databases

I like the idea of [turning the database inside out](https://www.youtube.com/watch?v=fU9hR3kiOK0), of modeling a database not as a collection of static states, but rather as a stream of updates. If I approach this correctly, the stream of updates should support both a *history* and support reactive *views* (whereby one process leads to another). I think this should be considered essential to my design. It was already part of the **acid-state** dictionary design. I believe it can be adapted to my filesystem model, and also to auxiliary data and cached views, and perhaps even to RDP computations. 

The approach I was pursuing before, for the dictionary:

* each transaction is a stream of updates to independent words
* each transaction also includes a set of annotations
* metadata tracking temporal information, updates collapsed, etc.

Unfortunately, it is too difficult to look up past states without building a lot of intermediate checkpoints. And it seems too difficult to parallelize computations that write to the dictionary, which isn't a problem for the dictionary itself but, sadly, doesn't generalize nicely to the filesystem model.

To look up past states:

* each transaction has a non-decreasing identifier, e.g. 64 bits
* when variable is updated, we keep ID for the prior version
* can then directly seek prior versions for each object in database

This technique will be compatible with exponential decay. When we merge two transactions, we keep the newest of the new versions and a reference to the older of the old transaction IDs. (Alternatively, we could use timestamps for transaction IDs. But I think it might be awkward if transactions serialize outside of the transaction timestamp ordering.) Merged transactions will then actually cover a volume of transaction IDs, and a volume in time.

An interesting idea is that values within the database should be monoidal, such that we can compose values when merging transactions, e.g. like adding numbers or collapsing RDP signals. However, I think this might complicate the database overly much, and tie it too much to a particular runtime environment.

In addition to access to prior versions of a word, we need easy access to the current version of each word. This suggests that we should keep a normal database mapping at least from word to a transaction ID. It wouldn't hurt to further map from a word to a transaction ID together with the current value, ensuring lookup of HEAD values  is relatively. This would also simplifiy parallelization, since reads and writes to HEAD values could leverage whatever concurrency scheme is supported by the underlying database.

An interesting possibility is to allow destruction of the 'cached' version of each word independently of the transaction history. When an object is first created, then, we'll need to scan through the history to find any versions of it. Or, alternatively, we can build a cache from the history...

Note that cached content may need to track whether a value has been 'deleted', since it might still have a prior value that we'll need to reference to track histories.

# Value References

I need the ability to work with very large values without loading them into memory all at once. This includes merged transactions for large databases and so on. I think a good way to model this is to model structured, persistent, memcached values that don't need to be fully loaded into memory.

Instead of saving a large structure into a massive binary, we can split a binary and save references for each piece. Later, we lazily load only the pieces we need. The main difficulty with this approach is garbage collection. Old values we don't need any more must be deleted from the database. Even if we use reference counts, we'll need to somehow *recursively* decref. This suggests that our database should know something about the internal structure of our objects.

Proposal: develop a simple variation on JSON, YAML, CBOR, MsgPack, etc. as the 'structured value' model within the database. Include a simple feature for 'further lookup'. Simply encode all values in this format.

If done correctly, we'll be able to model very large, structured values but load them into memory incrementally. Further, this might help 'intern' common structures, or (with content addressing) enable cheap 'diffs' between large values.

# Logarithmic History

If I model the database as a sequence of transactions, then I should be able to apply basically the same logarithmic history model that I applied before, repeatedly decimating the list to keep it hovering around a target size, gradually eliminating intermediate values to make room for new values.

I like the idea of adding 'protection' to snapshots in the history, along with version names. Rather than making protection absolute, I could make it tiered: a level N snapshot won't be lost while there exist level N-1 neighbors in its vicinity. This should allow for something like major and minor and sub-minor versions, along with nameless transactions.

# Transaction Structure

A potential model for transaction structure is something like a patricia trie of named elements, or an otherwise flattened, hierarchical map from keys to values. Each transaction would essentially become miniature database. This might work pretty well, especially using value references. 

Treating transactions as immutable values, rather than embedding them within the database, will likely have nicer properties for merging transactions. Albeit, at the cost of more expensive lookups. Which is better? 

# Faster History Lookups

Rather than just having one back-reference to the previous value in history, an interesting possibility is to keep multiple references into the past using a skip-list-like structure, e.g. a list of a few "other past values" that allow leap-frogging into the past. 

In this case, our skip list would only be adding to the head, and would be reverse-ordered. As normal, we might select a random height for each node in the history, perhaps deterministically (via hash function), or perhaps by estimated distance, or perhaps keeping a simple counter.

The difficulty will be how to merge nodes in a skip-list, locally. If we simply 'grow' the tower of historical references, the height will do us less good as multiple merges gradually eliminate them. OTOH, if we shrink this list, then we'll lose some skip-list invariants from upstream objects.

Let's try a variation. Goals: greedy search is optimal, easy local merge, efficient searches.

We can't just reference arbitrary locations into the past, or greedy search will fail. In a scenario such as:

          ___   ___
         /   \ /   \
        a--b--c--d--e--f--g
            \_________/

The best path for 'af' is 'abf', but a greedy search would find 'acef'. To avoid this scenario, we need to avoid crossings between regions.

                   _______________
                  /               \
        a--b--c--d--e--f--g--h--i--j--k
         \  \___/    \______/ \___/  /
          \_________________________/

We might say that 'a' has territory 'a..k', and 'd' has territory 'd..j', and nothing can jump into d's territory without going through d. This is a simple approach, and it should work very well. 

To simplify merge, we'll just keep two references: the immediately previous ID, and the most distant reference. The challenge, then, is ensuring logarithmic search. Before merge, we could easily ensure logarithmic search: territory could be constructed using a simple mechanism that ensures exponential sizes. Assuming we keep only two back-references (one for the previous item, one for the closed territory), this will cause us to gradually lose the smaller territories in favor of the large ones. OTOH, we might be able to heuristically favor merges with smaller territories.

The alternative is to support snapshots direct

            

# Dictionary

In this design, our dictionary is possibly a separate database from the filesystem, but uses the same basic structure (albeit, with much more memorable keys). 



# BDB

For BDB, I'll want to use the following flags:

        DB_CREATE, DB_REGISTER, DB_RECOVER  (get started and recover if needed)
        DB_THREAD, DB_INIT_LOCK, DB_INIT_LOG, DB_INIT_MPOOL, DB_INIT_TXN (subsystems)
        (no logging flags)

        DB_MULTIVERSION (copy-on-write, reduces risk of deadlock)
        DB_TXN_SNAPSHOT (but only for read-only transactions)

Also, I might want to relax durability, using explicit `sync` a few seconds after a transaction. 

