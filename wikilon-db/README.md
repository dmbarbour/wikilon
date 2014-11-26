A history-preserving key-value database for Wikilon.

# Why a new Database?

Wikilon, if successful, will host multitudes of program artifacts - i.e. not just reusable functions, but also the documents, spreadsheets, ray traced models, music, game worlds, and so on. My goal is to support procedural generation, refactoring, abstraction, and macro construction of these artifacts. I do this by embedding them in a computational context based around the Awelon project languages.

In addition, I aim to support a deep history based on an exponential decay process - a half-life for information - that preserves a few thousand snapshots over time. The exponential decay process can ensure a more or less constant space overhead (not quite constant due to structure sharing), but it does multiply the costs for artifacts based on the number of accessible versions.

Some temporal databases exist, but very few make logarithmic history readily feasible. 

Wikilon needs at least two databases: 

1. a dictionary database, with AO words, definitions, and reverse lookups
2. a file system, except with ABC embedded objects, redirects, and scripts

The file system supports IDE-layer state, cooperative work between users, and use of Wikilon as a software platform. The dictionary supports the main wiki pages and content. AO code might actually grow quite large, e.g. due to modeling a spreadsheet as a word per cell, and modeling various embedded objects by a long series of words. 

The primary queries on both databases tend to just be key-value lookups. I'll also want historical value lookups, and the ability to 'run' code against a historical snapshot of the filesystem for testing purposes.

# Proposed Structure

My current plan is to model each snapshot of the database as an associative array with a high level of structure sharing. To support structure sharing, I'll need a secondary key-value database to represent stored values. Further, I'll need support for garbage collection, e.g. based on reference counting.

So, the proposed organization is:

* a 'value' store for large shared values 
* a reference count database for the values
* a data model for the associative array (B-tree?)
* support for toplevel key-value pairs

My current plan is to essentially model value references using 64-bit keys. I also plan to build the value store above **Berkeley DB**. This results in something of an abstraction inversion: I'm modeling a key-value store above a key-value store. But this is necessary to support history. 

Garbage collection will be based on a separate database that keeps a reference count for each value. Further, rather than storing raw binaries, I'll have a simple intermediate data type so the GC will know how to recursively destroy collected objects. But this will all be hidden from the clients of each database. 

Rather than eager GC, I might also aim to have something like a GC queue or stack that is primarily handled by a separate thread.

# Keeping History

I think we might do pretty well to configure the history with just two numbers: a count of frames to preserve exactly (a windowed history), and a count of frames to preserve with exponential decay (logarithmic history).

# Structure Sharing

I should be able to achieve a high degree of structure sharing within the database by proposing keys based on a reasonably 'good' hash of the stored value, then using linear collision. Unlike a hash table, I'll be using the full space of 64 bits encoded in the BDB B-tree, i.e. a hashmap, so there is no need to rebuild the hash table. Linear collision could minimize paging in BDB. Note that this technique doesn't guarantee that equal values have equal names unless we do a lot of complex stuff to handle deletion. It just has a high tendency towards sharing.

Structure sharing at the AO layer is possible - it's just refactoring. And structure sharing in ABC can be achieved with ABC resources, and systematic passes to compress the database by use of ABC resources.

Question: should I expose the value structure to clients of the Wikilon DB?

I think it might complicate tracking reference counts, and it might not help much with normal use cases of AO or ABC code. OTOH, it might prove very useful for building simple concepts above the key-value databases and preserving a high level of value-layer sharing. It also might offer a more uniform model. The main trick would be forbidding value references to be propagated outside any given Wikilon DB transaction, and perhaps prevent direct view of the reference numbers. Maybe an approach similar to the STRef model could work?

I feel this idea is at least worth pursuing.

# Paging and Compaction

A compacting GC is a useful idea, but a somewhat difficult one. 

The idea is that if value A references value B, we might wish to place value A near to value B in memory. We might also associate values that are related *temporally*, i.e. by placing values that are created near the same time into the same BDB pages, under the expectation that temporal relationships are weakly associated with spatial relationships.

To support this seems viable. Minimally, we could have a sequential allocator for new values, and perhaps a simple way to track 'freed' space for GC purposes. To integrate with structure sharing is still useful, but might be split into a separate table of hash→locations list (a bucket hash). So we'd now need:

* a table from locations to value, storing hash value
* a table from locations to reference counts
* a table from hash values to a list of locations

This adds a little overhead for allocation, and it may create some conflicts for allocating values. Write transactions would need to select different locations or 'pages' to avoid conflict. On the plus side, there is no extra overhead for reads, and structure sharing could be very precise because we can now find all possible addresses for a stowed value, i.e. no issues with hash collision followed by deletion.

The remaining idea, then, is to create a little open space near values, such that we can try to stow values near one of the other values they reference. Even if it's just a limited attempt, it might be sufficient to get some benefits.

I'm not really sure where a 'compacting GC' might become involved. The idea would be to reorganize objects such that if A references B, A is more likely to be near B (and perhaps precede B) in physical storage, thus reducing the probability of loading big pages into memory just to access just one small value on it. Compaction seems possible, but I'm not sure it could readily be achieved concurrently. 

Maybe we could just move a few objects at a time, based on observed access patterns? I.e. if we load objects A,B,C in sequence, we might see if C can be moved. Unfortunately, moving C might require locating all references to it. Even if references are exposed, this might be too much without ad-hoc reverse lookup indices. 

Is all this extra complication worthwhile? Maybe. I'd need to profile to know. 

For now, I think I'll drop this idea, and simply store values based on secure hash with linear collision. If I really want, I can still achieve precise structure sharing by marking a cell as deleted rather than actually removing it. OTOH, precise structure sharing really isn't worth much if I'm not trying to compare values by their 'pointers'.

Conclusion: optimize later. A rather difficult endeavor for persistent data. It is feasible, but it requires a good export/import system.


# Gradual GC

Instead of "most objects die young", the preservation of history ensures there will often be a very large latency between an object's creation and collection. I think a lot of existing GC models won't be very effective with this context. Reference counting won't be especially hindered by it, but will still have the issue where GC almost always involves paging.

To reduce the performance burden of cascading destruction, I feel a simple queue of 'values to be deleted' is quite appropriate. Due to structure sharing, it is quite possible for a value to be revived before the GC gets to it, so it would follow a process of grabbing one value from the queue, checking whether its reference count is still zero, and if so: delete it, then decref all the objects to which it contains a reference, adding them to the queue if necessary.

Reference counting will undoubtedly add a fair amount of overhead, but I think it will be essential.


# Other Features

An interesting possibility is having read-only transactions that 'read their own writes', but where all writes are dropped in the end. Would this be worthwhile? Or should it be modeled more explicitly as operating on a branch?

I can potentially support concurrent writer transactions on a database, automatically detecting conflicts on commit. A bloom filter (or a simple set) might track all keys read by a transaction.
