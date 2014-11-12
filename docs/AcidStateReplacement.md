The **acid-state** package won't scale to my needs. 

Keeping everything in memory is too much. It scales poorly for GC. And I plan to scale upwards of a hundred gigabytes, including history and rich literal types and Wikilon as a software platform. I expect that a very small fraction of wiki will be 'hot' at any given time. I would like to treat memory as a cache, loading and unloading data as needed.

I also want to push Wikilon onto external servers, either VPS or cloud. Fortunately, it looks like I can get a good deal on servers. And I'll probably want a dedicated server (note: ovh.com & contabo.com look very promising), but I could get started with VPS at 1/10th the cost.

If I can, I'd also like to avoid the serialization bottleneck of **acid-state**. But I do want to keep the 

After studying a few options:

* **Perdure** - persistent IORefs, no transactions, unsuitable.
* **TCache** - persistent STM refs, backed by file or AWS
* **TX** - persistent STM ref, no checkpoint... unsuitable
* **berkeley db** - key-value store, ACID, memcache

Both **TCache** and **berkeley db** seem to be viable options. Of these, I think **berkeley db** may be the better choice, having a highly proven scalability. My intuition is that TCache code isn't very robust, and the IResource model should offer a better separation between data type and storage. 

Potentially, I could use TCache backed above BDB. OTOH, it isn't clear to me that this would offer any significant advantages. It might be worth emulating the cache from TCache, however, so I can reuse parsings? I'll need to think about it.

The **berkeley db** option is undoubtedly robust, fast, and scalable. 

Nested transactions, supported by both BDB and STM (via `orElse`), should be useful for `{try}`-like behaviors in imperative code. 

# Streaming Databases

I like the idea of [turning the database inside out](https://www.youtube.com/watch?v=fU9hR3kiOK0), of modeling a database not as a collection of static states, but rather as a stream of updates. If I approach this correctly, the stream of updates should support both a *history* and support more reactive *views* (whereby one process leads to another). I think this should be considered essential to my design. It was already part of the **acid-state** dictionary design. I believe it can be adapted to my filesystem model, and also to auxiliary data and cached views, and perhaps even to RDP computations. 

The approach I was pursuing before, for the dictionary:

* each transaction is a stream of updates to independent words
* each transaction also includes a set of annotations
* metadata tracking temporal information, updates collapsed, etc.

My prior design was missing a few features, notably easy lookup of past states, *easy integration with RDP*, and parallelization of non-overlapping transactions.

To lookup past states, I propose the following feature: in addition to recording the new value of each object updated in a transaction, I record an identifier for the LAST transaction that updated the same object. Then, even if my transaction history has 20k samples, I can easily skip backwards to each prior version of a given value. The transaction identifier could be an incremental 64-bit ID, or it could be a timestamp. 

I lean towards a simple, monotonic or non-decreasing, 64-bit transaction ID that lacks much semantic information. The idea, then, is that when transactions merge, we'll still be able to keep the information about a prior value for each variable. In case of non-decreasing IDs, when there are multiple transactions with the same ID, we might need to search all of them, but there is also no contention to create a transaction ID. Gradually, each transaction may come to represent a range of IDs due to merges and exponential decay. (Use of a timestamp as ID could also work, but we'd need to be careful because two transactions at *almost* the same time might serialize in a different order than their timestamps. This seems like an awkward observation.)

For easy integration with RDP, an interesting idea is to model each state as an RDP signal, or having a similar composition model. The difficulty, here, is that I really don't want objects to grow arbitrarily large, and it's unclear what decay properties should be applied within each value, and it may be difficult to expose the particular 'model' associated with each value in the database. For now, let's stick with: "states have a value, and a history", and shift the whole problem of RDP-level signals into another layer.

Finally, for parallelization, I should use a different variable/key for every attribute. What would this key contain? Possibly the current value, and the transaction ID that updated it. Or, maybe, just the transaction ID that updated it, thereby asking for us to look up the appropriate transaction to get the actual value. Storing the value directly is probably a better idea for scalable performance. Either way, the idea is that by reading and writing to this key/var, the underlying transaction model can manage the read-write conflicts (either optimistically or pessimistically).





The problem with this approach is that it further multiplies entities. 

it might be a good idea to model each state as an RDP signal, such that we can merge signals as we gradually decay. 


This identifier should be an incremental transaction ID.

let's say we have the following feature: each t

To support parallelization for non-overlapping transactions, I'll probably need to use the database directly so it can leverage its own transaction and locking model. With BDB, it seems feasible to use nested transactions to limit conflicts on finally updating the list of transactions.




 so long as I'm careful about it. To look up past states requires an index into the transaction list - e.g. a transaction ID or timestamp. 

If I use transaction IDs, 




I think this approach can be generalized, but it does have some costs:

* as we merge transactions, each transaction gradually covers more of the database
* 


I think we can generalize this technique to a large-scale database, so long as we accept a consistent update model for all objects. 

But my acid-state dictionary does have some weaknesses. First, I cannot easily look up prior versions of a value. Second, 




# Value References

I suspect the first thing I should do is model immutable value-refs, for values that are persistent but potentially shared within the database. I want to take advantage of memcache for long-lived values. I'll also need explicit destruction for these variables (or reference counts), to release them from persistence layer.

Immutable value-refs might use content addressing by default, and might automatically 'intern' smaller values into the identifier. The main problem with content addressing would be that I also need to track reference counts.






# Logarithmic History

Keeping a history can be challenging. We need to gradually delete old information to make room for new information. This would be easy to do on a per-variable basis, but doing so typically results in different variables decaying at different rates. I want a 'clean' history, in the sense that every available snapshot of history has a pristine view of the database at that time, and we only lose whole snapshots.

The basic alternatives I can think of:

1. database as one big value, Merkle tree
2. database as lots of small values, but reify transaction log

I'm not particularly keen on either of these techniques. Both serialize writes. The Merkle tree approach has, potentially, a very high overhead.






Reifying the transaction log has some advantages: it's easy to model stream processing, to 'turn the database inside out' such that I can easily add new views over the stream. 

 and add new processing to it. OTOH, it doesn't seem a very good fit for BDB or other databases. 

 But I wonder if there might be a good intermediate approach to relax serialization, e.g. append-only on tree values, such that each Merkle tree represents a range of values that are lazily or latently resolved into a sequence of snapshots, but minimizing serialization on any particular element.

An interesting technique might be to place variables themselves into a lazy Merkle tree, but logically 'opening' and 'closing' subtrees to model laziness and lift tree-split events gradually back to the top node.

I think the ideal use-case here is to offer a snapshot-like view for any given timestamp.

## Dictionary

A dictionary consists of a set of (word,definition) pairs where definitions are acyclic and do not use any unrecognized words. 

My earlier design for a dictionary involved explicitly reifying a list of transactions, and modeling the dictionary in terms of this list, mostly to simplify the computation of historical states. 

A dictionary is constructed from a list of transactions. We'll parse and validate each transaction before accepting it. Old transactions must be occasionally combined as we add new ones, providing a basis for loss of information. We might keep a little extra metadata, such as the timestamp for each transaction. 

The problem of collecting history could reasonably be separated from the transactions that update it. For simplicitly, let's do that. 

Loading an entire dictionary of words into memory should not be a problem, even for a million word dictionary. 

, but I'd rather avoid it if possible. This suggests that, for each word, I should also index which transactions modify it. And I might usefully  Consequently, loading a list of transaction IDs should be pretty cheap, e.g. 2 bytes each would be ideal (i.e. maxing out at less than 64k samples, but I'm expecting about 2k so I'm way under the limit.)

A new transaction, then, would involve touching the following:

* 

It should be feasible to load information 




I'll systematically combine old transactions while adding new ones, modeling a logarithmic history. The challenge is shifted from representing the basic dictionary to maintaining 'checkpoints' of it in a useful way. Alternatively, I should keep 

In addition, for every word I may need a list of update times, i.e. transactions. 

 Each transaction will have a corresponding 


* I shouldn't need to load the entire dictionary into memory. 
* I shouldn't need to load entire history for a word into memory.
* I should support multiple 'views' of dictionary on temporal axis.
* I should support metadata (types, compiled forms, etc.) from past.
* I would like to preserve transactional view of dictionary updates.
* I would like to preserve clean 'slices' in time for whole dictionary.
* It is okay to load compact metadata associated with a word's history.
* It is okay to load compact metadata associated with a dictionary's history.

Ok

Okay, let's assume we want to keep transactions on the dictionary as basically the same. Each transaction is given a unique name, either via content addressing or 'newID', as a read

I think the same basic design can apply: the 'whole dictionary' consists of a simple sequence of transactions. Each transaction is given a unique name. (Should I use content addressing for this? or just use a 'newID' mechanism?) We'll record each transaction as an object in the dictionary.

Each transaction should have multiple attributes:
* whole transaction value
* relevant metadata
* 
* relevant metadata

* list of words 

The difficulty, then, will be providing dictionaries.

We'll be able to look load timing information for each transaction, and other metadata.





* The "whole dictionary" consists of a list of transactions


* each transaction is given a unique identifier, e.g. an integer.
* history is recorded as a list of transactions on the dictionary.
* 
* 
* a history is a list of transaction IDs. 


Store transactio metadata


A dictionary will eventually contain lots of binary data, ELOs, and so on, and will grow very large, both in number of words and content for a small subset of words. Similarly, I shouldn't need to load the entire history for each word. I also want to make it easy for different users to have different 'views' of the dictionary on the temporal axis.

I do want to preserve the notion of a dictionary as a simple list of transactions.







One concern is that developers will frequently have historical views of the dictionary. In addition, I might want to preserve certain amounts of metadata associated with these historical views, e.g. compiled and optimized code, type information, and so on. 





I shouldn't need to load the entire history of the dictionary into memory. I should still support transactions-list as a way to share histories.

To avoid loading the entire dictionary, words must be loaded either individually or in small groups.











