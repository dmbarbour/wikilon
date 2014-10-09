# Dictionary as Bounded Path History

A dictionary is represented by a linear history of transactions of bounded size. 

Whenever we reach this maximum, e.g. 1000 nodes, we decimate the dictionary: take each group of ten transactions starting from root, then reduce it to a group of nine transactions. This is done by merging two adjacent transactions within the group (there are only nine candidate merges). We favor the merge with the least overlap (based on number of defined words) to preserve information. To break ties deterministically, we'll favor the merge candidate closer to root.

The **name** of a transaction, then, is a chain of secure hashes starting from root. For example, the name of the fourth transaction from root is something like:

         nameOfTx4 = h(h(h(h(hRoot++tx1)++tx2)++tx3)++tx4)

Where `hRoot` is just a zero buffer, and `tx1`..`tx4` are the UTF-8 serialized contents of each transaction. We should be able to improve reuse of transaction contents across forks, and improve predictable performance of the rehashing requirement, if we tweak this instead to use:

         nameOfTx4 = h(h(h(h(hRoot++h(tx1))++h(tx2))++h(tx3))++h(tx4)).

In practice, the result is that our transactions near root will be very 'fat' (almost whole dictionaries), while our transactions near the user session will be very thin and fine-grained (e.g. just updating a few words), albeit with occasional lumpiness due to big transactions. We preserve at least 80% of transaction contents and transaction-hash identifiers from one generation to another.

*Expected Performance:* Amortized, we're asked to touch and hash ten nodes in history for every node we add, which corresponds (more or less) to what we should expect from Merkle trees. If we increased the trimming factor, we could reduce this overhead a bit, but I think ten is about what I want.  (though, Merkle tree computation is a lot more incremental). By design decision, those nodes are scattered uniformly across the whole history, which hinders cache optimizations a fair bit. OTOH, it's all run at once, so we can implement a tight loop. The decision of which node to merge in each group is also pleasingly parallel.

*Pending Transactions:* In addition to the main-line history, a wiki may have a small set of pending transactions that don't count against the maximum history length. This can aid performance by providing a little slack in the renaming algorithms. It also provides greater opportunity to accept/reject changes, to obtain admin approval, to synchronize and gain acceptance across a distributed wiki, and so on. Properly, pending transactions should be treated as an entirely different construct than real transactions.

*Transaction Metadata:* In addition t

The processing costs for compacting the dictionary can generally be shifted to 'slow' periods.

# Orthogonal Concerns

Orthogonally to transaction naming, Wikilon will generally reject transactions that lead to inconsistent states (parse errors, failed tests, bad typechecks) and may support a few other policies such as requiring secondary approval if a `frozen!word` directive is defined and the change would impact that word.

Also, Wikis may have state outside the dictionary, e.g. for pull requests and issue tracking. These might essentially use the same pattern, but using a separate dictionary.

# Background

Dictionary states should be individually and deterministically named.

* **individually named** to precisely specify points in dictionary history.
* **deterministically named** to simplify reasoning and replication.

There are at least two basic approaches to naming:

* **point-based naming**, secure hash of dictionary state.
* **path-based naming**, secure hash of steps to generate dictionary.

Point-based naming has some nice properties. Updates are commutative, associative, and idempotent with respect to final name. Logarithmic histories (exponential decay) is very easy. Efficient point-based naming requires something like a Merkle tree (to avoid serializing and re-hashing the entire dictionary) which must be structured in a simple and deterministic way (so everyone computes the same hashes).

Path-based naming requires a complete history to recompute the current name, yet is trivial to compute incrementally. Also paths have utility. They tell a story, offer insights, allow backtracking. Path-based naming encourages us to track the past.

We can **hybridize point and path** by restriction to a bounded-length path.

In the extreme case, a path may be bounded to one step: formulate a singular transaction that will update us from the empty dictionary to the new one. This is functionally equivalent to a 'point'. If construction of the transaction is deterministic, so will be the name for this point.

A more interesting case is restriction to, for example, 1000 points. A 1000-point line on a surface can take many interesting shapes. A simple implementation of this: whenever we reach the 1000 point limit, compact the path back to 900 points. This gives us exponential decay, resulting in a log-scale history. 

The eventual space requirement is O(D*H) where D is the number of words in the dictionary, and H is the number of points in history, but in practice it will take a very long time (a great many generations) before we approach this limit, and old stable words will have a relatively low cost in the same long term. I expect the normal cost to be just a few times the dictionary size.

A locality property on the collapse can be useful, e.g. don't just select 100 of 1000 entries globally. But rather take each group of ten and reduce it to nine. This ensures a more uniform loss of information, and the paths will often be reusable across different forks of the dictionary (they'll decay in the same manner until they reach the point of divergence). Parallel computation is also easier.

There are many viable strategies for reducing a group of ten down to nine. 

Since Wikis will be distributed and forked, it's important we choose a simple strategy, at least as the default.

Wikilon Wikis should eventually become distributed objects. Thus, every clone of a wiki should be using the same logic for deciding which elements to merge. Further, if we *fork* wikis, it's ideal if the forks have the same decay path for the shared history. So...

My proposal, for each group of ten:

* Select candidate merge with least overlap.
* On tie, favor merge of candidate closer to root.

This is a simple computation with a deterministic tie-breaker. Favoring "least overlap" will cause us to merge small transactions into bigger ones, and tries to preserve more history about individual words. While the "ten" could be adjusted to eight or sixteen, 'decimate' is easy to explain and seems a nice medium.

All Wikilon wikis should use the same formula. However, it is feasible to adjust the amount of history, perhaps by specifying `wikilon:history` in the dictionary. A wiki with a longer history will absorb more edits and compact larger batches, but will still decay in the same predictable order as a wiki with fewer entries.

The greater difficulty, I think, will be deciding how to perform a deterministic merge for two transactions. But I think we can keep that simple, too.

NOTE: A wiki might also have a small buffer of 'pending' transactions that don't count as part of the history yet, e.g. waiting for admin approvals or distributed-system votes. 

