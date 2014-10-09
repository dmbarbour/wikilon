
# Conclusion up front

A dictionary is represented by a linear history of transactions of bounded size. 

Whenever we reach this maximum, we decimate the dictionary: take each group of ten transactions starting from root, then reduce it to a group of nine transactions. This is done by merging two adjacent transactions within the group (there are only nine candidate merges). We favor the merge with the least overlap (based on number of defined words) to preserve information. To break ties deterministically, we'll favor the merge candidate closer to root.

The **name** of a transaction, then, is a chain of secure hashes starting from root. For example, the name of the fourth transaction from root is something like:

         nameOfTx4 = h(h(h(h(hRoot++tx1)++tx2)++tx3)++tx4)

Where `hRoot` is just a zero buffer, and `tx1`..`tx4` are the UTF-8 serialized contents of each transaction.

Whenever we decimate the dictionary, we'll need to rename it by recomputing this whole chain of transactions. This might sound like a lot, but secure hashes are fast, and the 'whole chain' is bounded in size.

In practice, the result is that our transactions near root will be very 'fat' (almost whole dictionaries), while our transactions near the user session will be very thin and fine-grained (e.g. just updating a few words), albeit with occasional lumpiness due to big transactions.

## Orthogonal Concerns

Orthogonally to transaction naming, Wikilon will generally reject transactions that lead to inconsistent states (parse errors, failed tests, bad typechecks) and may support a few other policies such as requiring secondary approval if a `frozen!word` directive is defined and the change would impact that word.

Also, Wikis may have state outside the dictionary, e.g. for pull requests and issue tracking. These might essentially use the same pattern, but using a separate dictionary.

# Background

Dictionary states should be individually and deterministically named.

* **individually named** to precisely specify points in dictionary history.
* **deterministically named** to simplify reasoning and replication.

I have two basic ideas for naming:

* **point** based naming, based entirely on the dictionary state.
 * secure hash of dictionary; possibly based on merkle trees

* **path** based naming, where each step carries information from history.
 * hash chains involving prior secure hash and transaction contents
 * root transaction (empty dictionary) has simple name, e.g. zero.
 * e.g. name4 = h(h(h(h(0++tx1)++tx2)++tx3)++tx4)

Point-based naming has some nice properties. Updates are commutative, associative, and idempotent with respect to final name. Logarithmic histories (exponential decay) is very easy. However, point-based naming is rather sophisticated and (even with Merkle trees) expensive to compute. Also, it also doesn't encourage keeping much history around. 

Path-based naming requires a history to compute the current name, and is trivial to compute incrementally. Keeping histories is useful for users, statisticians, and eventual historians... but it can become expensive. 

We can **hybridize point and path**, e.g. as a finite path.

In the extreme case, a path may be reduced to one point: formulate a singular transaction that will update us from the empty dictionary to the new one. That's now a 'point'. If construction of the transaction is deterministic, so will be the name for the point. 

But paths have utility. They tell a story, offer insights, allow backtracking.

We can use exponential decay, logarithmic history, in bounded space.

A simple approach to logarithmic history: 

* select a maximum number of entries
* when we reach the limit, decimate the history

For example, limit our history to 1000 entries. Whenever we reach the limit, select one hundred entries to merge. Now we have 900 entries. We can do this repeatedly without much trouble, and the numbers can be tweaked a little. Since the path remains finite, the cost of computing the new dictionary ID remains bounded by the dictionary size rather than by the history length.

A locality property on the collapse can be useful, e.g. don't just select 100 of 1000 entries globally. But rather take each group of ten and reduce it to nine. This ensures a more uniform loss of information, and the paths will often be reusable across different forks of the dictionary (they'll decay in the same manner until they reach the point of divergence).

There are many viable strategies for reducing a group of ten down to nine. Wikilon Wikis should eventually become distributed objects. Thus, every clone of a wiki should be using the same logic for deciding which elements to merge. Further, if we *fork* wikis, it's ideal if the forks have the same decay path for the shared history. So...

My proposal, for each group of ten:

* Select candidate merge with least overlap.
* On tie, favor merge of candidate closer to root.

This is a simple computation with a deterministic tie-breaker. Favoring "least overlap" will cause us to merge small transactions into bigger ones, and tries to preserve more history about individual words. While the "ten" could be adjusted to eight or sixteen, 'decimate' is easy to explain and seems a nice medium.

All Wikilon wikis should use the same formula. However, it is feasible to adjust the amount of history, perhaps by specifying `wikilon:history` in the dictionary. A wiki with a longer history will absorb more edits and compact larger batches, but will still decay in the same predictable order as a wiki with fewer entries.

The greater difficulty, I think, will be deciding how to perform a deterministic merge for two transactions. But I think we can keep that simple, too.


NOTE: A wiki might also have a small buffer of 'pending' transactions that don't count as part of the history yet, e.g. waiting for admin approvals or distributed-system votes. 
