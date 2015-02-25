
Git, Mercurial, and other DVCS systems model *branching and merging* of content. This is useful: each developer can operate in his or her own little bubble while still interacting meaningfully with the community, by pushing and pulling content, requests, and issues. Consistency for updates is more readily enforced.

The challenge is to support both branches and logarithmic history. Options?

* Collect each branch independently.
* Collect all branches on each collection. 
* Collect time-ordered list including all branches.

Collecting branches independently doesn't seem very viable, i.e. because it doesn't scale nicely to a large number of branches or old branches that are abandoned. The latter two seem more or less equivalent, though collecting all branches individually is perhaps more predictable with respect to the loss within any given branch (i.e. there is no risk of probabilistically destroying every instance of one branch in a single collection).

So, I think perhaps that's the route to take right now: when we perform a collection, it touches known branches. Living branches will additionally guarantee that at least K previous versions are preserved, while dead branches may eventually be fully destroyed.

To draw branches in a diagram, we'll also want to preserve their origin branch:time, even if we don't preserve that specific version.

Rough design Alpha:

* Wikilon maintains a collection of named branches.
* A branch may be marked living or dead.
 * A living branch preserves at least head version.
 * A dead branch is trimmed more aggressively. 
* Users should be able to forcibly delete content for infosec reasons.
 * Cannot delete head version without killing branch.
* Wikilon "trims" the whole tree to keep total number of versions down.
 * Decimation for exponential decay. Applied to each branch independently.
 * Dead branch collects much higher fraction, e.g. keeping half. 

For graphing/diagrams, we should probably track a named origin for every living branch, and we might forbid reuse of branch names (or repair the tree if a branch is fully deleted). For security purposes, we might need to generate capabilities associated with a branch, but this should perhaps be a separate issue.

It seems feasible to use a similar technique for modeling snapshots of virtual machines, such that inactive machines gradually remove older versions to make room for active updates on new machines.

Thoughts on Alpha:

Unfortunately, we still lack nice, cross-cutting views for multiple dictionaries. But this suggests an alternative: we could model all branches together as a single object. I.e. instead of having `Map Branch [Image]` we have `[Map Branch Image]`, and we thus collect all branches. 

We could also generalize this to `[WikiImage]` where the Wiki includes both a dictionary AND any hosted virtual machines. I'll need to think about that possibility, because there are relevant performance concerns. In some ways, the need to keep "lifting" the version snapshots into the parent reminds me of how monads operate. Could I model a versioning monad (and would it help?)

This `[Map Branch Image]` approach has its own disadvantages, namely that it isn't clear how far one must seek to find prior versions of any specific branch. We might need to use timestamps to quickly discover old versions of data.

