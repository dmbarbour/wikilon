I want transactional editing, DVCS-like features, while sticking with the live programming environment.

DVCS detects write-write conflicts only. We need read-write conflicts could help keep types checking and tests passing. The concept 'read' here must be taken somewhat liberally. We're really saying that we depend on a given behavior. Unfortunately, it's difficult to precisely determine which behaviors program depends upon. A conservative estimate would work for now, and we might later try machine learning to help identify low-risk vs. high-risk dependencies.

I understand Git well enough at this point. I should probably read about [Darcs](http://en.wikibooks.org/wiki/Understanding_Darcs) to get more ideas. But some ideas:

* within any given transaction, a word must have *exactly* one definition
* we want the main history for a wiki (or any branch!) to be 99% linear
* transactions must interact with exponential decay of history
  * branches should prevent decay of their origin (cost should be marginal)
* want to observe tests, typechecks, web-apps, etc. behavior on many branches
* Wikilon will support multiple users; need multiplicity for working sets
  * each working set gives user near-illusion of working alone
  * potentially allow multi-user CSCW workspaces, on voluntary basis
  * even in single-user workspace, maintain awareness of sibling transactions
  *   yellow zone: proposed transactions are active and in conflict
  *   red zone: conflicting transaction committed; adjudication required
* A new Wikilon instance will often inherit from an existing one!
  * treat this as adding a parent to our 'master' plus a remote branch
* I want the ability to cherry-pick updates from other wikis and transactions
  * e.g. supporting import, ad-hoc renaming, another remote branch
  * more generally, might need some DAG concept for merging transactions

The workspace idea might be supported by a simple division: Either a transaction may have branches (thus operating as a trunk), or it may receive active edits (thus operating as a workspace), but never both. This might allow for more specialized treatment of active sessions, user information, user awareness. Also, a more blunt treatment of undo histories and so on.

The idea of a 'buffering' transaction is possible, corresponding to something like commits on a machine that haven't been pushed yet. But I think that's a bad idea for awareness purposes. We want sessions to be very close to the head, such that we can have useful 'feelers' for possible conflicts. OTOH, can we still have commits within a session, not pushed to the mainline? I'm not sure what that would mean in a formal sense. Perhaps better to say, we certainly may have *history* within a session, and some general 'undo' support. 

We might need to introduce a buffering transaction temporarily, but it might be a special case, such as merging the mainline with the current session. I think we can handle this as needed, e.g. by injecting a transaction representing the current state of the session.


older content
===================


We could model this conservatively in terms of which word definitions were viewed, or which web-apps were interacted with. But some views will likely be incidental, no real dependencies. (In the long run, machine learning might help us predict which views are relevant to a given task.)

So...

A transaction may view and edit many words. If two active sibling transactions are in conflict (where 'active' might be measured in hours/days since last activity), we should enter a 'yellow' (caution) state. If a sibling transaction is merged transaction is in conflict with a now committed/merged transaction, we enter a 'red' state, and we cannot merge/commit without first accepting changes into the current branch. 

We can choose to never merge/commit and instead treat treat our transaction as a branch, so we don't necessarily want to enter yellow zones for transactions that grow into branches.

Another important concept for






Cross-transaction merges should be possible, i.e. modeling a form of cherry picking. And forking a new wiki should be the same as creating a transaction on the origin wiki as a new base. At any given moment, we have only one 'origin' transaction. 

The idea is that the wiki should really be 

 Users are expected to keep their transactions alive

Anyhow, a single transaction will view multiple behaviors/definitions and edit multiple words. The idea is to ensure this viewing and editing is *atomic*. If someone else edits a word you viewed or edited as part of a transaction, the transaction will enter a *failure state* (we might use colors, like green, yellow, red). In practice, this means someone else committed a transaction before you committed yours.

In context of *logarithmic history*, we'll gradually eliminate intermediate transactions. The assumption is that we'll have few branches, long lines within a branch.

some information about past transactions will be gradually lost in order to improve performance of checkpoints and control growth of state. How should this interact with transactions? Well, we can either force the merge, or we can force the preservation. I think enforcing preservation would not be a big deal in these cases, gradually flattening the history but keeping the branches precise. In this sense, a 'branch' itself serves as a simple indicator that we want to preserve that point in history.





However, because these transactions are at the user level, a failed transaction MUST NOT require starting over. Rather, a user might be asked to explicitly acknowledge updates from other people to viewed items, and merge edits.

Users should also be able to remove 'viewed' items from their transaction, to prevent irrelevant views from interfering too much.

Usefully, users should also be able to see contexts related to the transaction, such as test results and typechecking, and whether 'potential' conflict exists because someone else is involved in an overlapping transaction.

Transactions must be:

* persistent, named, potentially multi-user; capability secure
* optional comments, descriptions, human names, metadata
* hierarchical: option for transactions within transactions
* very long running; efficient for storage or abandonment
* potentially multiple commits, i.e. serving as branches

Allowing multiple commits, i.e. for a transaction to 'continue' under the same identifier, would allow transactions to serve a role similar to branches. A 'commit' is essentially a way to merge changes back into the origin branch. (We can also cherry-pick changes from a given transaction/branch, e.g. bringing in a word, or a word and its dependencies. Actual conflict should be computed.)

Before we can commit/merge



In practice, such transactions may serve a similar role as branches in a DVCS system. We can allow multiple commits/merges into the parent. They can serve a role similar to 'session' identifiers for a user. 

I wonder if some sort of 'transaction temporary' variables might be appropriate, e.g. to keep information about the user's workspace? Hmm. Well, I could just have some pages that keep the transaction UID in the page name, and clear them later. Or I could have a server state element per transaction, which is later cleared when the exponential history gets around to it.

Usefully, a lot of 'small' edits *within* a transaction may be completely eliminated when that transaction commits. This can potentially reduce memory burdens a great deal. Though, I'll still need to think about how branches interact with transactions and exponential history.

Also usefully, transactions may offer some extra security. We don't need to worry about run-by use of PUT to update the dictionary with spam; we can associate every PUT with an authorized transaction.

Yeah... I think this is a very, very good direction to go.
