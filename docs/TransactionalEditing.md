I want transactional editing, DVCS-like features, while sticking with the live programming environment.

DVCS detects write-write conflicts only. We need read-write conflicts could help keep types checking and tests passing. The concept 'read' here must be taken somewhat liberally. We're really saying that we depend on a given behavior. Unfortunately, it's difficult to precisely determine which behaviors program depends upon. A conservative estimate would work for now, and we might later try machine learning to help identify low-risk vs. high-risk dependencies.

I understand Git well enough at this point. I should probably read about [Darcs](http://en.wikibooks.org/wiki/Understanding_Darcs) to get more ideas. But some ideas:

* within any given transaction, a word must have *exactly* one definition
* we want the main history for a wiki (or any branch!) to be 99% linear
* transactions double as DVCS-like branches
* transactions must interact with exponential decay of history
 * branches should prevent decay of their origin
* want to observe tests, typechecks, web-apps, etc. behavior on many branches
* Wikilon will support multiple users; need multiplicity for working sets
 * each working set gives user near-illusion of working alone
 * potentially allow multi-user CSCW workspaces, on voluntary basis
 * even in single-user workspace, maintain awareness of sibling transactions
  * yellow zone: proposed transactions are active and in conflict
  * red zone: conflicting transaction committed; adjudication required
 * never required to restart a transaction, only to either:
  * repair before commit (may cherry-pick commits!)
  * turn it into a new branch
* A new Wikilon instance will often inherit from an existing one!
 * treat this as adding a parent to our 'master' plus a remote branch
* I want the ability to cherry-pick updates from other wikis and transactions
 * e.g. supporting import, ad-hoc renaming, synchronizing more remote branches
 * might need some DAG concept for merging transactions
 * alternatively, support cross-branch word references and transactions
* guests can start a 'new' session, albeit only on a guest-friendly branch 
 * may commit on this branch like normal
 * can later upgrade session via properly registering
* branches and sessions have capabilitity IDs

The workspace idea might be supported by a simple division: Either a transaction may have branches (thus operating as a trunk), or it may receive active edits (thus operating as a workspace), but never both. This should also allow for more specialized treatment of active sessions, user information, user awareness, session variables. Also, a more blunt treatment of undo histories and so on.

The idea of a 'buffering' transaction is possible, corresponding to something like commits on a machine that haven't been pushed yet. But I think that's a bad idea for awareness purposes. We want sessions to be very close to the head, such that we can have useful 'feelers' for possible conflicts. OTOH, can we still have commits within a session, not pushed to the mainline? I'm not sure what that would mean in a formal sense. Perhaps better to say, we certainly may have *history* within a session, and some general 'undo' support. 

We might need to introduce a buffering transaction temporarily, but it might be a special case, such as merging the mainline with the current session. I think we can handle this as needed, e.g. by injecting a transaction representing the current state of the session.

