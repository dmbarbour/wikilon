
This is a high level overview of the design. 

* Wikilon *typically* hosts multiple wikis.
 * Initialize with one or two new wikis (e.g. /admin, /sandbox).
 * Admin has special control over the toplevel namespace.
 * Admin will typically fork or replicate external wiki
 * Access admin capabilities by URL printed to stdout

* Awelon Object (AO) is the primary language for Wikilon.
 * Wiki pages correspond to definitions of AO words.
 
* Dictionary is implemented as a bounded series of transactions.
 * Exponential decay gradually collapses transactions.
 * Each transaction has a bunch of useful monoidal metadata 
  * time ranges: session, proposed, accepted
  * loss heuristics: count of transactions, overlapping items
  * contributors: set of names?
  * etc.
 * Each transaction primarily defines, deletes, and renames words.
  * Potential for logical forks?
   * fork a list of words, and all their dependencies, with new suffix.
   * might be too difficult to merge transactions, though; global reasoning
 Need to consider merge properties.
  * may add other primitives later.
 * Each transaction is named by secure hash (content-addressed).
  * no need to ever 'update' a transaction
  * may garbage-collect unused transactions
  * consider keeping transactions outside of acid-state
  * use acid-state for transaction reference counts?
 * See also [TransactionNaming](TransactionNaming.md)

* Compression?
 * Consider using the `zlib` package for compression of transactions.
 * Don't need specialized LZSS except for ABC resources; implement later.

* A 'Wiki' is a central locus for much stuff
 * interaction with distributed replicas of wiki
 * work distribution across replicas
 * issue trackers
 * many user 'sessions' to edit the wiki
 * potential buffer of proposed transactions
 * support communication between sessions
 * caching, memoization, reactive updates
 * support for toplevel, long-running services
 * policies for new guests
 * scheduling, vats
 * chord/tapestry/etc. based routing

* A 'Session' is a user's interactive view of Wiki
 * user preferences: rendering options, shortcuts, etc.
 * words defined but not committed to transaction
 * alerts of pending conflicts with edits by other users
 * track interactions, suggest new automatic tests
 * state for web-applications and volatile services
 * unlike wikis, sessions can become 'inactive'


Design constraints:

To replicate or fork a wiki should generally be possible if you have a session ID. Consequently, the permissions of your replica would be based on the permissions of your session.



