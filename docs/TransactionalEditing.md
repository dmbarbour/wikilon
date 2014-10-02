
I think it would be very useful to support 'transactions' at the user level.

A single transaction will view and edit multiple words. The idea is to ensure this viewing and editing is *atomic*. If someone else edits a word you viewed or edited as part of a transaction, the transaction will *fail*. However, because these transactions are at the user level, a failed transaction MUST NOT require starting over. Rather, a user might be asked to explicitly acknowledge updates from other people to viewed items, and merge edits.

Users should also be able to remove 'viewed' items from their transaction, to prevent irrelevant views from interfering too much.

Usefully, users should also be able to see contexts related to the transaction, such as test results and typechecking, and whether 'potential' conflict exists because someone else is involved in an overlapping transaction.

Transactions must be:

* persistent, named, potentially multi-user; capability secure
* optional comments, descriptions, human names, metadata
* hierarchical: option for transactions within transactions
* very long running; efficient for storage or abandonment

In practice, such transactions may serve a similar role as branches in a DVCS system. They can serve a role similar to 'session' identifiers for a user. 

I wonder if some sort of 'transaction temporary' variables might be appropriate, e.g. to keep information about the user's workspace? Hmm. Well, I could just have some pages that keep the transaction UID in the page name, and clear them later. Or I could have a server state element per transaction, which is later cleared when the exponential history gets around to it.

Usefully, a lot of 'small' edits *within* a transaction may be completely eliminated when that transaction commits. This can potentially reduce memory burdens a great deal.

Also usefully, transactions may offer some extra security. We don't need to worry about run-by use of PUT to update the dictionary with spam; we can associate every PUT with an authorized transaction.

Yeah... I think this is a very, very good direction to go.
