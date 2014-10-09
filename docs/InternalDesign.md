
This is a high level overview of the design. 

* Awelon Object (AO) is the primary language for Wikilon.
 * Wiki pages correspond to definitions of AO words.
 
* The AO dictionary is edited transactionally.
 * Multiple words are updated in one transaction.
 * Sessions may try to track read-write conflicts.
 * See also [TransactionNaming](TransactionNaming.md)

* Wikilon shall maintain good dictionary state.
 * Committed AO words shall be required to parse.
 * May later require tests pass and code typechecks.
  * i.e. don't break anything that's not already broken.
 * Intermediate code remains in non-committed state.

* 

I'm using Awelon Object (AO) as the primary language for Wikilon. Wikilon pages correspond to definitions of AO words. Wikilon



The elements:

* Dictionary 
* Branch
* Project
* Session
* 
* 




However, ABC will also be accessible.





