

* Multiple [branching dictionaries](BranchingDictionary.md)
 * one branch per virtual machine (or set thereof)
 * one branch per user and purpose
 * lots of push/pull content migration between branches
 * easy import and export of dictionary or full history
 * logarithmic history, exponential decay for all branches
* [Extensible syntax and structured editors](ExtensibleSyntax.md)
 * Word defined by pair: structured value and compiler function
 * Both elements of pair are encoded in Awelon Bytecode (ABC)
 * tokens of form `{%foo}` access words from the dictionary
 * AO language corresponds to editing ABC blocks
 * Leverage other languages via `{:fooType}` sealers 
* Compression?
 * leverage structure sharing between versions
 * potential for structure sharing of large values
 * potential for automatic refactoring within dictionaries
 * favor specialized LZSS-style encryption for ABC?
 * probably not immediately critical 

* Issue trackers
 * somehow relating multiple branches? 
 * maybe take inspiration from github here

* Abstract Virtual Machines
 * abstract network and persistent data
 * host for any stateful applications
 * bound to dictionary branch for live programming
 * user sessions also modeled by AVMs
 * consider modeling chord/tapestry/etc. routing
 * replicate AVMs for development and testing

* Accounts and Administration?
 * guest policies? accounts?
 * create new administrative accounts?

* Replication and Sharding?
 * Wikilon as cloud host or control software?
 * Unikernels?


