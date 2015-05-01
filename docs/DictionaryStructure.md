
## General Goals

Related: see the [Extensible Syntax](ExtensibleSyntax.md) and [branching dictionary](BranchingDictionary.md) documentation. I've essentially reinvented AO language as bytecode all-the-way-down.

Other general goals include:

* maintaining 'clean' dictionaries
* detecting errors early and easily

Paul Chiusano is pursuing an interesting idea where some modules or terms are identified by secure hash, though displayed using metadata and nicknames [(1)](http://pchiusano.github.io/2015-04-23/unison-update7.html). Internally, a secure hash is used. I think this is an interesting possibility. 

Perhaps I could use a subset of 'frozen' words via `{#resourceId}` at the dictionary layer (or maybe `{%#resourceId}` or even `{%hash:resourceId}`), which may then reference other frozen words. Modifying a frozen word does not affect the current clients of that word, but instead creates a new word. A dictionary would also contain mutable metadata for each frozen word, or metadata could be associated with users, or both.

The difficulty is that these hashed resources require an entirely different set of tools and paradigms for effective development and refactoring, require interactive IDEs for editing, etc.. Is it worth developing both sets of tools, and teaching multiple models of development? 

The existing whole-dictionary fork and merge model is probably sufficient for most use cases, and is probably better for an interesting class of 'dictionary applications' that treat dictionaries like mutable spreadsheets or notebooks using compilation as dataflow.

## Validation

For the moment, I'm seeking a weak validation criteria:

1. parse
2. acyclic
3. deeply defined 

This is the bare minimum to ensure every AO word compiles into something (even if it's just nonsense). Later, I will validate features like: tests still pass, words still typecheck, etc.

Validating parse is trivial. Validating acyclic is easy: a simplified compilation recompile against the dictionary will do the job. To validate that I don't delete a word that someone else is using, I'll need either a reverse lookup or a reference count. Later, to determine which tests must be rerun and which programs must be re-typechecked, I'll certainly need a reverse lookup.

