# Immutable Awelon

Paul Chiusano's Unison Web project takes an interesting approach to modeling a codebase: instead of mutable definitions (e.g. via files in a filesystem), a program only ever references external code via self-authenticating secure hash. This gives us *immutable* definitions at any given development step. Conveniently, this simplifies indexing, static typing, cached compilation, acceleration, etc.. The advantage is we can assign global meaning for any program.

Awelon as it currently exists uses an immutable *dictionary*, within which definitions are mutable. Consequently, it is the pairing of dictionary-program that is immutable. But there is an important difference: the meaning of programs can change indirectly due to updates within the dictionary. This gives us ad-hoc spreadsheet-like characteristics, but it also complicates caching and sharing of code. 

With immutable code, our "dictionary" is replaced by a "development environment" within which we maintain a bi-directional relationship between secure hashes and local nicknames. References between code always use secure hashes. We may render and edit code using the nicknames in place of secure hashes. To simulate file-like edits, we can provide means to select and to edit multiple locations concurrently.

That's the idea. 

Whether it's a "good" idea is a separate question.

The advantage is that we simplify the sharing of code in a global codebase, and we simplify caching because the evaluation, type, etc. may be associated directly with the secure hash. The disadvantage is that maintaining and distributing data becomes relatively difficult. The codebase is no longer the "living sea of data" I desire for Awelon [application models](ApplicationModel.md). The spreadsheet-like characteristic of Awelon dictionaries is convenient for many of Awelon's use cases.

Further, it's unclear how to support convenient embedding of data if we must use huge secure hashes for data constructors. I suppose we could [add first-class modules to Awelon](AwelonFML.md) with local symbols to serve the same role, in which case we have first-class dictionary values.

