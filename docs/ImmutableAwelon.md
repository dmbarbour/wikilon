# Immutable Awelon

Paul Chiusano's Unison Web project takes an interesting approach to modeling a codebase: instead of mutable definitions (e.g. via files in a filesystem), a program only ever references external code via self-authenticating secure hash. This gives us *immutable* definitions at any given development step. Conveniently, this simplifies indexing, static typing, cached compilation, acceleration, etc.. The advantage is we can assign global meaning for any program.

Awelon as it currently exists uses an immutable *dictionary*, within which definitions are mutable. Consequently, it is the pairing of dictionary-program that is immutable. But there is an important difference: the meaning of programs can change indirectly due to updates within the dictionary. This gives us ad-hoc spreadsheet-like characteristics, but it also complicates caching and sharing of code. 

With immutable code, our "dictionary" is replaced by a "development environment" within which we maintain a bi-directional relationship between secure hashes and local nicknames. References between code always use secure hashes. We may render and edit code using the nicknames in place of secure hashes. To simulate file-like edits, we can provide means to select and to edit multiple locations concurrently.

That's the idea. 

Whether it's a "good" idea is a separate question. The main advantage is that we simplify the sharing of code in a "global" codebase, and we simplify caching because programs and their meanings are deeply immutable. Yet, if we envision use-cases like a Wiki, then support for spreadsheet-like characteristics isn't necessarily bad. Awelon's support for embedded dictionaries (together with *localization*) can provide similar benefits for sharing. If we explicitly model a (codebase,compiler) pair in the immutable language, we also get similar properties where changes to the codebase can require significant recomputation. 

One point to consider is how to represent embedded data. It's feasible to use a `succ` and `zero` interfaces to construct natural value types within the local namespace of a module. Otherwise, we could try to add built-in definitions for common data. 

