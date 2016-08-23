
# Can I find a better baseline code?

A lot of Wikilon's goals could be accomplished with alternative languages to ABC, assuming the basics are preserved. 

What are the basics?

* values represented by software
* purely functional programs
* functions are first class values
* simplified syntax, editable views
* large value stowage, database values
* apps via multi-agent codebase edits
* app states are values, composable
* minimal boiler plate for programs
* streamable code, to model user actions
* easy control of program structure

Extra properties that would be convenient:

* editable views even after computation
* weakly legible programs, debug text dump
* guarantee termination of computation
* predictable space requirements
* static, flexible, dependent types
* unification of types and values
* lightweight parallelism
* easy, high quality, fast compilation

ABC's weaknesses as it exists today:

* Separation of code from values (products, sums, numbers) complicates. 
 * extra operators in my 'primitive' bytecode
 * must provide arguments/contexts for evaluation
 * need paired annotations for value stowage
 * need paired annotations for parallel futures
 * both editable views of code and rendering of values

* Substructural types are great, but...
 * dynamic checking difficult with lazy or parallel futures
 * might be better to use annotations, weak type assertions

* Flat composition `[A][B] → [A B]` is O(sizeof(A))
 * can use `[[A] inline B]` for O(1). 
 * might be worthwhile to make explicit.

* Embedded texts as *linked lists* complicates things.
 * cannot easily compose, abstract, rewrite, or generate
 * consider instead [claw-like command sequences](CommandLine.md)

* Command sequences are AO only, depending on word view.
 * command sequences are a general computing feature!
 * ideally, we can do command sequences at ABC layer.

* Direct use of unicode default codepoints is problematic
 * results in very 'wide' tries or maps without 

* Serialization of fork/join futures is awkward.
 * must reconstruct parallel evaluation context.
 * futures cannot be allowed to escape context.
 * complicates interaction stowage, distrib, etc..

* Static type safety needs more attention.
 * higher order types, would like to accelerate a `runST` equivalent
 * consider explicit futures, pure single-assignment promises `runAsync`

## Proposal: Minimalist ABC

See [ABC_Minimalist.md](ABC_Minimalist.md)
