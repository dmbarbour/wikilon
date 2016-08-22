
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

Extra properties that would be convenient:

* editable views even after computation
* guarantee termination of computation
* predictable space requirements
* static, flexible, dependent types
* unification of types and values
* lightweight parallelism
* easy, high quality, fast compilation

Some thoughts regarding **original ABC**, things to tune/fix:

Separation of code (blocks) and values (products, sums, numbers) seems to hinder uniform rendering of computations, complicates partial or lazy evaluations, and effectively limits streaming evaluation to just the right-hand side of our program. In turn, that limits parallelism, e.g. a computation "modifies state" instead of "processes and produces streams".

It would be most convenient if command sequences (e.g. in [claw](CommandLine.md)) could be conveniently rendered within ad-hoc computations, i.e. such that I'm using standard formations rather than linking words like `{%after}`. It would also be convenient if *texts* were understood as streamable command sequences rather than lists.

Substructural types are cool, but it's difficult to get the strong typing correct in a dynamic context... especially in presence of lazy/parallel futures. It might be worthwhile to shift these to annotations like `{&rel}` and `{&aff}` and just make a best effort at enforcing them dynamically (weakly typed). Static type checks can enforce them more thoroughly.

Flat composition of the form `[A][B] → [A B]` is generally O(sizeof(A)). I can implement this in O(1) as `[[A] inline B]`. But it might be wortwhile to make this explicit by default.

Effective support for *futures* would be convenient, both for parallelism and integration of side effects. I'm not certain this is worth pursuing directly at the bytecode level. But a variant on `runAsync` could make for a nice monad to accelerate in the style of Haskell's `runST`. I'd also like to look into optimistic/speculative parallelism techniques suitable for pure functions. For such features, I might need to pursue a static type system more rigorously.

Having forty some basic ops gives okay base performance but fails to create sufficient pressure for accelerators or ABCD extensions. It may prove better to instead use a smaller 'base' and get started on extensions earlier.

## ABC Alt Proposal

