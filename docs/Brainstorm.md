
# Ideas of Dubious Utility

Just a scratch pad for ideas I'm contemplating but not committed to in context of Awelon.

## Automatic Definitions

Awelon currently provides automatic definitions for:

* natural numbers `[1-9][0-9]*`
* embedded texts

I find it tempting to introduce more general support for automatic definitions, e.g. such that we could derive `cddadr` from `car` and `cdr` - something like `c{[ad]+}r`. Or data rearrangement words such as `xyz--zzyyxx`. Or support for large columns of automatic definitions when modeling a spreadsheet.

There are a several points that have me resisting automatic definitions:

* introduces potential for naming conflicts
* hinders reverse lookup indexing
* which may hinder incremental computing
* overlaps with use cases for editable views

My intuition is that this isn't a great idea. I do like the idea of modeling 'spreadsheets' with automatic columns via Awelon dictionaries. But perhaps that should somehow be modeled at the projectional editing layer. I'll need to see how far the [application model](ApplicationModel.md) can get without automatic definitions.

## Lazy Memo Copy

The idea with lazy memo copy is to evaluate `[A]c` to produce `[A][A]` while tracking this equivalence relationship under the hood via use of aliasing. When either copy is evaluated, the other will also receive the same benefits of the evaluation effort. 

But this is a fragile optimization. It won't survive serialization, distribution, persistence. It may also complicate program debugging snapshots a little, since we'll need to distinguish snapshot copies (which cannot share further memoization of evaluation) from normal copies. This latter point could be mitigated by using the variable extraction algorithm when rendering with lazy values.

Further, we cannot rely on lazy memo copy for laziness for the serialized output anyway, thus it won't work nicely as a basis for coinductive data and incremental computing.

These weaknesses had me instead favor arity annotations to defer computation and independent `(memo)` to share computations where possible, with memoization having a more robust basis than aliasing in memory. The cost is expensive memoization and laziness that requires explicit attention.



