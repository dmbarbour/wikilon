# Static Type System for ABC and AO

While ABC programs are purely functional, special attention is required for:

* handling multiple inputs and outputs
* tracking substructural data types

What should our type descriptors look like? 

For multi-value operation, one viable option is to type the program *as a whole* essentially as a stack of values: `(A * (B * (C * ...)))`, and essentially use row-type polymorphism. This would be a decent choice because it would simplify use of existing type systems. The adaptations necessary for row polymorphism are reasonably well understood.

Tracking substructure additionally requires tracking whether we might copy or drop a value, and propagating this towards the input types. And we might also track whether we mark a value affine or relevant, and propagate that towards the output types.


## Brainstorming


Several languages use `A → B` to describe a function type, where `A` and `B` are value types. However, this seems a bit awkward in context of multiple values. For a literal, `A` might be empty. For drop actions, `B` might be empty.




In context of ABC, it would be most convenient if type descriptors are concatenative. That is, if `A` is a type description and `B` is a type description, then `A B` represents a type description for the composition of the functions. This would allow type checking to proceed (more or less) as a simple evaluation but with type descriptions instead of values. For example, given:

        [B][A]o == [B A]        (o = [wai]bb)

In this case we want `A` and `B` to work equally as values or type descriptions. Each of the primitives can be simply given a type descriptor more or less equivalent to the rewrite rule. 

Concatenative type descriptors will require that type descriptors themselves have simple evaluation rules. This requires some careful attention to how our types might be constrained. Consider if we use the infix `→` but also prefix each program type with `|`. 

        | X → Y | Y' → Z

In this case, our composition of `(| X → Y)` and `(| Y' → Z)` should support elimination of `Y | Y'`, perhaps unifying the two, then reduction of `→|→` to just `→`, ultimately resulting in `| X → Z`. But this seems rather awkward. We might instead use positive and negative types and drop the `→` in the middle, but we still need someplace for our evaluator to keep unification metadata. A stack, perhaps.

 In that case, 

 single vector



Optimally, type checking should be *bidirectional*, such that we can infer type structure just as easily from contextual use of a value as from its construction.







It seems to me that use of the arrow `→` in the type descriptor is awkward. It would be convenient if type descriptors are concatenative just like the ABC programs they describe. But we



The positional arrow at the center of that `A → B` description is slightly awkward.


At the human level, I would like to support human-meaningful, symbolically named types. This may require annotations to support naming types, or use of `word.type` declarations to do the same. 

I would also like to support human meaningful, symbolically named types.

 This might be achieved by use of `word.type` declarations.


I would like to support *bidirectional types* in general. 


As a general rule, type checking proceeds by inductive composition of 



That is, a program can be given a type `(A → B)` where `A` and `B` describe values.

However, the support for multiple inputs and outputs



## Sum Types

## Dependent Types
