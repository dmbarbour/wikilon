
# Extensible Syntax

Ideally, syntax is both meaningful to humans and convenient for the problem domain. But since there are a lot of problem domains, I think the only way to ensure the latter property is to develop *extensible* syntax. I have at least two ideas on how to approach this.

## Syntax as Sugar

My work on [command language for awelon (claw)](CommandLine.md) offers a viable basis for extensible syntax, based on bi-directional translation between high level code (for humans) and lower level bytecode (for machines). I currently focus on numbers (integers, decimals, ratios) and inline literals appropriate for a command line, but this same idea could be extended to structured programming (e.g. recognizing `[cond] [body] while_do_` as a while loop, and the like). 

This approach has an advantage of simplicity. Simplicity shouldn't be underestimated. OTOH, this does limit our ability to represent and manipulate ad-hoc objects within the dictionary, and it pushes all staged evaluations to the optimizer. Though we may use annotations to guide partial evaluations.

## Staged Definitions

We could represent staged definitions via type:

        type Def a b = ∃v.∀e. e → ([v→[a→b]]*(v*e))

Here, the `v` serves a role similar to an AST, while the `[v→[a→b]]` function becomes a compiler for this structure. The resulting `[a→b]` function is the *meaning* of the word. Trivially, a lot of definitions could have form `[meaning][]`. Other than the trivial case, the compiler function would typically be refactored into a single word such as `[{%fooLang}]`. 

The value `v` becomes 'syntax' if we render the value `v` for the users, and manipulate it directly.

This technique allows us to functionally manipulate definitions. It also provides a structural guarantee for staged programming. Representing unit tests within a dictionary becomes trivial. OTOH, this is also more complex, both for processing and for comprehension. Refactoring code, and the concatenative nature of Awelon, are less obvious.

## Thoughts

I've experimented with the staged version, but I don't like the extra processing and complexity it introduces. It seems worthwhile to pursue the bi-directional translations as far as they can go. But I still like the notion of manipulating embedded objects. Perhaps that can still be done as a dictionary convention, e.g. using separate words for separate stages, one function to build definitions and another to compile them. 

## Decision

I've decided to favor simplicity. Definitions will *not* be staged. Words will refer directly to the definition.

