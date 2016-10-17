
# Editable Views for AO

[Awelon Object (AO)](AboutAO.md) code can be edited by humans, but is not optimized for that use. AO is still a bytecode, designed to be unambiguous and easy to process. Instead, the expectation is that humans will manipulate AO through an editable view - an efficient and aesthetic presentation of code that may be edited in place, where the edits can be translated back into bytecode. 

This idea is close in nature to [projectional editing](http://martinfowler.com/bliki/ProjectionalEditing.html), with an AO dictionary as the storage representation. Ideally, the same dictionary would support user-defined projections and editing utilities.

I'll start with a concrete editable view - claw - and generalize from there. 

## Command Language for Awelon (Claw)

Claw is a Forth-like editable view of AO optimized for short one-liner programs or REPL-like scenarios. Claw focuses on lightweight support for numbers and small texts. Like Forth, Claw doesn't scale nicely beyond a dozen tokens (words, atoms, numbers, small texts). But we can construct larger applications from smaller words.

Claw optimizes for:

* AO dictionary words `foo swap bar`
* number data `42 -7 2/3 3.141 2.998e8`
* inline texts `"hello, world!"`

Numbers in claw receive the bulk of attention:

        42      →   #42 int
        -7      →   #7 int negate
        2/3     →   2 #3 ratio
        3.141   →   3141 #3 decimal
        -2.7    →   -27 #1 decimal
        0.001   →   1 #3 decimal
        2.998e8 →   2.998 8 exp10

Natural numbers can be expressed with prefix `#` as in `#42`, and are simply inlined into the final ABC.

Inline texts have simple constraints. They may not contain double quote or LF, and we'll follow them with `lit`. Multi-line texts are the same as in ABC but require an additional blank line at the start to both simplify alignment and indicate they are not inline texts. As such, `"hello, world!"` shall desugar as:

        "
         hello, world!
        ~ lit

Words are trivially expanded to their token forms, i.e. `foo` to `{%foo}`. However, words that might be ambiguous with other Claw features are presented in their compact forms. A word `{%42}` will 

Additionally, Claw supports `[blocks]`, `{tokens}`, and command sequences `(foo,bar,baz)`. The latter uses a desugaring unified with embedded texts:

        (foo,bar,baz) → [[foo] (bar,baz) y]
        (baz)         → [[baz]     ~     y]

Command sequences are potentially useful for embedding DSLs where each step is handled by an external interpreter. 

## Improving on Claw

Claw as defined is useful. I've used it and found it aesthetic and convenient. Numbers, short texts, and common words are enough to write out quick one-liners and tests. Like Forth, Claw doesn't scale nicely beyond about fifteen tokens for a definition because developers start losing track of context. But, also like Forth, it's easy to factor large functions into smaller definitions.

Claw also has its weaknesses:

* Lack of namespaces or dialects or DSLs. Claw words desugar in a context-independent manner, which is nice for refactoring and unambiguous entry but a problem for *ease* of reading and program entry. 

* Claw doesn't apply nicely after evaluation of code. Words like `int` and `decimal` may be eliminated by the evaluator and not be reintroduced by an accelerator. Ideally, we could tune Claw based on accelerators available to a dictionary and also view the evaluation output.

To improve on Claw, we might need to define our editable views within our dictionary. With AO, it's easy to specify a dictionary around (via secure hash) in order to provide an unambiguous dialect in which we might view code. 

