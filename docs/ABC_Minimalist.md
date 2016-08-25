
# Minimalist ABC

A simpler ABC is (hopefully) a better ABC.

THE BIG IDEAS: 

* confluent, context free rewriting combinators
* minimal base plus early start on accelerators
* elimination of any non 'program' data types
* evaluation has uniform type program → program
* evaluation may proceed to fixpoint or quota

Consider some code:

        [A] swap [B] [C] swap [D] cons
            -- evaluates to...
        [A] swap [C] [[B] D]

This looks and acts *a lot* like a stack language. However, there is no stack per se, just a program under evaluation by rewriting. A significant difference is that there is no *stack underflow*. When we do not have enough arguments to the left, no rewrite occurs. Simultaneously, our runtime is free to perform deeper rewriting. 

Potential benefits:

* easily evaluate and cache computations without context
* renders and views can focus on a common program type
* lazy blocks by default, no need for paired annotations
* pipeline processing implicit, much latent parallelism
* just one type simplifies compaction, stowage, sharing 
* toplevel doesn't need balanced `[]` for evaluation.

This idea for a minimalist ABC seems very promising. 

However, I have some concerns regarding:

* effective external systems integration
* controlling scope and program structure
* error recognition and static type safety
* performance, optimization, compilation

I must ensure these concerns are readily handled (or at least no worse than the ABC I'm replacing) before I commit.

## Systems Integration with Minimalist ABC

It's necessary for Awelon's vision to support efficient and predictable integration with external systems. In a broad sense, such integration includes data input, extraction of results, and use of time and space. Rendering of intermediate computations may also be part of this integration. If these things aren't efficient and predictable, there is no way minimalist ABC could become a practical language.

I posit that I should minimally support:

* small numbers and basic arithmetic
* embedding and construction of text

Within the 'minimalist' goal of having only one data type - a program - these values should themselves be understood as programs that we can evaluate. I can easily eschew primitive support for negative integers. And if I'm just interested in natural numbers, I can easily give those *meaning as programs* by treating them as shorthand for a Church encoding. 

        [B][A] #2 i == [[B][A]#1 i] A   == [[B]A]A
        [B][A] #1 i == [[B][A]#0 i] A   ==  [B]A
        [B][A] #0 i == B

This encoding can work. But I'm not committed to it. I'm contemplating whether I can unify *Numbers, Literals, and Command Sequences* (see below). If I can do so, I'll probably go with whatever I come up with there. Regardless of the specific encoding, I assume we will be able to Church-encode useful arithmetic operations: successor, predecessor, addition, subtraction, multiplication, divmod, etc.. By accelerating these arithmetic operators, our performance won't be hurt by the Church encoding. 

Conveniently, number types also act as a basis for conditional behavior, i.e. selecting the `A` vs. `B` behavior.

For consistency, we can leverage a similar model for texts. Something like:

        [B][A] "hello" i == #104 [[B][A] "ello" i] A
        [B][A]  "ello" i == #101 [[B][A]  "llo" i] A
        ...
        [B][A]     "o" i == #111 [[B][A]     "" i] A
        [B][A]      "" i == B

The original ABC's embedding of text is pretty useful, so I'll probably stick with it. I don't want to sacrifice legibility, so I'm still leaning in favor of 

*Aside:* I might explicitly use the UTF-8 expansion for text, e.g. such that `→` expands as three bytes rather than one larger codepoint. This would be more convenient for many use cases, especially those involving construction of maps, tries, hashes, etc.. It's generally more convenient for efficient processing and extraction.

### Effect Models Integration

A good effects model for purely functional programming must support interception of effects, such that we can evaluate our program in a simulated environment or wrap and adapt the effects to a different context. Minimalist ABC mustn't compromise this, so any effects models must be at the value level, operating only on the right-hand side of the program stream. We could model effects via a monadic `[Continuation][Request]` pair, for example.

## Controlling Scope and Structure

Controlling scope and structure of evaluation is very useful for comprehending programs and efficiently isolating errors. Ultimately, I think minimalist ABC does a better job in this area than the original flavor.

### Scoped Application

Minimalist ABC can easily hide information from a block on the toplevel by use of combinators like `dip`. Additionally, we can build computations within blocks by injecting content via `cons` or `cat` then using an annotation like `{&par}` or `{&seq}` to encourage immediate evaluation.

        [A][B]dip   = B[A]

        [A][B]cat   = [A B]
        [A][B]cons  = [[A] B]

        [A]{&seq}   = [A]       (evaluated)
        [A]{&par}   = [A]       (pending evaluation)

Between these mechanisms, developers have effective control over scope. 

### Discretionary Value Sealing

We can use paired sealer and unsealer tokens of form `{:x}` and `{.x}` to resist *accidental* access to a value's structure, i.e. providing a lightweight type wrapper of sorts. For *correct* code, removing sealer/unsealer pairs should have no effect (except perhaps to improve performance). But incorrect code will generally fail fast. And even if something funny is happening, our developers can easily search a codebase for tokens to isolate where the error might have been introduced.

Discretionary sealing is adapted to minimalist ABC by a simple pair of rewrite rules:

        {:s}{.s} == 
        {.s}{:s} ==

We must use the tokens within blocks `[{:x}]` for scoped application of seals. Also, while the latter rewrite rule is based on a valid optimization (unseal then seal with the same token is effectively a non-operation), it does improve our expressiveness because it means we can freely grow even sealed subprograms from both the left and the right. 

### Weak Substructural Types

Substructural types are very useful for structuring the *behavior* of an application, independently of structured syntax and data models. Minimalist ABC won't provide strong enforcement for these types, but will provide enough to guard against accidental errors. The proposal is as follows:

        [A]{&rel} == [A]    (mark relevant)
        [A]{&aff} == [A]    (mark affine)
        [A]{&nss} == [A]    (clear marks)

* a block marked relevant may not be dropped
* a block marked affine may not be copied
* a block both affine and relevant is called 'linear'
* block compositions preserve substructure

We could model a general copy or drop function that ignores substructure by prefixing with `[] cons {&nss}` (and unwrapping our copies). But the weak types could help developers isolate many errors.

## Recognizing Errors and Static Type Safety

Early detection of errors is very useful for software engineering. Ideally, we want to find errors before evaluation, or even during edits. If not that, we need to recognize error conditions dynamically, during evaluation, such that we can report, flag, count, and highlight the errors.

With term rewriting, we might understand an error as a 'stuck' computation - a computation that produces no values, no matter how many inputs it receives.

### Programmer Specified Errors

Programmers might specify errors to indicate:

* incomplete programs (todo some day soon!)
* incorrect use (partial functions)
* assertion failures (implementation error)

We could simply use an `{&error}` annotation for programmer errors.

        "todo: implement foo"{&error}

The annotation creates an *error value*. It can be copied or dropped like any other value. An attempt to observe the error value (e.g. through application) will cause our computation to become 'stuck' at that point. 

### Dynamic Error Recognition

Dynamic error recognition needs to be cheap or rare (ideally both). The most obvious candidate for dynamic error recognition is runtime type assertions for data that our runtime recognizes specifically. This might be expressed by annotation. For example:

        #42 {&nat}
        "hello" {&lit}

Our runtime could easily extend this set with any special representations it knows about, especially those it optimizes. A static type checker could later use these to guide inference, and remove redundant dynamic checks.

We can recognize errors due to substructural typing and value sealing. With substructural types, we just test on copy/drop whether our block value is marked. For sealer errors, we simply need available input to the token's left and an action that needs input to its right. 

        [A]{&aff} copy
        [A]{:s} action

It is also feasible to also make dynamic assertions regarding *arity* of functions (how many inputs it will observe), and more generally we could specify some lightweight stack-effects, which might reduced to a "X inputs and Y outputs" for a fast dynamic error check.

        [swap]{&arity/2}
        [swap]{&stack(ab→ba)}

As examples of relatively rare dynamic error recognition, we might utilize equivalence assertions when expressing tests.

        [A][B]{&eqv} ==  [A][B]   (if structurally equivalent)
        [A][B]{&beqv} == [A][B]   (if behaviorally equvialent)

Structural equivalence can be tested directly. Behavioral equivalence needs a rather more sophisticated analysis. We probably wouldn't want to use these annotations outside of 'leaf' expressions in the codebase.

Marking errors uniformly within our code will be convenient for any post-processing. Thus, dynamically recognized errors should generally be wrapped and marked by our runtime. 

        #42 {&lit}      == [#42 {&lit}]{&error} i
        [A]{&aff} copy  == [[A]{&aff} copy]{&error} i

### Static Type Safety

We can analyze our codebase, infer composable metadata for every word - a type description - and cache all this information. This metadata is potentially useful for many UX purposes (e.g. type driven lookup when editing code). But one benefit is the ability to rapidly detect probable errors for a *new* expression of code, without evaluating it.

At the very least, we can get started with a 'stack effect' basis for types. 

Not all functions will be amenable to type checking, e.g. variadic functions, dependently typed functions. But I think a large enough codebase can be usefully typed, and we can describe for each function what 'tests' it passes.




## Performance and Implementation

### API

The modified API will operate on a context that contains a program. 

We'll grow the program (towards the right), evaluate it with a quota, and extract results. I'll allow for growing internal programs by treating `[` and `]` as operators (more or less). It might also be feasible to capture the current program as a block, or serialize a program to a text. 

Injecting input to the left of a program will be feasible by a *move* semantics, taking a block off the right of one context and inlining it to the left of another (or the same) context.

### Bit Representations

A pointer has spare bits that are convenient for tagging and embedding of values. For my original ABC, these bits were oriented around pairs, sums, and small integers. With minimalist ABC, any focus for tight representation would instead orient around natural numbers, opcodes, tokens, and block embeddings. In both cases, the general escape for 'tagged' objects, allowing me to handle arbitrary special cases (like stowage or texts) is essential.

Candidate representation:

        x01     small naturals (0..999999999)
        011     other small constants 
        111     common operation codes

        000     blocks (cons cells)
        010     tagged objects

        100     (available)
        110     (available)

Tagged objects can fill most of anything I miss. The last two options are for performance advantages where saving 4 bytes per allocation is frequent enough to be a worthy effort. Tokens are a possibility, if I use enough *small* tokens to matter. Maybe reverse-ordered lists during evaluation.

### Program and Stack Representation

I'm leaning towards use of linked list based 'stacks' and 'programs' as the normal mode. We might use some zipper concepts to preserve a 'cursor' to wherever evaluation is occurring.

Use of 'compact' bytecode representations, and eventual JIT representations, will be valuable for efficient evaluation, albeit mostly in cases where evaluation can proceed to completion. For incomplete evaluations, a lot of allocations will be needed.

### Compilation

### Par/Seq

## The Minimal Bytecode




## Numbers, Literals, and Command Sequences

An intriguing possibility is that *natural numbers* and *embedded texts* might be modeled as specific instances of command sequences. Command sequences are useful for concise data representation, DSLs, monadic effects, modeling cooperative multi-threading, etc.. If we can unify the two concepts, that would be convenient both didactically and for uniform utility, accelerators, etc..

With [claw command sequences](CommandLine.md) our command sequence is given a general meaning: `[foo,bar,baz] == [[bar,baz] after foo]`. I've also experimented with the variant `[foo,bar,baz] == [foo [bar,baz] yield]`. In both cases, `foo` is not wrapped as `[foo]`, which is convenient because it means we do not need to distinguish `foo` 
 Notably, `foo` is not wrapped up as `[foo]`, and we do not explicitly terminate the sequence.

, such as we don't need to think hard about an *empty* command sequence vs. 



A challenge for command sequences is distinguishing a sequence containing a single empty command (the identity function) vs. a sequence containing an empty command. 

The main difficulty with command sequences, of course, is distinguishing an empty command 


        "hello" == (#104, #101, #108, #108, #111)
        "" == ()
        #3 == (,,,)
        #0 == ()

            generalized behavior:

        [B][A](foo,bar,baz) ap == foo [[B][A](bar,baz) ap] A
        [B][A]() ap == B

A potential difficulty with this is distinguishing an empty sequence `()` from a singleton sequence with an empty command, e.g. `(baz)`. 




This isn't a bad model for command sequences. It can iterate without a fixpoint loop. It can't capture the continuation implicitly, but we can model continuatio passing explicitly if we so desire. At the bytecode level, such command sequences might be represented as `[[foo][[bar][[baz][e]c]c]c]` with appropriate `e` and `c`. 

But that isn't the only option. We could instead generalize as:

        "hello" == (#104 C, #101 C, #108 C, #108 C, #111 C)
        #3 == (t,t,t,)




A variant model producing `[[B][A](bar,baz) ap][A] foo` would also work well, more sensitive to the type of `foo` and `A` but also more expressive due to implicit continuation capture

a bit more expressive but also )


OTOH, this is not the only option. With a tweak to how we model texts, we could be using:

        [B][A](foo,bar,baz) ap = [[B][A](bar,baz) ap] foo A

This option requires `A` to be aware of the type of commands like `foo`, and for commands like `foo` to be aware of the continuation.

*Note:* ABC will not include a syntax for command sequences. They aren't essential for systems integration, and the plain bytecode representation will be efficient enough (especially with appropriate accelerators). Instead, command sequences are presented as *editable views* of bytecode, such as with [claw command sequences](CommandLine.md).