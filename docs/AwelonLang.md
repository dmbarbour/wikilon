
# Awelon Language

Awelon is a purely functional language based on concatenative combinators. 

## Why Another Language?

Awelon differs from other languages in its adherence to simplicity and its choice of evaluation model. There are only four simple computational primitives, one data primitive, and a simple Forth-like syntax. Evaluation is by local rewriting with lazy linking.

Awelon's simple syntax lowers barriers for program rewriting as an evaluation model, editable views for HCI, and collaboration with software agents. Program rewriting ensures the evaluation is a program that may be serialized for debugging, distribution, or persistence. Lazy linking preserves human-meaningful words and link structure, ensuring results to be viewed the same as source. And software agents support the application layer effects.

Like any pure language, Awelon's evaluation is separate from its effects model. Awelon explores a [RESTful application model](ApplicationModel.md) where application state is represented within a codebase, and effects are performed in context of a multi-agent system. Relevant effects patterns include [publish-subscribe](https://en.wikipedia.org/wiki/Publish%E2%80%93subscribe_pattern) and a variation on the [tuple space](https://en.wikipedia.org/wiki/Tuple_space) oriented around [work orders](https://en.wikipedia.org/wiki/Work_order). More conventional effects models are possible within orders.

Intriguingly, [editable views](http://martinfowler.com/bliki/ProjectionalEditing.html) can feasibly present Awelon programs or codebases as [hypermedia](https://en.wikipedia.org/wiki/Hypermedia) objects. GUI forms with live coding properties. Words within a act as hypermedia links. Some relationships may be implicit in word structure (as between `foo`, `foo.doc`, and `foo.author`) to support more flexible hypermedia than is implied by direct word dependencies.

Awelon's simplicity, purity, and evaluation model each contribute greatly to its utility for the envisioned application model. I have not encountered another language that fits as nicely.

## Primitives

There are four primitive combinators:

            [B][A]a == A[B]         (apply)
            [B][A]b == [[B]A]       (bind)
               [A]c == [A][A]       (copy)
               [A]d ==              (drop)

Those square brackets `[]` enclose a 'block' of Awelon code, representing a function. With just this much, all computable functions can be defined. As a lightweight proof, I'll define the Curry-Schönfinkel SKI [combinators](https://en.wikipedia.org/wiki/Combinatory_logic).

            [B][A]w == [A][B]       (swap)           w = [] b a
               [A]i == A            (inline)         i = [] w a d
         [C][B][A]s == [[C]B][C]A   (S combinator)   s = [[c] a b w] a i
            [B][A]k == A            (K combinator)   k = a d

Awelon's primitive combinators are more convenient than SKI. Apply and bind provide structural control over scope, respectively hiding or capturing one value. Partial results are available from apply. Separation of 'copy' and 'drop' is convenient both for performance and [substructural type](https://en.wikipedia.org/wiki/Substructural_type_system) analysis. 

## Words

Words are identified by a non-empty sequence of UTF-8 characters with a few limitations. The blacklist is `@[]()<>{},;|&=\"`, SP, C0 (0-31), and DEL. Developers are encouraged to favor words that won't need escapes in external contexts (such as URLs, natural language text, or editable views), and that aren't too large.

A useful subset of words is automatically defined:

* the four Awelon primitive words `a`, `b`, `c`, `d`
* words to encode natural numbers, regex `[1-9][0-9]*`
* secure hash resources - `$secureHash` or `%secureHash`

Other words are user defined, through a dictionary. There is no hard limit on word size, but words should ideally be kept small.

## Dictionary

Awelon has a standard, DVCS-inspired patch based dictionary representation:

        secureHashOfPatch1
        secureHashOfPatch2
        @word1 definition1
        @word2 definition2
        ...

A patch contains a list of patches to logically include followed by a list of definitions. Each definition is indicated by `@word` at the start of a line, followed by Awelon code. The last definition for a word wins, so definitions override those from earlier patches. 

Cyclic definitions are erroneous. Loop behavior must be modeled via fixpoint combinator. However, trivially defining a word to itself as `@foo foo` will be accepted as meaning 'delete the word' rather than raising an error. Undefined words essentially evaluate to themselves.

Awelon's dictionary representation is not optimal for direct use by humans. It can be tolerated in small doses. But it is intended more as an import/export format, and for efficient sharing between humans and software agents. Humans will generally observe and influence a dictionary through an editable view, perhaps as hypermedia. 

## Secure Hash Resources

Awelon has built-in support for identifying resources via secure hash. 

* arbitrary definitions may be referenced as `$secureHash`
* external binary data may be referenced via `%secureHash`
* secure hashes are used to identify dictionary patches

Secure hashes implicitly name immutable structure outside the dictionary, which might be available on the local system or downloaded. If you hear about a secure hash resource from a remote server, you should be able to HTTP request it from that server (with potential redirects). Content-delivery networks are also viable. The resource is readily verified against the secure hash, and easily cached after download. 

In all cases, we use the same secure hash: 384 bits of [BLAKE2b](https://blake2.net/) encoded as 64 characters in [base64url](https://en.wikipedia.org/wiki/Base64).

*Note:* Secure hash resources may reference dictionary words, so have mutable semantics. Only their structure is immutable.

## Data

Awelon language has specialized representations for natural numbers and texts. Numbers are simply implicitly defined words like `42`. Texts have two embeddings, inline like `"hello, world!"` or multi-line:

        "
         multi-line texts starts with `" LF` (34, 10)
         each line is indented by one space (32)
         terminate the text with `LF ~` (10, 126) 
        ~

Texts may be placed only where a word is valid. Texts must be valid UTF-8, forbidding C0 (except LF) and DEL. Inline texts additionally forbid the double quote and LF. There are no character escapes, but the extra whitespace in the representation of multi-line text is not considered part of the text. Texts in Awelon are [syntactic sugar](https://en.wikipedia.org/wiki/Syntactic_sugar) for an expanded representation. 

        "hello" == [104 "ello" :]
        ""      == ~

Awelon language has exactly one primitive data type - the `[]` block of code, representing a function. Data is instead [Church encoded](https://en.wikipedia.org/wiki/Church_encoding) or use similar variants such as the [Scott encoding](https://en.wikipedia.org/wiki/Mogensen%E2%80%93Scott_encoding). Values are thus represented by functions. 

Before I explain encoding of numbers and texts, let us examine encodings for other useful data types. Booleans are perhaps the simplest data. We might encode booleans as follows:

        [onF][onT] false i == onF               false = [d i]
        [onF][onT] true  i == onT               true  = [a d]

        Using Definitions:
           [A] i == A                           i = [] w a d 
        [B][A] w == [A][B]                      w = [] b a   

Booleans can be generalized to algebraic sum type `(A + B)`.

        [onL][onR] [[A] inL] i  == [A] onL      inL = w d w i
        [onL][onR] [[B] inL] i  == [B] onR      inR = w b a d

Construction of `[[A] inL]` is trivial - `[A] mkL` where `mkL = [inL] b`. This is generally the case for value constructors. In practice, we may also wish to defer computation of partial constructions (see *Deferred Computations and Coinductive Data*). 

Pairs - algebraic `(A * B)` product types - may also be Church encoded:

        [onP] [[B][A] inP] i == [B][A]onP       inP = [] b b a i

However, in context of Awelon language, I find it more convenient to encode a pair as `[[B][A]]`. 

Between algebraic sums and products, we can represent any conventional data structure. 

The option type `(1 + A)` may be modeled as a choice of `false` or `[[A] R]`. Natural numbers can be encoded as `μNat.(1 + Nat)` - that is, Nat is recursively zero or successor of a Nat. Given natural numbers, integers can be represented by a pair representing `X - Y`.  We can encode rational numbers as a numerator-denominator pair. We can encode decimal numbers with a significand and exponent. A list can be modeled as `μList.(1 + (A * List))`. Text can be encoded as a list of natural numbers. Algebraic encodings are generally simple, e.g. `Succ = mkR` for natural numbers, or `Cons = mkP mkR`. 

An alternative is to model data as implicitly folding over its structure. The Church encoding of natural numbers is an example of this. Consider: 

        [X][F] 0 i == [X]
        [X][F] 1 i == [X] F         == [[X][F] 0 i] F
        [X][F] 2 i == [[X] F] F     == [[X][F] 1 i] F

        0 = false
        1 = [0 Succ]
        2 = [1 Succ]
        Succ = [c] a [b b] b a i

Folding over a recursive structure that carries no additional data isn't particularly interesting. But we can generalize easily to folds over list structures:

        [X][F] Nil i == X
        [X][F] [[L][A] Cons] i == [[X][F]L] [A]  F

        Nil = false
        Cons = [[c] a b [b] b a] a w i

However, with data structures more sophisticated than natural numbers, choosing a specific fold seems awkward and arbitrary. With lists, we have both left folds and right folds. With trees, we have both folds and a variety of [tree traversals](https://en.wikipedia.org/wiki/Tree_traversal). Further, in context of linear typed structure, it is convenient if we can operate upon or [unzip](https://en.wikipedia.org/wiki/Zipper_%28data_structure%29) just part of a structure without observing the whole thing. I believe the algebraic type encodings are generally superior.

Return attention to numbers and texts in Awelon.

It is my intention that Awelon developers be given relatively free reign over the choice of encoding and corresponding tradeoffs. In practice, this control may be limited by what the runtime accelerates. But the encoding is at least separate from the syntactic embedding.

This is achieved by allowing developers to partially define the encoding: 

        0 (zero) is user definable 
        S (succ) is user definable
        1 = [0 S]
        2 = [1 S]
        ...

        ~ (nil)  is user definable
        : (cons) is user definable
        "" = ~
        "→" = [8594 "" :]
        "hello" = [104 "ello" :]

For reasons of simplicity, Awelon language only provides built in support for encoding natural numbers and text data. This is sufficient for compact embeddings of most data. More sophisticated number types are left to editable views. 

## Binary Data

Awelon language supports reference to external binary data via `%secureHash`. We'll basically use the same text representation, but using values in range `0 .. 255` instead of Unicode codepoints.

For small binaries, we might instead choose to embed the binary within base16 or base64 text, and accelerate conversions between text to the binary. However, in context of Awelon's application model and hypermedia, referencing external binary data can be very convenient.

*Note:* Developers are encouraged to leverage [rope-like](https://en.wikipedia.org/wiki/Rope_%28data_structure%29) structures if modeling edits on large binary data. 

## Acceleration

Acceleration is a performance assumption for Awelon. 

A runtime will recognize and accelerate common functions. The accelerated implementation may be hand optimized and built into the runtime to achieve performance similar to a primitive. For example, many runtimes will accelerate the following functions:

           [A]i == A            (inline)         i = [] w a d
        [B][A]w == [A][B]       (swap)           w = [] b a

The runtime must look at the given definitions. Upon recognizing `[] b a`, the runtime may link `w` to the acclerated swap implementation. Same for `i`. In general, recognition of accelerators will be fragile, in the sense that `[] w a d` might be recognized where the equivalent definitions `[] [] b a a d` or `[[]] a a d` are not recognized. To mitigate this fragility, runtimes should carefully document recognized accelerators, e.g. by defining a seed dictionary. But, assuming it works out, `i` and `w` will effectively have the performance of primitive functions.

Critically, acceleration of functions extends also to *Data*, and efficient representations thereof. Natural numbers, for example, may be represented by simple machine words. Accelerated arithmetic functions could then operate directly on the compact natural number representation.

Long term, Awelon runtimes could accelerate labeled records and variants for convenient data embedding, evaluation of linear algebra for semi-transparent GPGPU parallelism, [process networks](https://en.wikipedia.org/wiki/Kahn_process_networks) for distributed cloud computing, and potentially interpretation of a register machine or C-- to more efficiently leverage the conventional CPU. 

*Note:* Acceleration replaces intrinsics and performance applications of FFI.

## Annotations

Annotations help developers control, optimize, and debug computations. Annotations are represented as parenthetical words like `(par)` or `(/3)`. Some useful examples of annotations include:

* `(/2)..(/9)` - arity annotations to defer computations
* `(0)..(9)` - tuple assertions for scope control
* `(aff) (rel)` - support substructural type safety
* `(:foo) (.foo)` - lightweight type tag and assertions
* `(nat)` - assert argument should be a natural number
* `(par)` - request parallel evaluation of computation
* `(jit)` - compile a function for use in future evaluations
* `(stow)` - move large values to disk, load on demand
* `(memo)` - memoize a computation for incremental computing
* `(error)` - mark a value as an error object
* `(@foo)` - gates for active debugging (logging, breakpoints)
* `(force)` - evaluate previously deferred computations

Annotations must have no internally observable effect on computation. Nonetheless, annotations may cause an incorrect computation to fail fast, defer unnecessary computation, simplify static detection of errors, or support useful external observations like debug logs or breakpoint states.

Annotations may be introduced and documented on a runtime basis. In case of porting code, runtimes that do not recognize an annotation may ignore it. Long term, we should reach some de-facto standardization on useful annotations.

## Stowage

Stowage is a simple idea, summarized by rewrite rules:

        [large value](stow) => [$secureHash]
        [small value](stow) => [small value]

Stowage enables programmers to work semi-transparently with data or computations much larger than working memory. Unlike effectful storage or virtual memory, stowage is friendly in context of parallelism and distribution, structure sharing, incremental computing, and backtracking. However, effective use of stowage is limited to [persistent data structures](https://en.wikipedia.org/wiki/Persistent_data_structure), optimally those that implicitly batch writes such as [LSM trees](https://en.wikipedia.org/wiki/Log-structured_merge-tree).

What 'large value' means is heuristic, based on time-space tradeoffs. But it should be deterministic, reproducible, and simple. A good default is that the value be moved to stowage if it is at least 256 bytes. Also, if our value is simple binary data, we might stow to a `%secureHash` external binary instead.

## Deferred Computations and Coinductive Data

The *arity annotations* `(/2)` to `(/9)` have simple rewrite rules:

                             [B][A](/2) == [B][A]
                          [C][B][A](/3) == [C][B][A]
                                        ..
        [I][H][G][F][E][D][C][B][A](/9) == [I][H][G][F][E][D][C][B][A]

To clarify, it is the *annotation* that has the given arity. Arity annotations specify nothing of their context.

Arity annotations serve a critical role in controlling computation. For example, the program `[[A](/2)F]` has the same type and semantics as `[[A]F]`, but the former prevents partial evaluation of `F` from observing `[A]`. Arity annotations can be used to guard against useless partial evaluations. For example, if we define swap as `w = (/2) [] b a` then we can avoid observing the useless intermediate structure `[A] w => [[A]] a`. 

Arity annotations serve a very useful role in modeling [thunks](https://en.wikipedia.org/wiki/Thunk) and [coinductive data](https://en.wikipedia.org/wiki/Coinduction). It is sometimes useful to model 'infinite' data structures to be computed as we observe them - procedurally generated streams or scene graphs.

We may want to 'force' a thunk. To do so, consider a `(force)` annotation.

        [computation](force)

This introduces a 'forced' evaluation mode for the given block. A forced evaluation operates as if there were sufficient arguments to the left of `computation` for purpose of deleting arity annotations without actually providing those arguments. The computation may still get stuck and prevent further evaluation, but it at least won't be stuck on an arity annotation.

Awelon language does not implicitly memoize computations to avoid rework. However, programmers can explicitly use `(memo)` to share work, and it is feasible for a runtime to optimize lightweight memoization of deferred computations. See *Memoization*.

*Aside:* I originally tried a `[F](lazy)` annotation to defer computation. However, the interaction with quotas, breakpoints, and parallelism are confusing. For example, given `[[A](par)F](lazy)` it is unclear whether this started as `[A](par)[F]b(lazy)` or `[A][(par)F]b(lazy)`. Arity annotations are simpler and more precise.

## Evaluation

Evaluation of an Awelon program results in an equivalent Awelon program, hopefully one from which it is easier to extract information or efficiently perform further evaluations. Awelon's primary evaluation mode proceeds by pure, local rewriting. The four primitives rewrite based on simple pattern matching:

            [B][A]a => A[B]         (apply)
            [B][A]b => [[B]A]       (bind)
               [A]c => [A][A]       (copy)
               [A]d =>              (drop)

Annotations may introduce special rewrite rules. For example, arity annotations remove themselves if sufficient arguments are available. Stowage evaluates the given program then rewrites to a small secure hash. Rewrite optimizations and staged computing modes are possible with annotations.

Words rewrite to their evaluated definitions - this is called linking. However, words link *lazily* to preserve human-meaningful hypermedia link structure in the evaluated output where feasible. Words do not link unless doing so leads to additional rewrites of primitives or annotations, something more interesting than simple inlining of the word's evaluated definition. Consequently, arity annotations are useful to control linking of words.

Awelon's evaluation strategy is simple:

* rewrite outer program
* evaluate before copy
* evaluate final values

This strategy isn't lazy or eager in the conventional sense. Rewriting the outer program first provides opportunity to apply annotations or drop values that won't be part of our output. Evaluation before copy guards against introduction of unnecessary rework. Evaluation of final values (blocks) brings us to a normal form that cannot be further evaluated.

*Note:* Undefined words do not rewrite further. If there any errors when parsing or evaluating a word's definition, or a cycle in the definition, that word should be treated as undefined. Partial evaluations with undefined words can be useful in context of development, debugging, staged computing, and the monotonic futures and promises application pattern.

## Fixpoint

Fixpoint is a function useful for modeling loop behaviors. For Awelon language, I recommend the following variant of the [strict Z fixpoint combinator](https://en.wikipedia.org/wiki/Fixed-point_combinator#Strict_fixed_point_combinator):

        [X][F]z == [X][[F]z]F 
        z = [[c] a [(/3) c i] b b w i](/3) c i

        Using Definitions:
           [A]i == A            (inline)         i = [] w a d
        [B][A]w == [A][B]       (swap)           w = (/2) [] b a

The arity annotation `(/3)` defers expansion within the `[[F]z]` result. This variation of the Z combinator has the advantage that the definition of `z` can be found in the naive evaluation, and that `[F]` is not replicated unnecessarily. I recommend that readers unfamiliar or uncomfortable with fixpoint step through evaluation of `[X][F]z` by hand a few times to grasp its behavior.

Fixpoint is notoriously difficult for humans to grok and frequently awkward to use. Instead of directly using fixpoint, we'll want to build more comfortable loop abstractions above it like list fold, foreach, while, and [generators](https://en.wikipedia.org/wiki/Generator_%28computer_programming%29). We would benefit from accelerating at least fixpoint, and possibly some of the more popular loop abstractions.

## Memoization

The primary basis for incremental computing in Awelon is [memoization](https://en.wikipedia.org/wiki/Memoization). Evaluation of dictionary definitions will tend to be implicitly memoized. But programmers may also request memoized evaluation explicitly:

        [computation](memo)

Memoization is conceptually performed by by seeking the computation in a runtime lookup table. This lookup must account for words having different meanings in context of different dictionaries. Assuming the computation is found in the table, the runtime will replace the computation by the evaluated result. If the computation is not found, we evaluate then heuristically store the computation into the table based on observed and estimated future time and space tradeoffs.

Naive use of a lookup table can work, but is not the most optimal approach to memoization. For example, ideally we want to control recomputing if there is a change to a word we do not link. To address this memoization would require instrumented evaluation that tracks linking, and only checks the relevant subset of dependencies. One might look into research on adaptive memorization, trace reuse, etc..

*Note:* To effectively support incremental computing, memoization must be used in context of cache-friendly application patterns such as command pattern and compositional views on persistent data structures.

## Static Linking

It is possible to perform static analysis on a word's evaluated definition, arities and types, etc. to determine context-free link structure. The most trivial example of this is redirects. Consider:

        @foo bar

Here, word `foo` will not evaluate any further because there is no cause to link `bar`. However, when we do eventually link `foo`, we'll immediately be asked to link `bar`. We can determine this by static analysis. For performance reasons, we may wish to skip the intermediate step rewriting `foo` to `bar` and jump straight to linking the definition of `bar`. 

Statically computing link structure can support inlining of definitions, flattening of redirect chains, and avoid rework for link decisions. It is a recommended as a performance optimization. However, this should not be observable 

## Value Words

Words like `true` or `false` or `42` can be treated as first-class values. Further, doing so is convenient for aesthetics and preservation of hypermedia link structure. 

        42 true w == true 42
        42 [] b   == [42]

In general, a value word is any word that evaluates to a singleton `[Value]`. In this case, we have `true = [a d]` and `42 = [41 #]`. A runtime should recognize value words and treat them as values until their internal structure is observed.

## Rewrite Optimization

Awelon's semantics allow for safe rewrites that aren't performed by evaluation. Consider:

        [] a    =>              apply identity is a NOP
        [i] b   =>              because ∀A. [[A]i] == [A]
        b i     =>  i           ∀A,F. [A][F]b i == [[A]F]i == [A]F == [A][F]i
        c d     =>              modulo substructural constraints
        c w     =>  c           no need to swap, both copies are equivalent
        [] b a  =>  w           definition of w

A typical example of rewrite optimizations regards mapping a pure function over values in a list or stream. In this case the principle observation is `[G] map [F] map == [G F] map`, and we might recognize this in a rewrite rule as `map [F] map => [F] compose map` (where `[G][F] compose == [G F]`).

Rewrite optimizations can also be utilized for compaction purposes, e.g. translationg `[0 S]` to `1` or recovering the `z` symbol after evaluation of the Z fixpoint combinator. An intriguing possibility is to localize some secure hash resources by rewriting to local dictionary words.

A runtime is free to support rewrite optimizations insofar as they are proven valid. Unfortunately, I lack at this time effective means for a normal programmer to propose and prove rewrite optimizations through the dictionary. But we can at least get a useful start by runtime rewriting of known, accelerated functions.

*Aside:* Rewrite optimizations in practice are ad-hoc and fragile. Use of annotation `[function](rwopt)` to indicate use of rewrite optimizations may help. Nonetheless, relying on rewrite optimizations for performance critical code is not highly recommended.

## Staged Evaluation

Staged programs are designed to process information in multiple distinct phases or stages. The goal is to improve performance through *deep* partial evaluations with available information. For robust staged computing, we must model stages explicitly. But as a convenient optimizer pass, we might express an intention for staged evaluation with `[function](stage)`. This might operate as follows:

* assuming words `A B C ..` are undefined and unused
* evaluate `..[C][B][A]function` as much as possible
* rewrite expression to extract `A B C ..` arguments

Evaluation with undefined 'future' values enables ad-hoc data plumbing and propagation of partial values. We propagate these futures as far as they will go, then systematically extract them as arguments. The evaluator may need to delay stowage, memoization, and other reflective annotations. The bulk of complexity is in the extraction step, but even that can be performed by a simple algorithm.

        Extract Argument: ∀X,E. [X] T(X,E) == E

        T(X, E) | E does not contain X      => d E
        T(X, X)                             => i
        T(X, [X])                           => 
        T(X, [E])                           => [T(X,E)] b
        T(X, E1 E2)
            | only E1 contains X            => T(X,E1) E2
            | only E2 contains X            => [E1] a T(X,E2)
            | otherwise                     => c T(X,[E1]) a T(X,E2)

In this case, we systematically extract `A` then `B` then `C` and however many arguments we provided. The cost is proportional to the arity and the size of our expression. This extract argument pattern has other cases, such as automatic refactoring, translation from lambda calculus, or even lambda based editable views.

Staged evaluation might be enhanced further. For example, sensitivity to type might support independent propagation of elements in a pair `B = [B2] [B1]`. But partial evaluation with futures followed by extract argument should be effective for many use cases, especially in context of programs designed to leverage staged evaluation.

## Compilation

Compilation of functions, especially those containing loops (fixpoints, folds, etc.), can offer significant performance benefits. This might be requested explicitly via `[function](jit)`. Compilation is subject to a lot of low level interactions with acceleration, quotas, GC, parallelism, and so on. I won't detail it here. But I don't have any particular reason to believe compiling Awelon will be difficult.

*Aside:* Extraction of Awelon functions for use without a runtime is also a form of compilation. For performance with Awelon project's application model, the focus should be JIT.

## Parallel Evaluation

Programmers may advise parallel evaluation of a block via `[computation](par)`. Essentially, this tells a runtime that we'll probably need the result of that computation, so go ahead and begin evaluation. We can abort the computation by dropping it.

If parallelism is sufficiently lightweight, a runtime is also free to parallelize the basic evaluation strategy, e.g. partitioning large programs into smaller chunks or parallelizing computation of final values.

However, that form of parallelism has severe limits on expressiveness.

For more powerful and scalable parallelism, we'll want to accelerate [process networks](https://en.wikipedia.org/wiki/Kahn_process_networks) to support cloud computing, and [linear algebra](https://en.wikipedia.org/wiki/Linear_algebra) to support SSE/GPGPU computing. Intriguingly, we can usefully update and observe a process network that is still undergoing evaluation without waiting for evaluation to complete.

*Aside:* Copying a parallel value should wait for completion. Dropping a parallel value should abort the computation. But basic linear data plumbing with parallel values is acceptable.

## Structural Scoping

Blocks naturally delimit the input scope for a computation. For example, we know that in `[B][[A]foo]`, `foo` can access `[A]` but not `[B]`. And we can trivially leverage this with the bind operation `b`. But Awelon also supports multiple outputs, and so scoping output is a relevant concern. To address this, Awelon introduces *tuple assertions* to annotate output scope:

                                   [](0) == []
                                [[A]](1) == [[A]]
                             [[B][A]](2) == [[B][A]]
                                         ..
        [[I][H][G][F][E][D][C][B][A]](9) == [[I][H][G][F][E][D][C][B][A]]

Tuple assertions can be deleted early if they are validated statically. Otherwise, some lightweight dynamic reflection may be necessary, and we'll fail fast if the tuple assertion is bad. Similar to arity annotations, tuples larger than `(5)` should be rare in practice.

In addition to controlling output counts, programmers may wish to fail fast based on declared structure. To support this, Awelon supports a structure annotation `(:T)` and paired assertion `(.T)` with the following rewrite semantics:

        (:foo) (.foo) ==

In practice, we might construct a tagged value with `[(:foo)] b` and deconstruct it with `i (.foo)`. The symbol `foo` or `T` is left to the programmer, but must match between annotation and assertion. These annotations are not labels (i.e. you cannot discriminate on them), but they do resist *accidental* access to structured data. As with tuple assertions, we can fail fast dynamically or detect an error statically.

## Substructural Scoping

[Substructural types](https://en.wikipedia.org/wiki/Substructural_type_system) allow us to reason about whether a value is used, or how many times it is used. This is convenient for modeling finite resources, intermediate states in a protocol, or ensuring certain steps are performed by a client computation. Awelon provides simple annotations for lightweight substructural types:

* `(aff)` - mark a value affine, non-copyable (op `c`)
* `(rel)` - mark a value relevant, non-droppable (op `d`)
* inherit substructure of bound values (op `b`).

We can eliminate substructural annotations by observing a value with `a`. It is not difficult to track and validate substructural properties dynamically, or to represent them within static types. 

## Error Annotations

We can mark known erroneous values with `(error)` as in `"todo: fix foo!"(error)`. If we later attempt to observe this value (with `i` or `a`), we will simply halt on the error. However, we may drop or copy the value like normal. In addition to user-specified errors, a runtime might use error annotations to highlight places where a program gets stuck, for example:

        [A](aff)c       => [[A](aff)c](error)i
        [[A][B][C]](2)  => [[A][B][C]](2)(error)

Errors presented in this manner are relatively easy to detect. 

In context of substructural scoping, we might also want a `(trash)` annotation that replaces a possibly large value with a small error value but preserves substructural attributes. Thus, we can free memory without dropping or stowing data, it just results in an error if we later attempt to observe the trashed data.

## Active Debugging

Active debugging broadly includes techniques to observe an evaluation in progress. Common techniques for active debugging include logging, profiling, breakpoints. To support active debugging, Awelon introduces `(@gate)` annotations. The symbol is user-defined, e.g. it could be `(@foo)` or `(@stderr)`. 

The `(@gate)` annotation is considered 'active' when it has a block to its left: 

        [A](@gate)

The behavior of an active gate is configured at the runtime. Outside of debugging, for example, we'll simply delete the active gates and pass the values. Consider a few debug configurations:

* stall - hold `[A](@gate)` for now
* trace - copy value into debug log
* profile - record evaluation of `[A]`

Stalls effectively give us breakpoints by preventing the program upstream of the gate from using the value. Unlike conventional breakpoints, Awelon will continue to evaluate other parts of the program as far as they can go despite the stalls. To avoid rework or significant changes in evaluation order, if evaluation of `P` stalls on a gate, copy operation `[P]c` must stall on `P`. 

Tracing gives us standard 'printf' style debugging. Note that copying the value implicitly requires evaluating it first, but is not subject to normal limitations like `(aff)`. The other copy of the value is passed. We can render trace logs as part of the program output, appending a comment-like structure `[[MsgN] ... [Msg3] [Msg2] [Msg1]] (@stderr/log) d`. 

Profiling might tweak `[A]` to record performance metadata (e.g. allocations, rewrites, times, use of memoization or stowage) to named statistics objects, then pass it on. This would allow us to accumulate a lot of useful performance information quickly.

In general, gates may have conditional configuration based on observation of the value. Also, a runtime may provide default configurations with naming conventions like `(@stderr)` defaulting to a trace.

## Program Animation and Time Travel Debugging

Program animation and time-travel debugging is an intriguing possibility with Awelon language made feasible by the intersection of local rewrite semantics and stall gate semantics. Instead of returning to a human programmer when we stall, we can systematically:

* evaluate as far as we can go
* take a snapshot / checkpoint
* delete all the active gates 
* repeat until no active gates

The snapshots then become 'frames' for evaluation. The frames will have predictable structure determined by the use of gates, which is convenient for rendering them in sequence. Conveniently, we can get by with preserving every tenth or hundredth frame (or heuristically, based on empirical compute effort) and deterministically recompute intermediate frames as needed. We can also associate trace logs with recorded frames. Putting recorded frames and trace logs on a timeline with a slider effectively gives us time-travel debugging.

I feel this gives Awelon an excellent debugging experience, much better than most languages.

*Aside:* Deleting all the active gates each frame is a convenient default, but other animation strategies can be useful to focus attention. For example, we could always delete the leftmost active gate, or prioritize deletion of gates by name, to better control the animation.

## Static Typing

Awelon can be evaluated without static typing. There is no type driven dispatch or overloading. But if we can detect errors early by static analysis, that is a good thing. Further, static types are also useful for verifiable documentation, interactive editing (recommending relevant words based on type context), and performance of JIT compiled code. Strong static type analysis makes a *very* nice default.

We can represent our primitive types as:

        a   : S B (S → S') → S' B
        b   : S B (E B → E') → S (E → E')
        c   : S A → S A A
        d   : S A → S
        [F] : S → S type(F)

The type sequence `S C B A` aligns with a program structure `S[C][B][A]`. Effectively, `S` is the remainder of our program 'stack' when we view the program as rewriting a stack-like structure. In a more conventional language, this might correspond to a product type `(((S * C) * B) * A)`.

Use of annotations can augment static type analysis by providing a validation. Structural and substructural assertions mentioned above can be validated statically. A remaining concern is typing of conditional behavior. As described under *Data* earlier, we can represent conditional data types:

        sum     S (S B → S') (S A → S') → S'
        opt     S (S   → S') (S A → S') → S'
        bool    S (S   → S') (S   → S') → S'

A runtime could support type assertions `(sum)`, `(opt)`, and `(bool)` to support static type analysis and detection of errors. If necessary, these assertions could be weakly verified dynamically. But with static verification, we can unify the `S` and `S'` types and hence detect inconsistencies between the left vs. right paths. 

Recursive types can potentially be recognized by use in context of fixpoint functions. However, annotations for common types like `(nat)`, `(list)`, `(text)`, `(binary)` certainly would not hurt. 

Type declarations can also be represented at the dictionary and application layers, using simple conventions like `foo.type` to declare a type for `foo`. Declared types are accessible for refactoring, rendering, and reflection. And they can potentially be more expressive than simple annotations can readily support. For example, a system might support [dependent types](https://en.wikipedia.org/wiki/Dependent_type) and auxiliary proofs with sufficient reflection on the dictionary.

*Aside:* Awelon systems are free to consider non-terminating evaluation to be an *error*, and perform static termination analysis by default. This analysis must be sensitive to arity annotations so we can support coinductive data types.

## Gradual Typing and Macro Evaluation

In context of metaprogramming, it is frequently useful to work with intermediate structures for which we do not know the type. For example, consider:

        1 2 3 4             "abcx → ax^2 + bx + c"  runPoly
        "Red Warrior" 42    "%s takes %d damage!"   printf

In scenarios like these, computing the type of `runPoly` or `printf` independent of context may require sophisticated dependent types. But computing a type for `"abcx → ax^2 + bx + c" computePoly` or `"%s takes %d damage!" printf` might only require some partial evaluation before static type checking. This implies staging similar to macro evaluation.

To make this intention explicit, I propose annotation `(dyn)`. Example usage:

        "abcx → ax^2 + bx + c"  runPoly
            == "abcx → ax^2 + bx + c" compilePoly (dyn) i
            == [polynomial behavior](dyn) i
            == [polynomial behavior] i

The presence of a `(dyn)` annotation informs a static type analysis that a volume of code might be difficult to statically validate, to try partial evaluation if possible, and perhaps to suppress warnings when no safety judgement can be made. Obvious type errors can still be raised. 

Dynamic evaluation is considered *complete* when a value is produced, that is `[A](dyn) == [A]`. However, many functions will remain dynamic even after evaluation. Careful use of `(dyn)` provides a simple basis for gradual typing in a system that defaults to static types.

## Labeled Data - Records and Variants 

Labeled sum types (aka variants) allow us to conditionally discriminate on the label. Labeled product types (aka records) allow us to access a heterogeneous collection of data by label. Primitive sum `(A + B)` and product `(A * B)` values are simple and sufficient for many use cases. But labeled data is frequently self-documenting (the label informs the human) and extensible (easy to introduce new labels).

Labeled data can be encoded using 'deep' primitive sum types. Use of sums instead of `(label,value)` pairs avoids need for dependent types. Editable views can potentially support labeled data at the syntax layer.

Consider constructors `mkL = [inL] b` and `mkR = [inR] b` for primitive sum types, as described under *Data*. We can construct 'deep' sums using sequences like `mkL mkL mkR mkR mkR mkL mkR mkR`. I'll shorthand that as `LLRRRLRR`. We can encode labels (or other information) within this left-right-left sequence. Consider one possible encoding:

        type N v = (v + N v)
        type T v = (v + N (T v))

Here, `N` labels a value `v` with a unary-encoded natural number. For example, `LRRR` would encode a label `3` Then `T` encodes a sequence of natural numbers. For example, `LLRRRLRRLRRRR` encodes a sequence `2 1 3`. Of course, we'll view this in reverse order upon deconstruction, so we might say instead that we have encoded the sequence `3 1 2`. A text label like `foo` can be encoded as a series of code points such as `102 111 111`. With the aforementioned encoding, this would expand to an `LLRRRR...` string of over 300 characters, so I won't bother. But it could be abstracted as something like `L.0 L.o L.o L.f` or possibly `"foo" label`. 

More efficient encodings are certainly feasible. For example, we could leverage an [entropy coding](https://en.wikipedia.org/wiki/Entropy_encoding) over a more constrained alphabet.

A single labeled value represents an element of a variant type. A collection of labeled values can model a record. A record can double as a switch, allowing conditional selection of an action based on the label of a variant. A collection based on a [radix tree](https://en.wikipedia.org/wiki/Radix_tree) could be efficient, avoiding redundant storage and computation on shared prefixes. 

Modeling labels as first-class structures allows for potentially generating new labels in infinite streams, or extending and extracting label prefixes to model routing-like behaviors.

## Editable Views

Awelon language has an acceptably aesthetic plain text form.

However, like Forth, Awelon does not scale nicely beyond about ten tokens per definition, and is awkward for some problem domains like expression of algebraic formula. Further, Awelon lacks *namespaces*, which results in redundant expression of context in words such as repeating a `trie:` prefix for `trie:insert` and `trie:fold`.

To ameliorate these weaknesses, Awelon is expected to leverage [editable views](http://martinfowler.com/bliki/ProjectionalEditing.html). As an example of editable views, we might support command lists:

        Editable View       Awelon Source Code
        {foo, bar, baz}     [[foo] [[bar] [[baz] ~ :] :] :]

Rich number types are useful target for editable views. Consider:

        Editable View       Awelon Source Code
        #42                 42
        42                  [42 0 Integer]
        -7                  [0 7 Integer]
        3.141               [[3141 0 Integer] 3 Decimal]
        -0.0070             [[0 70 Integer] 4 Decimal]
        2/3                 [[2 0 Integer] 3 Rational]

This `[data lang]` pattern can generally be applied to support arbitrary DSLs in a context-free manner, with `lang` potentially being a named identity function if we don't have need to process the data. No static analysis is performed or required.

Editable views could also provide a 'comments' syntax, for example:

        /* comment */  <=>  " comment " (/2) (@rem) d 

The arity annotation would preserve the comment, or allow comments to be injected after evaluations. The gate would allow conditional breakpoints or tracing with comments, and the `d` finally drops the comment so it has no semantic effect. Variations on this comment idea might be used to provide rendering or namespace hints.

Editable views should be evaluable. Awelon code evaluates into Awelon code via program rewriting, and this should correspond to editable views evaluating into editable views. This means view features like comments, decimal numbers, lambdas, and so on should be something we construct at runtime, and perhaps even accelerate.

An intriguing possibility is to recognize the fingerprint of an 'extract-argument' pattern (described under *Staged Evaluation*), with a comment to name variables, supporting lambda and let expressions within editable views.

Editable views are not limited to plain text representations. I encourage attention to plain text views because those are easy to integrate with existing development environments and editors, potentially via [Filesystem in Userspace](https://en.wikipedia.org/wiki/Filesystem_in_Userspace). However, views based on hypermedia representations, sliders and checkboxes, canvases and graphs, music notations, etc.. will provide a simple and powerful environment and a foundation for live coding applications.

A useful sanity check: a round-trip from Awelon code to view and back should be the identity function.
