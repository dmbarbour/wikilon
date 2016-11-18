
# Awelon Language

Awelon is a purely functional language based on concatenative combinators. 

## Why Another Language?

Awelon differs from other languages in its adherence to simplicity and its choice of evaluation model. There are only four simple computational primitives, one data primitive, and a simple Forth-like syntax. Evaluation is by local rewriting with lazy linking.

Awelon's simple syntax lowers barriers for program rewriting as an evaluation model, editable views for HCI, and collaboration with software agents. Program rewriting ensures the evaluation is a program that may be serialized for debugging, distribution, or persistence. Lazy linking preserves human-meaningful words and link structure, ensuring results to be viewed the same as source. And software agents support the application layer effects.

Like any pure language, Awelon's evaluation is separate from its effects model. Awelon explores a [RESTful application model](ApplicationModel.md) where application state is represented within a codebase, and effects are performed in context of a multi-agent system. Relevant effects patterns include [publish-subscribe](https://en.wikipedia.org/wiki/Publish%E2%80%93subscribe_pattern) and a variation on the [tuple space](https://en.wikipedia.org/wiki/Tuple_space) oriented around [work orders](https://en.wikipedia.org/wiki/Work_order). More conventional effects models are possible within orders.

Intriguingly, [editable views](http://martinfowler.com/bliki/ProjectionalEditing.html) can feasibly present Awelon programs or codebases as [hypermedia](https://en.wikipedia.org/wiki/Hypermedia) objects. GUI forms with live coding properties. Words within a act as hypermedia links. Some relationships may be implicit in word structure (as between `foo`, `foo.doc`, and `foo.author`) to support more flexible hypermedia than is implied by direct word dependencies. Editable views can also support numbers, comments, lambdas and let expressions for cases where tacit programming is annoying, and so on. 

Universal serializability and purity also lowers barriers for high-performance and large-scale computing. We can semi-transparently accelerate evaluation of linear algebra to use GPGPU computing. We can distribute evaluation of process networks across multiple virtual machines. We can use checkpoints and replay computations if part of the system fails. Big data or computations can be stowed away and referenced by secure hash. It will take time to build Awelon up to its full potential, but I believe Awelon has excellent long term prospects for performance.

Awelon's simplicity, purity, scalability, and evaluation model each contribute greatly to its utility for the envisioned application model. I have not encountered another language that would work as well.

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

Words are identified by a non-empty sequence of UTF-8 characters with a few limitations. The blacklist is `@[]()<>{},;|&=\"`, SP, C0 (0-31), and DEL. Developers are encouraged to favor words that won't need escapes in most external contexts (such as URLs, HTML, Markdown, natural language text, or editable views), and that aren't too large.

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

Texts must be valid UTF-8, forbidding C0 (except LF) and DEL. Inline texts additionally forbid the double quote and LF. There are no character escapes, but the extra whitespace in the representation of multi-line text is not considered part of the text. Texts in Awelon are [syntactic sugar](https://en.wikipedia.org/wiki/Syntactic_sugar) for a simple list of codepoints: 

        "hello" == [104 "ello" :]       (cons)
        ""      == ~                    (nil)
        "→"     == [8594 ~     :]

Awelon language has exactly one primitive data type - the `[]` block of code, representing a function. Data is instead [Church encoded](https://en.wikipedia.org/wiki/Church_encoding) or use similar variants such as the [Scott encoding](https://en.wikipedia.org/wiki/Mogensen%E2%80%93Scott_encoding). Values are thus represented by functions. 

Before I explain encoding of numbers and texts, let us examine encodings for other useful data types. Booleans are perhaps the simplest data. We might encode booleans as follows:

        [onF][onT] false i == onF               false = [d i]
        [onF][onT] true  i == onT               true  = [a d]

        Using Definitions:
           [A] i == A                           i = [] w a d 
        [B][A] w == [A][B]                      w = [] b a   

Booleans can be generalized to algebraic sum type `(A + B)`.

        [onL][onR] [[A] inL] i  == [A] onL      inL = w d w i
        [onL][onR] [[B] inR] i  == [B] onR      inR = w b a d

Construction of `[[B] inR]` is trivial - `[B] mkR` where `mkR = [inR] b`. This is generally the case for value constructors. In practice, we may also wish to defer computation of partial constructions (see *Deferred Computations and Coinductive Data*). 

Pairs - algebraic `(A * B)` product types - may also be Church encoded:

        [onP] [[B][A] inP] i == [B][A]onP       inP = [] b b a i

However, in Awelon language, we can encode a pair as `[[B][A]]`. 

Between algebraic sums and products, we can represent any conventional data. 

The option type `(1 + A)` may be modeled as a choice of `false` or `[[A] R]`. Natural numbers can be encoded as `μNat.(1 + Nat)` - that is, Nat is recursively zero or successor of a Nat. Given natural numbers, integers can be represented by a pair representing `X - Y`.  We can encode rational numbers as a numerator-denominator pair. We can encode decimal numbers with a significand and exponent. A list can be modeled as `μList.(1 + (A * List))`. Text can be encoded as a list of natural numbers. Algebraic encodings are often simple:

        0 = false       (zero)
        1 = [0 S], 2 = [1 S], ...
        S = inR         (successor)

        ~ = false       (nil)
        : = mkP inR     (cons)

An alternative is to model data as implicitly folding over its structure. The Church encoding of natural numbers is an example of this. Consider: 

        [X][F] 0 i == [X]
        [X][F] 1 i == [X] F         == [[X][F] 0 i] F
        [X][F] 2 i == [[X] F] F     == [[X][F] 1 i] F

        0 = false
        1 = [0 S], 2 = [1 S], ...
        S = [c] a [b b] b a i

Folding over a recursive structure that carries no additional data isn't particularly interesting. But we can generalize easily to folds over list structures:

        [X][F] ~ i == X
        [X][F] [[A] [L] :] i == [[X][F]L] [A]  F

        ~ = false
        : = w [[c] a b [b] b a] a w i

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

Annotations help developers control, optimize, view, and debug computations. Annotations are represented as parenthetical words like `(par)` or `(/3)`. Some useful examples of annotations include:

* `(/2)..(/9)` - arity annotations to defer computations
* `(0)..(9)` - tuple assertions for output scope control
* `(aff) (rel)` - support substructural type safety
* `(:foo) (.foo)` - lightweight type tag and assertions
* `(par)` - request parallel evaluation of computation
* `(seq)` - request immediate evaluation of computation
* `(nat)` - assert argument should be a natural number
* `(jit)` - compile a function for use in future evaluations
* `(stow)` - move large values to disk, load on demand
* `(memo)` - memoize a computation for incremental computing
* `(stage)` - evaluation mode for deep partial evaluation
* `(error)` - mark a value as an error object
* `(force)` - evaluate previously deferred computations
* `(@gate)` - extra symbols just for active debugging

Annotations must have no internally observable effect on computation. Nonetheless, annotations may cause an incorrect computation to fail fast, defer unnecessary computation, simplify static detection of errors, or support useful external observations like debug logs or breakpoint states.

Annotations may be introduced and documented on a runtime basis. In case of porting code, runtimes that do not recognize an annotation may ignore it. Long term, we should reach some de-facto standardization on useful annotations.

## Stowage

Stowage is a simple idea, summarized by rewrite rules:

        [large value](stow) => [$secureHash]
        [small value](stow) => [small value]

Stowage enables programmers to work semi-transparently with data or computations much larger than working memory. Unlike effectful storage or virtual memory, stowage is friendly in context of parallelism and distribution, structure sharing, incremental computing, and backtracking. However, effective use of stowage is limited to [persistent data structures](https://en.wikipedia.org/wiki/Persistent_data_structure), optimally those that implicitly batch writes such as [LSM trees](https://en.wikipedia.org/wiki/Log-structured_merge-tree).

What 'large value' means is heuristic, based on time-space tradeoffs. But it should be deterministic, reproducible, and simple. A good default is that a value be moved to stowage only if its encoding in Awelon is at least 256 bytes. Also, if our value is simple binary data, we might stow to a `%secureHash` external binary instead.

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

Words rewrite to their evaluated definitions - this is called linking. However, words link *lazily* to preserve human-meaningful hypermedia link structure in the evaluated output where feasible. Words do not link unless doing so leads to additional rewrites of primitives or annotations, something more interesting than a simple inlining of the word's evaluated definition. 

Arity annotations are very useful to control linking of words.

Awelon's evaluation strategy is simple:

* rewrite outer program
* evaluate before copy
* evaluate final values

This strategy isn't lazy or eager in the conventional sense. Rewriting the outer program first provides opportunity to apply annotations or drop values that won't be part of our output. Evaluation before copy guards against introduction of unnecessary rework. Evaluation of final values (blocks) brings us to a normal form that cannot be further evaluated.

Annotations may introduce special rewrite rules for evaluation, limited to identity semantics. For example, arity annotations wait for sufficient arguments then rewrite to the empty program. Stowage replaces a large value with a secure hash reference. Memoization, staging, and rewrite optimizations use alternative evaluation modes. 

*Note:* The `[A](seq)` annotation will cause evaluation of `[A]` as if it were about to be copied. This is mostly useful for precise profiling and fail-fast detection of stalled computations.

## Value Words

A 'value word' is any word whose evaluated definition is a singleton block. An Awelon runtime must treat value words as values with respect to binding, data plumbing, etc..

        true = [a d]
        false = [d i]
        42 = [41 S]

        42 true w == true 42
        42 [] b   == [42]

Support for value words is implied by the lazy link rules. I'm just making it explicit. Value words are essential for preserving human-meaningful structure and hypermedia resource references.

## Stalled Computation

A 'stalled' computation is incomplete and unable to progress. In Awelon, the main cause of stalled computation is undefined words. Specifically, a computation that *might* link a word depending on its arity, etc. should stall. Stalls are mostly relevant in context of the *evaluate before copy*. Evaluation of a stalled computation is not complete, therefore we do not copy. That is, if `P` stalls, `[P]c` stalls (and thus `[P](seq)` stalls). 

Accidental stalls are a bad thing. A development environment should ensure programmers are aware of potential issues. However, stalling may be performed on purpose, as part of future-based application patterns or active debugging. For that reason, stalls are not considered an error.

*Note:* Awelon will evaluate as much as possible even in context of some stalls.

## Fixpoint and Loops

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

## Rewrite Optimization

Awelon's semantics allow many rewrites not performed by evaluation. Consider:

        [] a    =>              apply identity is a NOP
        [B] a [A] a => [B A] a  application composes
        [i] b   =>              because [A][i]b == [[A]i] == [A]
        b i     =>  i           expansion of [X][F]b i == [X][F]i
        c d     =>              drop the copy
        c [d] a =>              drop the other copy
        [] b a  =>  w           by definition of w
        c w     =>  c           copies are equivalent
        [0 S]   =>  1           by definition of 1

Performing such rewrites can improve performance and aesthetics. For example, logically rewriting the `z` symbol after fixpoint so we don't see the expanded `[[c] a [(/3) c i] b b w i](/3) c i` is convenient for both machine performance and for human observers.

A runtime may perform rewrite optimizations at its own discretion if they are provably valid. We may also accept rewrite rules, via some convention, from a dictionary. In that case, burden of proof is shifted to the dictionary developers. Also, while rewrites tend in practice to be ad-hoc and fragile, annotation `[F](rwopt)` may help by providing explicit control of staging and enabling application of rewrites to programs constructed at runtime.

## Staged Evaluation

Staged programs are designed to process information in multiple distinct phases or stages. The goal is to improve performance through *deep* partial evaluations with available information. For robust staged computing, we must model stages explicitly. But as simple optimizer pass, we might express an intention for staged evaluation with `[function](stage)`. A viable implementation:

* assuming words `A B C ..` are undefined and unused
* evaluate `..[C][B][A]function` as much as possible
* rewrite expression to extract `A B C ..` arguments

That is, we perform evaluation with undefined 'future' values. This enables ad-hoc data plumbing to proceed, and supports construction and propagation of 'partial' values like `[[A] foo]`. Essentially, this gives us a lambda-calculus like ability to evaluate with free variables. The burden of complexity is shifted to the 'extract argument' step, but that can be performed by a simple reflective algorithm:

        T(X,E) - extract X from E such that:
            T(X,E) does not contain X
            [X] T(X,E) == E

        T(X, E) | E does not contain X      => d E
        T(X, X)                             => i
        T(X, [X])                           => 
        T(X, [E])                           => [T(X,E)] b
        T(X, F G)
            | only F contains X             => T(X,F) G
            | only G contains X             => [F] a T(X,G)
            | otherwise                     => c [T(X,F)] a T(X,G)

After staged evaluation, we systematically extract the arguments provided for staging. This is a whole-program rewrite, and has a cost proportional to the size, depth, and arity of our expression. Conveniently, this simple algorithm is also useful for converting lambda calculus terms, supporting lambda based editable views, and automatic refactoring.

Staged evaluation can be enhanced further by recognizing simple type annotations, such as tuples. For example, if we know argument `[B]` is a pair `[[B2] [B1]]`, we could proceed to propagate those components independently. If we know an argument is a sum or boolean, we might be able to lift the conditional behavior and stage the separate options independently.

While I haven't hammered out the details, specializing for common algebraic data types seems a worthy pursuit for performance benefits. However, even without such enhancements, the simple algorithm proposed above should be effective for many use cases. Especially assuming programs designed to leverage this evaluation mode.

## Compilation

Compilation of functions, especially those containing loops (fixpoints, folds, etc.), can offer significant performance benefits. I expect compilation to be a normal thing for Awelon code, performed at important word boundaries or upon explicit request via `[function](jit)`. JIT compilation is runtime dependent, and I imagine there will be a lot of low-level interactions with accelerators, quotas, GC, parallelism, and so on. I won't detail it here. 

Before compiling to JIT layer code, we should perform several optimization passes at the Awelon layer - static linking, staged evaluation based on arity (and type sensitive, ideally), rewrite optimizations, and so on. Only after all that has been performed should we compile to LLVM for machine-layer optimization passes and native code generation.

Compilation of a function for use in external systems like JavaScript - aka 'extraction' - is also viable, and will generally share the intermediate work of optimizing at the Awelon layer before compiling to a remote target.

## Parallel Evaluation

Programmers may advise parallel evaluation of a block via `[computation](par)`. Essentially, this tells a runtime that we'll probably need the result of that computation, so go ahead and begin evaluation. We can proceed to move the block with linear data plumbing. However, copying a parallel computation must wait for complete evaluation, and dropping it should efficiently abort the computation. 

Unfortunately, `(par)` has severe limitations on expressiveness. It is not expressive enough to represent pipeline parallelism or distributed communicating processes. It is too expressive to leverage low-level vector processing (e.g. SIMD, SSE, GPGPU) parallelism. 

For low level parallelism, we might accelerate [linear algebra](https://en.wikipedia.org/wiki/Linear_algebra) or evaluation of a DSL for GPGPU computing (I'm sure there is a suitable subset of OpenCL or WebCL). A solution here could help Awelon get into machine learning, physics simulations, graphics and audio, etc..

For high level parallelism, I propose acceleration of [Kahn process networks (KPNs)](https://en.wikipedia.org/wiki/Kahn_process_networks), or more precisely the *Reactive Process Network* variant detailed later. The state of a process network is described by a value. The process description might involve named processes with ports and wires between them, pending messages on wires. Evaluation of the network description results deterministically in another network description. Accelerated evaluation might use distributed processes and queues to precisely simulate the network, resulting in the evaluated description.

Usefully, process networks are *monotonic*. We can interact with them - inject messages, extract results - without waiting for evaluation to fully complete. Process networks can effectively represent first-class services within a purely functional computation.

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

We can mark known erroneous values with `(error)` as in `"todo: fix foo!"(error)`. If we later attempt to observe this value (with `i` or `a`), we will simply halt on the error. However, we may drop or copy an error value like normal. In addition to user-specified errors, a runtime might use error annotations to highlight places where a program gets stuck, for example:

        [A](aff)c       => [[A](aff)c](error)i
        [[A][B][C]](2)  => [[A][B][C]](2)(error)

Error values may bind further arguments as `[B][A](error)b == [[B]A](error)`. Error values will evaluate like any other value, and will collapse normally from `[[A](error)i]` to `[A](error)`. The error annotation is idempotent and commutative with other annotations on a block.

Errors are not a stalling condition. Rather, indicate 'complete' but with errors. However, we can also introduce annotation `[P](stall)` to simulate a stalled or non-terminating computation. This would enable programmers to more precisely control their debugging experience. In this case, we'd simply treat `[P]` as a stalling computation.

## Garbage Data

For relevant data, we always have an option to drop data into a logical bit bucket then never look at it again. If we tell our runtime that we will never look at it again, we can also recover memory resources associated with that data. We can represent this pattern by use of a `(trash)` annotation:

        [A](trash) => [](error)
        [A](rel)(trash) => [](rel)(error)

We destroy the data but preserve substructure. Because the data has been destroyed, the resulting value is marked erroneous.

## Active Debugging

Active debugging is an umbrella term for techniques to observe a computation in-progress. These techniques include logging, profiling, breakpoints. In Awelon, active debugging may occur via instrumentation of words. That is, a runtime can be configured to log arguments to a word, profile evaluation of a word, or stall evaluation of a word. Usefully, words can be instrumented without intrusive modification of a program. 

* stall - prevent or delay linking of word 
* trace - record argument(s) into debug log
* profile - record evaluation statistics

These configurations could generally be composed or conditional.

There are use cases for pseudo-words just for debugging purposes. For this role, I introduce a class of `(@gate)` annotations, such as `(@foo)` or `(@trace)`. I call these annotations 'gates'. Gates have a trivial identity behavior - `[A](@gate) => [A]`. The ability to know at a glance that gates are semantically irrelevant could be convenient for local reasoning about correctness. Gates can serve other useful roles such as rendering hints.

Stalled computations provide implicit 'breakpoints'. However, stalling is more precise and predictable than breakpoints in conventional development environments. Continuing breakpoints will generally be specified declaratively, using phrases such as 'leftmost `(@foo)` breakpoint' or 'all `bar, baz, qux` breakpoints'. 

Tracing gives us standard 'printf' style debugging. This will copy the traced values, bypassing `(aff)` constraints. Intriguingly, because Awelon code evaluates to Awelon code, we can easily record a trace as a comment in the program output. For example:

        [[Msg1] [Msg2] .. [MsgN]] (@trace.log) d

But we can use more conventional mechanisms like an external console or log, depending on the configuration.

Profiling would aggregate performance statistics over multiple uses of a word. We could potentially profile with gates, too, by configuring the gate to also perform a `(seq)` operation.

## Program Animation and Time Travel Debugging

Program animation and time-travel debugging is an intriguing possibility with Awelon language made feasible by the intersection of local rewrite semantics and stall gate semantics. Instead of returning to a human programmer when we stall, we can systematically:

* evaluate as far as we can go
* take a snapshot / checkpoint
* select, continue breakpoints
* repeat until no active gates

The resulting animation will be deterministic up to the strategy by which we select breakpoints. Different strategies, like 'continue leftmost' vs. 'prioritize xyzzy breakpoints') will result in different animations, different focus of attention, but the same final result. Conveniently, deterministic evaluation means we can save a lot of space by recording a tiny fraction of checkpoints into a time-index and recomputing the remainder as needed. 

Trace logs can be associated with each frame to place them on the timeline.

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

Recursive types can potentially be recognized by use in context of fixpoint functions. However, annotations for common types like `(nat)`, `(list)`, `(text)`, `(binary)` certainly would not hurt. Also, we might benefit from specialized static safety analysis for accelerated structures like KPNs.

Type declarations can also be represented at the dictionary and application layers, using simple conventions like `foo.type` to declare a type for `foo`. Declared types are accessible for refactoring, rendering, and reflection. And they can potentially be more expressive than simple annotations can readily support. For example, a system might support [dependent types](https://en.wikipedia.org/wiki/Dependent_type) and auxiliary proofs with sufficient reflection on the dictionary.

*Aside:* Awelon systems are free to consider non-terminating evaluation to be an *error*, and perform static termination analysis by default. This analysis must be sensitive to arity annotations so we can support coinductive data types.

## Gradual Typing and Macro Evaluation

In context of metaprogramming, it is frequently useful to work with intermediate structures for which we do not know the type. For example, consider:

        1 2 3 4             "abcx → ax^2 + bx + c"  runPoly
        "Red Warrior" 42    "%s takes %d damage!"   printf

In scenarios like these, computing the type of `runPoly` or `printf` independent of context may require sophisticated dependent types. But computing a type for `"abcx → ax^2 + bx + c" computePoly` or `"%s takes %d damage!" printf` might only require some partial evaluation before static type checking. This implies staging similar to macro evaluation.

To make this intention explicit, we could introduce a `(dyn)` annotation:

        "abcx → ax^2 + bx + c"  runPoly
            == "abcx → ax^2 + bx + c" compilePoly (dyn) i
            == [polynomial behavior](dyn) i
            == [polynomial behavior] i

The presence of `(dyn)` then informs a static analysis that some code might be difficult to validate. It also introduces a mode for partial evaluations: we could easily target elimination of `(dyn)` annotations. In presence of `(dyn)` we might suppress warnings when no static safety judgement can be made, but still raise obvious type errors. 

Dynamic evaluation is considered *complete* when a value is produced, that is `[A](dyn) == [A]`. However, many functions will remain dynamic even after evaluation. Careful use of `(dyn)` provides a simple basis for gradual typing in a system that defaults to static types.

*Note:* While `(dyn)` can be useful for otherwise problematic DSLs

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

## Reactive Process Networks

A weakness of conventional Kahn Process Networks (KPNs) is that they cannot merge asynchronous data from multiple channels. There is no record for when messages on one channel arrive relative to messages on other channels. This weakness makes it difficult to efficiently integrate KPNs with real-world events. Fortunately, this weakness can be solved by adding time to KPNs, while preserving other nice features (determinism, monotonicity). Here's how:

* Every process has an automatic time value. 
* Every message is stamped with the process time.
* Wires have logical latency, e.g. add 10 to time.
* Process can only read messages up to its time.
* Reading returns nothing if no messages at time.
* Process can explicitly wait on a set of ports.
* Waiting advances time to next message in set.
* We can explicitly advance times at input ports. 

The advance of time is driven externally at open input ports, internally via latencies. Advancing time at the input port essentially says, "the next message will have *at least* this future time". Cyclic wiring with latency permits precise expression of ad-hoc clock-like behaviors. Conventional KPN behavior is preserved if we never advance time and use zero latency wiring. That is, reads wait until either a message is available OR the upstream process advances past the reader's time, which ever happens first.

Time stamps and latencies can easily be represented by natural numbers. We can usefully normalize times by subtracting the minimum time stamp from all time stamps, such that at least one time stamp in the network description is 0.

Reactive process networks fill out the remaining expressive gap of KPNs, enabling us to work with asynchronous inputs, merge streams as needed. Further, we can now control evaluation 'up to' a given logical time. This is very useful for interacting with the real world, and in real-time.

## Editable Views

Awelon language has an acceptably aesthetic plain text syntax. However, like Forth, Awelon does not scale nicely beyond about ten tokens per definition because humans lose track of context. Expression of sophisticated data plumbing can be awkward. 

To preserve its simple structure and syntax, Awelon shifts the burden to [editable views](http://martinfowler.com/bliki/ProjectionalEditing.html). 

I put some emphasis on *plain text* editable views, such that we can readily integrate favored editors and perhaps even leverage [Filesystem in Userspace (FUSE)](https://en.wikipedia.org/wiki/Filesystem_in_Userspace) to operate on a dictionary through filesystem and CLI. However, graphical views could potentially include checkboxes, sliders, drop-down lists, graphs and canvases, and may offer an interesting foundation for graphical user interfaces within Awelon's application model. 

Numbers are a useful target for editable views. A viable sketch:

        #42         == (AWELON 42)
        42          == [#42 #0 integer]
        -7          == [#0 #7 integer]
        3.141       == [3141 -3 decimal]
        -0.0070     == [-70 -4 decimal]
        2.998e6     == [2998 3 decimal]
        -4/6        == [-4 #6 rational]

Awelon's natural numbers are given the `#` prefix in favor of an aesthetic view for Church-encoded signed integers. The original natural numbers are escaped - every editable view should provide a simple, unambiguous escape to raw Awelon code. Most views build upon existing views rather than raw code. Building views incrementally upon views is portable and extensible. For example, if a programmer sees `[-4 #6 rational]` because their view doesn't support rational numbers, the intention is obvious, and the programmer could easily install support for rationals into their view.

Besides the numeric tower, command lists are another critical feature:

        {foo, bar, baz} == [[foo] {bar, baz} :]
        {baz}           == [[baz] ~ :]

Command lists are useful for just about everything. We can use them to embed structured data - e.g. `{{1,2,3},{4,5,6},{7,8,9}}` might represent a matrix. We can use them for construction of continuation-passing code and effects models. And so on. 

Ideally, all editable views should also be *evaluable*. That is, program evaluation should generate the same structures we use to view and edit programs. When we add 3.141 and -0.007, we want to see 3.134 in the program output. That is, `[3134 -3 decimal]` - or whatever we use to represent decimal numbers - should be a viable result from a computation. 

Design of evaluable editable views is very sensitive to arity annotations and accelerators. A consequence is that editable views should be *defined* in the same dictionary they're used to view, for example with a word defining a `[[view→code][code→view]]` pair where `code` and `view` are represented as text. Representing views within the dictionary is convenient for testing and caching of views, and for updating views based on application or edit session state (e.g. so we can separate namespace from code). 

With evaluable views in mind, we might represent comments as:

        /* comment */  ==  " comment " (/2) (@rem) d

The arity annotation allows embedding of comments into computed values. The `(@rem)` gate serves as a lightweight indicator of the comment's 'type' (so we can add other comment types) and additionally permits integration with active debugging - for example, tracing comments to see progress, or conditionally stalling on certain comments.

*Aside:* Between command lists and numbers, word definitions can easily scale to a thousand tokens. If we start representing graphical programs with tables, graphs, canvases, radio buttons, drop-down options lists, and similar features we might scale another order of magnitude. Of course, we'll also divide larger programs into small words that can be viewed and edited together. 

## Named Locals

An intriguing opportunity for editable views is support for lambdas and let-expressions. This would ameliorate use cases where point-free programming is pointlessly onerous. Consider adapting the locals syntax used in [Kitten language](http://kittenlang.org/):

        -> X Y Z; CODE   ==  "X Y Z"(/2)(@λ)d CODE'

The `"X Y Z"(/2)(@λ)d` fragment is a lambda comment. It allows us to later render the program using the same variable names. We can compute `CODE' = T(Z, T(Y, T(X, CODE)))` with the following algorithm:

        Extract Value Argument

        T(X,E) - extract *value* X from E such that:
            T(X,E) does not contain X
            X T(X,E) == E

        T(X, E) | E does not contain X      => d E
        T(X, X)                             => 
        T(X, [E])                           => [T(X,E)] b
        T(X, F G)
            | only F contains X             => T(X,F) G
            | only G contains X             => [F] a T(X,G)
            | otherwise                     => c [T(X,F)] a T(X,G)

This is the extract argument algorithm adjusted for value words. Conveniently, we only use the four Awelon primitive combinators, and the result is about as concise as we can reasonably expect. These rewrites can be reversed, or we could just partially compute `X Y Z CODE'`, to recover the original `CODE`. 

A remaining challenge regards conditional behaviors. Consider:

        -> X Y Z; [onF] [onT]
        [-> X Y Z; onF] [-> X Y Z; onT]

The first program will copy `X Y Z` into the `onF` and `onT` paths. In a conditional behavior, we'll drop one of these paths, so the copy effort is wasted, and this is unsuitable for potential affine types. The second program is superior from that perspective, but the expression of `-> X Y Z;` is redundant. 

I want the convenience of the first and the zero-copy property of the second. This may require specializing the translation of `CODE` so it recognizes and optimizes conditional expressions.

Even so, this approach to named locals is simple and immediately useful.

## Namespaces

Qualified namespaces are readily supported by editable views. Trivially, we could support a comment like `using large_prefix as x; ...` such that subsequent code may use `x` in place of `large_prefix` (and perhaps `x.foo` in place of `large_prefix.foo`). 

More intriguingly, namespaces can be built into an editable view. 

If ever we want humans to work effectively with `$secureHash` resources, a built in namespace would be essential. If we model a view per edit session, we could tune our namespace to optimize based on whichever edits we're performing. We could also package a set of namespace declarations and give it a nickname within the view function. We might still include a `using named,view,tweaks;` hint to specialize a generic view for a program, but we can avoid the common problem of namespace boiler-plate.
