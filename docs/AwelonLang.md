
# Awelon Language

Awelon is a purely functional language based on concatenative combinators. 

## Why Another Language?

Awelon differs from other languages in its adherence to simplicity and its choice of evaluation model. There are only four simple computational primitives, one data primitive, and a simple Forth-like syntax. Evaluation is by local rewriting with lazy linking.

Like any pure language, Awelon's evaluation is separate from its effects model. Awelon explores a [RESTful application model](ApplicationModel.md) where application state is represented within a codebase, and effects are performed in context of a multi-agent system. Relevant effects patterns include [publish-subscribe](https://en.wikipedia.org/wiki/Publish%E2%80%93subscribe_pattern) and a variation on the [tuple space](https://en.wikipedia.org/wiki/Tuple_space) oriented around [work orders](https://en.wikipedia.org/wiki/Work_order). More conventional effects models are possible within orders.

Awelon's simple syntax lowers barriers for software agents, editable views, and program rewriting as a viable evaluation model. Rewrite based evaluation ensures the output of evaluation, at every step, is a program that may be serialized for debugging, distribution, or persistence. Lazy linking preserves human-meaningful words and link structure, allowing both the program and evaluated results to be uniformly viewed and understood. Favoring combinators instead of 

 human meaningful words and link structure to be preserved in evaluation output, which is convenient when interpreting b

 interpretating a result and for treating Awelon programs and values as hypermedia.

[Editable views](http://martinfowler.com/bliki/ProjectionalEditing.html) build upon Awelon's simple structure to support human-computer interactions. This applies to both direct editing of code and applications. A program could be presented as an editable GUI form or a hypermedia object.

Awelon's simplistic syntax is valuable for software agents and for generating rewritten programs as the output of evaluation. Preserving link structure in the output enables
, which is convenient for persistence, distribution, debugging, and hypermedia

Awelon's simple syntax and structure is convenient for software agents.

Support for software agents

Awelon systems leverage [editable views](http://martinfowler.com/bliki/ProjectionalEditing.html) to model domain-specific syntax, namespaces, and edit sessions. Editable views will likely be provided through web services, but it is feasible to utilize editable views through conventional text editors via [Filesystem in Userspace](https://en.wikipedia.org/wiki/Filesystem_in_Userspace) adapters. Ultimately, the human interface to Awelon's application models generalizes to an editable view.

Awelon's simplicity, purity, and evaluation model each contribute to its utility for the proposed application model. I am not aware of any other language that is similarly appropriate in the same role.

*Note:* Unlike minimalist 'esoteric' languages, Awelon takes performance seriously. A runtime will recognize and accelerate a set of common functions, effectively extending the set of primitives for purpose of performance. Acceleration of linear algebra could support GPGPU computing, and acceleration of Kahn process networks could support efficient distributed computing. JIT computation is quite feasible and can leverage accelerators. Incremental computing is important for the application model.

## Primitives

There are four primitive combinators:

            [B][A] a == A[B]    (apply)
            [B][A] b == [[B]A]  (bind)
               [A] c == [A][A]  (copy)
               [A] d ==         (drop)

Those square brackets `[]` enclose a 'block' of Awelon code, representing a function. With just this much, all computable functions can be defined. As a lightweight proof, I'll define the Curry-Schönfinkel SKI [combinators](https://en.wikipedia.org/wiki/Combinatory_logic).

            [B][A]w == [A][B]       (swap)           w = [] b a
               [A]i == A            (inline)         i = [] w a d
         [C][B][A]s == [[C]B][C]A   (S combinator)   s = [[c] a b w] a i
            [B][A]k == A            (K combinator)   k = a d

Awelon's primitive combinators are more convenient than SKI. Apply and bind provide structural control over scope, respectively hiding or capturing one value. Separation of 'copy' and 'drop' is convenient both for performance and [substructural type](https://en.wikipedia.org/wiki/Substructural_type_system) analysis. The value ignored by apply is accessible for further partial evaluations.

## Words

Words are identified by a sequence of UTF-8 characters with a few limitations. The blacklist is `@[]<>(){},;|&=\"`, SP, C0, and DEL. Developers are encouraged to further favor words that will not escapes in external contexts such as URLs, natural language, or editable views. There is no hard restriction on word sizes, but words should be kept reasonably short.

User defined words are represented in a dictionary, described later. Some words are automatically defined, including the four primitives `a`, `b`, `c`, `d` and number words such as `42`. 

## Data

Awelon language optimizes embedded representations for numbers and texts. Numbers are simply implicitly defined words like `42`. Texts have two embeddings, inline like `"hello, world!"` or multi-line:

        "
         multi-line texts starts with `" LF` (34, 10)
         each line is indented by one space (32)
         terminate the text with `LF ~` (10, 126) 
        ~

Texts must be valid UTF-8, forbidding C0 (except LF) and DEL. LF is the only special case, requiring the extra space at the start of the new line. There are no character escapes. The indent spaces are not considered part of the text, nor are the extra LF characters at the start and end.

Awelon language has exactly one primitive value type - the `[]` block of code, representing a function. Data is instead [Church encoded](https://en.wikipedia.org/wiki/Church_encoding) or favors alternatives like [Scott encoding](https://en.wikipedia.org/wiki/Mogensen%E2%80%93Scott_encoding), representing values as functions. The support for numbers and texts is close in nature to [syntactic sugar](https://en.wikipedia.org/wiki/Syntactic_sugar) for an encoding.

Before I jump to the encoding of numbers and texts, let us examine encodings for other useful data types. Booleans select from a pair of functions:

        [onF][onT] false i == onF               false = [d i]
        [onF][onT] true  i == onT               true  = [a d]

        Using Definitions:
           [A] i == A                           i = [] w a d 
        [B][A] w == [A][B]                      w = [] b a   

Booleans can be generalized to algebraic sum type `(A + B)`.

        [onL][onR] [[A] inL] i  == [A] onL      inL = w d w i
        [onL][onR] [[B] inL] i  == [B] onR      inR = w b a d

Construction of the `[[A] inL]` construct is trivial. Use `[A] [inL] b`, with `b` binding the argument. This might be abstracted as `mkL`.  This is generally the case for value constructors. In practice, we may also wish to defer computation of partial constructions (see *Deferred Computations and Coinductive Data*). 

Pairs - algebraic `(A * B)` product types - may also be Church encoded:

        [onP] [[B][A] inP] i == [B][A]onP       inP = [] b b a i

However, in context of Awelon language, it's simpler and more convenient to encode a pair as `[[B][A]]` and simply use `a i` to apply it. 

Between algebraic sums and products, we can represent any conventional data structure. The option type `(1 + A)` may be modeled as a choice of `false` or `[[A] R]`. Natural numbers can be encoded as `μNat.(1 + Nat)` - i.e. zero or successor. A list can be modeled as `μList.(1 + (A * List))`. Text can be encoded as a list of natural numbers. Algebraic encodings are generally simple, e.g. `Succ = mkR` for natural numbers, or `Cons = mkP mkR`. 

An alternative is to model data as folding over its structure. The Church encoding of natural numbers is an example of this. Consider: 

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

It is my intention that Awelon developers be given relatively free reign over the choice of encoding and corresponding tradeoffs. In practice, this control may be limited by runtime performance support (see *Acceleration*). But the encoding is at least separate from the syntactic embedding.

This is achieved by allowing developers to partially define the encoding. For natural numbers and texts, we end up with something like the following:

        #0 is user definable (zero)
        S# is user definable (succ)
        #1 = [#0 S#]
        #2 = [#1 S#]
        ...

        ~ is user definable (nil)
        : is user definable (cons)
        "" = ~
        "hello" = [#104 "ello" :]
        "→" = [#8594 ~ :]

At the moment, only natural numbers are supported. 

Long term, Awelon will likely support signed integers, rationals, decimal numbers. The normal Church encoding for signed numbers is a pair of natural numbers where one of the pair is zero. We might similarly support rationals (a numerator and denominator pair) or decimal numbers (a significand and exponent pair). Awelon language conservatively reserves words with prefix `[+-~.#]?[0-9]` for purpose of representing and efficiently recognizing numbers.

*Aside:* Beyond numbers and texts, users might be interested in more structured data with human-meaningful symbolic labels. Awelon language does not optimize for those cases, but support for *Editable Views* may present structured data to humans.

## Binary Data

Binaries can be embedded with base64 text. But support for external binaries avoids space and conversion overheads of embedding binary data within the Awelon syntax, and is generally more convenient in context of integration with external systems and hypermedia. 

Awelon language supports reference to external binary data via `%secureHash`. 

A runtime will have implicit access to resources named by secure hash, perhaps through the filesystem or network. The binary in question is easily loaded into memory, easily verified. Any change in the binary must be reflected in the Awelon code, which is convenient for partial evaluations or caching. Binary data is treated like embedded text, except our values are in the range `#0 .. #255` instead of codepoints.

*Note:* Developers are encouraged to model [rope-like](https://en.wikipedia.org/wiki/Rope_%28data_structure%29) structures if partial structure sharing could offer significant benefits.

## Acceleration

Acceleration is a performance assumption for Awelon language. A runtime should recognize and accelerate common functions. The accelerated function may use a hand-optimized or built-in implementation to achieve performance similar to a primitive. For example, most runtimes will accelerate the following functions:

           [A]i == A            (inline)         i = [] w a d
        [B][A]w == [A][B]       (swap)           w = [] b a

Accelerated functions serve a role similar to 'built in' functions in other languages. However, accelerated functions *must* be given an explicit definition in the Awelon language as part of the user's dictionary. Users should be able to review and interact with this definition like any other. However, modifying or renaming an accelerated function may generally have a severe and negative impact on performance.

Critically, acceleration of functions extends to *data*, and compact representation thereof. Natural numbers, for example, might be represented during computation by normal machine words. Accelerated functions may be optimized for operation directly on compact representations. Hence, if we accelerate both natural numbers and *arithmetic* upon them, we can effectively treat natural numbers as language primitives for performance.

The long term intention is for Awelon language to accelerate not just arithmetic, but also binary and collections processing, conversion of texts to binary and vice versa, linear algebra, and parallel [process networks](https://en.wikipedia.org/wiki/Kahn_process_networks). Acceleration of linear algebra, for example, could permit semi-transparent GPGPU parallelism. And acceleration of process networks could enable Awelon computations to semi-transparently leverage cloud computing. 

A few carefully targeted accelerators can achieve excellent performance in a broad array of problem domains.

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

Annotations, by nature, must have no internally observable effect on computation. Nonetheless, annotations may cause an incorrect computation to fail fast, simplify static detection of errors, or support useful external observations like debug logs or breakpoint states. 

Annotations may be introduced and documented on a runtime basis. In case of porting code, runtimes that do not recognize an annotation may ignore it. Long term, we should reach some de-facto standardization on useful annotations.

## Deferred Computations and Coinductive Data

The *arity annotations* `(/2)` to `(/9)` have simple rewrite rules:

                             [B][A](/2) == [B][A]
                          [C][B][A](/3) == [C][B][A]
                                        ..
        [I][H][G][F][E][D][C][B][A](/9) == [I][H][G][F][E][D][C][B][A]

To clarify, it is the *annotation* that has the given arity. Arity annotations specify nothing of their context.

Arity annotations serve a critical role in controlling computation. For example, the program `[[A](/2)F]` has the same type and semantics as `[[A]F]`, but the former prevents partial evaluation of `F` from observing `[A]`. Arity annotations can be used to guard against useless partial evaluations. For example, if we define swap as `w = (/2) [] b a` then we can avoid observing the useless intermediate structure `[A] w => [[A]] a`. 

Arity annotations serve a very useful role in modeling [thunks](https://en.wikipedia.org/wiki/Thunk) and [coinductive data](https://en.wikipedia.org/wiki/Coinduction). It is sometimes useful to model 'infinite' data structures to be computed as we observe them - procedurally generated streams or scene graphs.

As a related utility, I introduce a `(force)` annotation. 

        [computation](force)

This introduces a 'forced' evaluation mode for the given block. A forced evaluation operates as if there were sufficient arguments to the left of `computation` for purpose of deleting arity annotations without actually providing those arguments. The computation may still get stuck and prevent further evaluation, but it at least won't be stuck on an arity annotation.

*Note:* Awelon language does not implicitly memoize computations to avoid rework. However, programmers can explicitly use `(memo)` to share work, and it is feasible for a runtime to optimize lightweight memoization of deferred computations. See *Memoization*.

*Aside:* Arities should rarely stray above `(/5)`. Beyond a handful of parameters, programmers should refactor for simplicity or introduce parameter objects.

## Stowage

Stowage is a simple idea, summarized by rewrite rules:

        [large value](stow) => [$secureHash]
        [small value](stow) => [small value]

The resulting `$secureHash` resource is similar to external binary resources, excepting we interpret the binary as Awelon code. Use of `(stow)` will wait for evaluation. Use of `(stow)` on large binary data will instead produce a `%secureHash` external binary.

Stowage enables programmers to work semi-transparently with computations that are much larger than working memory. Unlike conventional virtual memory or effectful storage, stowage is friendly for parallelism and distribution, structure sharing, incremental computing, and backtracking. However, the most effective use of stowage is more limited to [persistent data structures](https://en.wikipedia.org/wiki/Persistent_data_structure), optimally those that implicitly batch writes such as [LSM trees](https://en.wikipedia.org/wiki/Log-structured_merge-tree).

Stowage has significant lookup and GC overheads. Values with small representations should not be stowed. The threshold for 'small' is heuristic, but must at least be larger than the encoded secure hash and be deterministically reproducible. A reasonable threshold is six times the size of the secure hash. 

## Dictionary

An Awelon dictionary doubles as a codebase and a substrate for multi-agent application state. The dictionary representation must support efficient lookup in context of high frequency concurrent updates. I propose a DVCS-inspired patch based representation:

        secureHashOfPatch1
        secureHashOfPatch2
        @word1 definition1
        @word2 definition2
        ...

Each patch contains a list of patches to logically include (via secure hash) followed by a list of definitions overriding specific words. A word may be logically deleted by defining a trivial cycle like `@foo foo`. The last definition of a word wins. Whitespace (SP, LF) surrounding each definition is implicitly trimmed. A dictionary is represented by a 'root' patch.

As a dictionary shared with software agents, this is not optimal for humans. The intention is that human programmers should be operating on a dictionary through editable views or edit sessions, browsing the dictionary as hypermedia and bringing relevant definitions into scope as needed.

A runtime might usefully document recognized accelerators, annotations, and other features by specifying seed dictionary, or a set of seeds (to account for portability, versioning). 

*Note:* Non-trivial cycles are errors - loop behavior must be modeled via *fixpoint*, not link layer recursion. But the trivial cycle conceptually matches the behavior of an undefined word, so we'll take advantage.

*Note:* Some words are implicitly defined. This includes the four primitives `a b c d`, most number words, and secure hash resources. These should not be represented within the dictionary, or if represented should be equivalent to the implicit definition.

## Secure Hash Resources 

Binaries, stowage, and dictionaries will use the same secure hash: 384 bits of [BLAKE2b](https://blake2.net/) encoded as 64 characters in [base64url](https://en.wikipedia.org/wiki/Base64). BLAKE2b is an efficient, high quality hash function. I feel that 64 characters offers a reasonable balance between collision resistance and aesthetics.

Secure hash resources are generally not defined within the normal dictionary representation. Instead, they are defined in an implicit global space and may be shared among many dictionaries and runtimes. These resources are very easy to cache and distribute through [content delivery networks (CDNs)](https://en.wikipedia.org/wiki/Content_delivery_network). For an untrusted CDN we could compress and encrypt the content, using 128 bits of the secure hash for a lookup key and the rest for AES decryption.

## Evaluation

Evaluation of an Awelon program results in an equivalent Awelon program, hopefully one from which it is easier to extract information or use efficiently in further evaluations. Evaluation proceeds by pure, local rewriting. The four primitives rewrite based on simple pattern matching:

            [B][A] a => A[B]    (apply)
            [B][A] b => [[B]A]  (bind)
               [A] c => [A][A]  (copy)
               [A] d =>         (drop)

Annotations may have special rewrite rules. For example, arity annotations remove themselves if sufficient arguments are available. Stowage rewrites large definitions to smaller secure hashes.

Words rewrite to their evaluated definitions - this is called linking. However, words link *lazily* to preserve human-meaningful hypermedia link structure in the evaluated output where feasible. Words do not link unless doing so leads to additional rewrites of primitives or annotations, something more interesting than simple inlining of the word's evaluated definition. Consequently, arity annotations are useful to control linking of words.

Awelon's evaluation strategy is simple:

* rewrite outer program
* evaluate before copy
* evaluate final values

This strategy isn't lazy or eager in the conventional sense. Rewriting the outer program first provides opportunity to apply annotations or drop values that won't be part of our outputs. Evaluation before copy guards against introduction of unnecessary rework. Evaluation of final values (blocks) brings us to a normal form that cannot be further evaluated. 

Annotations don't much affect this evaluation strategy. The exception is `[computation](par)` to advise early, parallel evaluation of a result we'll probably need.

*Aside:* Undefined words do not rewrite further. In practice, evaluation with undefined words is useful for some application patterns, e.g. involving monotonic futures and promises. And also for initial development and debugging.

## Fixpoint

Fixpoint is a function useful for modeling loop behaviors. For Awelon language, I recommend the following variant of the [strict Z fixpoint combinator](https://en.wikipedia.org/wiki/Fixed-point_combinator#Strict_fixed_point_combinator):

        [X][F]z == [X][[F]z]F 
        z = [[c] a [(/3) c i] b b w i](/3) c i

        Using Definitions:
           [A]i == A            (inline)         i = [] w a d
        [B][A]w == [A][B]       (swap)           w = (/2) [] b a

The arity annotation `(/3)` defers further expansion of the `[[F]z]` result. This variation of the Z combinator has the advantage that the definition of `z` can be found in the naive evaluation, and that `[F]` is not replicated unnecessarily. I recommend that readers unfamiliar with fixpoint step through evaluation of `[X][F]z` by hand a few times to grasp its behavior.

Fixpoint is notoriously difficult for humans to grok and sometimes awkward to use. Instead of directly using fixpoint, we'll want to build more comfortable loop abstractions above it like [generators](https://en.wikipedia.org/wiki/Generator_%28computer_programming%29) and folds. We would benefit from accelerating at least fixpoint, and possibly some of the more popular loop abstractions.

## Memoization

The primary basis for incremental computing in Awelon is [memoization](https://en.wikipedia.org/wiki/Memoization). Evaluation of word definitions will tend to be implicitly memoized. But programmers may also request memoized evaluation explicitly:

        [computation](memo)

Memoization is conceptually performed by by seeking the computation in a runtime lookup table. This lookup must account for words having different meanings in context of different dictionaries. Assuming the computation is found in the table, the runtime will replace the computation by the evaluated result. If the computation is not found, we evaluate then heuristically store the computation into the table based on observed and estimated future time and space tradeoffs.

Naive use of a lookup table can work, but is not the most optimal approach to memoization. For example, ideally we want to control recomputing if there is a change to a word we do not link. To address this memoization would require instrumented evaluation that tracks linking, and only checks the relevant subset of dependencies. One might look into research on adaptive memorization, trace reuse, etc..

*Note:* To effectively support incremental computing, memoization must be used in context of cache-friendly application patterns such as command pattern, and compositional views on persistent data structures.

## Static Linking

It is possible to perform static analysis on a word's evaluated definition, arities and types, etc. to determine context-free link structure. The most trivial example of this is redirects. Consider:

        @foo bar

Here, word `foo` will not evaluate any further because there is no cause to link `bar`. However, when we do eventually link `foo`, we'll immediately be asked to link `bar`. We can determine this by static analysis. For performance reasons, we may wish to skip the intermediate step rewriting `foo` to `bar` and jump straight to linking the definition of `bar`. 

Statically computing link structure can support inlining of definitions, flattening of redirect chains, and avoid rework for link decisions. It is a recommended as a performance optimization. However, this should not be observable 

## Value Words

Words like `true` or `false` or `#42` can be treated as first-class values. Further, doing so is convenient for aesthetics and preservation of hypermedia link structure. 

        #42 true w == true #42
        #42 [] b   == [#42]

In general, a value word is any word that evaluates to a singleton `[Value]`. In this case, we have `true = [a d]` and `#42 = [#41 S#]`. A runtime should recognize value words and treat them as values until their internal structure is observed.

## Rewrite Optimization

Awelon's semantics allow for some rewrites that aren't performed by the basic evaluation strategy. Consider:

        [] a    =>
        [i] b   =>
        b i     =>  i
        c d     =>
        c w     =>  c

A typical example of rewrite optimizations regards mapping a pure function over values in a list or stream. In this case the principle observation is `[G] map [F] map == [G F] map`, and we might recognize this in a rewrite rule as `map [F] map => [F] compose map` (where `[G] [F] compose == [G F]`).

Rewrite optimizations can also be utilized for compaction purposes, e.g. translationg `[#0 S#]` to `#1` or recovering the `z` symbol after evaluation of the Z fixpoint combinator. An intriguing possibility is to localize some secure hash resources by rewriting to local dictionary words.

A runtime is free to support rewrite optimizations insofar as they are proven valid. Unfortunately, I lack at this time effective means for a normal programmer to propose and prove rewrite optimizations through the dictionary. But we can at least get a useful start by runtime rewriting of known, accelerated functions.

*Aside:* Rewrite optimizations in practice are ad-hoc and fragile. Use of a `[function](rw)` to precisely stage rewrite optimizations can help, but relying upon rewrite optimizations for performance critical code is not highly recommended.

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

In addition to controlling output counts, programmers may wish to fail fast based on declared structure. To support this, Awelon supports a structure annotation `(:T)` and assertion `(.T)` with the following rewrite semantics:

        (:foo) (.foo) ==

In practice, we might construct a tagged value with `[(:foo)] b` and deconstruct it with `i (.foo)`. The symbol `foo` or `T` is left to the programmer, but must match between annotation and assertion. These annotations resist *accidental* access to structured data. As with tuple assertions, we can fail fast dynamically or detect an error statically.

## Substructural Scoping

[Substructural types](https://en.wikipedia.org/wiki/Substructural_type_system) allow us to reason about whether a value is used, or how many times it is used. This is convenient for modeling finite resources, intermediate states in a protocol, or ensuring certain steps are performed by a client computation. Awelon provides simple annotations for lightweight substructural types:

* `(aff)` - mark a value affine, non-copyable (op `c`)
* `(rel)` - mark a value relevant, non-droppable (op `d`)
* inherit substructure of bound values (op `b`).

We can eliminate substructural annotations by observing a value with `a`. It is not difficult to track and validate substructural properties dynamically, or to represent them within static types. 

## Error Annotations

We can mark known erroneous values with `(error)` as in `"todo: fix foo!"(error)`. If we later attempt to observe this value (with `i` or `a`), we will simply halt on the error. However, we may drop or copy the value like normal. In addition to user-specified errors, a runtime might use error annotations to highlight places where a program gets stuck, for example:

        [A](aff)c   =>  [[A](aff)c](error)i
        [[A][B][C]](2) => [[A][B][C]](2)(error)

Errors presented in this manner are relatively easy to detect. 

## Active Debugging

Active debugging broadly includes techniques to observe an evaluation in progress. Common techniques for active debugging include logging, profiling, breakpoints. To support active debugging, Awelon introduces `(@gate)` annotations. The symbol is user-defined, e.g. it could be `(@foo)` or `(@stderr)`. 

The `(@gate)` annotation is considered 'active' when it has a block to its left: 

        [A](@gate)

The behavior of an active gate is configured at the runtime. Outside of debugging, for example, we'll simply delete the active gates and pass the values. Consider a few debug configurations:

* stall - keep `[A](@gate)` for now
* trace - copy value to debug log
* profile - record stats for value

Stalls effectively give us breakpoints by preventing the program upstream of the gate from using the value. Unlike conventional breakpoints, Awelon will continue to evaluate other parts of the program as far as they can go despite the stalls. To avoid rework or significant changes in evaluation order, if evaluation of `P` stalls on a gate, copy operation `[P]c` must stall on `P`. 

Tracing gives us standard 'printf' style debugging. Note that copying the value implicitly requires evaluating it first, but is not subject to normal limitations like `(aff)`. The other copy of the value is passed.

Profiling might tweak `[A]` to record performance metadata (e.g. allocations, rewrites, times, use of memoization or stowage) to named statistics objects, then pass it on. This would allow us to accumulate a lot of useful performance information quickly.

These are just example configurations. In general, gates may have conditional configuration based on observation of the value. Also, a runtime should probably have a reasonable default configuration, such that `(@stderr)` traces to a standard debug log or `(@prof:x)` will profile the arguments under name `x`.

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

Awelon can be evaluated without static typing. But if we can detect some errors early by static analysis, that's a good thing. Further, static types are also useful for verifiable documentation, interactive editing (recommending relevant words based on type context), and performance of JIT compiled code. Strong static types makes a nice default.

We can represent our primitive types as:

        a   : S B (S → S') → S' B
        b   : S B (E B → E') → S (E → E')
        c   : S A → S A A
        d   : S A → S
        [F] : S → S type(F)

In this case the sequence `S C B A` is shorthand for `(((S * C) * B) * A)` where `*` is a product constructor. This aligns with a program structure `S[C][B][A]`. Effectively, `S` is the rest of our stack when we view the program as operating on a stack-like structure. 

It is easiest in Awelon to detect static type errors in context of annotations like `(3)`, `(nat)`, `(bool)`, `(aff)`, `(.foo)` that allow for fail fast dynamic errors. For example, `(bool)` might effectively assert its argument is a function of type: `∀S,S'. S (S → S') (S → S') → S'`. This provides a basis for unification of `S` and `S'` on the conditional paths.

*Aside:* Support for `(bool)`, `(opt)` and `(sum)` to cover the common conditional types `(1+1)`, `(1+A)`, and `(B+A)` would be pretty useful.

## Dynamic Typing

It is sometimes difficult to compute a strong static type - e.g. for print-formatting or processing of DSLs.

For these cases, we might use `(dyn)` to explicitly document that we probably won't be able to statically typecheck a given function. This will a simple rewrite semantics: `[A](dyn) => [A]`. That is, once we've computed a block, we can presumably provide a static type for that block. Example usage:

        "abcx → ax^2 + bx + c" runPoly =>
            "abcx → ax^2 + bx + c" compilePoly (dyn) i =>
            [polynomial behavior](dyn) i =>
            [polynomial behavior]i

Dynamic typed functions can serve a useful role for macro-like partial evaluation.

## Editable Views

## Comments

