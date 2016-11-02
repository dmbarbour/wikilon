
# Awelon Language

Awelon is a purely functional language based on concatenative combinators. 

## Why Another Language?

Awelon differs from other languages in its adherence to simplicity and its choice of evaluation model. There are only four simple computational primitives, one data primitive, and a minimal syntax. Evaluation is by local rewriting and preserves human-meaningful symbols and link structure, which is convenient for persistence, distribution, debugging, and hypermedia.

Like any pure language, evaluation is separate from effects. Awelon explores a [RESTful application model](ApplicationModel.md) where application state is represented within a codebase, and effects are performed in context of a multi-agent system. Relevant effects patterns include [publish-subscribe](https://en.wikipedia.org/wiki/Publish%E2%80%93subscribe_pattern) and a variation on the [tuple space](https://en.wikipedia.org/wiki/Tuple_space) oriented around [work orders](https://en.wikipedia.org/wiki/Work_order). More conventional effects models are possible within orders.

Awelon systems will leverage [editable views](http://martinfowler.com/bliki/ProjectionalEditing.html) to model domain-specific syntax, namespaces, and edit sessions. Editable views will likely be provided through web services, but it is feasible to utilize editable views through conventional text editors via [Filesystem in Userspace](https://en.wikipedia.org/wiki/Filesystem_in_Userspace) adapters. Ultimately, the human interface to Awelon's application models generalizes to an editable view.

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

Words are identified by a sequence of UTF-8 characters with a few limitations. The blacklist is `@[]<>(){},;|&=\"`, SP, C0, and DEL. Developers are encouraged to further limit words to symbols that will not need escaped in external contexts, such as URLs, natural language, or editable views. While there is no hard restriction on word sizes, I would also encourage developers to keep words relatively short.

User defined words are represented in a dictionary, described later. Some words are automatically defined, including the four primitives `a`, `b`, `c`, `d` and number words such as `42`. 

## Data

Awelon language optimizes embedded representations for numbers and texts. Numbers leverage predefined words like `42`. Texts have two embeddings, inline like `"hello, world!"` or multi-line:

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

## Acceleration

Acceleration is a performance assumption for Awelon language. A runtime should recognize and accelerate common functions. The accelerated function may use a hand-optimized or built-in implementation to achieve performance similar to a primitive. For example, most runtimes will accelerate the following functions:

           [A]i == A            (inline)         i = [] w a d
        [B][A]w == [A][B]       (swap)           w = [] b a

Accelerated functions serve a role similar to 'built in' functions in other languages. However, accelerated functions *must* be given an explicit definition in the Awelon language as part of the user's dictionary. Users should be able to review and interact with this definition like any other. However, modifying or renaming an accelerated function may generally have a severe and negative impact on performance.

Critically, acceleration of functions extends to *data*, and compact representation thereof. Natural numbers, for example, might be represented during computation by normal machine words. Accelerated functions may be optimized for operation directly on compact representations. Hence, if we accelerate both natural numbers and *arithmetic* upon them, we can effectively treat natural numbers as language primitives for performance.

The long term intention is for Awelon language to accelerate not just arithmetic, but also compact binaries, collections processing, linear algebra, and parallel [process networks](https://en.wikipedia.org/wiki/Kahn_process_networks). Acceleration of linear algebra, for example, could permit semi-transparent GPGPU parallelism. And acceleration of process networks could enable Awelon computations to semi-transparently leverage cloud computing. 

I believe a few well targeted accelerators can achieve excellent performance.

## Annotations

Annotations help developers control, optimize, and debug computations. Annotations are represented as parenthetical words like `(par)` or `(/3)`. Some useful examples of annotations include:

* `(/2)..(/9)` - arity annotations to defer computations
* `(0)..(9)` - tuple type assertions for scope control
* `(aff) (rel)` - support substructural type safety
* `(:foo) (.foo)` - lightweight type tag and assertions
* `(nat)` - use accelerated representation for natural number
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

The *arity annotations* `(/2)` to `(/9)` have the following rewrite rules:

                             [B][A](/2) == [B][A]
                          [C][B][A](/3) == [C][B][A]
                                        ..
        [I][H][G][F][E][D][C][B][A](/9) == [I][H][G][F][E][D][C][B][A]

These annotations serve a critical role in controlling computation. Consider construction of `[[A](/2)F]`. This has the same semantics and static type as `[[A]F]`, but the arity annotation prevents partial evaluation with the argument. Arity annotations are convenient for guarding against 'pointless' partial evaluations. For example, we might define `w = (/2) [] b a` because a partial swap isn't useful or interesting.

Arity annotations can be leveraged to represent 'infinite' [coinductive data structures](https://en.wikipedia.org/wiki/Coinduction), such as procedurally generated streams or scene graphs. The structure is computed only insofar as it is observed.

To simplify work with deferred compuations, I will introduce two more annotations: 

        [computation](force)
        [computation](memo)

Use of `(force)` eliminates arity annotations waiting on extra arguments to the computation. This forces partial evaluation without actually providing the additional arguments. Use of `(memo)` enables caching of a computation to avoid rework. Memoization is more broadly relevant for incremental computing, and is discussed later.

## Stowage

Stowage is a simple idea, summarized by rewrite rules:

        [large value](stow) => [$secureHash]
        [small value](stow) => [small value]

Effectively, stowage is a form of [virtual memory](https://en.wikipedia.org/wiki/Virtual_memory), but is performed at a language-aware link layer. The large value is pushed to disk and replaced by a word containing a secure hash of the value. If that word is later required during evaluation, we'll load it back from memory.

Use of a secure hash has many nice properties. It is deterministic, reproducible by parallel computations, stable for memoization and caching. Structure sharing is implicit. Each definition is verifiable. It's easy to conceptualize stowage as discovery of an already defined word rather than allocation of a word. We effectively have a global stowage namespace. Provider-independent secure distribution is feasible, using part of the hash for lookup and the rest as an decryption key.

Of course, stowage has storage, lookup, and GC overheads. Values with small representations should not be stowed. The threshold for 'small' is heuristic, but should at least be larger than the secure hash. A reasonable threshold is six times the size of the secure hash.

*Aside:* Conventional programming uses *effects* to interact with external storage services like a file system, database, or block device. However, use of effects to simply work with large data is awkward in contexts that may involve incremental computing, backtracking and partial replay, or explicit control over effects. Stowage separates the concerns of large data and effects.

*Note:* Stowage is most efficient when used to model persistent structures with built-in write batching like [LSM trees](https://en.wikipedia.org/wiki/Log-structured_merge-tree) or the recently developed [hitchhiker tree](https://github.com/datacrypt-project/hitchhiker-tree). [Finger trees](https://en.wikipedia.org/wiki/Finger_tree), to model queues or deques mostly modified at the ends, are also an excellent structure. 

## Binary Data

Awelon language can reference external binary data via `%secureHash`.

Binaries may also be embedded within Awelon code, for example with base64 text and a conversion word. Embedding binaries may be convenient for relatively small binary data. However, external binaries are more efficient at large scales, and are easier to integrate with external systems. 

Binaries will share sequence structure with embedded texts (that is, the `:` cons and `~` nil). The only difference is that binaries will use a sequence of bytes in the range `#0 .. #255` instead of Unicode codepoints.

*Notes:* Stowing a large binary may generate an external binary reference, but dedicated annotations might be necessary for precise control. If small parts of a binary are updated frequently, developers should consider use of [rope-like](https://en.wikipedia.org/wiki/Rope_%28data_structure%29) structures to simplify incremental computing.

## Dictionary

The Awelon dictionary is a set of defined words, and doubles as both a codebase and a substrate for [application state](ApplicationModel.md). We can expect frequent updates, history and versioning, forks and merges, need for efficient distribution, shared objects, and reusable cached compilation. To support these features, Awelon uses a DVCS-inspired patch-based dictionary representation: 

        secureHashOfPatch1
        secureHashOfPatch2
        @word1 definition1
        @word2 definition2
        ...

A dictionary is represented by a root patch. A patch is a UTF-8 string with a head and a body. The head is a series of secure hashes, one per line, each a binary data reference to a patch to logically include in place of the secure hash. The body is a series of word definitions, each starting a new line, overriding the prior definition for a word. Use of `@` at the start of a line is not valid within Awelon language, so it's easy to partition definitions without parsing them. The whitespace (SP, LF) surrounding a definition is implicitly trimmed.

This patch-based representation gives a lot of freedom for how dictionaries are constructed and shared. For example, a dictionary might be an append-only log, or (with clever external indexing) an LSM-tree. A large dictionary with a small head patch can cheaply be forked. Three-way merges are possible if we can efficiently identify a common origin. Use of secure hashes to name patches simplifies caching, distribution, structure sharing, incremental indexing, and separate compilation of dictionaries. 

*Note:* This is ultimately a *machine oriented* representation. Secure hashes generally aren't human-meaningful organizations of code. Awelon language is intended for use at a larger scale through editable views.

Words may be logically deleted from a dictionary by defining trivial cycles like `@foo foo`. Some words are automatically defined like numbers, stowage, or external binaries. Users may represent such words within the dictionary, but their definitions should be obviously equivalent the automatic definition (otherwise an error may be raised). The four primitive words `a b c d` may not be defined, but may be represented within the dictionary as logically deleted words `@a a` and so on. 

*Note:* Non-trivial cycles are errors. Loop behavior must be modeled via *Fixpoint*, not at the link layer. It is always possible to inline valid definitions to produce a finite program with the same behavioral semantics. Of course, this expansion might be exponential, so is not performed by default.

*Note:* Specifying a dictionary is a decent way to document annotations, accelerators, etc.. A runtime might specify multiple accelerated dictionaries for versioning and portability.

## Secure Hash Data

Awelon language uses secure hashes for dictionary patches, binary resources, and stowage. Data referenced via secure hash may be held in an implicit global system, separate from any specific dictionary or runtime.

The secure hash of choice is BLAKE2b, 384 bits, encoded as 64 characters of base64url. This is small enough for aesthetic purposes (less than one line of text), large enough to resist collisions, and suitable for use in URLs. BLAKE2b is a very efficient secure hash.

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

Annotations can tune the basic strategy a little. In particular, use of `[computation](par)` will begin parallel evaluation of the computation, and might be understood as "will probably need" advice. There are no strictness or laziness annotations, but see *Deferred Computations and Coinductive Data*.

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

As we define more words, valid rewrites will cover a broader spectrum. A typical example is `map [G] map => [G] compose map` where `map` applies a function to every value in a collection. And a very useful example would be writing some constructs back to accelerated words. Translating `[#0 S#]` to `#1` would be considered a rewrite optimization, as is recovering the `z` symbol after evaluation of the fixpoint combinator.

A runtime is free to support rewrite optimizations insofar as they can be proven valid. Unfortunately, I do not currently have a good way for users to recommend rewrites and demonstrate proofs. But a runtime can at least support rewrite optimizations for accelerated functions.

*Aside:* Rewrite optimizations tend to be ad-hoc and fragile, sensitive to staging, best used for small gains or aesthetic benefits. A runtime could support a `[function](opt)` annotation to perform rewrite optimizations and others. 

## Compilation

Compilation of functions, especially those containing loops (fixpoints, folds, etc.), can offer significant performance benefits. This might be requested explicitly via `[function](jit)`. Compilation is subject to a lot of low level interactions with acceleration, quotas, GC, parallelism, and so on. I won't detail it here. But I don't have any particular reason to believe compiling Awelon will be difficult.

*Aside:* Extraction of Awelon functions for use without a runtime is also a form of compilation. For performance with Awelon project's application model, the focus should be JIT.

## Parallel Evaluation

Local rewriting is naturally parallel. A runtime can leverage this by supporting the `(par)` annotation or by implicitly parallelizing computation of final values. For incremental computing, we might also parallelize propagation of updates through a dictionary.

But to truly maximize parallelism, we'll want to accelerate [process networks](https://en.wikipedia.org/wiki/Kahn_process_networks) and [linear algebra](https://en.wikipedia.org/wiki/Linear_algebra). 

Acceleration of process networks would enable distributed evaluation using processes and message queues but still result in a pure, deterministic computation. Intriguingly, we can usefully interact with a process network that is still under evaluation, only waiting when a requested result is still being computed. Variations of process networks can optimize for reactive models.

Acceleration of linear algebra enables us to take greater advantage of low-level parallelism: SSE, GPGPUs, etc.. This could provide an effective basis for real-time graphics, sound, physics simulations, and so on.


## Structural Scoping

## Substructural Scoping

## Runtime Errors

## Active Debugging

## Type Safety

## Editable Views

