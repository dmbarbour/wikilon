
# Awelon Language

Awelon is a purely functional language based on concatenative combinators.

## Why Another Language?

Modern programming languages can produce useful artifacts - applications, games, and so on. But they do not permit flexible composition and decomposition of these artifacts. There are a lot of complicating factors - hidden runtime structure, entanglement of effects and environment, separation of programmer and user interfaces, scalability to benefit from network effects, even how software is shared in a community.

Awelon language aims to address those complications. Important design points:

* Awelon evaluates by local confluent rewriting, much like expression `(6 * 7)` evaluates to `42` in arithmetic. Every evaluation step has representation within Awelon language and may thus be serialized or rendered. Rewrite based evaluation simplifies debugging, distribution, integration of the program with HCI, and further use of program results.

* Awelon specifies 'lazy linking' of words by the evaluator. That is, a word is not rewritten to its definition unless doing so would result in further rewrites. This allows for a more compact representation of results and ensures human-meaningful link structure and symbols are preserved by the evaluator.

* Awelon has a simple, Forth-like syntax. This lowers barriers for [projectional editing](http://martinfowler.com/bliki/ProjectionalEditing.html). Editable views can support domain-specific notations or allow interactive editing of Awelon via forms or graphs. This feature combines nicely with rewriting and lazy linking, enabling the same views to apply to results. 

* Awelon has a simple, pure, and portable semantics. Programs are concatenative, which simplifies composition and decomposition. Scope is easy to control, which simplifies sharing of code and integration of untrusted code. Performance is achieved via annotations and acceleration of useful models, as opposed to ad-hoc language extension with primitive types or FFI.

* Awelon is structured in terms of hierarchical, self-contained dictionaries instead of packages and libraries. The self-contained dictionary ensures control and versioning of deep dependencies, while hierarchy enables robust integration and maintenance of independently developed resources. This structure encourages community curation instead of individual ownership.

* Awelon scales easily, leveraging uniform serializability with other features. Lazy linking enables larger than memory structures. This extends dynamically with stowage annotations to move large objects to disk and reference them by secure hash word. Pure evaluation allows memoization for spreadsheet-like incremental computations, and this extends readily to stowed objects. Acceleration of Kahn process networks would extend Awelon to long-running distributed systems computations.

Awelon also experiments with alternative [application models](ApplicationModel.md). There is a preference for RESTful approaches - documents, publish subscribe, tuple spaces, etc. - where application state is integrated with a codebase and maintained by a community of human and software agents.

In any case, Awelon diverges significantly from conventional language design. This isn't just a new language, it's a new way of thinking about programming that uses ideas from spreadsheets, functional programming, and web services. Awelon does not attempt to integrate directly with existing systems - there is no FFI, for example. But indirect systems integration is feasible through web services, software agents, and compilation between languages.

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

Awelon's primitive combinators are significantly more convenient than SKI: Apply and bind provide structural control over scope, respectively hiding or capturing one value. It is possible to abstract and refactor patterns involving multiple arguments or results. Explicit copy and drop simplify [substructural](https://en.wikipedia.org/wiki/Substructural_type_system) analysis and uniqueness tracking of type and value references.

## Words

Words are identified by a non-empty sequence of UTF-8 codepoints with a few limitations. The blacklist is `@#[]()<>{}\/,;|&='"`, C0 (0-31), SP (32), grave (96), DEL (127), or the replacement character. Developers are encouraged to favor words that won't need escapes in most external contexts (such as URLs, HTML, Markdown, natural language text, or editable views), and that aren't too large. 

A useful subset of words is automatically and implicitly defined:

* the four Awelon primitive words `a`, `b`, `c`, `d`
* words to encode natural numbers, regex `[1-9][0-9]*`
* secure hash resources `$secureHash` or `%secureHash`

Other words are user defined, through a dictionary. There is no hard limit on word size, but words should ideally be kept small. Words in Awelon are specific to their UTF-8 encoding - for example, use of a combining diaeresis is distinct from use of a precombined codepoint, and Awelon is case sensitive.

*Note:* Words may have extrinsic meaning and structural conventions understood by external agents and services, such as `foo.doc` to represent documentation, or `foo.type` to provide hints and assertions for static type analysis. This meaning is independent of Awelon language semantics, yet is relevant in context of [application models](ApplicationModel.md) and integration with real world systems.

## Dictionary

Awelon has a simple, DVCS-inspired patch-based dictionary representation:

        secureHashOfPatch1
        secureHashOfPatch2
        @word1 definition1
        @word2 definition2
        ...

Each patch consists of a list of patches to logically include followed by a list of definitions. Each definition is indicated by `@word` at the start of a line, followed by Awelon code. The last definition for a word wins, so definitions may override those from earlier patches, representing a trivial update model. The dictionary as a whole is a large patch updating the initially empty dictionary.

Definitions of words must be acyclic, and cyclic definitions are generally considered errors. Loop behavior in Awelon uses fixpoint combinators, not recursion (though clever application of *Editable Views* can hide a local fixpoint). However, undefined words essentially evaluate to themselves. As a special case, we may represent deletion or explicitly undefined words via trivial cyclic definition `@foo foo`.

Humans will usually observe and edit a dictionary through a level of indirection, such as a web service. Direct text editing of dictionary binaries doesn't scale nicely (I speak from experience). Also, a service has freedom to reorganize dictionaries for convenience to maintain history, improve structure sharing, optimize indexing or caching. Awelon does not specify how dictionaries and secure hash resources should be organized within a filesystem because that is not the expected use case.

*Note:* See *Hierarchical Dictionaries* for more information.

## Data

Awelon language has exactly one primitive data type - the `[block of code]`, representing a first class function. However, Awelon does provide automatic definition for natural numbers like `42` and a syntactic sugar for embedded texts like `"hello, world!"`. Multi-line texts are also supported:

        "
         multi-line texts start with `" LF` (34 10)
         non-empty lines are indented one space (32)
         text terminates with final `LF "` (10 34)
        "

Semantically, a natural number is implicitly defined as a unary structure: 

        1 = [0 S]
        2 = [1 S]
        3 = [2 S]
        ..
        42 = [41 S]
        ... ad infinitum

Definitions of `0` and `S` (zero and successor) are left to the programmer, allowing for a choice of semantics. Similarly, texts essentially act as special words with built-in definition as a list-like structure with `~` and `:` (nil and cons) being user defined:

        "hello" = [104 "ello" :]
        "→"    = [8594 "" :]
        ""      = ~

Texts must be valid UTF-8, forbidding C0 and DEL (except LF, in multi-line texts). Inline texts additionally forbid the double quote. There are no character escapes excepting indentation after LF for multi-line text.

Awelon is designed to work with acceleration and editable views to provide a complete programming experience. Acceleration supports efficient arithmetic with natural numbers and other common models, while editable views can effectively extend Awelon's syntax to support decimal numbers, lambdas, records, and so on. These are discussed in detail, below.

## Binary Data

Awelon provides no syntax for embedding binaries. It is feasible to encode relatively small binaries as base16 or base64 text like `["bdf13a76c2" hex-to-bin]`, or even directly encode a list of byte values - e.g. `[23 [148 [247 ~ :] :] :]`. However, such representations are inconvenient and inefficient. 

The recommendation for large binary data is to leverage secure hash resources, via `%secureHash`. Logically, the referenced binary would expand to the list of byte values in `0..255`. Awelon runtimes should accelerate this model, using a byte array under the hood. By referencing binary resources in this manner, we can effectively include large multimedia objects (music, textures, etc.) directly in an Awelon codebase, without translation overheads.

## Secure Hash Resources

Awelon code is evaluated in an implicit environment consisting both of a dictionary and a larger, shared environment where binaries may be referenced by secure hash. Awelon dictionaries rely on this environment to reference dictionary patches and hierarchical dictionaries. But we can also embed secure hashes within Awelon code to reference binary data and large value stowage.

* external code or stowage is referenced via `$secureHash`
* external binary data may be referenced via `%secureHash`
* dictionary patches are always referenced by secure hash

Awelon uses a 360-bit [BLAKE2b](https://blake2.net/) algorithm, and encodes the result as 60 characters [base64url](https://en.wikipedia.org/wiki/Base64). In a network context, it should be possible to request resources given their secure hashes, and perhaps form a content distribution network. (We might use only a fragment of the resource ID for lookup, and the rest for encryption.)

Example hashes, starting with `hash("test")` then hashing the result twice:

        J7URBffnfK_NVVcQNQ6D21k5A7J8Zhhwb2Ry3WLYfFc7Vy1TiE01Q4H7duKE
        Kve-Zbz23Zz28x0tTsmnuJv8dj0YGvwEVVWCbxLkAM7S6FLp6gCA0M2n_Nee
        MnrYTJyeGxLz5OSGwTwW7WAiC9alwYaOBFuu2_flmK1LGCCMqEDjkzPDL-Rl

The base64 encoding does risk hashes containing naughty words, and they're clearly too large for convenient use. However, secure hash resources are not primarily intended for direct use by humans. When humans do use hashes, I would hope for indirection via editable views or projectional editors that provide human meaningful nicknames. Editable views are discussed later.

## Acceleration

Acceleration is a performance assumption for Awelon. 

A runtime will recognize and accelerate common functions. The accelerated implementation may be hand optimized and built into the runtime to achieve performance similar to a primitive. For example, runtimes might accelerate the following functions:

           [A]i == A            (inline)         i = [] w a d
        [B][A]w == [A][B]       (swap)           w = [] b a

The runtime will look at the given definitions. Upon recognizing `[] b a`, the runtime may link `w` to an acclerated swap implementation. Whenever `i` appears at the end of a subprogram, we might leverage the tail-call optimization.

In general, recognition of accelerators may be fragile. It may be that `i = [] w a d` is recognized where the logically equivalent `i = [[]] a a d` is not recognized. Even changing function names could break accelerators. This is ultimately up to the runtime. Due to this fragility, a runtime should carefully document recognized accelerators, and provide some means to warn developers in case of bad assumptions. A useful convention is to define a prelude dictionary including the recognized accelerators, and to flag assumed accelerators by defining `foo.accel` if we expect `foo` to be accelerated.

Critically, acceleration of functions extends to data representation. Natural numbers, for example, have a unary structure `42 = [41 S]`. But under the hood they could be represented by simple machine words, and arithmetic on natural numbers could be reduced to a machine operation. We can accelerate lists to use array representations and in-place indexed updates for unique references. We can feasibly accelerate linear algebra to leverage a GPGPU, or accelerate Kahn process networks to leverage distributed CPUs and memory.

## Annotations

Annotations help developers control, optimize, view, and debug computations. Annotations are represented as parenthetical words like `(par)` or `(a3)`. Some useful examples of annotations include:

* `(a2)..(a9)` - arity annotations to defer computations
* `(t0)..(t9)` - tuple assertions for output scope control
* `(nc) (nd)` - support substructural type safety
* `(:foo) (.foo)` - lightweight type tag and assertions
* `(par)` - request parallel evaluation of computation
* `(eval)` - request immediate evaluation of computation
* `(nat)` - assert argument should be a natural number
* `(optimize)` - rewrite a function for efficient evaluation
* `(jit)` - compile a function for efficient evaluation
* `(stow)` - move large values to disk, load on demand
* `(memo)` - memoize a computation for incremental computing
* `(trace)` - record value to a debug output log
* `(trash)` - erase data you won't observe, leave placeholder
* `(error)` - mark a value as an error object
* `(~foo)` - reduce code to known name (quines, loops)

Annotations must have no observable effect within a computation. Nonetheless, annotations may cause an incorrect computation to fail fast, defer unnecessary computation, simplify static detection of errors, support useful external observations like debug logs or breakpoint states or a change in how an evaluated result is represented or organized.

Annotations may be introduced and documented on a runtime basis. In case of porting code, runtimes that do not recognize an annotation may ignore it. Long term, we should reach some de-facto standardization on useful annotations.

## Stowage

Stowage is a simple idea, summarized by rewrite rules:

        [large value](stow) => [$secureHash]
        [small value](stow) => [small value]

Stowage enables programmers to work semi-transparently with data larger than working memory. Unlike effectful storage or virtual memory, stowage is friendly in context of parallelism and distribution, structure sharing, incremental computing, and backtracking. Stowage works nicely with [persistent data structures](https://en.wikipedia.org/wiki/Persistent_data_structure), especially structures that batch writes such as [LSM trees](https://en.wikipedia.org/wiki/Log-structured_merge-tree). 

What 'large value' means is heuristic, based on time-space tradeoffs. A runtime is in charge of what annotations mean, so it can vary by runtime. But ideally, stowage should be simple, predictable, reproducible, easily shared across systems for caching and structure sharing. My recommendation is that "large value" mean "at least 256 bytes" as serialized, sufficient for a few hashes and some structure.

## Evaluation

Evaluation of an Awelon program results in an equivalent Awelon program, one from which it is hopefully easier to extract information or more efficiently perform further evaluations. Awelon's primary evaluation mode proceeds by local rewriting. The four primitives rewrite by simple pattern matching:

            [B][A]a => A[B]         (apply)
            [B][A]b => [[B]A]       (bind)
               [A]c => [A][A]       (copy)
               [A]d =>              (drop)

Words rewrite to their evaluated definitions. However, words do not rewrite unless doing so leads a result other than a trivial inlining of the word's evaluated definition. This constraint is called lazy linking, and it supports various performance and aesthetic goals. An undefined word represents an unknown and does not evaluate further.

Awelon's basic evaluation strategy is simple:

* rewrite outer program
* evaluate before copy 
* evaluate final values

Evaluating the outer program before values gives us the greatest opportunity to drop values or annotate them with memoization or other features. Evaluation before copy resists introduction of rework without introducing need for memoization, and covers the common case. Final values are reduced because we assume the program as a whole might be copied for use in many locations.

This is really just a recommended default strategy. A runtime may adjust this at its own discretion, so long as it preserves semantics. Annotations will also affect evaluation strategy. With annotations we could precisely defer computations, control linking, leverage parallelism, memoize results for incremental computing, stow large but infrequently referenced data to disk, fail-fast in case of obvious errors, enable visible optimizations, request JIT compilation. And so on. See also *Optimization*, below.

## Value Words

A 'value word' is a word whose evaluated definition is a single block. Value words have a nice interaction with lazy linking: data plumbing with natural numbers, `true` and `false` booleans, named module or data resources, and so on will effectively treat value words as blocks values.

        42 true w == true 42
        42 [] b   == [42]

Value words are effectively the 'nouns' of Awelon language. They are convenient for preserving human-meaningful structure and support for hypermedia resources.

## Deferred Computations, Link Control, and Coinductive Data

The *arity annotations* `(a2)` to `(a9)` have simple rewrite rules:

                             [B][A](a2) == [B][A]
                          [C][B][A](a3) == [C][B][A]
                                        ..
        [I][H][G][F][E][D][C][B][A](a9) == [I][H][G][F][E][D][C][B][A]

To clarify, it is the *annotation* that has the given arity. Arity annotations specify nothing of their context.

Arity annotations serve a critical role in controlling computation. For example, the program `[[A](a2)F]` has the same type and semantics as `[[A]F]`, but the former prevents partial evaluation of `F` from observing `[A]`. Arity annotations can be used to guard against useless partial evaluations and control linking. For example, if we define swap as `w = (a2) [] b a` then we can avoid observing the useless intermediate structure `[A] w => [[A]] a`. An evaluator must wait for two arguments to `w` before linking.

Arity annotations serve a very useful role in modeling [thunks](https://en.wikipedia.org/wiki/Thunk) and [coinductive data](https://en.wikipedia.org/wiki/Coinduction). It is sometimes useful to model 'infinite' data structures to be computed as we observe them - procedurally generated streams or scene graphs.

## Fixpoints and Loops

Fixpoint is a function useful for modeling loop behaviors. For Awelon language, I favor the following variant of the [strict Z fixpoint combinator](https://en.wikipedia.org/wiki/Fixed-point_combinator#Strict_fixed_point_combinator):

        [X][F]z == [X][[F]z]F 
        z = [[(a3) c i] b (~z) [c] a b w i](a3) c i

        Using Definitions:
               [A]i == A            (inline)         i = [] w a d
            [B][A]w == [A][B]       (swap)           w = (a2) [] b a

        Assuming Annotation:
            [(def of foo)](~foo) => [foo]
            and arity annotations

The arity annotation `(a3)` defers further expansion of the `[[F]z]` result. The `(~z)` annotation supports aesthetic presentation and preserves accelerated performance across serialization. Readers unfamiliar or uncomfortable with fixpoint may benefit from evaluating `[X][F]z` by hand a few times to grasp its behavior.

*Note:* Fixpoint is notoriously difficult for humans to grok and awkward to use by hand. Use of *Editable Views* and especially *named locals* can help, potentially hiding the fixpoint. Even better is to hide most loops behind various collections or stream processing abstractions.

## Memoization

The primary basis for incremental computing in Awelon is [memoization](https://en.wikipedia.org/wiki/Memoization). Evaluation of dictionary definitions will tend to be implicitly memoized. But programmers may also request memoized evaluation explicitly:

        [computation](memo)

Memoization is conceptually performed by by seeking the computation in a runtime lookup table. This lookup must account for words having different meanings in context of different dictionaries. If the computation is found in the table, the runtime will simply replace the computation with the evaluated result. If the computation is not found, we evaluate then heuristically store the computation into the table based on observed and estimated future time and space tradeoffs.

To effectively support incremental computing, memoization must be used with cache-friendly patterns, persistent data structures, and large-value stowage. So some careful design is required to leverage this feature.

## Static Linking

It is possible to perform static analysis on a word's evaluated definition, arities and types, etc. to determine context-free link structure. The most trivial example of this is redirects. Consider:

        @foo bar

Here, word `foo` will not evaluate any further because there is no cause to link `bar`. However, when we do eventually link `foo`, we'll immediately be asked to link `bar`. We can determine this by static analysis. For performance reasons, we may wish to skip the intermediate step rewriting `foo` to `bar` and jump straight to linking the definition of `bar`. 

However, static linking is not constrained to trivial redirects. Statically computing link structure can further inline definitions, flatten redirect chains. Computing a static-link object provides an excellent opportunity to perform transparent optimizations.

A runtime might provide `(link)` to evaluate further to a static link object.

## Optimization

There are many semantically valid rewrites that Awelon's basic evaluator does not perform. For example:

        [A] a [B] a => [A B] a      apply composes
        [A] a d     => d A          tail call optimization
        [] a        =>              apply identity is a NOP
        b d         => d d          either way we drop two values
        c d         =>              drop the copy
        c [d] a     =>              drop the other copy
        [i] b       =>              because [A][i]b == [[A]i] == [A]
        b i         => i            expansion of [X][F]b i == [X][F]i
        [] b a      => w            by definition of w
        c w         => c            copies are equivalent
        [E] w d     => d [E]        why swap first?

A runtime has discretion to perform optimizations that are not visible in the evaluated result, based on escape analysis. The static link object is a good target for such efforts. Any visible optimizations or simplifications should be explicitly controlled by annotations or evaluator options.

Pattern-matching rewrite optimizations tend in general to be fragile, affected by abstraction and order of evaluation. There are more robust optimization techniques with good results. For example, partial evaluation in Awelon is usually limited by inability to represent partial values. Evaluating with 'free variables' in the form of undefined words can help:

* assume `A B C` words unused and undefined 
* evaluate `[C][B][A]function` to completion
* rewrite to extract `A B C` free variables

The evaluator does not rewrite the `A` annotation. But its presence can push partial information through the program like `[4 A 1]` where `A` might later be `3 2` but we don't know. Argument extraction logic is a simple, reflective rewrite:

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

So this optimization looks like: `T(A, T(B, T(C, Eval([C][B][A]function))))`. But before we extract the variables, we might perform other optimizations. We can search for common subexpressions, variables included, and extract those first. We can heuristically search equivalencies like `T(A, T(B, E)) == w T(B, T(A, E))`, or we might attempt a topological sort on subexpressions to minimize and simplify data shuffling within the program. 

Static type information may also support optimizations. For example, if we know our argument is a pair, we might further propagate the elements as independent variables:

        type (A * B) = ∀S. S → S A B
        for E : S (A * B) → S'
            E => i T(B, T(A, EVAL([[A][B]] E)))

For sum types, the analog is to precompute programs for different arguments:

        a generic binary 'if'
        if : S (S → (A | B)) (A → S') (B → S') → S'
        if = (a3) [] b b a (cond) i

        type Bool = ∀S,S'. S (S → S') (S → S') → S'
        for E : S Bool → S', where E observes argument
            E => [false E] [true E] if

        type (A + B) = ∀S,S'. S (S A → S') (S B → S') → S'
        for E : S (A + B) → S', where E observes argument
            E => [[inL] b E] [[inR] b E] if

In this case, we must weigh program expansion versus gains from static partial evaluations. Achieving a good result here may require a heuristic search - e.g. when `E == F G` we might be better off just expanding `F`, and when we have multiple boolean values we might need to expand several to see benefits. 

For sums, we must weigh program expansion versus the gains from static partial evaluation. But for at least some cases, the idea could work very well.

There are likely many more optimizations that can be performed directly at the Awelon layer. For example, we could try to move `(eval)` up front if we know it will happen regardless. The simple semantics of purely functional combinators, and the ability to inject or extract 'variables' as needed, make this safe and easy.

## Fast Interpretation

Naive interpretation of Awelon can be reasonably efficient, but involves a lot of pointer-chasing. We can do very well with a few minor tweaks on the representation. Consider Awelon extended with the following features:

* program pointers
* auxiliary stack 

A program will be represented by an array of words and values, terminated by a special `\return` word. The `\return` word will pop a program pointer from the auxiliary and jump to it. To 'call' a word, we will generally push a 'next' program pointer onto our return stack, then jump. We can also perform tail-call optimizations (e.g. for `... a d]` or `... i]`) such that we avoid returning to a finished subprogram.

The auxiliary stack can also optimize some data hiding:

        [A]a    =>  \push A \pop
        [A]b a  =>  \push2nd A \pop
        ...

This allows us to avoid some indirection, construction, and return actions. This particular transform doesn't help for every use case. But we can optimize temporary data hiding for known common cases, such as working with `T(A, T(B, ..E))` up to a limited arity.

A valuable feature is that we can trivially 'decompile' these call-return and push-pop patterns. We start with a logical copy of the auxiliary stack. On `\return`, we pop a return address and continue serializing from the new location. For `\push` we write `[` and add a special `]a` term to the auxiliary stack. For `\pop`, we pop a term from the auxiliary then serialize it. And so on. This interpreted representation can be used as our primary representation under the hood without any decompilation overhead to recover the computed program.

This slightly modified Awelon is a good fit for a threaded interpreter. But we can also compile Awelon to make it faster, and either interpret the partially compiled code

## Compilation

Awelon can be compiled to more effectively use stock hardware.

The most important step is register allocation, mapping active values from a program into memory or CPU registers. This enables us to eliminate intermediate data shuffling and volatile binding, especially within a program loop. Compilation can leverage acceleration, for example by specializing floating point arithmetic, translating common `if` conditional behavior into local branching, and reducing tail-call fixpoint functions to a local jump. A small bank for floating point registers could reduce need for boxing/unboxing and further improve loop performance when accumulating a small set of results.

Compilation can also extract a function for efficient use in another language, such as JavaScript to target the browser. Registers would in this case might translate to variables or fields in an object, or a mutable array.

Awelon's program rewriting semantics generally means we should be able to decompile back to the Awelon program after evaluation. This ability for programs to self-decompile is convenient for persistence, checkpointing, distribution, sharing, and debugging code. An Awelon compiler can feasibly preserve metadata such as an associative index from program counters to source and a register-to-stack map to eliminate CPU overheads during evaluation.

## Parallel Evaluation

The simplest parallelism that clients might request is parallel evaluation of a block via `[computation](par)`. The block can be moved around while computing, and we can potentially accelerate composition involving parallel computations to form a linear pipeline. 

Unfortunately, `(par)` has severe limitations on expressiveness. It is at once too expressive to leverage low-level parallelism (vector processing, SIMD, SSE, GPGPU) and insufficiently expressive for flexible communication channels between parallel components.

For low level parallelism, we could accelerate [linear algebra](https://en.wikipedia.org/wiki/Linear_algebra). Alternatively, we could accelerate evaluation for a safe subset of OpenCL or similar, above which we might later accelerate linear algebra. Either approach can help Awelon in the domains of machine learning, physics simulations, graphics and audio, and other high performance number crunching computations.

For high level parallelism, I propose acceleration of [Kahn process networks (KPNs)](https://en.wikipedia.org/wiki/Kahn_process_networks), more specifically a variant with temporal extensions (see *Reactive Process Networks*, below). Essentially, we would describe a process network as a first-class value, and accelerate an 'evaluation' function (of type `KPN → KPN`) such that it splits the process network among distributed memory and CPUs to run the computation. The *monotonic* nature of KPNs allows distributed computation to continue in the background even as we inject and extract messages. 

## Structural Scope

Blocks naturally delimit the input scope for a computation. For example, we know that in `[B][[A]foo]`, `foo` can access `[A]` but not `[B]`. And we can trivially leverage this with the bind operation `b`. But Awelon also supports multiple outputs, and so scoping output is a relevant concern. To address this, Awelon introduces *tuple assertions* to annotate output scope:

                                   [](t0) == []
                                [[A]](t1) == [[A]]
                             [[B][A]](t2) == [[B][A]]
                                         ..
        [[I][H][G][F][E][D][C][B][A]](t9) == [[I][H][G][F][E][D][C][B][A]]

Tuple assertions can be deleted early if they are validated statically. Otherwise, some lightweight dynamic reflection may be necessary, and we'll fail fast if the tuple assertion is bad. Similar to arity annotations, tuples larger than `(t5)` should be rare in practice.

In addition to controlling output counts, programmers may wish to fail fast based on declared structure. To support this, Awelon supports a structure annotation `(:T)` and paired structure assertion `(.T)` with the following rewrite semantics:

        (:foo) (.foo) ==

These might also be called 'sealer' and 'unsealer' respectively. In practice, we might construct a tagged value with `[(:foo)] b` and later access it with `i(.foo)`. The symbol `foo` or `T` is left to the programmer, but must match between annotation and assertion. These annotations are not labels (i.e. you cannot discriminate on them), but they do resist *accidental* access to structured data. As with tuple assertions, we can fail fast dynamically or detect an error statically.

## Substructural Scope

[Substructural types](https://en.wikipedia.org/wiki/Substructural_type_system) allow us to reason about whether a value is used, or limit how many times a value is used. This can be convenient for modeling finite resources, intermediate states in a protocol, or ensuring certain steps are performed by a client computation. Awelon lacks primitive support for substructural types, but annotations can be leveraged:

* `(nc)` - mark a value non-copyable, aka 'affine'
* `(nd)` - mark a value non-droppable, aka 'relevant'
* inherit substructure of bound values (op `b`).

        [A](nc) [B] b == [[A](nc) B](nc)

Substructural attributes do not prevent application of a value with `a`. Copy and drop are explicit in Awelon, so dynamically enforcing these attributes is feasible, a lot easier than it would be in a variable substitution based language. But ideally, they would be enforced statically. 

## Error Annotations

An `(error)` annotation marks a value erroneous and non-applicable. We cannot observe an error value with operator `a`. 

        [B][E](error)a == [][E](error)a d [B]

That is, we simply halt rewriting wherever we attempt to apply the error value. But an erroneous value can otherwise be bound, copied, dropped like normal. 

 would only become a problem if we attempt later to observe this divide-by-zero result. A runtime may also wrap recognized errors to highlight them in the output.

        [A](nc)c        => [][[A](nc)c](error)a d
        [[A][B][C]](t2) => [[A][B][C]](t2)(error)

## Garbage Data

For potentially relevant `(nd)` data, we often have an option to drop data into a logical bit bucket then never look at it again. If we inform our runtime that we plan to never look at it again, we can also recover memory resources associated with that data. We can represent this pattern by use of a `(trash)` annotation:

        [A](trash)      => [](error)
        [A](nd)(trash)  => [](nd)(error)

We drop data but preserve substructure. Because the data has been lost, the resulting value is marked erroneous. Memory required for `A` may then be recycled. This is essentially a form of manual memory management.

## Active Debugging

Awelon's program rewrite semantics make it relatively easy to observe a computation in progress. Data is visible in the program representation, rather than hidden behind variables that must be queried via debugger. It is feasible to predictably animate evaluation by pausing evaluation and taking snapshots just before linking of specific words, recording a series of 'frames' to render.

Conventional debugging techniques also apply. 

I propose `(trace)` annotation to serve as a basis for conventional printf style debugging. For example, `[message](trace)` would evaluate to simply `[Msg]` but would implicitly copy the message to a second stream. Traced outputs could easily be rendered within the same program by essentially injecting a trace comments in the output (perhaps of form `[[message1](trace)[message2](trace)..[messageN](trace)](eval)(a2)d`

Of course, tracing is invasive and is not ideal for debugging in general. Configuring words or certain annotations to operate as breakpoints or frame separators, or even just profile their use, is much less invasive but requires a more sophisticated configuration.

## Static Typing

Awelon can be evaluated without static typing. There is no type driven dispatch or overloading. But if we can detect errors early by static analysis, that is a good thing. Further, static types are also useful for verifiable documentation, interactive editing (recommending relevant words based on type context), and performance of JIT compiled code. Strong static type analysis makes a *very* nice default.

We might represent our primitive types as:

        a   : S B (S → S') → S' B
        b   : S B (E B → E') → S (E → E')
        c   : S A → S A A
        d   : S A → S
        [F] : S → S type(F)

The type sequence `S C B A` aligns with a program structure `S[C][B][A]`. Effectively, `S` is the remainder of our program 'stack' when we view the program as rewriting a stack-like structure. In context of Awelon, we know that value types `C B A` must be first class functions, which potentially encode data.

Annotations can augment static type analysis by providing an assertion against which we can validate an inference. Structural and substructural assertions mentioned above can be validated statically. A remaining concern is static typing of conditional behavior. We can represent various conditional data types:

        (bool)      S (S   → S') (S     → S')   → S'
        (opt)       S (S   → S') (S A   → S')   → S'
        (sum)       S (S B → S') (S A   → S')   → S'
        (list)      S (S   → S') (S A B → S')   → S'
        (cond)      S (A   → S') (B     → S')   → S'

Knowing these types, we can also check for consistency between conditional branches. Unfortunately, inferring these types is difficult. Annotations can provide a much needed hint. I imagine programmers will want annotations for many common types - naturals, texts, binaries, lists, labels, records, and so on. Anything we accelerate or use frequently enough for a runtime to recognize.

*Note:* Heterogeneous lists are easily represented and useful in Awelon. A typical 'fold' or 'map' operation might not apply, but many other operations like zip-map, indexing, or sequencing operations are still applicable. Users might benefit from distinct annotations for homogeneous vs. heterogeneous lists, and for structured intermediate forms like command lists.

*Aside:* Many languages have a 'unit' type, a type with only one value. In Awelon, the best representation of the unit type is `[]` - the identity function. There is only one value of type `∀x.(x→x)`. 

### Deferred Typing

Simple static types are oft inexpressive, constraining more than helping.

We can introduce an explicit escape. Consider a `(dyn)` annotation used as `[F] b b (dyn)` with formal behavior `[A](dyn) => [A]`. The presence of `(dyn)` does not suppress obvious static type errors, but may suppress warnings or errors that result from being *unable* to infer static types for a given subprogram, types too sophisticated for our simple type checker. Dependent types are an example where types are likely too sophisticated for a simple checker. Conveniently, the ability to eliminate `(dyn)` via partial evaluations at compile time would enable us to leverage dynamically typed macro-like behaviors while still supporting a statically typed system. 

### Sophisticated Types

I propose a convention of defining `foo.type` to declare a type metadata for `foo`. This enables flexible abstraction and composition of type descriptions, expression of sophisticated types (contracts, Hoare logic, etc.), and provision of auxiliary hints or proofs. If we want to properly support dependent, existential, higher order, GADT, etc. types, we'll probably need to do so at this layer. By also providing the type check algorithms via the same dictionary, we might also simplify portable consistency checks.

Related to static typing, non-terminating evaluation in Awelon is always an error. There is no utility in unbounded divergence for a pure computation, though we might use arity annotations to defer computations and represent coinductive structure. In any case, static type analysis should attempt a limited termination analysis. While solving the halting problem isn't possible in general, obvious errors can always be reported.

## Editable Views

Awelon language has an acceptably aesthetic plain text syntax. But, like Forth, Awelon does not scale nicely beyond twelve tokens per definition because humans easily lose track of context. This could be mitigated by live feedback on types and examples. But Awelon is designed to use another simple technique to support more conventional programming styles, DSLs, and program scale: Awelon shifts the rich syntax burden to [editable views](http://martinfowler.com/bliki/ProjectionalEditing.html). 

My initial emphasis is textual views, such that we can readily integrate favored editors and perhaps even leverage [Filesystem in Userspace (FUSE)](https://en.wikipedia.org/wiki/Filesystem_in_Userspace) to operate on a dictionary through a conventional filesystem. Numbers are a useful example for textual editable views. A viable sketch:

        #42         == (AWELON 42)
        42          == [#42 #0 integer]
        -7          == [#0 #7 integer]
        3.141       == [3141 -3 decimal]
        -0.0070     == [-70 -4 decimal]
        2.998e8     == [2998 5 decimal]
        -4/6        == [-4 #6 rational]

Awelon's natural numbers are given the `#` prefix in favor of a more aesthetic representation of signed numbers. From there, we build a tower of numbers. The basic approach of building views upon views is convenient because it makes views more extensible. For example, if we have no support for rational numbers, we'd still see `[-4 #6 rational]` which is still sensible to a human reader. Support for rational numbers or hexadecimal or similar can be added if missing.

Every editable view should have an unambiguous escape to raw Awelon code. While I use `(AWELON 42)` above to make it obvious, I could just as easily use `#` as the primary escape, or `'(42)` or `\42`, etc.. Whatever users find acceptable. The nature of editable views does make it easy to experiment for aesthetics. Use of escapes permits an editable view to support key-words, if desired.

Command lists are another valuable view feature:

        {foo, bar, baz} == [[foo] {bar, baz} :]
        {baz}           == [[baz] ~ :]

Command lists are useful for various purposes, supporting continuation-passing style or a concise embedding of interruptable code. While I use a normal nil/cons list structure here, a view could favor something more specialized like the Haskell `do` notation.

Ideally, all editable views should be computable, in the sense of having a normal form. That is, program evaluation should be able to generate the same structure we use to view and edit programs. When we add 3.141 and -0.007, we want to see 3.134 in the program output. That is, `[3134 -3 decimal]` (or whatever we use to represent decimal numbers) should be a viable result from a computation. 

Design of computable, editable views is very sensitive to arity annotations and accelerators. A consequence is that editable views should be *defined* within the same dictionary they're used to view by some simple convention. Perhaps a word defining a `[[view→code][code→view]]` pair where `code` and `view` are represented as text. Representing views within the dictionary is convenient for testing and caching of views, and for updating views based on application or edit session state (e.g. so we can separate namespace from code). 

With computable views in mind, we might represent comments as:

        /* comment */  ==  " comment "(a2)d

The arity annotation ensures the comment is not deleted until it might prevent progress from the left side, and hence we can always inject comments into values. A relevant point is that we aren't limited to one 'type' of comment, and comments of other types can easily inject flexible rendering hints into Awelon code. The discussion on *Named Local Variables* offers one very useful example, or a comment might include trace output for active debugging.

Editable views essentially form a compiler/decompiler that treats Awelon language as a functional macro-assembly. The main difference from conventional languages above a bytecode is that the Awelon code is treated as the canonical source, and we're forced to 'decompile' said code into our language of editing. The indirection provided by the decompiler simplifies issues like whitespace formatting and forwards/backwards compatibility.

With editable views, individual definitions can scale many orders of magnitude. It is even possible to represent conventional 'modules' in terms of expressions that construct first-class records of functions, though I'd encourage one word per independently useful function as the normal case.

Although initial emphasis is textual views, it is feasible to model richly interactive graphical views involving tables, graphs, canvases, checkboxes, sliders, drop-down menus, and so on. A sophisticated projectional editor could support frames or a zoomable interface where a word's definition may be logically inlined into the current view. 

## Named Local Variables

An intriguing opportunity for editable views is support for local variables, like lambdas and let expressions. This would ameliorate use cases where point-free programming is pointlessly onerous (like working with fixpoints or algebraic expressions). It also supports a more conventional programming style where desired. Consider a lambda syntax of form:

        \ X Y Z -> CODE ==  ["X Y Z"(:λ)](a2)d CODE'

This plucks three items off the stack, giving them local names within `CODE`. On the right hand side, the lambda comment - `["X Y Z"(:λ)](a2)d` - enables us to later recover the variable names and lambda structure when we decompile for future edits. We can compute `CODE' = T(Z, T(Y, T(X, CODE)))` using a simple algorithm:

        T(X, E) | E does not contain X      => d E
        T(X, X)                             => 
        T(X, [onF][onT] if)                 => [T(X, onF)][T(X, onT)] if
        T(X, [E])                           => [T(X,E)] b
        T(X, F G)
            | only F contains X             => T(X,F) G
            | only G contains X             => [F] a T(X,G)
            | otherwise                     => c [T(X,F)] a T(X,G)

This algorithm is adapted from the partial evaluation optimization leveraging free variables. The main difference from the optimization is that we know our variables are value words and we may desire special handling for conditional behaviors like `if` to avoid copying data into each branch.

Lambdas can be leveraged into let expressions (like `let var = expr in CODE` or `CODE where var = expr`) or the Haskell-like `do` notation. Also, given named local variables, it is feasible to support infix expressions like `((X + Y) * Z) => X Y + Z *` for assumed binary operators. I leave these developments as an exercise for the reader. :D

## Qualified Namespaces

Awelon's hierarchical dictionaries support a simple form of namespacing. But it falls to editable views to support local shorthand, e.g. some form of `using large_prefix as x` or `using package_of_nicknames`. If we assume editable views are maintained within the dictionary, it is feasible to use comments to help control the view, tweaking the language as needed. An intriguing possibility is to integrate a database of nicknames for secure hash resources into the view, where said database is represented within the dictionary.

## Labeled Data - Records and Variants 

Labeled sum types (variants) allow conditional discrimination on a label. Labeled product types (records) allow us to access to heterogeneous data by a label. Primitive sum `(A + B)` and product `(A * B)` types are simple and sufficient for many use cases. But labeled data is self-documenting (label informs human) and extensible (add more labels).

A useful way to encode labeled sums is by deep primitive sum structures. That is, we use a `[[[value] inL] inR] inL]` structure where the left-right-left path is extended to multiple bytes encoding a human-meaningful label. Unlike label-value pairs, deep sums do not require dependent types. A labeled product can similarly be modeled as a heterogeneous trie on the label. Consider:

        (Deep Sums)
        [[[A] inL] inL]
        [[[[[B] inL] inL] inR] inR]

        (Singleton Tries)
        [[[A] ~ :] ~                     :]
        [~         [~ [[[B] ~ :] ~ :] :] :]

        (Merged Trie)
        [[[A] ~ :] [~ [[[B] ~ :] ~ :] :] :]

A useful label encoding is `(RL|RR)*LL`, where `RL` corresponds to constructor `[inL] b [inR] b`. The `(RL|RR)*` structure represents a finite `(0|1)*` bitfield, within which we encode texts or numbers. The final `LL` terminates the label. This encoding has the nice properties of being a self-synchronizing code. Naive construction of the trie supports enumeration of labels and merging. The unused `LR` slot can potentially be used in the record as a name shadowing list. 

Unfortunately, the trie is awkward and inefficient to work with directly. A better alternative is instead to work with a trie *constructor* - a function that, given an initial record object, loads it with data. In Awelon text, this might look something like `[[A] "foo" :: [B] "bar" :: ...]`. Relevantly, the ordering of labels in this representation is not relevant, composition of record functions would essentially represent update of a record, and the encoding is not sparse. I'm assuming the type of `::` is dependent on the text argument, but we could use an expanded label structure if necessary. An editable view could feasibly reduce either to a more aesthetic `[[A] :foo [B] :bar ...]`. 

Acceleration of records would logically construct a trie and extract an updated new record function with every operation but really just using an optimized representation like a hashmap under the hood. Acceleration of functions related to labeled variants could serve a similar role of improving performance and aesthetics.

Assuming aesthetic, accelerated, labeled data, Awelon can support parameter objects, extensible event types, labeled case expressions, and a more conventional programming style with events and routing on human labels. 
 
## Unique References and In-Place Updates

Persistent structures are great and should be used frequently. But in-place update has some performance advantages that are reasonably difficult to give up. 

Fortunately, purely functional languages can support in-place update whenever we have a unique reference. We can model copy-on-write shared arrays, where subsequent writes are applied to the unique array without further copying. Lambda calculus makes this feature difficult to achieve (because the lexical environment is implicitly 'shared'). But Awelon makes copying explicit with operator `c`, so it's very easy to dynamically track uniqueness of a reference.

Arrays and records are the most useful targets for this treatment.

We can represent a list as an array (guided by `(array)` annotations). We can accelerate functions to access and update lists at indexed offsets. When the update function is applied to a unique array, it can update it in place. If applied to a shared array, it must copy the array first to get a unique array, but then all subsequent updates are in-place until the array is shared by logical copying via operator `c`. Records would receive similar treatment, albeit using a hashmap in place of the array.

*Note:* The `(nc)` annotation restricts copying of the marked value. Use of this can help enforce preservation of uniqueness, or at least help fail-fast debug cases where we want to restrict copying.

## Reactive Process Networks

A weakness of conventional [Kahn Process Networks (KPNs)](https://en.wikipedia.org/wiki/Kahn_process_networks) is that they cannot merge asynchronous data from multiple channels. There is no record for when messages on one channel arrive relative to messages on other channels. This weakness makes it difficult to efficiently integrate KPNs with real-world events. Fortunately, this weakness can be solved by adding a temporal dimension to KPNs, while preserving other nice features (determinism, monotonicity). Here's how:

* Every process has an implicit time value. 
* Outgoing message is stamped with the process time.
* Incoming messages are bounded by process time.
* Wires have logical latency, e.g. add 10 to time.
* Reading returns Nothing if no messages at time.
* Process can explicitly wait on a set of ports.
* Waiting advances time to next message in set.
* We explicitly advance time for open input ports.

Reactive process networks could be modeled explicitly in KPNs by simply adding 'time advances by X units' messages to every port, and being careful to explicitly propagate this information to every output port. But it's more convenient to treat this as an alternative effects model at the process model, dividing 'wait' from 'read' within a process. Of course, reads will still implicitly wait until either a message arrives or time explicitly advances on the input port beyond the reader's current time. 

Reactive process networks permit interesting expressions. We can interpret input ports as stateful memory, maintaining a value of type `S` and receiving messages of type `S→S`. We can model clock-like behaviors that periodically send messages via cyclic wiring with latency. Ultimately, a reactive network's advance of time is driven externally by advancing time on open input ports and internally via clock-like behavior. Conventional KPN behavior is preserved if we never advance time and use zero latency wiring. 

This simple temporal extension completes the KPN model for systems programming. 

Accelerated evaluation of the reactive process network can readily operate across distributed memory and CPUs. If compiled as an effects model we can also attach open input-output ports to real-world IO sources (keyboard, mouse, cameras, video, sound, etc.) without a central IO bottleneck. My intention is for Awelon systems to leverage reactive process networks as a scalable, composable alternative to monadic effects, in addition to a convenient model for network scale parallel stream processing.

*Note:* The model of time, and the current time, is not explicitly visible within the processes. We can use natural numbers to represent time. We can easily normalize times by subtracting the minimum time stamp from all time stamps. We can freely assign times an external meaning, e.g. logical 'microseconds' or 'nanoseconds' or similar.

## Hierarchical Dictionaries and Namespaces

Awelon supports a simple model for hierarchical structure. Words of qualified form `foo@dict` reference the meaning of `foo` as defined within dictionary `dict`. Named dictionaries are specified by defining symbol `@dict` to a secure hash of the dictionary root.

        @@dict secureHashOfDict

As with normal words, the last definition wins. It is possible to update `foo@dict` by updating the definition of `@dict` to reference a dictionary that's almost the same but with a slightly different `foo`. That's also the only means of update: `foo@dict` cannot be defined directly, and a dictionary cannot reference its parent.

The namespace qualifier can be attached to any Awelon operation, not just words. If attached to a block, the semantics is essentially that every element of the block is qualified: `[42 foo]@d == [42@d foo@d]`. If attached to a text, it's the same as attaching to an equivalent block: `"hello"@d == [104 "ello" :]@d`. Even annotations may be qualified, although this is relevant only for annotations that somehow reflect upon the dictionary, such as `(~z)@d` (where `[def of foo](~foo) => [foo]`). But like the dictionary hashes, these qualifiers are second-class: no space is permitted between the operation and its qualifier.

Namespace qualifiers may be hierarchical. For example, `foo@xy@zzy` would refer to the definition of `foo@xy` within dictionary `zzy`. Explicit use of hierarhical namespaces is discouraged by the Law of Demeter. But they may appear naturally during evaluation or optimization.

Namespace qualifiers may be eliminated when doing so does not affect observable behavior. For example, `42@d` may be rewritten to `42` when we know that natural numbers have the same meaning (based on definitions of `0` and `S`). This optimization is called *localization*. Localization may have a non-trivial effect on external associations such as documentation and rendering hints. For example, when we rewrite `foo@d` to `foo`, we implicitly re-associate documentation from `foo.doc@d` to `foo.doc`. 

*Aside:* Hierarchical dictionaries are primarily useful at the meta-level for [application models and agents](ApplicationModel.md). They enable dictionaries to represent databases, documents, ontologies, or other objects. Inability to reference the parent dictionary simplifies reasoning about information flow and security. Centralization to a secure hash simplifies structure sharing and integration with publish-subscribe. 

