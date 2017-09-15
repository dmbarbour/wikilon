
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

Awelon experiments with alternative [application models](ApplicationModel.md). The most promising models are RESTful - documents, publish subscribe, tuple spaces, etc. - but where application state is integrated with the codebase instead of an external filesystem or database. The codebase then becomes a 'smart' filesystem or database, with more deeply integrated linking and computational structure similar to a spreadsheet. The codebase is maintained by a community of human and software agents, exactly as we'd maintain a database or filesystem.

Awelon diverges significantly from conventional systems and programming language design. This isn't just a new language, it's a new way of thinking about programming that integrates ideas from spreadsheets, functional programming, and RESTful web services. Awelon does not *directly* interact with existing systems - there is no FFI, for example. But indirect integration is feasible through bots and web services and cross compilation for some application models.

## Primitives

There are four primitive combinators:

            [B][A]a == A[B]         (apply)
            [B][A]b == [[B]A]       (bind)
               [A]c == [A][A]       (copy)
               [A]d ==              (drop)

The square brackets `[]` enclose a 'block' of Awelon code, and represent a first-class function. This, together with various Church or Moegensen-Scott encodings, is the entire basis for representing data and computations in Awelon. All Awelon computations are pure. However, to achieve performance, we additionally leverage *Acceleration* as Awelon's alternative to built-in functions or performance-motivated FFI. Acceleration is discussed below.

This set of combinators is Turing complete, able to represent all deterministically computable functions. As a lightweight proof, I'll define the Curry-Schönfinkel SKI [combinators](https://en.wikipedia.org/wiki/Combinatory_logic).

            [B][A]w == [A][B]       (swap)           w = [] b a
               [A]i == A            (inline)         i = [] w a d
         [C][B][A]s == [[C]B][C]A   (S combinator)   s = [[c] a b w] a i
            [B][A]k == A            (K combinator)   k = a d

A simple translation from lambda calculus is also provided in discussion of *Named Local Variables* (in context of *Editable Views*). Compared to lambda calculus or SKI combinators, Awelon's semantics are simpler: 

1. Combinator rewriting is much simpler than variable substitution. There is no need for environment management, variable capture or lexical scope. There is no risk of representing free variables. 
1. The stack-like environment enables uniform abstraction for multiple arguments or results without explicit tupling, and provides a foundation for *Static Typing* based on arity without need for atomic value types.
1. The explicit copy and drop operations simplify substructural type analysis, reference counting GC, and potential acceleration via in-place update for indexed data structures.

## Encoding

Awelon is encoded using a subset of ASCII, bytes 32..126. 

Newlines and tabs are among the rejected characters. However, humans will frequently manipulate Awelon code through *Editable Views* that may present a more sophisticated surface syntax, potentially including tables or graphical representations. Large texts or binaries cannot be efficiently embedded in Awelon, but may be referenced via *Secure Hash Resources* and constructed via *Stowage*. 

## Words

Words are the user-definable unit for Awelon code. Structurally, a word starts with a lower-case alpha and may contain underscores or digits. As a regular expression, this is: `[a-z][a-z0-9_]*`. Additionally, there is a size constraint: words must not surpass 31 characters in length.

Words are evaluated in context of a *Dictionary*, where each word is defined by an Awelon program. Definitions of words within a dictionary must form a directed acyclic graph. The semantics for words are extremely trivial: we lazily substitute the word by its definition.

The four primitive words `a`, `b`, `c`, and `d` may not be defined. The words `zero`, `succ`, `null`, and `cons` must be defined for natural numbers and embedded texts to become usable.

## Natural Numbers

Awelon has native support for natural numbers. Syntactically, numbers are represented by regex `0|[1-9][0-9]*` wherever a word may appear. 

        0 = zero
        1 = [0 succ]
        2 = [1 succ]
        3 = [2 succ]
        ...
        42 = [41 succ]
        (et cetera)

Definitions for `zero` and `succ` are left to the dictionary. In practice, natural numbers must be defined around available *Accelerators*, such that we can add two numbers in constant or log-time rather than linear time.

Awelon does not support any other number types natively, nor does it support alternative encodings 

## Embedded Texts

Awelon has native support for embedding texts inline between double quotes such as `"Hello, world!"`. Semantically, a text represents a binary list.

        ""      = null
        "hello" = [104 "ello" cons]

Definitions for `null` and `cons` are left to the dictionary.

Embedded text may contain any valid Awelon character (32-126) excepting the double quote `"` (34) which terminates the text. There are no escape characters. When developers eventually need text outside these limits, the primary options are:

* interpret, e.g. `"multiple\nlines" lit` to rewrite escapes
* structure, e.g. `["multiple" ["lines" null cons] cons] unlines`
* use *Secure Hash Resources* to reference external binary

Interpeted text is a convenient hack but doesn't scale, compose, or abstract nicely. Structure is easy to abstract, compose, and evolve to support mixed data, but needs *Editable Views* to provide a usable syntax. Use of *Secure Hash Resources* is the most convenient wherever you'd conventionally use an external text or binary file.

Embedded texts are suitable for simple things like labels, test data, inline comments, and micro-DSLs such as regular expressions. They are not suitable for general use.

## Secure Hash Resources

It is possible to identify binaries by *secure hash*. Doing so has many nice properties: immutable and acyclic by construction, cacheable, securable, provider-independent, self-verifying, and implicitly shared. They are also much smaller than many URLs or file paths. Awelon systems leverage secure hashes to reference binaries and code outside the dictionary:

* external binary data may be referenced via `%secureHash`
* code and structured data is referenced via `$secureHash`

Semantically, secure hash resources are treated as words. They may appear anywhwere a word may be used. Binaries use the same list encoding as embedded texts. The referenced definition is lazily inlined. Of course, secure hashe resources share a global namespace, independent of dictionary.

Awelon uses the 280-bit [BLAKE2b](https://blake2.net/) algorithm, encoding the hash with 56 characters in a [base32](https://en.wikipedia.org/wiki/Base32) alphabet specialized to avoid conflicts with numbers or human meaningful words. Some example hashes, chained from the word `test`:
        
        HSjFNGRnqHpFFbPhlThmqCbqkmDSHCBlJNnmDPnDtnCpKHqtNgqhRMJG
        BRqMkFknGGncjKTdrTGMjFFHlGlFmmGGNmcFGPSmGbstsLtpdJnhLNKS
        NLsTsGdQrtFLfDtHJcmqDSmMsDRjnMpCFlkqGfLdgSRhFtTsGqhJrfNN

        Base32 Alphabet: bcdfghjklmnpqrstBCDFGHJKLMNPQRST
            encoding 0..31 respectively

We can safely neglect the theoretical concern of secure hash collisions. Physical corruption of dictionaries is of greater concern in practice. If collision becomes a concern in the future, it is trivial to rewrite entire Awelon systems to use a more robust hash. I won't further belabor the issue.

Awelon runtime systems must know where to seek secure hash resources, whether that be in a filesystem, database, web server, or content distribution network. Using *Stowage* annotations, Awelon runtime systems may also generate new secure hash resources during evaluation, treating this external space as a persistent virtual memory or binary data server.

Secure hash resources are frequently subject to [garbage collection (GC)](https://en.wikipedia.org/wiki/Garbage_collection_%28computer_science%29). Conservative reference counting GC is simple and effective in context. 

*Security Note:* Secure hash resources may embed sensitive information, yet are not subject to conventional access control. Awelon systems should treat a secure hash as an [object capability](https://en.wikipedia.org/wiki/Object-capability_model) - a bearer token that grants read authority. Relevantly, Awelon systems should resist timing attacks that might leak secure hashes.

## Dictionary

Awelon words are defined in a dictionary. Evaluation of Awelon code occurs in context of an immutable dictionary. Awelon doesn't specify the dictionary representation, but I imagine de-facto standards will arise around the import, export, sharing, and backup of dictionaries.

Definitions for words must form a directed acyclic graph. Trivial cycles, such as `foo = foo`, should be treated as deleting the word from the dictionary. Non-trivial cycles should be treated as errors. Cyclic behavior or structure should instead use fixpoint combinators, cf. *Fixpoints and Loops* below.

Dictionaries will usually be manipulated through services, such as a web application or a [FUSE](https://en.wikipedia.org/wiki/Filesystem_in_Userspace) filesystem layer. The use of such a layer is also necessary to provide useful *Editable Views*.

See also *Hierarchical Dictionaries*. Dictionaries may contain dictionaries.

## Acceleration

Acceleration is a performance assumption for Awelon. 

A runtime will recognize and accelerate common functions. The accelerated implementation may be hand optimized and built into the runtime to achieve performance similar to a primitive. For example, runtimes might accelerate the following functions:

           [A]i == A            (inline)         i = [] w a d
        [B][A]w == [A][B]       (swap)           w = [] b a

The runtime will look at the given definitions. Upon recognizing `[] b a`, the runtime may link `w` to an acclerated swap implementation. Whenever `i` appears at the end of a subprogram, we might leverage the tail-call optimization.

In general, recognition of accelerators may be fragile. It may be that `i = [] w a d` is recognized where the logically equivalent `i = [[]] a a d` is not recognized. Even changing function names could break accelerators. This is ultimately up to the runtime. Due to this fragility, a runtime should carefully document recognized accelerators, and provide some means to warn developers in case of bad assumptions. A useful convention is to define a prelude dictionary including the recognized accelerators, and to flag assumed accelerators by defining `foo.accel` if we expect `foo` to be accelerated.

Critically, acceleration of functions extends to data representation. Natural numbers, for example, have a unary structure `42 = [41 S]`. But under the hood they could be represented by simple machine words, and arithmetic on natural numbers could be reduced to a machine operation. We can accelerate lists to use array representations and in-place indexed updates for unique references. We can feasibly accelerate linear algebra to leverage a GPGPU, or accelerate Kahn process networks to leverage distributed CPUs and memory.

## Annotations

Annotations help developers control, optimize, view, and debug computations. Unlike words, which are mostly user-defined, annotations are given meaning by the runtime or compiler. Annotations are represented as parenthetical words like `(par)` or `(a3)`. Potential useful annotations:

* `(a2)..(a9)` - arity annotations to defer computations
* `(t0)..(t9)` - tuple assertions for output scope control
* `(nc) (nd)` - support substructural type safety
* `(seal_foo) (open_foo)` - lightweight type tag and assertions
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
* `(eq_foo)` - reduce code to known name (quines, loops)

Annotations must have no observable effect within a computation. Nonetheless, annotations may cause an incorrect computation to fail fast, defer unnecessary computation, simplify static detection of errors, support useful external observations like debug logs or breakpoint states or a change in how an evaluated result is represented or organized.

Annotations may be introduced and documented on a runtime basis. In case of porting code, runtimes that do not recognize an annotation may ignore it. Long term, we should reach some de-facto standardization on useful annotations.

## Stowage

Stowage is a simple idea, summarized by rewrite rules:

        [large binary](stow) => %secureHash
        [large value](stow) => [$secureHash]
        [small value](stow) => [small value]

Stowage allows secure hash resources to be constructed at runtime and removed from working memory until later required. Essentially, this gives us a functional, persistent virtual memory model. Further, the hashes can be useful at the client side, e.g. simplifying progressive disclosure, structural diffs, memoization and caching, or efficient binary download.

What "large value" means is heuristic. But should be simple, predictable to maximize structure sharing and simplify reproducable computation. A simple option is that anything smaller than `stow_threshold` bytes should be inlined, while anything larger is stowed. 

## Evaluation

Evaluation of an Awelon program results in an equivalent Awelon program, one from which it is hopefully easier to extract information or more efficiently perform further evaluations. Awelon's primary evaluation mode proceeds by local rewriting. The four primitives rewrite by simple pattern matching:

            [B][A]a => A[B]         (apply)
            [B][A]b => [[B]A]       (bind)
               [A]c => [A][A]       (copy)
               [A]d =>              (drop)

Words rewrite to their evaluated definitions. However, words do not rewrite unless doing so leads to a result other than a trivial inlining of the word's evaluated definition. This constraint is called lazy linking, and it supports various performance and aesthetic goals. An undefined word represents an unknown and does not evaluate further.

Awelon's basic evaluation strategy is simple:

* rewrite outer program
* evaluate before copy 
* evaluate final values

Evaluating the outer program before values gives us the greatest opportunity to drop values or annotate them with memoization or other features. Evaluation before copy resists introduction of rework without introducing need for memoization, and covers the common case. Final values are reduced because we assume the program as a whole might be copied for use in many locations.

This is really just a recommended default strategy. A runtime may adjust this at its own discretion, so long as it preserves semantics. Annotations will also affect evaluation strategy. With annotations we could precisely defer computations, control linking, leverage parallelism, memoize results for incremental computing, stow large but infrequently referenced data to disk, fail-fast in case of obvious errors, enable visible optimizations, request JIT compilation. And so on. See also *Optimization*, below.

## Value Words

A 'value word' is any word whose evaluated definition is a single block. Natural numbers would be a common example, but we might also include booleans like `true` or `false`, or references such as `story_chapter32`. Lazy linking should ensure that value words are not linked before necessary. Thus, data plumbing can operate on value words directly:

        42 true w == true 42
        42 [] b   == [42]

Value words are convenient for preserving human-meaningful structure and support for hypermedia resources. 

*Aside:* Value words might be considered the 'nouns' of Awelon language, whereas most words are verbs. However, an analogy to natural language quickly breaks down. Awelon lacks an equivalent to adjectives, adverbs, or anaphora, at least without a lot of development work involving staged programming and *Editable Views*.

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
        z = [[(a3) c i] b (eq_z) [c] a b w i](a3) c i

        Using Definitions:
               [A]i == A            (inline)         i = [] w a d
            [B][A]w == [A][B]       (swap)           w = (a2) [] b a

        Assuming Annotation:
            [(def of foo)](eq_foo) => [foo]
            and arity annotations

The arity annotation `(a3)` defers further expansion of the `[[F]z]` result. The `(eq_z)` annotation supports aesthetic presentation and preserves accelerated performance across serialization. Readers unfamiliar or uncomfortable with fixpoint may benefit from evaluating `[X][F]z` by hand a few times to grasp its behavior.

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

For high level parallelism, I propose we accelerate [Kahn process networks (KPNs)](https://en.wikipedia.org/wiki/Kahn_process_networks), or specifically a variant with lightweight temporal extensions (see *Reactive Kahn Process Networks*, below) so we can model asynchronous message streams. Essentially, we would describe a process network as a first-class value, and accelerate an 'evaluation' function (of type `KPN → KPN`) such that it distributes the process network across physically distributed memory and CPUs to perform the computation. The *monotonic* nature of KPNs allows distributed computation to continue in the background even as we inject and extract messages. KPNs can also be used to model effects, binding some IO ports to the real world.

## Structural Scope

Blocks naturally delimit the input scope for a computation. For example, we know that in `[B][[A]foo]`, `foo` can access `[A]` but not `[B]`. And we can trivially leverage this with the bind operation `b`. But Awelon also supports multiple outputs, and so scoping output is a relevant concern. To address this, Awelon introduces *tuple assertions* to annotate output scope:

                                   [](t0) == []
                                [[A]](t1) == [[A]]
                             [[B][A]](t2) == [[B][A]]
                                         ..
        [[I][H][G][F][E][D][C][B][A]](t9) == [[I][H][G][F][E][D][C][B][A]]

Tuple assertions can be deleted early if they are validated statically. Otherwise, some lightweight dynamic reflection may be necessary, and we'll fail fast if the tuple assertion is bad. Similar to arity annotations, tuples larger than `(t5)` should be rare in practice.

In addition to controlling output counts, programmers may wish to fail fast based on declared structure. To support this, Awelon supports a structure annotation `(seal_key)` and paired structure assertion `(open_key)` with the following rewrite semantics:

        (seal_foo) (open_foo) ==

Otherwise, they won't rewrite at all, and prevent further computation. This could be combined with a simple codebase constraint, that `(seal_foo)` annotation may be directly used only from words with a matching prefix `foo`. This would offer a simple basis for ADTs, and aid in early detection of errors. 

## Substructural Scope

[Substructural types](https://en.wikipedia.org/wiki/Substructural_type_system) allow us to reason about whether a value is used, or limit how many times a value is used. This can be convenient for modeling finite resources, intermediate states in a protocol, or ensuring certain steps are performed by a client computation. Awelon lacks primitive support for substructural types, but annotations can be leveraged:

* `(nc)` - mark a value non-copyable, aka 'affine'
* `(nd)` - mark a value non-droppable, aka 'relevant'
* inherit substructure of bound values (op `b`).

        [A](nc) [B] b == [[A](nc) B](nc)

Substructural attributes do not prevent application of a value with `a`. Copy and drop are explicit in Awelon, so dynamically enforcing these attributes is feasible, a lot easier than it would be in a variable substitution based language. But ideally, they would be enforced statically. 

*Note:* Whether Awelon systems implement substructural types is entirely optional. And it might be more efficiently represented at the type description layer, in which case these annotations would be more to support type inference.

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

Awelon's program rewrite semantics make it relatively easy to observe a computation in progress. Data is visible in the program representation, rather than hidden behind variables that must be queried via debugger. And of course, more conventional debugging applies. Some things we can easily do:

* set breakpoints for linking of specific words
* animate evaluation via frame capture on breakpoints
* evaluate in small steps more generally
* `(trace)` annotation for console/log style debugging
* log inputs to specific words (specified arity)

Debugging should be configurable through the runtime's interpeter or compiler.

## Static Typing

Awelon can be evaluated without static typing. There is no type driven dispatch or overloading. But if we can detect errors early by static analysis, that is a good thing. Further, static types are also useful for verifiable documentation, interactive editing (recommending relevant words based on type context), and performance of JIT compiled code. Strong static type analysis makes a *very* nice default.

We might represent our primitive types as:

        a   : S B [S → S'] → S' B
        b   : S B [E B → E'] → S [E → E']
        c   : S A → S A A
        d   : S A → S
        [F] : S → S [type(F)]

The type sequence `S C B A` aligns with a program structure `S[C][B][A]`. Effectively, `S` is the remainder of our program 'stack' when we view the program as rewriting a stack-like structure. In context of Awelon, we know that value types `C B A` must be first class functions, which potentially encode data.

Value sealers like `(seal_foo)` would require special types.

        (seal_foo) : S -> S <foo>
        (open_foo) : S <foo> -> S

Annotations can augment static type analysis in many ways, providing extra structure and assertions against which we can validate inference. Structural and substructural annotations would ideally be validated statically and have no dynamic behavior. A remaining concern is static typing of conditional behavior. We might represent various conditional data types:

        (bool)      S [S   → S'] [S     → S']   → S'
        (opt)       S [S   → S'] [S A   → S']   → S'
        (sum)       S [S B → S'] [S A   → S']   → S'
        (list)      S [S   → S'] [S A B → S']   → S'
        (cond)      S [A   → S'] [B     → S']   → S'

Knowing these types, we can also check for consistency between conditional branches. Unfortunately, inferring these types is difficult. Annotations can provide a much needed hint. I imagine programmers will want annotations for many common types - naturals, texts, binaries, lists, labels, records, and so on. Anything we accelerate or use frequently enough for a runtime to recognize.

### Deferred Typing

Simple static types are oft inexpressive, constraining more than helping.

We can introduce an explicit escape. Consider a `(dyn)` annotation used as `[F] b b (dyn)` with formal behavior `[A](dyn) => [A]`. The presence of `(dyn)` does not suppress obvious static type errors, but may suppress warnings or errors that result from being *unable* to infer static types for a given subprogram, types too sophisticated for our simple type checker. Dependent types are an example where types are likely too sophisticated for a simple checker. Conveniently, the ability to eliminate `(dyn)` via partial evaluations at compile time would enable us to leverage dynamically typed macro-like behaviors while still supporting a statically typed system. 

### Sophisticated Types

I propose a convention of defining `foo_type` to declare a type metadata for `foo`. This enables flexible abstraction and composition of type descriptions, expression of sophisticated types (contracts, Hoare logic, etc.), and provision of auxiliary hints or proofs. If we want to properly support dependent, existential, higher order, GADT, etc. types, we'll probably need to do so at this layer. By also providing the type check algorithms via the same dictionary, we might also simplify portable consistency checks.

Related to static typing, non-terminating evaluation in Awelon is always an error. There is no utility in unbounded divergence for a pure computation, though we might use arity annotations to defer computations and represent coinductive structure. In any case, static type analysis should attempt a limited termination analysis. While solving the halting problem isn't possible in general, obvious errors can always be reported.

## Editable Views

Awelon is designed to use a simple technique to support richer programming styles, DSLs, and larger programs: Awelon shifts the rich syntax burden to [projectional editing](http://martinfowler.com/bliki/ProjectionalEditing.html) in the form of *editable views*. 

*Aside:* By *editable views* I mean to emphasize purely functional, bidirectional rewrites between serializable representations. Similar to functional lenses. Projectional editing encompasses editable views and a lot more.

My initial emphasis is textual views, such that we can readily integrate favored editors and perhaps even leverage [Filesystem in Userspace (FUSE)](https://en.wikipedia.org/wiki/Filesystem_in_Userspace) to operate on a dictionary through a conventional filesystem. Numbers are a useful example for textual editable views. A viable sketch:

        #42         == (Awelon's 42)
        42          == [#42 #0 integer]
        -7          == [#0 #7 integer]
        3.141       == [3141 -3 decimal]
        -0.0070     == [-70 -4 decimal]
        2.998e8     == [2998 5 decimal]
        -4/6        == [-4 #6 rational]

Awelon's natural numbers here are given the `#` prefix in favor of a more aesthetic representation for signed numbers. From there, we build a tower of numbers. The basic approach of building views upon views is convenient because it makes views more extensible. For example, if we have no support for rational numbers, we'd still see `[-4 #6 rational]` which is still sensible to a human reader. Support for rational numbers or hexadecimal or similar can be added if missing.

Similarly, we could develop a view for command sequences:

        {A,B,C} == [[A] [[B] [[C] null cons] cons] cons]
        {A,B|C} == [[A] [[B] C cons] cons]
            such that {A,B,C|null} == {A,B,C}

There is a lot of flexibility with simple recognizing and rewriting of texts. Importantly, because these editable views are separate from the language compilers and interpreters, it is easy to experiment, use multiple views, and tune the programming experience to the programmer or the problem.

Many editable views will have an escape for code that they isn't recognized. For example, the `#42` escape for natural numbers can be generalized as an escape for words `#foo` or blocks `#[raw awelon code]`. This ensures universal access to Awelon behavior and the dictionaries. Of course, some DSL-inspired views might cleverly restrict programs to a subset of Awelon.

Ideally, most editable views should be computable, in the sense of having a normal form that can be the result of a normal evaluation process. Computable views enable the *evaluated result* of the program to have the *same view* as our programs. Or perhaps another, more suitable view. When we add 3.141 and -0.007, we want to see 3.134 in the program output. Hence, `[3134 -3 decimal]` should be a normal form, perhaps via arity annotations in `decimal`.

Design of computable views is very sensitive to arity annotations and accelerators. A consequence is that editable views should be *defined* within the same dictionary they're used to view by some simple convention. Perhaps a word defining a `[[view→code][code→view]]` pair where `code` and `view` are represented as text. Representing views within the dictionary is convenient for testing and caching of views, and for updating views based on application or edit session state (e.g. so we can separate namespace from code). 

With computable views in mind, we might represent comments as:

        // comment  ==  "comment"(a2)d

The arity annotation ensures the comment is not deleted until it might prevent progress from the left side, and hence we can always inject comments into values. A relevant point is that we aren't limited to one 'type' of comment, and comments of other types can easily inject flexible rendering hints into Awelon code. The discussion on *Named Local Variables* offers one very useful example, or a comment might include trace output for active debugging.

Editable views essentially form a compiler/decompiler that treats Awelon language as a functional macro-assembly. The main difference from conventional languages above a bytecode is that the Awelon code is treated as the canonical source, and we're forced to 'decompile' said code into our language of editing. The indirection provided by the decompiler simplifies issues like whitespace formatting and forwards/backwards compatibility.

With editable views, individual definitions can scale many orders of magnitude. It is even possible to represent conventional 'modules' in terms of expressions that construct first-class records of functions, though I'd encourage one word per independently useful function as the normal case.

Although initial emphasis is textual views, it is feasible to model richly interactive graphical views involving tables, graphs, canvases, checkboxes, sliders, drop-down menus, and so on. A sophisticated projectional editor could support frames or a zoomable interface where a word's definition may be logically inlined into the current view. 

*Aside:* The vast majority of punctuation characters were reserved for use with editable views. It's a lot easier to develop sophisticated editable views when we don't need to worry about ambiguity with Awelon words. 

### Named Local Variables

An intriguing opportunity for editable views is support for local variables, like lambdas and let expressions. This would ameliorate use cases where point-free programming is pointlessly onerous, such as working with fixpoints or algebraic math expressions. It also supports a more conventional programming style where desired. Consider a lambda syntax of form:

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

Lambdas can be leveraged into let expressions (like `let var = expr in CODE` or `CODE where var = expr`) or the Haskell `do` notation. Local recursion is possible if a view automatically introduces a fixpoint. Further, with variables we can feasibly introduce infix expressions like `((X + Y) * X)`, though our view may need to embed assumptions about arity and preferred associativity of the chosen operators.

### Qualified Namespaces

Awelon's hierarchical dictionaries support a simple form of namespacing. But it falls to editable views to support local shorthand, e.g. some form of `using large_prefix as x` or `using package_of_nicknames`. If we assume editable views are maintained within the dictionary, it is feasible to use comments to help control the view, tweaking the language as needed. An intriguing possibility is to integrate a database of nicknames for secure hash resources into the view, where said database is represented within the dictionary.

### Labeled Data - Records and Variants 

Labeled sum types (variants) allow conditional discrimination on a label. Labeled product types (records) allow us to access to heterogeneous data by a label. Primitive sum `(A + B)` and product `(A * B)` types are simple and sufficient for many use cases. But labeled data is self-documenting (label informs human) and extensible (add more labels).

A useful way to encode labeled sums is by deep primitive sum structures. That is, we use a `[[[value] l] r] l]` structure where the left-right-left path is extended to multiple bytes encoding a human-meaningful label. Unlike label-value pairs, deep sums do not require dependent types. A labeled product can similarly be modeled as a heterogeneous trie on the label. Consider:

        (Deep Sums)
        [[[A] l] l]
        [[[[[B] l] l] r] r]

        (Singleton Tries)
        [[[A] null cons] null cons]
        [null [null [[[B] null cons] null cons] cons] cons]

        (Merged Trie)
        [[[A] null cons] [null [[[B] null cons] null cons] cons] cons]

A useful label encoding is `(RL|RR)*LL`, where `RL` corresponds to constructor `[l] b [r] b`. The `(RL|RR)*` structure then represents a finite `(0|1)*` bitfield, within which we encode texts or numbers. The final `LL` terminates the label. This encoding has the nice properties of being a self-synchronizing code. Naive construction of the trie supports enumeration of labels and merging. The unused `LR` slot can potentially be used in the record as a name shadowing list. 

Unfortunately, the trie is awkward and inefficient to work with directly. A better alternative is instead to work with a trie *constructor* - a function that, given an initial record object, loads it with data. In Awelon text, this might look something like `[[A] "foo" tc [B] "bar" tc ...]`. Relevantly, the ordering of labels in this representation is not relevant, composition of record functions would essentially represent update of a record, and the encoding is not sparse. I'm assuming the type of trie-cons `tc` is dependent on the text argument. OTOH, we could use an expanded label structure if necessary. An editable view could feasibly reduce to a more aesthetic `[[A]:foo [B]:bar ...]` (ordered to taste). 

Acceleration of records would logically construct a trie and extract an updated new record function with every operation but really just using an optimized representation like a hashmap under the hood. Acceleration of functions related to labeled variants could serve a similar role of improving performance and aesthetics.

Assuming aesthetic, accelerated, labeled data, Awelon can support parameter objects, extensible event types, labeled case expressions, and a more conventional programming style with events and routing on human labels. 
 
## Unique References and In-Place Updates

Persistent structures are great and should be used frequently. But in-place update has some performance advantages that are reasonably difficult to give up. 

Fortunately, purely functional languages can support in-place update whenever we have a unique reference. We can model copy-on-write shared arrays, where subsequent writes are applied to the unique array without further copying. Lambda calculus makes this feature difficult to achieve (because the lexical environment is implicitly 'shared'). But Awelon makes copying explicit with operator `c`, so it's very easy to dynamically track uniqueness of a reference.

Arrays and records are the most useful targets for this treatment.

We can represent a list as an array (guided by `(array)` annotations). We can accelerate functions to access and update lists at indexed offsets. When the update function is applied to a unique array, it can update it in place. If applied to a shared array, it must copy the array first to get a unique array, but then all subsequent updates are in-place until the array is shared by logical copying via operator `c`. Records would receive similar treatment, albeit using a hashmap in place of the array.

*Note:* The `(nc)` annotation restricts copying of the marked value. Use of this can help enforce preservation of uniqueness, or at least help fail-fast debug cases where we want to restrict copying.

## Reactive Kahn Process Networks

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

Awelon reserves the `@` character to support hierarchical structure. 

Words of form `foo@dict` refers to the definition of `foo` in context of `@dict`. Similarly, `42@d` is `[41 succ]@d` is `[41@d succ@d]`, and `"hello"@d` is `[104 "ello" cons]@d`, and `$secureHash@d` will interpret the secure hash definition in context of `d`. Even some annotations may be usefully qualified, e.g. `(eq_z)@d` would reference the definition of `z@d`. These namespace qualifiers are second-class. No space is permitted between a word or block and the `@dict` qualifier. Logically, `foo@d` can be understood as just another word.

The dictionary name must also be a valid word, syntactically. How dictionary `@d` is defined is left to the dictionary representation, which Awelon language doesn't specify, but ideally should support lightweight dictionary structure sharing and efficient update.

Importantly, hierarchical dictionaries only permit the parent to reference child. There is no means for child to reference parent, no `..` path. Consequently, hierarchical dictionaries can be validated and evaluated and shared without context. This structural constraint is valuable for many [application model](ApplicationModel.md) patterns that involve messaging or publish-subscribe of full dictionaries, or use of dictionaries as documents or databases or other objects.

By Law of Demeter, programmers should avoid multi-level qualifiers like `foo@xy@zzy`, and it would be sensible to raise a warning when observed. However, such constructs may appear in the course of evaluation, and are read right to left, i.e. we look for meaning of `foo@xy` within dictionary `@zzy`.

### Localization

As a special optimization for hierarchical dictionaries, we may eliminate qualifiers that don't contribute to the program's behavior. For example, if natural numbers mean the same thing to the parent as they do to the child (based on underlying definitions of `zero` and `succ`), then `42@child` may be rewritten to just `42`.

## Staged and Generic Programming

Staged programming is a form of constant propagation. Awelon does not support this explicitly, but can model it. However, for staging to work in practice usually requires some syntactic support to clearly distinguish which computations are in which stage, and ensure uniform propagation of the staged constants. I am interested in use of editable views to extend Awelon with staging.

I'm especially interested in staging for generic programming - e.g. working with functions overloaded on data types or traits. In this case, our earlier stage would propagate information about the future program context, the types that will be on the stack. Multiple stage passes may be required, depending on how sophisticated the static type inference algorithms. A suitable *Editable View* can act as an extension to Awelon to make this feasible, and dictionaries can be developed that almost uniformly use generic programming.

