
# Awelon Language

Awelon is a purely functional language based on concatenative combinators. This document describes Awelon as a list of talking points, very roughly organized.

## Why Another Language?

Awelon differs from other languages in its adherence to simplicity and its choice of evaluation model. There are only four simple computational primitives, one data primitive, and a simple Forth-like syntax. Evaluation is by local confluent rewriting with lazy linking.

Awelon's simple syntax lowers barriers for program rewriting as an evaluation model, editable views for HCI, and collaboration with software agents. Program rewriting ensures the evaluated result is a program that may be serialized for debugging, distribution, or persistence. Lazy linking preserves human-meaningful words and link structure, ensuring results to be viewed the same as source. And software agents support the application layer effects.

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

Words are identified by a non-empty sequence of UTF-8 characters with a few limitations. The blacklist is `@#[]()<>{}\/,;|&='"`, SP, C0 (0-31), and DEL. Developers are encouraged to favor words that won't need escapes in most external contexts (such as URLs, HTML, Markdown, natural language text, or editable views), and that aren't too large. 

A useful subset of words is implicitly defined:

* the four Awelon primitive words `a`, `b`, `c`, `d`
* words to encode natural numbers, regex `[1-9][0-9]*`
* secure hash resources `$secureHash` or `%secureHash`

Other words are user defined, through a dictionary. There is no hard limit on word size, but words should ideally be kept small. In many cases, words may have simple, informal structure - e.g. `foo.doc` to represent documentation for `foo`. 

## Dictionary

Awelon has a simple, DVCS-inspired patch based dictionary representation:

        secureHashOfPatch1
        secureHashOfPatch2
        @word1 definition1
        @word2 definition2
        ...

A patch contains a list of patches to logically include followed by a list of definitions. Each definition is indicated by `@word` at the start of a line, followed by Awelon code. The last definition for a word wins, so definitions override those from earlier patches.

Cyclic definitions are erroneous. Loop behavior must be modeled via fixpoint combinator. However, trivially defining a word to itself as `@foo foo` will be accepted as meaning 'delete the word' rather than raising an error. Undefined words essentially evaluate to themselves.

Awelon's dictionary representation is not optimal for direct use by humans. It can be tolerated in small doses. But it is intended more as an import/export format, and for efficient sharing between humans and software agents. Humans will generally observe and influence a dictionary through an editable view, perhaps as a hypermedia web service or mounted into a filesystem.

While a filesystem is not a primary intended medium for dictionaries, an Awelon dictionary file in the file system should use the suffix **.ao** (for 'Awelon Object'). 

*Note:* See *Hierarchical Dictionaries* for more information.

## Secure Hash Resources

Awelon has built-in support for identifying resources via secure hash. 

* external code or stowage is referenced via `$secureHash`
* external binary data may be referenced via `%secureHash`
* dictionary patches are always referenced by secure hash

Awelon will use a 360-bit [BLAKE2b](https://blake2.net/) algorithm, and will encode the resulting 384-bit string as 60 characters [base64url](https://en.wikipedia.org/wiki/Base64). In a network context, it should be possible to request resources given their secure hashes, and perhaps form a content distribution network. (We might use only a fragment of the resource ID for lookup, and the rest for encryption.)

## Data

Awelon language has specialized representations for natural numbers and texts. Numbers are simply implicitly defined words like `42`. Texts have two embeddings, inline like `"hello, world!"` or multi-line:

        "
         multi-line texts starts with `" LF` (34 10)
         each line is indented by one space (32)
         excepting if line is empty `LF LF` (10 10) 
         terminate the text with `LF ~` (10 126) 
        ~

Texts must be valid UTF-8, forbidding C0 (except LF) and DEL. Inline texts additionally forbid the double quote and LF. There are no character escapes, but the extra whitespace in the representation of multi-line text is not considered part of the text. Texts in Awelon are [syntactic sugar](https://en.wikipedia.org/wiki/Syntactic_sugar) for a simple list of codepoints: 

        "hello" == [104 "ello" :]

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

        ~ = false       (null)
        : = mkP inR     (cons)

An alternative is to model data as implicitly folding over its structure. The Church encoding of natural numbers is an example of this. Consider: 

        [X][F] 0 i == [X]
        [X][F] 1 i == [X] F         == [[X][F] 0 i] F
        [X][F] 2 i == [[X] F] F     == [[X][F] 1 i] F

        0 = false
        1 = [0 S], 2 = [1 S], ...
        S = [c] a [b b] b a i

Folding over a recursive structure that carries no additional data isn't particularly interesting. But we can generalize easily to folds over list structures:

        [X][F] 0 i == X
        [X][F] [[A] [L] cons] i == [[X][F]L] [A]  F

        ~ = false
        : = w [[c] a b [b] b a] a w i

However, with data structures more sophisticated than natural numbers, choosing a specific fold seems awkward and arbitrary. With lists, we have both left folds and right folds. With trees, we have a variety of [tree traversals](https://en.wikipedia.org/wiki/Tree_traversal). Further, in context of linear typed structure, it is convenient if we can operate upon or [unzip](https://en.wikipedia.org/wiki/Zipper_%28data_structure%29) just part of a structure without observing the whole thing. 

I believe the algebraic data encodings are generally superior. Regardless, it is my intention that Awelon developers be given reasonably free reign over the choice of encoding and corresponding tradeoffs, that the relevant policy shouldn't be built into what is essentially a syntactic sugar. This is achieved by allowing developers to partially define the encoding: 

        0 (zero) is user definable 
        S (succ) is user definable
        : (cons) is user definable
        ~ (null) is user definable

        1 = [0 S]
        2 = [1 S]
        ...

        "hello" = [104 "ello" :]
        "→" = [8594 "" :]
        "" = ~

Awelon doesn't bother with anything beyond natural numbers and texts - just barely enough to efficiently embed data. The rest is left to *editable views* discussed toward the end of this document. Well, that and a special feature to reference external binaries.

## Binary Data

Awelon language supports reference to external binary data via `%secureHash`. We'll basically use the same text representation, but using values in range `0 .. 255` instead of Unicode codepoints.

For small binaries, we might instead choose to embed the binary within base64 or base85 text, and accelerate conversions between text to the binary. Or just use a list of byte values. However, in context of Awelon's application model and hypermedia, referencing external binary data can be very convenient.

*Note:* Developers are encouraged to leverage [rope-like](https://en.wikipedia.org/wiki/Rope_%28data_structure%29) structures if modeling edits on large binary data. 

## Acceleration

Acceleration is a performance assumption for Awelon. 

A runtime will recognize and accelerate common functions. The accelerated implementation may be hand optimized and built into the runtime to achieve performance similar to a primitive. For example, many runtimes will accelerate the following functions:

           [A]i == A            (inline)         i = [] w a d
        [B][A]w == [A][B]       (swap)           w = [] b a

The runtime will look at the given definitions. Upon recognizing `[] b a`, the runtime may link `w` to an acclerated swap implementation. Same for `i`.

In general, recognition of accelerators may be fragile. It may be that `i = [] w a d` is recognized where the logically equivalent `i = [] [] b a a d` or `i = [[]] a a d` are not recognized. We might not even recognize `j = [] w a d` because we changed the function name. This is ultimately up to the runtime. Every runtime should carefully document sufficient criteria for achieving acceleration. One robust approach is to define a 'seed' dictionary containing and documenting accelerated programs, from which users may derive their own dictionaries.

Critically, acceleration of functions extends also to *data* and even further to *organization* of code, and efficient representation thereof. A natural number, for example, might be represented by simple machine words. Arrays are possible via acceleration of list representation together with indexed update and access functions. An accelerator for linear algebra might support vectors and matrices of unboxed floating point numbers, which might be represented and computed on a GPGPU. Accelerated evaluation of process networks might use physically distributed processes and shared queues.

In general, accelerators may be compilers. For example, we might represent a computation in terms of interpreting a safe subset of OpenCL. Acceleration of that interpreter might involve compiling that code for performance on a CPU or GPGPU. 

Acceleration replaces conventional use of intrinsics and FFI.

## Annotations

Annotations help developers control, optimize, view, and debug computations. Annotations are represented as parenthetical words like `(par)` or `(a3)`. Some useful examples of annotations include:

* `(a2)..(a9)` - arity annotations to defer computations
* `(t0)..(t9)` - tuple assertions for output scope control
* `(nc) (nd)` - support substructural type safety
* `(:foo) (.foo)` - lightweight type tag and assertions
* `(par)` - request parallel evaluation of computation
* `(eval)` - request immediate evaluation of computation
* `(nat)` - assert argument should be a natural number
* `(bool)` - assert argument is a boolean typed value
* `(optimize)` - rewrite a function for efficient evaluation
* `(jit)` - compile a function for efficient evaluation
* `(stow)` - move large values to disk, load on demand
* `(memo)` - memoize a computation for incremental computing
* `(trace)` - record value to a debug output log
* `(trash)` - erase data you won't observe, leave placeholder
* `(error)` - mark a value as an error object
* `(@gate)` - symbols for editable views or active debugging 
* `(=foo)` - reduce code to a known name (quines, loops)

Annotations must have no observable effect within a computation. Nonetheless, annotations may cause an incorrect computation to fail fast, defer unnecessary computation, simplify static detection of errors, support useful external observations like debug logs or breakpoint states or a change in how an evaluated result is represented or organized.

Annotations may be introduced and documented on a runtime basis. In case of porting code, runtimes that do not recognize an annotation may ignore it. Long term, we should reach some de-facto standardization on useful annotations.

## Stowage

Stowage is a simple idea, summarized by rewrite rules:

        [large value](stow) => [$secureHash]
        [small value](stow) => [small value]

Stowage enables programmers to work semi-transparently with data or computations much larger than working memory. Unlike effectful storage or virtual memory, stowage is friendly in context of parallelism and distribution, structure sharing, incremental computing, and backtracking. However, effective use of stowage is limited to [persistent data structures](https://en.wikipedia.org/wiki/Persistent_data_structure), optimally those that implicitly batch writes such as [LSM trees](https://en.wikipedia.org/wiki/Log-structured_merge-tree).

What 'large value' means is heuristic, based on time-space tradeoffs. But it should be deterministic, reproducible, and simple. A good default is that a value be moved to stowage only if its encoding in Awelon is at least 256 bytes. Also, if our value is simple binary data, we will stow to a `%secureHash` external binary instead.

## Deferred Computations and Coinductive Data

The *arity annotations* `(a2)` to `(a9)` have simple rewrite rules:

                             [B][A](a2) == [B][A]
                          [C][B][A](a3) == [C][B][A]
                                        ..
        [I][H][G][F][E][D][C][B][A](a9) == [I][H][G][F][E][D][C][B][A]

To clarify, it is the *annotation* that has the given arity. Arity annotations specify nothing of their context.

Arity annotations serve a critical role in controlling computation. For example, the program `[[A](a2)F]` has the same type and semantics as `[[A]F]`, but the former prevents partial evaluation of `F` from observing `[A]`. Arity annotations can be used to guard against useless partial evaluations. For example, if we define swap as `w = (a2) [] b a` then we can avoid observing the useless intermediate structure `[A] w => [[A]] a`. 

Arity annotations serve a very useful role in modeling [thunks](https://en.wikipedia.org/wiki/Thunk) and [coinductive data](https://en.wikipedia.org/wiki/Coinduction). It is sometimes useful to model 'infinite' data structures to be computed as we observe them - procedurally generated streams or scene graphs.

Awelon does not implicitly memoize computations to avoid rework. Programmers can explicitly use `(memo)` to share work, and it is feasible for a runtime to optimize lightweight memoization for deferred computations. See *Memoization*.

## Evaluation

Evaluation of an Awelon program results in an equivalent Awelon program, hopefully one from which it is easier to extract information or efficiently perform further evaluations. Awelon's primary evaluation mode proceeds by local rewriting. The four primitives rewrite by simple pattern matching:

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

A runtime is not constrained by this basic strategy. For example, a runtime could perform logical copies without immediate evaluation at its own discretion, perhaps using implicit memoization. Evaluation could be tuned by annotations or runtime evaluation options. Parallelism, memoization, stowage, deferred computation, fail-fast errors, optimizations, etc.. are accessible via annotations. See also *Optimization*, below.

## Named Values

A 'named value' is a word whose evaluated definition is a singleton block. An Awelon runtime must treat named values as values with respect to binding, data plumbing, etc..

        true = [a d]
        false = [d i]
        42 = [41 S]

        42 true w == true 42
        42 [] b   == [42]

Support for named values is implicit with the lazy link constraint. I'm just making it explicit. Named values are essential for preserving human-meaningful structure and broad support for hypermedia resources.

## Fixpoints and Loops

Fixpoint is a function useful for modeling loop behaviors. For Awelon language, I favor the following variant of the [strict Z fixpoint combinator](https://en.wikipedia.org/wiki/Fixed-point_combinator#Strict_fixed_point_combinator):

        [X][F]z == [X][[F]z]F 
        z = [[(a3) c i] b (=z) [c] a b w i](a3) c i

        Using Definitions:
               [A]i == A            (inline)         i = [] w a d
            [B][A]w == [A][B]       (swap)           w = (a2) [] b a

        Assuming Annotation:
            [(def of foo)](=foo) => [foo]
            and arity annotations

The arity annotation `(a3)` defers further expansion of the `[[F]z]` result. The `(=z)` annotation supports both aesthetic presentation and accelerated performance. I recommend that readers unfamiliar or uncomfortable with fixpoint step through evaluation of `[X][F]z` by hand a few times to grasp its behavior. 

*Note:* Fixpoint is notoriously difficult for humans to grok. It is also awkward to use, with tacit style doing no favors here. Use of *named locals* does help (see below), but we'll want to build useful loop and [generator](https://en.wikipedia.org/wiki/Generator_%28computer_programming%29) abstractions above fixpoint - folds, sorts, collections oriented functions, etc.. 

*Aside:* The `(=foo)` annotation is also useful as an assertion. It becomes an error if not equal.

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

## Interpretation

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

For low level parallelism, we might accelerate [linear algebra](https://en.wikipedia.org/wiki/Linear_algebra). Alternatively, we accelerate evaluation for a safe subset of OpenCL or WebCL or some Khronos group spec, with which we might later accelerate linear algebra. Either approach can get Awelon into machine learning, physics simulations, graphics and audio, and other high performance number crunching systems.

For high level parallelism, I propose acceleration of [Kahn process networks (KPNs)](https://en.wikipedia.org/wiki/Kahn_process_networks), or more precisely the *Reactive Process Network* variant detailed later. The state of a process network is described by a value. This state may involve named processes with ports and wires between them, messages pending on wires. Evaluation results deterministically in another state. Accelerated evaluation can use physically distributed processors and queues to simulate the network. Usefully, process networks are *monotonic*. A client program can inject messages and extract results without waiting for a final network state.

## Structural Scoping

Blocks naturally delimit the input scope for a computation. For example, we know that in `[B][[A]foo]`, `foo` can access `[A]` but not `[B]`. And we can trivially leverage this with the bind operation `b`. But Awelon also supports multiple outputs, and so scoping output is a relevant concern. To address this, Awelon introduces *tuple assertions* to annotate output scope:

                                   [](t0) == []
                                [[A]](t1) == [[A]]
                             [[B][A]](t2) == [[B][A]]
                                         ..
        [[I][H][G][F][E][D][C][B][A]](t9) == [[I][H][G][F][E][D][C][B][A]]

Tuple assertions can be deleted early if they are validated statically. Otherwise, some lightweight dynamic reflection may be necessary, and we'll fail fast if the tuple assertion is bad. Similar to arity annotations, tuples larger than `(t5)` should be rare in practice.

In addition to controlling output counts, programmers may wish to fail fast based on declared structure. To support this, Awelon supports a structure annotation `(:T)` and paired assertion `(.T)` with the following rewrite semantics:

        (:foo) (.foo) ==

In practice, we might construct a tagged value with `[(:foo)] b` and deconstruct it with `i(.foo)`. The symbol `foo` or `T` is left to the programmer, but must match between annotation and assertion. These annotations are not labels (i.e. you cannot discriminate on them), but they do resist *accidental* access to structured data. As with tuple assertions, we can fail fast dynamically or detect an error statically.

## Substructural Scoping

[Substructural types](https://en.wikipedia.org/wiki/Substructural_type_system) allow us to reason about whether a value is used, or how many times it is used. This is convenient for modeling finite resources, intermediate states in a protocol, or ensuring certain steps are performed by a client computation. Awelon provides simple annotations for lightweight substructural types:

* `(nc)` - mark a value non-copyable, aka 'affine'
* `(nd)` - mark a value non-droppable, aka 'relevant'
* inherit substructure of bound values (op `b`).

We can eliminate substructural annotations by observing a value with `a`. It is not difficult to track and validate substructural properties dynamically, or to represent them in static types. For fail-fast debugging, we can also introduce annotations `(c)` and `(d)` that respectively assert a value is copyable and droppable (without actually copying or dropping it). 

## Error Annotations

We can mark known erroneous values with `(error)` as in `"todo: fix foo!"(error)`. If we later attempt to observe this value (with `i` or `a`), we will simply halt on the error. However, we may drop or copy an error value like normal. In addition to user-specified errors, a runtime might use error annotations to highlight places where a program gets stuck, for example:

        [A](nc)c        => [[A](nc)c](error)i
        [[A][B][C]](t2) => [[A][B][C]](t2)(error)

Error values may bind further arguments as `[B][A](error)b == [[B]A](error)`. Error values will evaluate like any other value, and will collapse normally from `[[A](error)i]` to `[A](error)`. The error annotation is idempotent and commutative with other annotations on a block.

## Garbage Data

For relevant data, we always have an option to drop data into a logical bit bucket then never look at it again. If we tell our runtime that we will never look at it again, we can also recover memory resources associated with that data. We can represent this pattern by use of a `(trash)` annotation:

        [A](trash) => [](error)
        [A](rel)(trash) => [](rel)(error)

We destroy the data but preserve substructure. Because the data has been destroyed, the resulting value is marked erroneous.

## Active Debugging

Awelon's program rewrite semantics make it relatively easy to observe a computation in progress. Data is visible in the program representation, rather than hidden behind variables that must be queried via debugger. It is feasible to predictably animate evaluation by pausing evaluation and taking snapshots just before linking of specific words, recording a series of 'frames' to render.

Conventional debugging techniques also apply. 

I propose `(trace)` annotation to serve as a basis for conventional printf style debugging. For example, `[message](trace)` would evaluate to simply `[Msg]` but would implicitly copy the message to a second stream. Traced outputs could easily be rendered within the same program by prepending a comment like `[message](a2)d` to the evaluated program.

Of course, tracing is invasive and is not ideal for debugging in general. Configuring words or `(@gate)` annotations to operate as breakpoints or frame separators, or even just profiling their use, is much less invasive, but pays for that in management of the debug configuration.

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

### Delayed Typing

Simple static types are frequently inexpressive, constraining more than helping.

We can introduce a simple escape. Consider a `(dyn)` annotation used as `[F] b b (dyn)` with formal behavior `[A](dyn) => [A]`. The presence of `(dyn)` does not suppress obvious static type errors, but could generally suppress errors that result from being *unable* to infer static types that might be too sophisticated for our simple checker - especially dependent types. Intriguingly, the ability to eliminate `(dyn)` via partial evaluations could mean we can improve static types as we provide more static information and usage context.

### Sophisticated Types

More generally, we can introduce a simple convention of defining `foo.type` to declare a type for `foo`. This enables flexible abstraction and composition of type descriptions, expression of sophisticated types (contracts, Hoare logic, etc.), and provision of auxiliary hints or proofs. If we want to properly support dependent, existential, higher order, GADT, etc. types, we'll probably need to do so at this layer. Optimally, the type checker operating at this layer would itself be defined as a dictionary function.

Related to static typing, non-terminating evaluation in Awelon is always an error. There is no utility in unbounded divergence for a pure computation, though we might use arity annotations to defer computations and represent coinductive structure. In any case, static type analysis should attempt a limited termination analysis. While solving the halting problem isn't possible in general, obvious errors can always be reported.

## Editable Views

Awelon language has an acceptably aesthetic plain text syntax. However, like Forth, Awelon does not scale nicely beyond perhaps twelve tokens per definition because humans too easily lose track of context. This could be mitigated by better type feedback or live examples. But Awelon is designed for another simple technique to support more conventional programming styles, DSLs, and program scale. Awelon shifts the burden to [editable views](http://martinfowler.com/bliki/ProjectionalEditing.html). 

I put some emphasis on *plain text* editable views, such that we can readily integrate favored editors and perhaps even leverage [Filesystem in Userspace (FUSE)](https://en.wikipedia.org/wiki/Filesystem_in_Userspace) to operate on a dictionary through a conventional filesystem. However, graphical views could potentially include checkboxes, sliders, drop-down lists, graphs and canvases, and may offer an interesting foundation for graphical user interfaces within Awelon's application model. 

Numbers are a useful example for editable views. A viable sketch:

        #42         == (AWELON 42)
        42          == [#42 #0 integer]
        -7          == [#0 #7 integer]
        3.141       == [3141 -3 decimal]
        -0.0070     == [-70 -4 decimal]
        2.998e8     == [2998 5 decimal]
        -4/6        == [-4 #6 rational]

Awelon's natural numbers are given the `#` prefix in favor of a more aesthetic representation of signed numbers. From there, we build a tower of numbers. The basic approach of building views upon views is convenient because it makes views more extensible. For example, if we have no support for rational numbers, we'd still see `[-4 #6 rational]` which is still sensible to a human reader. And support for viewing rational numbers could be added quickly.

Every editable view should have an unambiguous escape to raw Awelon code. While I use `(AWELON 42)` above to make it obvious, I could just as easily use `#` as the primary escape, or `'(42)` or `\42`, etc.. Whatever users find acceptable. The nature of editable views does make it easy to experiment for aesthetics. Use of escapes permits an editable view to support key-words, if desired.

Command lists are another valuable view feature:

        {foo, bar, baz} == [[foo] {bar, baz} :]
        {baz}           == [[baz] ~ :]

Command lists are useful for various purposes, supporting continuation-passing style or a concise embedding of interruptable code. While I use a normal `~ :` list structure here, a view could try something more specialized.

Ideally, all editable views should be *evaluable*. That is, program evaluation should generate the same structures we use to view and edit programs. When we add 3.141 and -0.007, we want to see 3.134 in the program output. That is, `[3134 -3 decimal]` (or whatever we use to represent decimal numbers) should be a viable result from a computation. 

Design of evaluable editable views is very sensitive to arity annotations and accelerators. A consequence is that editable views should be *defined* within the same dictionary they're used to view by some simple convention. Perhaps a word defining a `[[view→code][code→view]]` pair where `code` and `view` are represented as text. Representing views within the dictionary is convenient for testing and caching of views, and for updating views based on application or edit session state (e.g. so we can separate namespace from code). 

With evaluable views in mind, we might represent comments as:

        /* comment */  ==  " comment "(a2)(@rem)d

The arity annotation allows embedding of comments into computed values. The `(@rem)` gate serves as a lightweight indicator of the comment's 'type' (so we can add other comment types) and additionally permits integration with active debugging - for example, tracing comments to see progress, or conditionally stalling on certain comments.

*Aside:* Between command lists and numbers, word definitions can easily scale to a thousand tokens. If we start representing graphical programs with tables, graphs, canvases, radio buttons, drop-down options lists, and similar features we might scale another order of magnitude. Of course, we'll also divide larger programs into small words that can be viewed and edited together. 

## Named Local Variables

An intriguing opportunity for editable views is support for lambdas and let-expressions. This would ameliorate use cases where point-free programming is pointlessly onerous, and support a more conventional programming style if desired. Consider the locals syntax used in [Kitten language](http://kittenlang.org/):

        -> X Y Z; CODE   ==  "X Y Z"(a2)(@λ)d CODE'

When used leftmost in a subprogram the arrow effectively forms a lambda. This syntax also supports local variable expressions as in `6 7 * -> X; CODE`. On the right hand side, the `"X Y Z"(a2)(@λ)d` fragment is a lambda comment. We need the comment to recover the variable names when later rendering and editing the code, but the specific form is arbitrary.

We can compute `CODE' = T(Z, T(Y, T(X, CODE)))` with a simple algorithm:

        T(X, E) | E does not contain X      => d E
        T(X, X)                             => 
        T(X, [onF][onT] if)                 => [T(X, onF)][T(X, onT)] if
        T(X, [E])                           => [T(X,E)] b
        T(X, F G)
            | only F contains X             => T(X,F) G
            | only G contains X             => [F] a T(X,G)
            | otherwise                     => c [T(X,F)] a T(X,G)

This algorithm is adapted from the optimization using free variables. The main difference is that we know our variables are named values and we need special handling for conditional behaviors. 

Conditional behavior like `if` should be specialized because we do not want to copy data into each branch before selecting one. Instead, we leave the value on the stack then select one branch to handle it. The simple rule for `if` above would also work well for sums, optional values, lists, etc.. (indeed, we could define a generic `if = [] b b a (cond) i` for all binary conditions). However, we might need an additional rule for working with *labeled data* and switching on labels.

Assuming named locals, we might also benefit from infix expressions of code, e.g. `((X + Y) * Z)`. Of course, use of parentheses in this role might mean we need to escape or tune expression of Awelon-level annotations.

## Namespaces

Qualified namespaces are readily supported by editable views. Trivially, we could support a comment like `using large_prefix as x; ...` such that subsequent code may use `x` in place of `large_prefix` (and `x.foo` in place of `large_prefix.foo`). Suffixes can also be supported. And in contexts where our view is not limited to pure text, we could leverage color or other indicators for namespace.

More intriguingly, namespaces can be built into an editable view, such that we can skip the boiler-plate namespace comment and just start using `x`. We can fully separate namespaces from source code through views. If ever we want humans to work effectively with `$secureHash` resources, having a built-in namespace at the view layer would be essential. Namespaces could instead be shifted to 'views' at the edit session or user layer, that can be maintained statefully for edits to many definitions.

## Labeled Data - Records and Variants 

Labeled sum types (variants) allow conditional discrimination on a label. Labeled product types (records) allow us to access to heterogeneous data by a label. Primitive sum `(A + B)` and product `(A * B)` types are simple and sufficient for many use cases. But labeled data is self-documenting (label informs human) and extensible (add more labels).

A useful way to encode labeled sums is by deep primitive sum structures. That is, we use a `[[[value] inL] inR] inL]` structure where the left-right-left path encodes the label. Unlike label-value pairs, deep sums do not require dependent types. A labeled product could feasibly be modeled as a heterogeneous trie on the label. Consider:

        (Deep Sums)
        [[[A] inL] inL]
        [[[[[B] inL] inL] inR] inR]

        (Singleton Tries)
        [[[A] ~ :] ~                     :]
        [~         [~ [[[B] ~ :] ~ :] :] :]

        (Merged Trie)
        [[[A] ~ :] [~ [[[B] ~ :] ~ :] :] :]

Here the label is encoded as `(RL | RR)* LL`, where `RL` corresponds to constructor `[inL] b [inR] b`. The `(RL | RR)*` structure represents a finite `(0 | 1)*` bitfield, within which we might encode texts or numbers. The final `LL` terminates the label. This encoding has several nice properties. It is a simple regular language and a self-synchronizing code. Naive construction of the trie supports enumeration of labels and merging. The unused `LR` slot can potentially be used in the record as a name shadowing list. 

Unfortunately, while tries are an excellent data structure, they are awkward to work with. Rigid ordering of labels within a trie is inconvenient for human use. Human meaningful labels are very sparse in the above encoding, which does not result in space-efficient representatins. (Use of a radix tree might help, but has a significant complexity cost.) For acceleration, exposing the record's in-memory representation to normal user functions is not optimal.

Instead of working with the trie directly, we should represent a function that writes a trie by injecting a series of label-value pairs. For example, a function `[[A] "foo" KV [B] "bar" KV]` might write labels `foo` and `bar` into a record with the associated values (or perhaps `label 'o 'o 'f`  to simplify types). An editable view could provide a more aesthetic presentation like `[[A] :foo [B] :bar]`. The resulting function is composable, commutative for different labels, and has a much better signal-to-noise ratio and HCI story than the trie structure.

An accelerated runtime could use a hashmap or other conventional structure to represent a record. Each accelerated operation on a record could perform the full construct-manipulate-extract sequence, such that the trie is not visible to normal user code and might never be represented in memory. The logical existence of the trie is only necessary to understand the record model, to reason about its formal type, commutativity, or correctness. We might similarly defer construction of labeled variants to simplify acceleration and HCI for them, too.

By accelerating labeled data, Awelon can support parameter objects, flexible variants, labeled case expressions, and a more conventional programming style with events and routing on human labels. If *necessary*, I might introduce primitive syntactic sugar for labels like I have for natural numbers and texts. But I feel the need to see how far we can get without that, first.

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

The advance of time is driven externally at open input ports, internally via latencies. Advancing time at an open input port essentially says, "the next message will have *at least* this future time". Cyclic wiring with latency permit precise expression of ad-hoc clock-like behaviors. Conventional KPN behavior is preserved if we never advance time and use zero latency wiring. That is, reads wait until either a message is available OR the upstream process advances past the reader's time, which ever happens first. 

Time stamps and latencies can easily be represented by natural numbers. We can usefully normalize times by subtracting the minimum time stamp from all time stamps, such that at least one time stamp in the network description is 0.

Reactive process networks fill out the remaining expressive gap of KPNs, enabling us to work with asynchronous inputs, merge streams as needed. Further, we can now control evaluation 'up to' a given logical time. This is very useful for interacting with the real world, and in real-time.

## Hierarchical Dictionaries

Awelon supports a simple model for hierarchical structure. The motivation is to support dictionary passing or synchronizing application patterns and corresponding security models that are otherwise difficult to represent. Awelon dictionaries are most readily referenced and shared via secure hash, so we leverage that here:

        secureHashOfPatch1
        secureHashOfPatch2
        @word1 definition1
        ...
        @@dict secureHashOfDict
        ...
        @wordN definitionN

We update the definition of special symbol `@dict` to define a child dictionary. By default, all hierarchical dictionaries are empty. Use of a blank line in place of the secure hash is treated as a synonym for the empty dictionary. As with words, only the final definition of a dictionary symbol applies.

Child dictionaries are referenced indirectly. A word of the form `foo@dict` refers to the meaning of `foo` within `dict`. When linked, if the definition of `foo` in `dict` is `x y z` then `foo@dict` will link as `x@dict y@dict z@dict`. Similarly, we may annotate a block for evaluation in context of a child dictionary, `[x y z]@dict == [x@dict y@dict z@dict]`. Since text is sugar for blocks, we also support `"hello"@dict == [104 "ello" :]@dict`. No space is permitted between a word, block, or text and the `@dict` annotation.

Deep hierarchical references such as `foo@bar@baz` are possible. These are left-associative, that is `foo@bar` under `baz`. I recommend against such references in source code (cf. Law of Demeter), but such references may be the result of an evaluation. 

A child cannot reference the parent. But parent and child frequently share structure and meaning - a consistent interpretation of numbers, texts, math and utility functions. 

*Localization* is an optimization that takes advantage of this sharing. Whenever `foo@bar` has the same meaning as `foo`, a localizing evaluator may forget the origin and simply replace `foo@bar` by `foo`. Localization can improve performance in context of stowage or memoization. More importantly, localization improves aesthetics and comprehension: we avoid noise like `42@bar` when it just means `42`, and applications will implicitly link localized metadata such as `foo.doc` instead of `foo.doc@bar`. Intriguingly, it is feasible for `foo@bar@baz` to localize to `foo` or `foo@bar`.

