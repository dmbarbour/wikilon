
# Awelon Language

Awelon is a purely functional language based on concatenative combinators. Awelon is designed for an [RESTful application model](ApplicationModel.md) that maintains application state within a codebase and handles effects in context of a multi-agent system (for example, via [publish-subscribe](https://en.wikipedia.org/wiki/Publish%E2%80%93subscribe_pattern) and [tuple space](https://en.wikipedia.org/wiki/Tuple_space) patterns). Awelon systems support [incremental computing](https://en.wikipedia.org/wiki/Incremental_computing) with [hypermedia](https://en.wikipedia.org/wiki/Hypermedia) at a large scale.

Syntactically, Awelon is simple and minimalist, similar to Forth. Where more sophisticated or domain specific syntax is appropriate, Awelon systems are expected to leverage [editable views](http://martinfowler.com/bliki/ProjectionalEditing.html) to avoid complicating parsers. These editable views will be defined within the Awelon system itself.

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

Words are identified by a sequence of UTF-8 characters with a few limitations. The blacklist is `@[]<>(){},;|&=\"`, SP, C0, and DEL. Developers are encouraged to further limit words to symbols that will not need escaped in external contexts, such as URLs, natural language, or editable views.

Words are not defined by the evaluator. Instead, words are defined in an external dictionary, described later. There are some limitations on which words may be user-defined. For example, the four primitives `a`, `b`, `c`, and `d` may not be redefined. Words such as `42` are reserved for defining the associated number.

## Data

Awelon language optimizes representations for numbers and texts. Numbers effectively use predefined words like `42`. Texts have two embeddings, inline like `"hello, world!"` or multi-line:

        "
         multi-line texts starts with `" LF` (34, 10)
         each line is indented by one space (32)
         terminate the text with `LF ~` (10, 126) 
        ~

Texts must be valid UTF-8, forbidding C0 (except LF) and DEL. LF is the only special case, requiring the extra space at the start of the new line. There are no character escapes. The indent spaces are not considered part of the text, nor are the extra LF characters at the start and end.

Awelon language has exactly one primitive value type - the `[]` block of code, representing a function. Data is instead [Church encoded](https://en.wikipedia.org/wiki/Church_encoding) or favors alternatives like [Scott encoding](https://en.wikipedia.org/wiki/Mogensen%E2%80%93Scott_encoding), representing values as functions. The support for numbers and texts is close in nature to [syntactic sugar](https://en.wikipedia.org/wiki/Syntactic_sugar) for an encoding.

Before I jump to the encoding of numbers and texts, let us examine encodings for other useful data types. Booleans select from a pair of functions:

        [onF][onT] false i == onF               false = [d i]
        [onF][onT] true i  == onT               true = [a d]

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

Natural numbers use the `#` prefix to allow Awelon language to later aesthetically support signed integers and other number types without the prefix. Other number types will probably derive from naturals. For example, the normal Church encoding for signed numbers is a pair of natural numbers where (in normal form) one of the pair is zero. We might similarly support rational (a numerator and denominator pair) or decimal numbers (a significand and exponent pair). Awelon language conservatively reserves words with prefix `[+-~.#]?[0-9]` for purpose of representing numbers.

*Aside:* Beyond numbers and texts, users might be interested in more structured data with human-meaningful symbolic labels. Awelon language does not optimize for those cases, but support for *Editable Views* may present structured data to humans.

## Acceleration

Acceleration is a performance assumption for Awelon language. A runtime should recognize and accelerate common functions. The accelerated function may use a hand-optimized or built-in implementation to achieve performance similar to a primitive. For example, most runtimes will accelerate the following functions:

           [A]i == A            (inline)         i = [] w a d
        [B][A]w == [A][B]       (swap)           w = [] b a

Accelerated functions serve a role similar to 'built in' functions in other languages. However, accelerated functions *must* be given an explicit definition in the Awelon language as part of the user's dictionary. Users should be able to review and interact with this definition like any other. However, modifying or renaming an accelerated function may generally have a severe and negative impact on performance.

More critically, acceleration of a function allows for *compact representations*, which is important because all *Data* is encoded within functions. Natural numbers, for example, might be represented during computation by normal machine words. Accelerated functions may be optimized for operation directly on compact representations. Hence, if we accelerate both natural numbers and *arithmetic* upon them, we can effectively treat natural numbers as language primitives for performance.

The long term intention is for Awelon language to accelerate not just arithmetic, but also collections processing, linear algebra, and parallel [process networks](https://en.wikipedia.org/wiki/Kahn_process_networks). Acceleration of linear algebra, for example, could permit semi-transparent GPGPU acceleration. And acceleration of process networks could enable Awelon computations to semi-transparently leverage cloud computing. 

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

## Fixpoint

Fixpoint is a function useful for modeling loop behaviors. For Awelon language, I recommend the following variant of the [strict Z fixpoint combinator](https://en.wikipedia.org/wiki/Fixed-point_combinator#Strict_fixed_point_combinator):

        [X][F]z == [X][[F]z]F 
        z = [[c] a [(/3) c i] b b w i](/3) c i

        Using Definitions:
           [A]i == A            (inline)         i = [] w a d
        [B][A]w == [A][B]       (swap)           w = (/2) [] b a

The arity annotation `(/3)` defers further expansion of the `[[F]z]` result. This variation of the Z combinator has the advantage that the definition of `z` can be found in the naive evaluation, and that `[F]` is not replicated unnecessarily. I recommend that readers unfamiliar with fixpoint step through evaluation of `[X][F]z` by hand a few times to grasp its behavior.

Fixpoint is notoriously difficult for humans to grok and sometimes awkward to use. Instead of directly using fixpoint, we'll want to build more comfortable loop abstractions above it like [generators](https://en.wikipedia.org/wiki/Generator_%28computer_programming%29) and folds. We would benefit from accelerating at least fixpoint, and possibly some of the more popular loop abstractions.

## Stowage

Stowage is a simple idea, summarized by rewrite rules:

        [large value](stow) => [%secureHash]
        [small value](stow) => [small value]

Effectively, stowage is a form of [virtual memory](https://en.wikipedia.org/wiki/Virtual_memory), but is performed at a language-aware link layer. The large value is pushed to disk and replaced in memory by a word that will load that value if it is later observed. In practice, the word will include a secure hash of the value. Use of a secure hash simplifies many contextual challenges: reproducible results, structure sharing, stable names for memoization, transparent persistence, etc..

Values not much larger than the secure hash will not be stowed. The threshold is heuristic (e.g. 3x secure hash size). But at the very least, stowage should be idempotent.

Stowage performs an essential role for purely functional programming where access to external storage (file system, database, etc.) is indirect and effectful. Stowage is optimal for cases like [LSM trees](https://en.wikipedia.org/wiki/Log-structured_merge-tree) that are designed to batch writes out to disk. 

For a specific secure hash, I favor BLAKE2b 360 bits encoded as 72 characters in Crockford's Base32, albeit favoring lower case characters on encode.

## Dictionary

The Awelon dictionary is a set of defined words, and doubles as both a codebase and a substrate for [application state](ApplicationModel.md). We can expect frequent updates, history and versioning, forks and merges, need for efficient distribution, shared objects, and reusable cached compilation. To support these features, Awelon favors a DVCS-inspired patch-based dictionary model. 

        secureHashOfPatch1
        secureHashOfPatch2
        @word1 definition1
        @word2 definition2
        ...

A dictionary consists of a root patch - a UTF-8 string. Each patch has a header consisting of a sequence of secure hashes, and a body consisting of a sequence of definitions. The secure hashes identify immutable patches that are logically inlined into the current dictionary. If a word is defined more than once, the last update always wins. A word is logically deleted by defining the trivial cycle, `@foo foo`.

This representation is independent of the filesystem, supports lightweight updates and shared strucure, and can support many useful dictionary structures such as the append-only log or LSM tree. However, it has no built-in indexing. To take full advantage will require external, incremental index models.

*Note:* Not every word is defined this way. Awelon number words are implicitly defined. The four primitives `a b c d` may not be defined. Stowage words `%secureHash` may only be given a definition that is valid based on the secure hash.

*Aside:* This should probably use the same secure hash as Stowage. 

## Evaluation

An Awelon string evaluates into an equivalent Awelon string, much like `(3 + 4)` evaluates to `7` under base ten arithmetic. Evaluation of Awelon code proceeds in small steps based on local rewriting, starting with the four primitives:

            [B][A] a => A[B]    (apply)
            [B][A] b => [[B]A]  (bind)
               [A] c => [A][A]  (copy)
               [A] d =>         (drop)

Awelon's evaluation model is 



A string of Awelon code evaluates to an equivalent string of code, but generally 

 rules. The primitives are evaluated as t


## Safety

## Incremental Computing

If users define a word that conflicts with the stowage naming conventions, an error may be raised at the dictionary layer. Also, while developers cannot directly compare stowage IDs for equivalence, use of *memoization* can leverage stowage for efficient incremental computing.


