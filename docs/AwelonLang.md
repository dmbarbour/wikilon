
# Awelon Language

Awelon is a Turing complete, purely functional language based on concatenative combinators with confluent rewrite rules. Specifically, Awelon has four primitive combinators:

        [B][A]a == A[B]         (apply)
        [B][A]b == [[B]A]       (bind)
           [A]c == [A][A]       (copy)
           [A]d ==              (drop)

Besides the four primitives, programmers develop a *Dictionary* within which each word is defined by an Awelon encoded function. For example, if we want an inline function `[A]i == A` then we could define `i = [[]] a a d`. Evaluation proceeds by rewriting according to the primitive combinators and lazily substituting words for definitions when doing so permits further progress. As a practical language, Awelon additionally has built-in support for encoding data: natural numbers, embedded texts, binary resources, and labeled structures. 

Those `[]` square brackets contain Awelon code and represent first-class functions. Values in Awelon are always first-class functions, typically using [Church encodings](https://en.wikipedia.org/wiki/Church_encoding) or [Scott encodings](https://en.wikipedia.org/wiki/Mogensen%E2%80%93Scott_encoding). However, effective Awelon compilers or interpreters will recognize and optimize common functions and value types. This is a concept of software *Accleration* to improve efficient use of CPU and memory. Acceleration for collections-oriented operations, such as matrix multiplication and linear algebra, can feasibly leverage SIMD instructions or GPGPU.

Compilers or interpreters will also recognize a set of annotations, represented by parenthetical words. For example, `[A](par)` might indicate parallel evaluation for the subprogram `A`, or `[F](accel)` might indicate that `F` should be recognized and accelerated. Annotations are represented by parenthetical words and have identity semantics - programs must have the same formal behavior even if annotations are ignored. However, external observers should be affected. Annotations serve valuable roles in debugging, guiding performance, and expressing programmer intentions.

By itself, Awelon is a very simplistic language - a purely functional assembly. 

The intention is to leverage Awelon together with [projectional editing](http://martinfowler.com/bliki/ProjectionalEditing.html) tools. We can project rich textual or even graphical programming interfaces above the simple Awelon code. Because Awelon evaluates by rewriting, we can rewrite the code and project a similar interface from the evaluation result. This can support a close relationship between a program source and its user interface, similar to a spreadsheet. Awelon project explores alternative [application models](ApplicationModel.md) that leverage the language's special characteristics, with goals for composition and sharing of data and applications between users.

## Words

Words are the user-definable unit for Awelon code. Syntactically, a word has regex `[a-z][a-z0-9-]*`. That is, a word consists of lower case alphanumerics and hyphens, and starts with an alpha. Definitions of words are acyclic, Awelon encoded functions.

The formal meaning of a word within Awelon code is equivalence to its definition. But words are often given special connotations in context of an environment. For example, `foo-doc` may associate documentation with word `foo`, or `main` may serve as the default entry point for a monadic process.

## Natural Numbers

Awelon has native support for natural numbers. Syntactically, numbers are represented by regex `0 | [1-9][0-9]*` wherever a word may appear. 

        0 = [zero]
        1 = [0 succ]
        2 = [1 succ]
        3 = [2 succ]
        ...
        42 = [41 succ]
        (et cetera)

Definitions for `zero` and `succ` are left to the dictionary. However, in practice you'll want to use definitions that are recognized and accelerated. Awelon does not have any built-in support for signed integers, floating point numbers, etc.. Those should be introduced using an *Editable View* through a projectional editor.

## Embedded Texts

Awelon has limited native support for embedding texts inline between double quotes such as `"Hello, world!"`. Embedded texts are limited to ASCII, specifically the subset valid in Awelon code (32-126) minus the double quote `"` (34). There are no escape characters, and there is no Unicode support. Semantically, a text represents an ASCII binary list.

        ""      = [null]
        "hello" = [104 "ello" cons]

Embedded texts are suitable for lightweight DSLs, test data, rendering hints, comments, and similar use cases. There are no escape characters. For larger or more sophisticated texts it might be better to treat them as external *Secure Hash Resources*, or structured texts represented by suitable data structure. Like natural numbers, definitions for `null` and `cons` are left to the dictionary.

## Secure Hash Resources

It is possible to identify binaries by *secure hash*. Doing so has many nice properties: immutable and acyclic by construction, cacheable, securable, provider-independent, self-authenticating, implicitly shared, automatically named, uniformly sized references, and smaller than full URLs or file paths. Awelon systems widely leverage secure hashes to reference binaries:

* external binary data may be referenced via `%secureHash`
* code and structured data is referenced via `$secureHash`
* dictionary tree nodes are referenced using `/prefix secureHash`

Use of `$secureHash` is essentially an anonymous word, whereas `%secureHash` is widely used as an alternative to external data file references. The semantics for `%secureHash` is to expand the binary as a list of bytes, much like embedded texts. Storage of resources is unspecified. We simply assume that Awelon systems have built-in or configurable knowledge about where to seek secure hashes - whether that be fileystem, database, web service, content delivery network, etc.. 

Awelon uses the 320-bit [BLAKE2b](https://blake2.net/) algorithm, encoding the hash using 64 characters in a [base32](https://en.wikipedia.org/wiki/Base32) alphabet.

        Base32 Alphabet: bcdfghjklmnpqrstBCDFGHJKLMNPQRST
            encoding 0..31 respectively

        Example hashes, chained from "test":

        rmqJNQQmpNmKlkRtsbjnjdmbLQdpKqNlndkNKKpnGDLkmtQLPNgBBQTRrJgjdhdl
        cctqFDRNPkprCkMhKbsTDnfqCFTfSHlTfhBMLHmhGkmgJkrBblNTtQhgkQGQbffF
        bKHFQfbHrdkGsLmGhGNqDBdfbPhnjJQjNmjmgHmMntStsNgtmdqmngNnNFllcrNb
        qLDGfKtQHhhTthNTDMMqDMDKnrCTpSSBHHBjDNtsKrTdNRGgtmtqQFTdGjsnfJDR

We can safely neglect the theoretical concern of secure hash collisions. If BLAKE2b is cracked in the future, we can address it then by transitively rewriting all secure hashes in our Awelon dictionaries. I won't further belabor the issue. 

*Security Note:* Secure hash resources may embed sensitive information, yet are not subject to conventional access control. Awelon systems should treat a secure hash as an [object capability](https://en.wikipedia.org/wiki/Object-capability_model) - a bearer token that grants read authority. Relevantly, Awelon systems should guard against timing attacks that might leak these secure hashes. Favor constant-time comparisons when using hashes as keys, for example.

## Labeled Data

Most modern programming languages have built-in support for labeled products and sums - aka records and variants. Labeled data is extensible, weakly commutative, and human meaningful. Awelon introduces label functions of form `:label` and `.label`. Each label must also be syntactically valid as a word. Effectively, a record works like this:

        [[A] :a [B] :b [C] :c] .c == [C] [[A] :a [B] :b]
        [A] :a [B] :b == [B] :b [A] :a      (labels commute)

Logically, we operate linearly on abstract row-polymorphic record constructors:

        :a      {R without a} [A] → {R, a=[A]}
        .a      S [{} → {R, a=[A]}] → S [A] [{} → {R}]

A record value `{a=[A],b=[B],c=[C]}` is abstract, never represented syntactically. But the record constructor `[[A]:a [B]:b [C]:c]` can effectively be treated as a record value. Like other functions, the record constructor is subject to ad-hoc composition, abstraction, and factoring. Unlike other functions, we can easily leverage commutativity of labels when refactoring.

For variants, the basic sum type `(A+B)` has a Church encoding `∀r.(A→r)→(B→r)→r`. That is, an observer must supply a "handler" for each case, and the value itself selects and applies one handler, dropping the others. For labeled sums, we simply need a labeled product of handlers - `[[OnA] :a [OnB] :b [OnC] :c ...]`. Variant values could then have concrete representation like `[.label [Value] case]` or `[[value] [.label] case]`, depending on how we define `case` and which option is more convenient for projections.

*Note:* Labeled data could be modeled in Awelon without a primitive feature, e.g. using a concrete trie for the abstract record value together with accelerators and projectional editing. However, primitive labels significantly simplify static analysis and debugging. 

## Annotations

Annotations in Awelon take the form of a parenthetical word, such as `(par)` or `(lazy)`. Annotations formally have identity semantics, but informally may hint an interpreter or compiler to optimize representations, influence evaluation order, trace debug outputs, or fail fast on assertions. The set of supported annotations depends on the runtime system and should be documented carefully and adhere to de-facto standards. 

Potential annotations:

* `(trace)` - record argument to a debug log
* `(error)` - prevent progress within computation
* `(par)` - evaluate argument in parallel, in background
* `(eval)` - evaluate argument before progressing further
* `(stow)` - move large values to disk, load on demand
* `(accel)` - assert software acceleration of a function
* `(optimize)` - rewrite function for efficient evaluation
* `(jit)` - compile a function for multiple future uses
* `(stat)` - assert a value is computed statically
* `(memo)` - memoize a computation for incremental computing
* `(nat)` - assert argument should be a natural number
* `(type)` - describe type of stack at given location
* `(quota)` - impose limits on argument evaluation effort

Some annotations such as `(par)` or `(jit)` are tags. They attach to a value:

        [A](tag) [B]b ==  [[A](tag) B]

Some annotations, such as `(type)` or `(quota)`, require an extra argument: 

        [Stack Descriptor](type)d
        [Quota Descriptor](quota)d

Awelon does not constrain annotations beyond requirement for identity semantics.

## Acceleration

Acceleration is a performance assumption for Awelon.

An obvious target for acceleration is natural numbers and arithmetic. We should be able to add or multiply two numbers in a small constant time, and represent big numbers in a small space. A more sophisticated example might involve representations for matrices and functions for linear algebra, enabling use of SIMD or GPGPU. An even more advanced variation might accelerate an "interpreter" function for a safe subset of OpenCL, to effectively embed that subset of OpenCL into the Awelon language.

Accelerators are essentially "built-in" functions with a reference implementation in Awelon. They extend the set of "performance primitives" for an Awelon interpreter or compiler. In general, accelerated functions should be recognized using: `[reference impl](accel)`. The annotation both documents our performance assumption and provides an opportunity for feedback to resist invisible performance rot when porting code or tweaking definitions.

Development and standardization of accelerators is a long term project and performance path for Awelon systems. Once we have enough of them, much Awelon code will be viewed as glue code between accelerators.

## Stowage

Stowage is a simple idea, summarized by rewrite rules:

        [large value](stow) => [$secureHash]
        [small value](stow) => [small value]
        [large binary](stow) => %secureHash

Stowage uses the *Secure Hash Resources* space to offload data from working memory. This actual offload effort would usually occur lazily, when space is needed. The data will be loaded again if necessary. Essentially, this gives us an immutable, persistent virtual memory model. What "large" means is heuristic, but should be simple to understand, predict, and reproduce. 

## Dictionary

Awelon words are defined in a codebase called a "dictionary". A dictionary is simply an association between words and Awelon encoded functions. However, for Awelon project's goals, we require a standard import/export representation that supports efficient update, sharing, snapshots, versioning, and diffs at scales of many gigabytes or terabytes.

The proposed representation:

        /prefix1 secureHash1
        /prefix2 secureHash2
        :symbol1 definition1
        :symbol2 definition2
        ~symbol3

A dictionary 'node' is a line-oriented ASCII text, representing an update log. Most lines will define or delete symbols (`:` or `~` respectively), but we may also index a prefix to a subtree (via `/`). Symbols usually correspond to Awelon words, and definitions to Awelon code. Internal nodes are identified by their secure hash, cf. *Secure Hash Resources*. Symbols for inner nodes are stripped of the matched prefix, hence `:poke` under `/p` becomes `:oke`. For lookup, only the last update for a given symbol or prefix is used. Hence, `/p` will mask all prior updates with prefix `p` such as `/prod` and `:poke`. We can normalize our dictionary nodes by erasing irrelevant updates and sorting whatever remains.

This representation combines characteristics of the LSM-tree, radix tree, and Merkle tree. It supports deeply immutable structure, structure sharing, lightweight version snapshots, lazy compaction, distributed storage, efficient diffs, and lightweight real-time working set updates. The empty prefix `/ secureHash` can be used to represent prototype inheritance, checkpoints, or resets. Like other LSM-trees, this does allow capture of multiple definitions for a symbol. But even that can be useful to optimize caching based on relative stability of definitions.

### Libraries and Modules 

For Awelon project, the intention is that we'll usually curate and share entire dictionaries - ensuring all definitions are versioned, managed, tested together. Instead of libraries, software distribution would be modeled via DVCS-inspired mechanisms - pull requests, bug reports, etc.. 

However, a dictionary can represent conventional libraries using subsets of words with a common prefix. For example, `/math- secureHash` patches in the specified version of a math library. If users insist, it would not be difficult to distribute software based on this. A projectional editor could support namespaces that hide the prefix for reading the code.

### Hierarchical Dictionary Structure

Several of Awelon's proposed [application models](ApplicationModel.md) rely on storing data into the dictionary. In this context, the dictionary serves as a filesystem or database with spreadsheet-like characteristics. But with multiple humans and software agents maintaining the data, we introduce several concerns related to name conflicts and information security for data flows. To simplify these issues, Awelon permits hierarchically embedding one dictionary within another. A dictionary is confined, unable to access its host. But the host can easily access embedded dictionaries through extended words of form `dictname/foo`. We can also interpret other Awelon operations under a hierarchical context:

        d/bar       (use `bar` from dictionary `d`)
        d/42        => d/[41 succ]
        d/[41 succ] => [d/41 d/succ]
        d/"hello"   => d/[104 "ello" cons]

In the dictionary representation, we simply define the extended symbols. For example, we can can write `:d/bar def` to update the definition for word `bar` in dictionary `d`. We can also use `/d/ secureHash` to logically embed or update an entire dictionary. 

Common functions and types will frequently be replicated between hierarchical dictionaries. The space overhead is mitigated by structure sharing. But writing out `d/42` is just ugly and inefficient if it has the same meaning as `42`. So we permit localization: an evaluator may rewrite a hierarchical qualifier whenever doing so does not affect behavior.

*Note:* It may be useful to encode a developer's primary dictionary under a prefix such as `d/word`. This enables embedding of metadata (such as timestamps or access control) via associated sibling dictionaries. 

## Evaluation

Evaluation of an Awelon program simply rewrites it to an equivalent program. An external agent will presumably extract data from the evaluated result, then potentially modify the program and continue. Awelon is a pure language, but interactions with external agents provides a basis for effects. 

Primitives rewrite by simple pattern matching:

            [B][A]a => A[B]         (apply)
            [B][A]b => [[B]A]       (bind)
               [A]c => [A][A]       (copy)
               [A]d =>              (drop)

Words rewrite into their evaluated definitions. If a word is undefined, it will not rewrite further. However, words will not rewrite unless doing so leads to further progress. There is no benefit in rewriting a word if it only leads to the inlined definition. This rule is called lazy linking. Lazy linking also ensures words denoting first-class values, such as `true = [a d]`, should be bound and moved directly, e.g. `true [] b => [true]`. 

Evaluation strategy is unspecified, and the default may be a heuristic mix of lazy, eager, and parallel. Awelon's primitives are confluent, therefore valid computations should reach the same result regardless of strategy. If evaluation halts early on a breakpoint or quota, we may expose evaluation strategy and other optimizations. Annotations may guide evaluation more explicitly, and may even influence the visible result.

### Arity Annotations

Arity annotations are very useful for Awelon, and have simple rewrite rules:

        [B][A](a2) == [B][A]
        [C][B][A](a3) == [C][B][A]
        ...

These annotations can be used to defer linking of words where a partial evaluation isn't useful. For example, consider a swap function `w = (a2) [] b a`. Ignoring the arity annotation, we'd rewrite `[A]w => [[A]]a`, which isn't useful progress. With the arity annotation, `[A]w` does not evaluate further, but `[B][A]w` evaluates to `[A][B]`. Arity annotations are also useful for modeling codata. For example, `[[A](a2)F]` has the observable behavior as `[[A]F]`, but the former defers computation until it the result is required. 

## Loops

Awelon definitions are acyclic, but we can express fixpoint combinators:

        [X][F]z == [X][[F]z]F
        z = [[(a3) c i] b (eq-z) [c] a b w i](a3) c i

        assuming:
            [def of foo](eq-foo) == [foo]
            [B][A]w == [A][B]       w = (a2) [] b a
               [A]i == A            i = [] w a d

This is the strict fixpoint combinator, which awaits one additional argument before evaluating. Fixpoint combinators are general but relatively painful to use directly. In practice, we'll want to develop specialized loop combinators covering the common conditional and foreach loops.

*Aside:* I intend for Awelon to evolve towards collection-oriented programming styles. Most loops are implicit in collections processing.

## Memoization

The primary basis for incremental computing in Awelon is [memoization](https://en.wikipedia.org/wiki/Memoization). Memoization may be implicit for definitions of words or secure hash resources, but requires annotations to work with anonymous data:

        [computation](memo) => [result]

Memoization involves seeking a representation of the computation in a separate table then directly replacing it by the result from that table. The exact mechanism may vary, and may involve partial traces to allow partial reuse between similar computations. Memoization may be probabilistic in nature, e.g. allowing for old entries to be expired.

For effective incremental computing, memoization must be carefully combined with cache-friendly patterns. Persistent data structures and stowage are useful for developing these patterns, insofar as they provide implicit boundaries for memoization.

## Optimization

There are many behavior-preserving rewrites that Awelon does not normally perform. For example:

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

A runtime has discretion to perform rewrites that don't affect the final evaluated program (i.e. same rendered result). Explicit annotations are required to permit visible optimizations.

*Note:* It is feasible to perform high level optimizations, such as rewriting `[F] map [G] map` to `[F G] map` or reordering matrix multiplications to minimize number of operations. Unfortunately, it's unclear how to express these optimizations or prove their validity, and such optimizations are fragile to abstraction. I would recommend modeling such optimizations explicitly, via intermediate data structure. 

## Compilation

Direct interpretation of Awelon code can be reasonably efficient. But for optimal performance, we'll need to compile Awelon to an intermediate language that uses an auxiliary call/return stack and registers for local dataflow. Additionally, static fixpoint loops could be compiled into the more conventional labeled jumps. Every accelerator can become a primitive operator for a compiled intermediate language.

## Error Reporting

We can represent errors by simply introducing an `(error)` annotation that acts as an undefined word, unable to be further rewritten. Then, we can define words such as `divide-by-zero = (error)` to create explicit, named errors that never rewrite further. Error values can be expressed as `[(error)]`. Errors in the top-level of an evaluated definition should be reported to programmers, except in the trivial case.

## Static Typing

Awelon doesn't depend on types. There is no type-driven dispatch or overloading. However, the language implies a simple static type model. If we can discover errors earlier by using static type analysis, that's a good thing. The stack-like environment can be typed as a tuple, and values as functions. Types for our primitive operations:

        a       ((s * x) * (s → s')) → (s' * x)
        b       ((s * x) * ((e * x) → e')) → (s * (e → e'))
        c       (s * x) → ((s * x) * x)
        d       (s * x) → s
        [F]     s → (s * type(F))
        :label  ({R/label} * x) → {R, label:x}
        .label  (s * ({} → {R, label:x})) → ((s * x) * ({} → {R}))

Type annotations can be expressed using Awelon annotations. We can use specific annotations such as `(nat)` or `(bool)` or `(t3)` for a 3-tuple. Or we can favor general annotations using `[Type Descriptor](type)d`. Suitable annotations will need to be standardized, eventually, based on what our type analysis tools will accept.

Unfortunately, simple static types are sometimes too simplistic and restrictive. For example, the `pick` function from Forth isn't amenable to static typing without sophisticated dependent types:

        [Vk]..[V1][V0] k pick == [Vk]..[V1][V0][Vk]

In this context, we could develop a series of functions like `pick2nd` and `pick3rd`, at cost of much boiler-plate. Or we could try to defer static typing until after we've specialized on the first parameter, treating `pick` as a macro. Intention to defer type checking can be indicated by annotation, e.g. adding a `(dyn)` comment to the subprogram with `[A](dyn) => [A]` behavior.

*Note:* Besides static types, termination analysis is also useful. As a purely functional language, non-termination or divergence is always an error for Awelon programs.

## Structural Equivalence

A structural equivalence assertion has surprising utility.

        [A][B](eq) => [A][B]     iff A,B, structurally equivalent

Using this, we can assert that two key-value structures share the same key comparison or hash functions, or the same first-class 'typeclass' model. We can perform lightweight unit testing. Although Awelon lacks nominative types, asserting structural equivalence for certain functions can serve a similar role.

## Editable Views

Awelon's simple syntax must be augmented by [projectional editing](http://martinfowler.com/bliki/ProjectionalEditing.html) techniques to support richer programming interfaces, DSLs, namespaces, application models, and larger programs. As a simple example, we could support a numeric tower:

        #42         == (Awelon's 42)
        42          == [#42 #0 integer]
        -7          == [#0 #7 integer]
        3.141       == [3141 -3 decimal]
        -0.0070     == [-70 -4 decimal]
        2.998e8     == [2998 5 decimal]
        -4/6        == [-4 #6 rational]

This builds one view upon another, which is convenient for extending views. If our view left out rational numbers, we'd still render a sensible `[-4 #6 rational]`. Besides numeric towers, editable views could easily support lists, continuation-passing style, Haskell-inspired do-notation, generators with yield, and other features. Line comments can easily be supported, e.g. `// comment == "comment"(a2)d`. Qualified namespaces are easy to support, e.g. such that `long-prefix-foo` can be abbreviated as `lp-foo`. It is also feasible for projections to leverage color, such that `html-div` vs. `math-div` both render as `div` but in different colors.

We can also project edit sessions that view and edit multiple words together. In simplest form, we might have `my-session = [foo][bar][baz]` so we can 'open' the session then edit those three words together.

Although our initial emphasis is plain text views, the eventual goal is to support richly interactive graphical views involving tables, graphs, canvases, checkboxes, sliders, drop-down menus, spreadsheets, and so on. A sophisticated projectional editor could support frames or a zoomable interface where a word's definition may be logically inlined/opened into the current view.

### Named Local Variables

We can leverage editable views to model named local variables, like lambdas or let expressions. For example, consider adapting Kitten programming language's syntax for local vars:

        7 -> X; EXPR            let-in equivalent
        [-> X; EXPR]            lambda equivalent

We can extract `X` from our expression by simple algorithm:

        EXPR == X T(X,EXPR) for value X

        T(X,E) | E does not contain X       => d E
        T(X,X)                              =>
        T(X,[E])                            => [T(X,E)] b
        T(X,F G)                            
            | only F contains X             => T(X,F) G
            | only G contains X             => [F] a T(X,G)
            | F and G contain X             => c [T(X,F)] a T(X,G)

For performance, we can optimize static conditionals to avoid copying:

        T(X,[F][T]if) => [T(X,F)][T(X,T)]if

It makes sense to record variable names as comments - that's how we use them.

        -> X; EXPR
            becomes
        "lambda X"(a2)d T(X,EXPR)

Named local variables hint at how to build higher level languages above Awelon.

## Arrays and In-place Updates

Awelon doesn't have an array data type, but we can use annotations and accelerators to insist on array representation where beneficial for lists. In-place mutation of arrays is also valuable for efficiency, avoiding unnecessary intermediate copies. We can support these in-place updates assuming two conditions:

* we hold the unique reference to the array
* we accelerate indexed access to the array

Awelon's explicit copy and drop makes it relatively easy to track dynamically whether we hold a unique reference to an array. We can leverage copy-on-write techniques if necessary, to obtain a unique reference upon the first update. Then further updates (until we copy the array to share it again) could update in place. This feature would enable Awelon systems to effectively work with arrays for many algorithms.

## Generic Programming in Awelon

A typical example of generic programming is that `add` should add two numbers of the same type, without local knowledge of the type involved. This might be achieved by Haskell type classes or Rust traits. Unfortunately, Awelon does not provide any built-in features for generic programming, so it will eventually need to be explicitly modeled.


