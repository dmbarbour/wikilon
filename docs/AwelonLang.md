
# Awelon Language

Awelon is a purely functional language based on concatenative combinators.

Specifically, Awelon has four primitive combinators:

        [B][A]a == A[B]         (apply)
        [B][A]b == [[B]A]       (bind)
           [A]c == [A][A]       (copy)
           [A]d ==              (drop)

Besides the four primitives, programmers develop a *Dictionary* within which each word is defined by an Awelon encoded function. For example, if we want `[A]i == A` then we can define `i = [[]] a a d`. Evaluation proceeds by rewriting according to the primitive combinators, or lazily substituting a word for its definition when doing so permits further progress. As a practical language, Awelon additionally has built-in support for encoding data: natural numbers, embedded texts, binary resources, and labeled structures. 

Those `[]` square brackets contain Awelon code and represent first-class functions. Values in Awelon are always first-class functions, typically using [Church encodings](https://en.wikipedia.org/wiki/Church_encoding) or [Scott encodings](https://en.wikipedia.org/wiki/Mogensen%E2%80%93Scott_encoding). However, effective Awelon compilers or interpreters will recognize and optimize common functions and value types. This is a concept of software *Accleration* to improve efficient use of CPU and memory. Acceleration for collections-oriented operations, such as matrix multiplication and linear algebra, can feasibly leverage SIMD instructions or GPGPU.

Compilers or interpreters will also recognize a set of annotations, represented by parenthetical words. For example, `[A](par)` might indicate parallel evaluation for the subprogram `A`, or `[F](accel)` might indicate that `F` should be recognized and accelerated. Annotations are represented by parenthetical words and have identity semantics - programs must have the same formal behavior even if annotations are ignored. However, external observers should be affected. Annotations serve valuable roles in debugging, guiding performance, and expressing programmer intentions.

By itself, Awelon is a very simplistic language - a purely functional assembly. 

The intention is to leverage Awelon together with [projectional editing](http://martinfowler.com/bliki/ProjectionalEditing.html) tools. We can project rich textual or even graphical programming interfaces above the simple Awelon code. Because Awelon evaluates by rewriting, we can rewrite the code and project a similar interface from the evaluation result. This can support a close relationship between a program source and its user interface, similar to a spreadsheet. Awelon project explores alternative [application models](ApplicationModel.md) that leverage the language's special characteristics, with goals for composition and sharing of data and applications between users.

## Words

Words are the user-definable unit for Awelon code. Structurally, a word has regular expression `[a-z][a-z_0-9]*`. That is, it consists of alphanumerics and the underscore, and must start with an alpha. There is no size limit, but words should be small in practice. The definition for a word must be a valid Awelon subprogram and be acyclic, i.e. having no direct or indirect dependency on itself.

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

Awelon words are defined in a codebase called a "dictionary". A dictionary is simply an association between words and Awelon encoded functions. However, for Awelon project's goals, we require a standard import/export representation that supports efficient update, sharing, and diffs at scales of many gigabytes or terabytes.

The proposed representation:

        /prefix1 secureHash1
        /prefix2 secureHash2
        :symbol1 definition1
        :symbol2 definition2
        ~symbol3

A dictionary 'node' is a line-oriented ASCII text, representing an update log. Most lines will define or delete symbols (`:` or `~` respectively), but we may also index a matched prefix to a subtree. Symbols usually correspond to Awelon words, and definitions to Awelon code. Internal nodes are identified by their secure hash, cf. *Secure Hash Resources*. Symbols for inner nodes are stripped of the matched prefix, hence `:poke` under `/p` becomes `:oke`. For lookup, only the last update for a given symbol or prefix is used. Hence, `/p` will mask prior updates such as `/prod` and `:poke`. We can normalize our dictionary nodes by erasing irrelevant updates and sorting whatever remains. The empty prefix or symbol is permitted.

This representation combines characteristics of the LSM-tree, radix tree, and Merkle tree. It supports deeply immutable structure, structure sharing, lightweight version snapshots, lazy compaction, scaling beyond local memory or disk, efficient diffs, and lightweight real-time working set updates. The empty prefix `/ secureHash` can be used to represent prototype inheritance, checkpoints, or resets. Like other LSM-trees, this does allow capture of multiple definitions for a symbol. But even that can be useful to optimize caching based on relative stability of definitions.

*Note:* Comments are not supported at the dictionary representation layer. They would be inaccessible and easily lost due to compaction operations. It is not difficult to cheat this by defining non-word symbols. But developers are encouraged instead to embed metadata in the dictionary in a more stable and accessible manner.

### Libraries and Modules 

For Awelon project, the intention is that we'll usually curate and share entire dictionaries - ensuring all definitions are versioned, managed, tested together. Instead of libraries, software distribution would be modeled via DVCS-inspired mechanisms - pull requests, bug reports, etc.. 

However, a dictionary can represent conventional libraries using subsets of words with a common prefix. For example, `/math_ secureHash` patches in the specified version of a math library. If users insist, it would not be difficult to distribute software based on this. A projectional editor could support namespaces that hide the prefix for reading the code.

### Hierarchical Dictionary Structure

Several of Awelon's proposed [application models](ApplicationModel.md) rely on storing data into the dictionary. In this context, the dictionary serves as a filesystem or database with spreadsheet-like characteristics. But with multiple humans and software agents maintaining the data, we introduce several concerns related to name conflicts and information security for data flows. To simplify these issues, Awelon permits hierarchically embedding one dictionary within another. A dictionary is confined, unable to access its host. But the host can easily access embedded dictionaries through extended words of form `dictname/foo`. We can also interpret other Awelon operations under a hierarchical context:

        d/bar       (use `bar` from dictionary `d`)
        d/42        => d/[41 succ]
        d/[41 succ] => [d/41 d/succ]
        d/"hello"   => d/[104 "ello" cons]

In the dictionary representation, we simply define the extended symbols. For example, we can can write `:d/bar def` to update the definition for word `bar` in dictionary `d`. We can also use `/d/ secureHash` to logically embed or update an entire dictionary. 

Common functions and types will frequently be replicated between hierarchical dictionaries. The space overhead is mitigated by structure sharing. But writing out `d/42` is just ugly and inefficient if it has the same meaning as `42`. So we permit localization: an evaluator may rewrite the hierarchical qualifier when doing so does not affect behavior.

*Note:* It may be useful to encode a developer's primary dictionary under a prefix such as `d/word`. This enables embedding of metadata (such as timestamps or version tracking) via auxiliary dictionaries. 

## Evaluation

Evaluation of an Awelon program simply rewrites it to an equivalent program. An external agent will presumably extract data from the evaluated result, then potentially modify the program and continue. Awelon is a pure language, but interactions with external agents provides a basis for effects. 

Primitives rewrite by simple pattern matching:

            [B][A]a => A[B]         (apply)
            [B][A]b => [[B]A]       (bind)
               [A]c => [A][A]       (copy)
               [A]d =>              (drop)

Words rewrite into their evaluated definitions. However, words will not rewrite unless doing so leads to further progress. There is no benefit in rewriting a word if it only leads to the inlined definition. This rule is called lazy linking. If a word is undefined, it will not rewrite further.

Awelon does not strongly specify evaluation strategy. Order of evaluation may be a heuristic mix of lazy, eager, parallel. Awelon's rules are confluent. Modul

If evaluation halts early, e.g. due to debug breakpoints or quota limitations, the rendered program state may even expose ad-hoc optimizations. However, 

modulo use of annotations, the result of evaluation will not vary based on the o

However, developers should be able to guide evaluation strategy by use of annotations.

## Value Words

A 'value word' is any word whose evaluated definition is a single block. Natural numbers would be a common example, but we might also include booleans like `true` or `false`, or references such as `story_chapter32`. Data binding (and data plumbing in general) can operate on value words directly:

        true [] b == [true]
        42 [] b   == [42]

Value words are implied by lazy linking. 

## Arity

Arity annotations have simple rewrite rules:

        [B][A](a2) == [B][A]
        [C][B][A](a3) == [C][B][A]
        ...

These annotations can be used to defer linking of words where a partial evaluation isn't useful. For example, consider a swap function `w = (a2) [] b a`. Ignoring the arity annotation, we'd rewrite `[A]w => [[A]]a`, which isn't useful progress. With the arity annotation, `[A]w` does not evaluate further, but `[B][A]w` evaluates to `[A][B]`. 

Arity annotations are also useful for modeling codata. For example, `[[A](a2)F]` has the observable behavior as `[[A]F]`, but the former defers evaluation. Whereas `(lazy)` values are evaluated if they're included in a program result, computations deferred by arity annotations are not evaluated until arity conditions are met - they're effectively [call-by-name](https://en.wikipedia.org/wiki/Evaluation_strategy#Call_by_name).

## Loops

Awelon does not permit cyclic definitions. We can define fixpoint combinators:

        [X][F]z == [X][[F]z]F
        z = [[(a3) c i] b (eq_z) [c] a b w i](a3) c i

        assuming:
            [def of foo](eq_foo) == [foo]
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

A runtime has discretion to perform rewrites that are invisible to the final evaluated result. These rewrites may be visible in case of an incomplete evaluation, e.g. when stopping on quota or debug breakpoints. Optimizations can feasibly save a lot of computation resources.

*Note:* It is feasible to perform high level optimizations, such as rewriting `[F] map [G] map` to `[F G] map` or reordering matrix multiplications to minimize number of operations. Unfortunately, it's generally unclear how to express these optimizations or prove their validity, and such optimizations are fragile to abstractions. I would recommend modeling such optimizations explicitly, constructing an intermediate program 'plan' then optimizing and compiling it into an Awelon function.

## Compilation

Direct interpretation of Awelon code can be reasonably efficient, using threaded interpreters. 

But to make it faster we can potentially rewrite code to an intermediate language that uses an auxiliary data/return stack, call-return operations, location labels and jumps for tail-call optimized loops and so on. Even better, we can feasibly target a simple "register machine" model to eliminate data plumbing from the stack and avoid bindings on the heap if they're used statically, instead tracking a compile-time stack of logical register-objects.

Of course, when compiling a register machine, you might need to keep an extra code map back from the program pointer to the logical register objects so we can serialize code. But that isn't a new idea.

## Error Reporting

We can represent errors by simply introducing an `(error)` annotation that cannot be directly removed by any rewrite rule. Hence, once `(error)` appears within a computation, that computation is stuck, and obviously so. Error values can be represented, such as `[(error)]`, and may be dropped or copied but not applied. This could be coupled with a comment like `["divide by zero"(error)]`. Errors in evaluation or static linking for a word should be raised to the attention of developers. We can also introduce a `(trace)` annotation to copy messages and values to a debug log.

## Static Typing

Awelon doesn't depend on types. There is no type-driven dispatch or overloading. However, the language implies a simple static type model founded on arity with a stack-like environment. If we can discover types earlier by using static type analysis, that's a good thing. Types for our primitive operations:

        a       ((s * b) * (s → s')) → (s' * b)
        b       ((s * b) * ((e * b) → e')) → (s * (e → e'))
        c       (s * a) → ((s * a) * a)
        d       (s * a) → s
        [F]     s → (s * type(F))
        :label  ({R/label} * a) → {R, label:a}
        .label  (s * ({} → {R, label:a})) → ((s * a) * ({} → {R}))

Type annotations, naturally, should be expressed by Awelon annotations. We could use specific annotations like `(nat)` or `(bool)`, or try for more general annotations using `[Type Descriptor](type)d`. However, we have not yet standardized any particular representation for type descriptors. Ultimately, it depends on what our type analysis tools will accept.

Unfortunately, static types are sometimes too simplistic and restrictive. For example, the `pick` function from Forth isn't amenable to static typing without some variant on dependent types:

        [Vk]..[V1][V0] k pick == [Vk]..[V1][V0][Vk]

In this context, we might wish to defer static typing of `pick` until after the specific `k` argument is provided. This might be expressed by annotation as well, such as tagging a `(dyn)` annotation to a block until it is bound or applied.

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

In this view, Awelon's natural numbers are given a `#` prefix, hence the view is optimized for signed integers. This particular view takes the path of building one view upon another. If the view left out rational numbers, we'd still render a sensible `[-4 #6 rational]`.

Besides numeric towers, editable views could easily support lists, continuation-passing style, generators with yield, and other features. Line comments can easily be supported, e.g. `// comment == "comment"(a2)d`. Qualified namespaces are easy to support, e.g. such that `long_prefix_foo` can be abbreviated as `lp_foo`. It is feasible to leverage color such that `html_div` vs. `math_div` both render as `div` but in different colors with a small legend.

We can also represent sessions to view and edit multiple words together.

        my_session = [foo][bar][baz]

        #my_session
        foo = def of foo
        bar = def of bar
        baz = def of baz

Although initial emphasis is plain text views, it is feasible to model richly interactive graphical views involving tables, graphs, canvases, checkboxes, sliders, drop-down menus, and so on. A sophisticated projectional editor could support frames or a zoomable interface where a word's definition may be logically inlined/opened into the current view.

### Named Local Variables

We can use editable views to model named local variables, like lambdas or let expressions. For example, consider adapting Kitten programming language's syntax for local vars:

        7 -> X; EXPR            let-in equivalent
        [-> X; EXPR]            lambda equivalent

When writing this view to Awelon, we must extract `X` from our expression. We can achieve this with a simple variable extraction algorithm:

        EXPR == X T(X,EXPR) for value X

        T(X,E) | E does not contain X       => d E
        T(X,X)                              =>
        T(X,[E])                            => [T(X,E)] b
        T(X,F G)                            
            | only F contains X             => T(X,F) G
            | only G contains X             => [F] a T(X,G)
            | F and G contain X             => c [T(X,F)] a T(X,G)

We also need a simple comment to record the variable names. 

        -> X; EXPR  
            is encoded as 
        "lambda X"(a2)d T(X,EXPR)

It makes sense to record variable names as comments, given that's very often how we use them. In any case, to read it back out we'd need to propagate the variable back into our program whenever we see the lambda comment. This feature is easily extended to multiple arguments, and feasibly optimized for conditional behaviors.

## Arrays and In-place Updates

We can support in-place updates for arrays assuming two conditions:

* we hold the unique reference to the array
* we accelerate indexed access to the array

The benefit of in-place updates is efficiency - less copying, less garbage.

Awelon's explicit copy and drop makes it relatively easy to track dynamically whether we hold the unique reference to an array. We can also use copy-on-write techniques to make it true, in case it was not. Then we can update the array in place. Instead of copying the array for every update, we copy a shared array once if necessary then edit in place until we explicitly copy the array value. Besides arrays, in-place updates might also be supported for records.

## Generic Programming in Awelon

Awelon does not have built-in support for generic programming. For example, we cannot add two numbers without either knowing their type or being handed the appropriate `add` function as an argument. But it might be worth exploring staged programming models that can propagate the necessary `add` function into an algorithm and specialize the code. 

