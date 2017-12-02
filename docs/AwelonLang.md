
# Awelon Language

Awelon is a purely functional language based on concatenative combinators.

## Why Another Language?

Modern programming languages can produce useful artifacts - applications, games, and so on. But they do not support flexible composition and decomposition of these artifacts. There are a lot of complicating factors: hidden runtime structure, entanglement of effects and environment, separation of programmer and user interfaces, even how software is packaged and shared in a community.

Awelon language aims to be simple and scalable, addressing many complications. It explores a new way of developing and sharing code and computational artifacts. Some design points:

* Awelon evaluates by local confluent rewriting, much like expression `(6 * 7)` evaluates to `42` in arithmetic. Every evaluation step has representation within Awelon language and may thus be serialized or rendered - i.e. it's easy to "show the work". Rewrite based evaluation simplifies debugging, distribution, integration of the program with HCI, and further use of program results.

* Awelon has a simple, Forth-like syntax. This lowers barriers for [projectional editing](http://martinfowler.com/bliki/ProjectionalEditing.html). Editable views can support domain-specific notations or allow interactive editing of Awelon via forms or graphs. 

* Awelon specifies 'lazy linking' of words by the evaluator. That is, a word is not rewritten to its definition unless doing so would result in further rewrites. This allows for results to contain human-meaningful references and code fragments. Importantly, editable views can be applied to results, which helps unify programmer and user interfaces.

* Awelon has a simple, pure, deterministic, portable semantics. It is easy to share code with others. Programs are concatenative, which simplifies composition and decomposition of code fragments. Definitions are acyclic, so it's easy to extract only necessary fragments of a dictionary.

* Awelon uses *Acceleration* as the performance alternative to built-in functions or FFI. This enables an interesting path of language growth that doesn't complicate semantics or security. Minimally, runtimes should accelerate natural numbers and arithmetic. But acceleration can potentially leverage GPGPUs (by accelerating linear algebra) or cloud computing (by accelerating Kahn process networks).

Awelon experiments with alternative [application models](ApplicationModel.md). The most promising models are RESTful - documents, publish subscribe, tuple spaces, etc. - integrating application state with the codebase instead of an external filesystem or database. The codebase then becomes a 'smart' filesystem or database, with more deeply integrated linking and computational structure similar to a spreadsheet. The codebase is maintained by a community of human and software agents, exactly as we'd maintain a database or filesystem.

Integration of Awelon systems with external systems will initially rely on web services or publish-subscribe systems. 

## Primitives

There are four primitive computing combinators:

            [B][A]a == A[B]         (apply)
            [B][A]b == [[B]A]       (bind)
               [A]c == [A][A]       (copy)
               [A]d ==              (drop)

Square brackets `[]` enclose Awelon code and represent first-class functions. This, together with various Church or Moegensen-Scott encodings, is the basis for representing data and computation in Awelon. Awelon computations are semantically pure, and their formal behavior can be understood in terms of these few primitives. However, Awelon also provides a few lightweight features for numbers, texts, words, big data, and provisionally for labeled data (records and variants).

This set of combinators is Turing complete, able to represent all deterministically computable functions. As a lightweight proof, see a translation from lambda calculus at *Named Local Variables* or see this simple definition of the Curry-Schönfinkel SKI [combinators](https://en.wikipedia.org/wiki/Combinatory_logic):

            [B][A]w == [A][B]       (swap)           w = [] b a
               [A]i == A            (inline)         i = [] w a d
         [C][B][A]s == [[C]B][C]A   (S combinator)   s = [[c] a b w] a i
            [B][A]k == A            (K combinator)   k = a d

Awelon systems should ideally perform static analysis for termination and type safety. Compared to lambda calculus or SKI combinators, Awelon's semantics are simpler: 

1. Local combinator rewriting is simpler than variable substitution. There is no need for environment models, lexical scopes, or variable capture hygiene for multi-stage metaprogramming.
1. A stack-like environment enables uniform abstraction for multiple arguments or results, and supports *Static Typing* based on consistent arities instead of atomic value types.
1. Explicit copy and drop operations simplify substructural type analysis, reference counting GC, and dynamic tracking of uniqueness to accelerate in-place updates for indexed data structures.

To achieve performance, we additionally leverage *Acceleration*. This is Awelon's alternative to intrinsic functions, built-in data-types, and performance-motivated FFI. Acceleration is discussed below. We can also leverage *Annotations*, to improve performance and support other non-functional requirements like debugging.

## Encoding

Awelon is encoded using a subset of ASCII, bytes 32..126. 

Newlines are among the rejected characters. However, line breaks may be transparently replaced by spaces where appropriate upon input of a program. Humans will usually manipulate Awelon code through *Editable Views* that may present a more sophisticated surface syntax, potentially including tables or graphical representations or *Named Local Variables* and infix notations. 

Arbitrary texts and binaries cannot be directly embedded into Awelon, but may be referenced indirectly as *Secure Hash Resources*, or even constructed at runtime by leveraging *Stowage*.

## Words

Words are the user-definable unit for Awelon code. Structurally, a word has regular expression `[a-z][a-z_0-9]*`. That is, it consists of alphanumerics and the underscore, and must start with an alpha. There is no size limit, but words should be small in practice. 

Words are evaluated in context of a *Dictionary*. Each word is defined by an Awelon program, using other words. Valid definitions must form a directed acyclic graph and be block balanced (i.e. no unmatched `[` or `]` block characters). The formal semantics for words are extremely trivial: we lazily substitute the word by its definition.

Structure within a word has no formal semantic properties. But there may be informal connotations, conventions, or special rendering hints in context of a development environment or [application model](ApplicationModel.md). For example, we might associate `foo_type` or `foo_doc` with `foo` in context of linking a development environment. Lightweight namespaces are also feasible. 

The four primitive words `a`, `b`, `c`, and `d` cannot be redefined. The four words `zero`, `succ`, `null`, and `cons` should be defined to support natural numbers, embedded texts, and binary resources.

*Aside:* A trivial cyclic definition of a word to itself is equivalent to an undefined word.

## Natural Numbers

Awelon has native support for natural numbers. Syntactically, numbers are represented by regex `0|[1-9][0-9]*` wherever a word may appear. 

        0 = [zero]
        1 = [0 succ]
        2 = [1 succ]
        3 = [2 succ]
        ...
        42 = [41 succ]
        (et cetera)

Definitions for `zero` and `succ` are left to the dictionary. A typical encoding might copy and apply a function N times to represent number N. Wrapping everything in blocks provides a simple structural guarantee that numbers may be treated as values regardless of definition. In practice, natural numbers must be defined based on *Acceleration*. With accelerated arithmetic, for example, we could add two numbers in log-time rather than linear time. Awelon does not support any other number types natively, but *Editable Views* can be leveraged to support numerical towers at the syntactic layer.

## Embedded Texts

Awelon has native support for embedding texts inline between double quotes such as `"Hello, world!"`. Embedded texts are limited to ASCII, specifically the subset valid in Awelon code (32-126) minus the double quote `"` (34). There are no escape characters. Semantically, a text represents a binary list.

        ""      = [null]
        "hello" = [104 "ello" cons]

Definitions for `null` and `cons` are left to the dictionary. A typical encoding might fold over every element in the list. Like natural numbers, we have a structural guarantee of value type, and definitions are likely to be determined by available runtime acceleration. In this case, acceleration could support lists via arrays.

Embedded texts are suitable for simple things like test data, comments, and micro-DSLs such as regular expressions. They are not suitable for general use. To work around these limitations, the primary options are:

* interpret, e.g. `"multiple\nlines" lit` to rewrite escapes
* structure, e.g. `["multiple" ["lines" [null] cons] cons] unlines`
* use *Secure Hash Resources* to reference external binary

## Labeled Data (experimental!)

Labeled data is convenient for human meaningful documentation, lightweight extensibility of data models, and commutative structure. Many modern programming languages have built-in support for labeled products and sums - aka records and variants. Awelon provisionally supports labeled data by introducing pairs of symbolic functions of form `:label` and `.label` where labels must be syntactically valid words. Effectively, a record works like this:

        [[A] :a [B] :b [C] :c] .c == [C] [[A] :a [B] :b]
        [A] :a [B] :b == [B] :b [A] :a      (labels commute)

Logically, we operate linearly on abstract row-polymorphic record constructors:

        :a      {R without a} [A] → {R, a=[A]}
        .a      S [{} → {R, a=[A]}] → S [A] [{} → {R}]

However, there is no curly brace syntax to introduce a record. Instead, the record constructor `[[A]:a [B]:b [C]:c]` is effectively the record value. Like other functions, the record constructor is subject to ad-hoc composition, abstraction, and factoring. Unlike other functions, we can easily leverage commutativity of labels when refactoring. 

For variants, first consider that basic sum type `(A+B)` has a Church encoding `∀r.(A→r)→(B→r)→r`. That is, an observer will supply a "handler" for each case, and the value itself selects and applies one handler, dropping the others. For labeled sums, we simply need a labeled product of handlers - `[[OnA] :a [OnB] :b [OnC] :c ...]`. Variant values could have concrete representation like `[.label [Value] case]`.

        [[OnA] :a [OnB] :b] .a [ValA] case == [ValA] OnA
        case = [] b b a a d

This model of labeled data adds no expressiveness to Awelon. In general, a record can be encoded as a trie, a label as a path. It is something we could model explicitly by leveraging accelerators and editable views - and doing so was my original intention. However, making labels primitive and records abstract better preserves the several benefits of labeled data. For example, we don't need to decode partial labels when reporting type or partial evaluation errors. We don't need to worry about dynamic construction of labels or ad-hoc composition of records. We essentially can reason about commutativity at a syntactic layer, without non-local knowledge of how labels are implemented. 

That said, I'm reluctant to introduce new language primitives. Before I remove "experimental" status, I'll need to see how well this feature works in practice, whether an alternative model would be more appropriate (perhaps lens-based), and also how nicely this labeled data model works in context of accelerators, especially accelerated evaluation for Kahn process networks involving labeled ports and processes.

## Secure Hash Resources

It is possible to identify binaries by their *secure hash*. Doing so has many nice properties: immutable and acyclic by construction, cacheable, securable, provider-independent, self-authenticating, implicitly shared, automatically named, uniformly sized references, and smaller than many full URLs or file paths. Awelon systems leverage secure hashes to reference binaries and code outside the dictionary:

* external binary data may be referenced via `%secureHash`
* code and structured data is referenced via `$secureHash`

Secure hash resources may appear anywhwere an Awelon word may appear. However, in source code, one should usually define a word to the secure hash simplify editing and human meaningful references. Like other definitions, resources are interpreted in context of a dictionary. For example, binaries use the same list encoding as embedded texts, relying on definitions of `zero`, `succ`, `null`, and `cons` - albeit permitting a full `0-255` range for each byte. 

Awelon uses the 320-bit [BLAKE2b](https://blake2.net/) algorithm, encoding the hash with 64 characters in a [base32](https://en.wikipedia.org/wiki/Base32) alphabet specialized to avoid conflicts with numbers or human meaningful words. 

Some example hashes, chained from the word `test`:

        rmqJNQQmpNmKlkRtsbjnjdmbLQdpKqNlndkNKKpnGDLkmtQLPNgBBQTRrJgjdhdl
        cctqFDRNPkprCkMhKbsTDnfqCFTfSHlTfhBMLHmhGkmgJkrBblNTtQhgkQGQbffF
        bKHFQfbHrdkGsLmGhGNqDBdfbPhnjJQjNmjmgHmMntStsNgtmdqmngNnNFllcrNb
        qLDGfKtQHhhTthNTDMMqDMDKnrCTpSSBHHBjDNtsKrTdNRGgtmtqQFTdGjsnfJDR

        Base32 Alphabet: bcdfghjklmnpqrstBCDFGHJKLMNPQRST
            encoding 0..31 respectively

We can safely neglect the theoretical concern of secure hash collisions. Even if hash collision becomes a concern in the future, it is feasible to transitively rewrite entire Awelon systems to use a more robust hash. In practice, physical corruption should be a greater concern. I won't further belabor the issue.

Awelon runtime systems must know where to seek secure hash resources, whether that be in a filesystem, database, web server, or content distribution network. Using *Stowage* annotations, Awelon runtime systems may also compute new secure hash resources during evaluation, treating this external space as a persistent virtual memory or a binary data server. 

Secure hash resources are generally subject to [garbage collection (GC)](https://en.wikipedia.org/wiki/Garbage_collection_%28computer_science%29). Conservative reference counting GC is simple and effective due to the acyclic and high-latency nature of secure hash resources.

*Security Note:* Secure hash resources may embed sensitive information, yet are not subject to conventional access control. Awelon systems should treat a secure hash as an [object capability](https://en.wikipedia.org/wiki/Object-capability_model) - a bearer token that grants read authority. Relevantly, Awelon systems should resist timing attacks that might leak secure hashes.

## Annotations

Annotations help developers control, optimize, view, and debug computations. Unlike words, which are mostly user-defined, annotations are given meaning by the runtime or compiler. Annotations are represented as parenthetical words like `(par)` or `(a3)`. Potential useful annotations:

* `(a2)..(a9)` - arity annotations to defer computations
* `(t1)..(t9)` - tuple assertions for output scope control
* `(unit)` - assert empty block
* `(nc) (nd)` - support substructural type safety
* `(seal_foo) (open_foo)` - lightweight symbolic types
* `(par)` - request parallel evaluation of computation
* `(eval)` - request immediate evaluation of computation
* `(lazy)` - delayed, shared evaluation across copies
* `(nat)` - assert argument should be a natural number
* `(optimize)` - rewrite a function for efficient evaluation
* `(jit)` - compile a function for efficient evaluation
* `(stow)` - move large values to disk, load on demand
* `(memo)` - memoize a computation for incremental computing
* `(trace)` - record value to a debug output log
* `(trash)` - erase data you won't observe, leave placeholder
* `(error)` - prevent further progress within computation
* `(assert)` - describe an environment at a given point
* `(accel)` - assert software acceleration of a function
* `(eq)`, `(eq_foo)` - assertions of structural equality
* `(now)`, `(later)` - declarations for multi-stage programming

Annotations have identity semantics: they have no observable effect within a computation. However, annotations do have observable effects outside the scope of the computation - generally impacting performance, active debugging, or static analysis. 

Annotations are defined by the Awelon runtime system. But over enough time we should develop de-facto standard annotations that are widely supported. And experimental or runtime-specific annotations should generally be indicated with appropriate prefix.

## Acceleration

Acceleration is a performance assumption for Awelon. 

A runtime will recognize and accelerate common functions. The accelerated implementation may be hand optimized and built into the runtime to achieve performance similar to a primitive. As trivial examples, runtimes might accelerate the following functions:

           [A]i == A            (inline)         i = [] w a d
        [B][A]w == [A][B]       (swap)           w = [] b a

The runtime will look at the given definitions. Upon recognizing `[] b a`, the runtime may link `w` to an acclerated swap implementation. Whenever `i` appears at the end of a subprogram, we might leverage the tail-call optimization.

In general, recognition of accelerators is fragile under refactoring and abstraction. For example, it may be that `i = [] w a d` is recognized where the logically equivalent `i = [[]] a a d` is not recognized. Even changing function names might break accelerators. Due to this fragility, developers need some means to gain confidence in accelerators. Performance mustn't silently rot due to independent development of the runtime. A simple convention is to support an `[code fragment](accel)` annotation that asserts the given fragment of code should be recognized and accelerated. Further, simple data plumbing accelerators like `i` and `w` or even fixpoint `z`, could be robustly recognized based on a static type analysis instead of specific definitions.

Acceleration naturally extends to data representation. Natural numbers, for example, have a unary structure `42 = [41 succ]`. But with runtime acceleration, natural numbers may be compactly represented by machine words and arithmetic on math might be accelerated to add and multiply these words directly. Similarly, accelerated records could use hashtables or be compiled to structs. And lists could be accelerated to use arrays. And evaluation of Kahn Process Network descriptions could be accelerated to leverage distributed CPUs and memory.

Accelerators are not trivial. An accelerator represents a significant compatibility investment, and dependency on accelerators will impact portability of code to other runtimes. It is best to develop a small set of accelerators that is useful for a wide variety of use cases. Development of carefully curated accelerators will be a long-term performance path for Awelon systems.

## Stowage

Stowage is a simple idea, summarized by rewrite rules:

        [large value](stow) => [$secureHash]
        [small value](stow) => [small value]
        [large binary](stow) => %secureHash

Stowage allows secure hash resources to be constructed at runtime and removed from working memory until later required. Essentially, this gives us a functional, persistent virtual memory model. Further, these hashes can be used efficiently together with memoization as a basis for incremental computing for very large systems.

What "large value" means is heuristic, but should be reproducible. If configured, it should be configurable from the dictionary, perhaps by defining `stow_threshold`. It might also be useful to support a few different size thresholds, enabling use of `(stow_small)` vs. `(stow_large)`.

## Dictionary

Awelon words are defined in a dictionary. Evaluation of Awelon code occurs in context of an immutable dictionary. Definitions for words must form a directed acyclic graph. Assigning a trivial cycle such as `foo = foo` may be treated as equivalent to deleting word `foo` from the dictionary.

Awelon doesn't specify a dictionary representation. I hope for de-facto standards to arise around the import, export, sharing, and backup of dictionaries, and potentially dictionaries as first-class values. Dictionaries are usually manipulated through services, such as a web application or a [FUSE](https://en.wikipedia.org/wiki/Filesystem_in_Userspace) filesystem adapter. The use of such layers is also necessary to provide useful *Editable Views*.

As an interesting point, it is feasible to leverage `(eq_foo)` to implicitly define a dictionary from within an Awelon program. 

        [definition of foo](eq_foo) => [foo]

This could be understood as an implied single-assignment of `foo = definition of foo`. Hence, it is feasible to represent dictionaries directly as Awelon programs, albeit in a rather second-class manner.

See also *Hierarchical Dictionaries*.

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

Annotations can greatly affect evaluation strategy. As two examples, `[A](eval)` could enforce evaluation of the value `[A]` before erasing `(eval)`, while `[A](lazy)` could replace "evaluate before copy" with a call-by-need strategy for a specific value.

## Value Words

A 'value word' is any word whose evaluated definition is a single block. Natural numbers would be a common example, but we might also include booleans like `true` or `false`, or references such as `story_chapter32`. Lazy linking should ensure that value words are not linked before necessary. Thus, data plumbing can operate on value words directly:

        42 true w == true 42
        42 [] b   == [42]

Value words are convenient for preserving human-meaningful structure and support for hypermedia resources. 

*Aside:* Value words might be considered the 'nouns' of Awelon language, whereas most words are verbs. However, an analogy to natural language quickly breaks down. Awelon lacks an equivalent to adjectives, adverbs, or anaphora, at least without a lot of development work involving staged programming and *Editable Views*.

## Arity

The *arity annotations* `(a2)` to `(a9)` have simple rewrite rules:

        [B][A](a2) == [B][A]
        [C][B][A](a3) == [C][B][A]
        ...

To clarify, it is the *annotation* that has the given arity. Arity annotations specify nothing of their context.

Arity annotations serve a valuable role in controlling computation. For example, the program `[[A](a2)F]` has the same type and semantics as `[[A]F]`, but the former prevents `F` from observing `[A]` until more data is available. Arity annotations can be used to guard against wasteful partial evaluations and premature linking. It can also represent a [call-by-name](https://en.wikipedia.org/wiki/Evaluation_strategy#Call_by_name) evaluation strategy.

## Loops

Modulo termination analysis, we can use a general fixpoint combinator:

        [X][F]z == [X][[F]z]F
        z = [[(a3) c i] b (eq_z) [c] a b w i](a3) c i

A fixpoint combinator is very expressive and can be used to construct any other loop. Unfortunately, this expressiveness hinders termination analysis. To simplify termination analysis, we might favor data-driven loop models. For example, the Church-encoded natural number copies and applies a function many times, effectively folding over every element. Short circuiting is also possible, if explicitly included in the type, for example:

        Nat     :  s * (s → s) → s
        Nat'    :  (s * a) * ((s * a) → (s * (a + r))) → (s * (a + r))

The latter encoding for supports early 'return' with type `r`. Further variants could include remainder data together with the return value. 

## Memoization

The primary basis for incremental computing in Awelon is [memoization](https://en.wikipedia.org/wiki/Memoization). 

The essential idea is to record a computation and its result, and then to lookup the result rather than recompute it. In context of Awelon systems, word definitions and `$secureHash` references are natural targets for memoization because the computation has already been recorded and the overhead for lookup of the result is much smaller. But we can support flexible and precise access to runtime memoization through annotations:

        [computation](memo)

The main challenge for effective memoization is precise lookup. Irrelevant differences between very similar dictionaries should not interfere with this lookup. Ideally, even comments and basic data plumbing can be excluded. Hence, our lookup might operate on a partially optimized representation of the computation that potentially knows about types and arities. However, even imprecise implementations of memoization can be effective and useful. The sophistication of memoization is left to the runtime.

To support incremental computing, memoization must be combined with cache-friendly computing patterns and data structures. Persistent data structures leveraging stowage - finger trees, tries - are especially useful. For example, if we memoize a computed monoidal value for each node in a persistent tree, computing a slightly different tree could easily reuse most of the memoization effort.

## Static Linking

Consider a redirect, `foo = bar`. When we link `foo`, we'll always link `bar`. This is due to the lazy evaluation rule: we won't link `foo` unless doing so also results in rewrites other than a trivial inlining of foo's definition. Hence, we could skip the intermediate rewrite to word `bar` and simply rewrite to bar's definition. Transitively, we can say foo's static link definition is bar's static link definition.

This isn't limited to redirects. Similar analysis can be performed in context of arity annotations and type analysis. It is feasible to optimize static link definitions far beyond simple evaluation. Annotations could make static link optimizations explicit and visible in the program.

## Optimization

There are many semantically valid rewrites that Awelon's naive evaluator does not perform. For example:

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

A runtime has discretion to perform optimizations that are invisible in the evaluated result, e.g. for *Static Linking*. Visible optimizations are permitted only when guided by annotation. 

Simple pattern-matching optimizations tend to be fragile, affected heavily by abstraction and associativity. Fortunately, there are many robust optimization techniques with good results. For example, we can propagate 'variables' through an evaluation then extract said variables using an algorithm like the one described at *Named Local Variables* - doing so would normalize data plumbing, penetrate arity barriers, and enable partial evaluations wherever data is partially known.

Development of useful optimizations will be a major area for Awelon's development, even before we compile which may expose new optimizations within the intermediate language.

## Compilation

Direct interpretation of Awelon code can be reasonably efficient, using threaded interpreters. 

But to make it faster we can potentially rewrite code to an intermediate language that uses an auxiliary data/return stack, call-return operations, location labels and jumps for tail-call optimized loops and so on. Even better, we can feasibly target a simple "register machine" model to eliminate data plumbing from the stack and avoid bindings on the heap if they're used statically, instead tracking a compile-time stack of logical register-objects.

Of course, when compiling a register machine, you might need to keep an extra code map back from the program pointer to the logical register objects so we can serialize code. But that isn't a new idea.

## Parallelism

A simple form of fork-join parallelism can be expressed as `[computation](par)`. The block could be moved around while computing in parallel, but upon observation (via apply) we'll need to wait on the result. This covers a lot of use cases, and it's also relatively easy to implement.

For more sophisticated forms of parallelism, we'll need *Acceleration*.

For example, to leverage SIMD, SSE, or GPGPU we'll probably need to accelerate a [linear algebra](https://en.wikipedia.org/wiki/Linear_algebra), or alternatively accelerate evaluation for a DSL that represents a safe subset of OpenCL. 

Similarly, to leverage cloud or mesh network computing with decentralized communications, we could accelerate evaluation for [Kahn process networks (KPNs)](https://en.wikipedia.org/wiki/Kahn_process_networks). A weakness of conventional KPNs is that they cannot merge asynchronous data from multiple channels. But it is possible to model temporal, reactive KPNs where 'time' or clock 'tick' messages are implicitly added to every channel. These could then be compiled into conventional KPNs.

Accelerators aren't trivial, but a couple accelerators could cover the vast majority of parallel evaluation for a wide variety of problems and domains. 

*Aside:* KPNs are also an interesting alternative to monadic effects for modeling purely functional applications.

## Error Annotations

We can represent errors by simply introducing an `(error)` annotation that cannot be directly removed by any rewrite rule. Hence, once `(error)` appears within a computation, that computation is obviously stuck. Error values can be represented, such as `[(error)]`.  can be dropped or copied but not applied. This can be coupled with a comment like `["divide by zero"(error)]`. Errors in evaluation or static linking for a word should be raised to the attention of developers.

## Assertions

Assertions are a simple, convenient, and conventional debugging feature. For many languages, the big difference between a 'debug' and 'release' build is whether assertions are checked. Assertions can also be leveraged by static analysis tools, hinting at types or representing contracts. In Awelon, we can use annotations to express annotations:

        [code we could inline](assert)d

An `(assert)` passes if the given function would evaluate inline without error. But we don't actually inline this code, instead we drop it with the following `d` operation. Based on code within the assertion, we can infer a description of the stack for static analysis, or we can perform dynamic sampling of assertions by copying the stack.

## Garbage Data

In some cases, it might be convenient to erase data that we know we shouldn't use in the future to recover memory resources, yet leave a placeholder and preserve other annotations (such as `(nd)` for relevant data). We could represent this pattern by use of a `(trash)` annotation:

        [A](trash)      => [](error)
        [A](nd)(trash)  => [](nd)(error)

We drop data but preserve substructure. Because the data has been lost, the resulting value is marked erroneous. Memory required for `A` may then be recycled. This is essentially a form of manual memory management.

## Active Debugging

Awelon's program rewrite semantics make it relatively easy to observe a computation in progress. Data is visible in the program representation, rather than hidden behind variables that must be queried via debugger. And of course, more conventional debugging applies. Some things we can easily do:

* set breakpoints for linking of specific words
* animate evaluation via frame capture on breakpoints
* evaluate in small steps, animate evaluation generally
* `(trace)` annotation for console/log style debugging
* log inputs to specific words (specified arity)
* dynamic checking of assertions 

Active debugging should be configurable through the runtime's interpeter or compiler. To achieve reasonable performance when debugging, we might consider sampling policies - i.e. instead of breaking on a word every time, or logging inputs to a word every time, we could sample every hundredth or thousandth time, perhaps with pseudo-random variation.

## Scope

Scopes are all about control and comprehension of computation. A few forms:

### Spatial Scope

Blocks provide a simple foundation for modeling regions and spatial scopes within a computation. For example, we know that in `[B][[A]foo]`, `foo` can access `[A]` but not `[B]`. And we can trivially control scopes using bind operation `b`. But by itself this is incomplete; we also need to control outputs, and understand the impact of observing a function upon our environment or 'stack'. 

To control output, we can generally use annotations to make assertions about common data types. Such annotations can be validated statically in many cases, and dynamically in many more. Annotations asserting tuple structure are simple and convenient:

        [[A]](t1) == [[A]]
        [[B][A]](t2) == [[B][A]]
        ...

Between blocks and tuple annotations, we scope both inputs and outputs to a computation.

### Temporal Scope

For performance reasons, it isn't unusual that we want to control *when* a computation occurs. The simplest distinction is between static (when evaluating definitions) versus dynamic. More generically, multi-stage programming involves specializing functions based on a curried subset of stable inputs.

I suggest a simple basis for temporal scope inspired from temporal logic:

        [A](now) == [A]     if A doesn't contain (now)
        [F](later)          hides F from (now)

We may need to partially evaluate `A` to eliminate exposed `(now)` annotations before we proceed. For a valid program, after partial evaluation, `(now)` may appear only within blocks tagged `(later)`. For definitions, `(now)` essentially marks computations that must complete statically. Otherwise we must report appropriate errors to our programmers.

### Symbolic Scope

[Abstract data types (ADTs)](https://en.wikipedia.org/wiki/Abstract_data_type) are convenient to control coupling and simplify maintenance issues within a large codebase. To express ADTs in Awelon, we need specialized annotations. Consider pairs of annotations that cancel when adjacent:

        (seal_foo)(open_foo) ==
        (seal_bar)(open_bar) ==
        ...

A sealed value could be expressed as `[[value](seal_foo)]`. An accessor could then use `i(open_foo)`. These annotations would do a lot to resist accidental access to data, and we could simply search a codebase for the annotations to determine which functions access certain data structures. Further, a linter could raise an error when `(seal_foo)` or `(unseal_foo)` appears in source code outside of words with prefix `foo_`. This would allow us to treat `foo_` or any `prefix_` as an implicit module or package that can hide some implementation details from other parts of the codebase.

### Substructural Scope

[Substructural types](https://en.wikipedia.org/wiki/Substructural_type_system) allow us to reason about protocols and life cycles, whether a value is used and how many times. Especially in combination with abstract data types or effects models. As usual, we can introduce a few annotations to help out:

* `(nc)` - mark a value non-copyable, aka 'affine'
* `(nd)` - mark a value non-droppable, aka 'relevant'
* inherit substructure of bound values (op `b`).

        [A](nc) [B] b == [[A](nc) B](nc)

An affine value models a limited resource while a relevant value models a responsibility. If we're responsible for explicitly disposing of a resource, then it should be both affine and relevant, which corresponds to the 'linear' type.

Because copy and drop are explicit in Awelon, it isn't difficult to check substructural properties dynamically. However, it's still undesirable overhead. Ideally we should validate substructural types statically.

## Static Typing

Awelon can be evaluated without static typing. There is no type driven dispatch or overloading. But if we can detect errors early by static analysis, that is a good thing. Further, static types are also useful for verifiable documentation, interactive editing (recommending relevant words based on type context), and performance of JIT compiled code. Strong static type analysis makes a *very* nice default.

We can represent our primitive types using a tuple for the stack:

        a   : ((s * b) * (s → s')) → (s' * b)
        b   : ((s * b) * ((e * b) → e')) → (s * (e → e'))
        c   : (s * a) → ((s * a) * a)
        d   : (s * a) → s
        [F] : s → (s * type(F))

Here `s` is a universally typed argument for the input stack. Because our types are always structured this way in Awelon, we can simplify syntax and match type descriptions to program structure:

        a   : S [B] [S → S'] → S' [B]
        b   : S [B] [E B → E'] → S [E → E']
        c   : S [A] → S [A] [A]
        d   : S [A] → S
        [F] : S → S [type(F)]

The type `S[C][B][A]` then clearly aligns with a program of the same shape.

Label functions can be typed based on row-polymorphic records:

        :a  : {R without a} [A] → {R, a:[A]}
        .a  : S [{} → {R, a:[A]}] → S [A] [{} → {R}]

Conditional behavior requires special attention. In Awelon, we use Church-encodings. For example, the basic sum type `(A+B)` is encoded as a function of type `∀r.(A→r)→(B→r)→r`. It is difficult to infer locally that our sum must be parametric in `r` and have similar input arities. We can leverage a few 'typed identity' annotations to address this.

        (choice) : S [S → S'] [S → S']
        (option) : S [S → S'] [S V → S']
        (either) : S [S A → S'] [S B → S']
        (switch) : S [{} → {a:[S A → S'], b:[S B → S', ...}]

Annotations can also augment static type analysis with hints, value sealers, substructural types, multi-stage programming, and so on. 

### Staged and Deferred Typing

Consider the `pick` function from Forth, which copies an element from the stack at a depth based on a given natural number. Although `pick` isn't very interesting, it can be taken as an example of metaprogramming.

        [Vk]..[V1][V0] k pick == [Vk]..[V1][V0][Vk]

A static type for `pick` would require a sophisticated, dependent type system. However, for `3 pick` it is feasible to compute a simple static type. Trivially, we could define `pick` such that `3 pick` statically expands into a program `[] b b b w c [w a] a`, where the number of sequential `b` operations is based on the specific value `3`. We need only to defer type inference until after this expansion.

Delay of type analysis until after performing some useful evaluations should not be unfamiliar to users of preprocessors, macro systems, or template metaprogramming in conventional languages. It's simple, powerful, and convenient. For Awelon, we don't need a separate layer or feature for partial evaluations. But having some means to inform our type analysis system of our intentions is quite useful. I propose two annotations:

        [F](dyn) == [F]     
        [F](stat) == [F]    iff F does not contain (dyn)

A definition of `pick` could have the form `[pick body here]b(dyn)i`. The `(dyn)` annotation simply tells our static type analysis tools to suppress complaints due to incomplete type inference. Recognized type errors should still be reported. Meanwhile, `(stat)` would enable us to scope use of dynamic structure and enforce clean boundaries or staging between macros and normal functions.

### Termination Analysis

Awelon is Turing complete, so we cannot always decide whether a program terminates. However, Awelon is also purely functional, and any non-termination is a form of error. As much as possible, by default, Awelon systems should use static analysis to detect non-termination among other errors.

*Note:* Even with a guarantee of termination, it's trivial to express computations that would require extraordinary time and space resources. We still need quotas. The main benefit of termination analysis is to eliminate accidental sources of error, and to require extra annotations where reasoning is difficult.

### Structural Equivalence

A structural equivalence assertion has a surprising amount of utility.

        [A][B](eq) == [A][B]     iff A,B, structurally equivalent

Using this, we can assert that two key-value structures share the same key comparison or hash functions. We can perform lightweight unit testing. And we can effectively assert shared origins for values if taken together with symbolically scoped value sealers. Although Awelon lacks nominative types, asserting shared origins can serve a similar role.

### Sophisticated Types

For the more sophisticated types - such as GADTs, signatures, existential types, dependent types - we cannot rely on inference or simple annotations. We need a type description language with flexible abstraction and factoring. A viable option:

        [type description of stack](type)d

Awelon doesn't specify a type description language, but we're free to use strings or data structures. Static analysis tools should recognize de-facto standards by ad-hoc means, such as a version comment within the type description. 

*Aside:* Type descriptions related to words can generally be moved outside of mainline code, into auxiliary definitions. But it's better to use annotations even within these auxiliary definitions, to avoid relying on naming conventions. E.g. `foo_type = [[foo][type description](type)]`. 

## Hierarchical Dictionaries

Awelon reserves character `@` for symbolic context, primarily for hierarchical dictionaries. The intention is to support [application models](ApplicationModel.md) that share dictionaries through messaging or publish-subscribe systems, dictionaries as documents or spreadsheets or databases, explicitly versioned dictionaries, and other coarse grained applications. Hierarchical dictionaries are unsuitable for use as namespaces or modules.

Words of form `foo@dict` refer to definition of `foo` in context of `dict`. Blocks also may be qualified with context: `[foo bar]@d` is equivalent to `[foo@d bar@d]`. Similarly, `42@d` is `[41 succ]@d` is `[41@d succ@d]`, and `"hello"@d` is `[104 "ello" cons]@d`, and `$secureHash@d` must interpret the secure hash resource in context of `d`. Even annotations may be usefully qualified, and we'll move the `@` character to the word within the parentheses. For example, `[X](eq_z@d)` is an assertion that `[X]` is equivalent to `[z@d]`.

Symbolic context is monotonic and second-class. Hierarchical dictionaries permit the parent to reference the child, the child dictionary cannot reference the parent. There is no stepping back, no `..` path, and every dictionary must be fully self-contained. The context for any fragment of Awelon code can be determined locally. No space is permitted to separate a word or block from its `@dict` qualifier. The symbol `dict` must also be a valid word.

Deep hierarchical contexts are possible and are left-associative, e.g. `foo@xy@zzy` is `foo@xy` in context of `zzy`. However, by Law of Demeter, this should rarely appear in source code. A development environment should raise a warning unless it is suppressed for a dictionary.

*Note:* Although Awelon doesn't specify representation for dictionaries, I strongly recommend that hierarchical dictionaries leverage persistent data structures and secure hash resources. This enables a great deal of structure sharing, which is valuable because child dictionaries will have many definitions in common with each other and the parent.

### Localization

As a special optimization, an evaluator can erase insignificant hierarchical qualifiers. For example, if `42@d` means the same thing as `42` due to equivalent definitions for `zero` and `succ`, then we may freely rewrite `42@d` to `42`. This helps mitigate the inability for child dictionaries to reference the parent.

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

Beyond numbers, we can develop editable views that are convenient for continuation-passing style, generators, or monadic computation. Editable views can feasibly support records and labeled data. Support for tables and graphs are feasible. Conveniently, it isn't difficult to experiment with editable views, tuning them to a user or project or developing them over time, because the views aren't the primary representation for Awelon code.

Most editable views should have an escape for code that they isn't recognized. For example, the `#42` escape for natural numbers might be generalized as an escape for words `#foo` or even full blocks `#[raw awelon code]`. This ensures universal access to Awelon behavior and the dictionaries. Of course, some DSL-inspired views might cleverly constrain subprograms to a subset of Awelon to better support acceleration.

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

An intriguing opportunity for editable views is support for local variables, like lambdas and let expressions. This would ameliorate use cases where point-free programming is pointlessly onerous, such as working with algebraic math expressions. It also supports a more conventional programming style where desired. 

Consider a syntactic sugar for let or lambda assignment of form:

        -> X Y Z; EXPR

This should pluck three values off the stack, giving local names `X`, `Y`, and `Z` within `EXPR`, and produce equivalent code without the variables. I'll preserve stack order, such that `Z` would be the top item on the stack. Hence, the following is true: 

        -> X Y Z; == -> Z; -> Y; -> X;

Translation may then occur one variable at a time.

        EXPR == X T(X,EXPR)     for any value X

        T(X,E) | E does not contain X       => d E
        T(X,X)                              =>
        T(X,[E])                            => [T(X,E)] b
        T(X,F G)                            
            | only F contains X             => T(X,F) G
            | only G contains X             => [F] a T(X,G)
            | F and G contain X             => c [T(X,F)] a T(X,G)

That much is sufficient, avoids unnecessary copies, and conveniently uses only Awelon primitive words. But assuming certain words, we could further optimize conditional behaviors to eliminate unnecessary copying and composition of variables into conditional paths:

        T(X, [onT][onF]if)                  => [T(X,onT)][T(X,onF)]if

Similarly, it is feasible to optimize splitting of tuples. Anyhow, this gives us a convenient translation from a lambda calculus and simplifies data plumbing and working with closures. 

Local variable names are not semantically meaningful, but are often human meaningful. Essentially, they're comments. An editable view might recognize comments of form `"lambda X"(a2)d` and rewrite to `-> X;` and inject variables into the following code appropriately, reversing `T(X,E)`.

        -> X; EXPR == "lambda X"(a2)d T(X,EXPR)

Assuming local variables, it's also feasible to develop infix notations, e.g. `((X + Y) * X)`. Thus we can achieve conveniences of conventional languages where doing so is useful, but keep it at the editable view without complicating the semantics.

### Qualified Namespaces

Awelon's hierarchical dictionaries support a simple form of namespacing. But it falls to editable views to support local shorthand, e.g. some form of `using large_prefix as x` or `using package_of_nicknames`. If we assume editable views are maintained within the dictionary, it is feasible to use comments to help control the view, tweaking the language as needed. An intriguing possibility is to integrate a database of nicknames for secure hash resources into the view, where said database is represented within the dictionary.

## Arrays and In-place Updates

In-place updates are efficient - they can avoid overheads of persistent data structures related to indirection, allocation, and garbage collection, and they benefit from stronger cache locality. Although usually associated with imperative languages, purely functional languages can safely support in-place updates if they track sharing. For example, we can update an array in-place if that array is not shared; nobody will observe the mutation. Even if the array is shared, we can simply produce an unshared copy then update that - and this will be efficient if updates are batched or a lot more frequent than copies. 

In Awelon, copying is explicit so it is not difficult to track sharing. And although we don't have arrays, a runtime can accelerate lists to use arrays under the hood. 

## Generic Programming

A common example of generic programming is that an `add` function should have appropriate meaning based on the arguments given to it - e.g. natural numbers, rational numbers, or vectors. This requires some model for overloading. My preferences lean towards solutions like Haskell type classes. Consider the Haskell `add`:

        add :: (Num a) => a -> a -> a

Informally, we can interpret this from a perspective of multi-stage programming. The first argument is a value of type `Num a` (a typeclass instance), and the arrow `=>` suggests we receive this argument in an earlier stage. Haskell makes *implicit* this propagation of typeful metadata, treating the set of typeclasses as a global database. But it's entirely feasible to manage a type context explicitly.

Anyhow, Awelon has a story for multi-stage programming via `(now)` and `(later)` (see *Temporal Scope*). But to complete the generic programming story we also need a model for typeful metadata and a convenient model to manage and propagate this context in advance of our normal arguments. Such developments are beyond the scope of this document, but should be an interesting subject for the future.

