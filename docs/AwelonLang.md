
# Awelon Language

Awelon is a purely functional language based on concatenative combinators.

Specifically, we use four primitive combinators:

        [B][A]a == A[B]         (apply)
        [B][A]b == [[B]A]       (bind)
           [A]c == [A][A]       (copy)
           [A]d ==              (drop)

In Awelon, all first-class values are functions, and first-class functions may be represented by code enclosed in square brackets. But there is syntactic sugar for natural numbers and texts. Evaluation proceeds by rewriting, but an interpreter or compiler should further optimize representations and processing for common types, such as arithmetic on natural numbers. Besides these primitives, programmers may define words. Awelon language also supports annotations, which provide performance or debugging hints to a compiler or interpreter without affecting observable results within a computation.

Awelon is a very simplistic language. Arguably, it's simpler even than a pure lambda calculus: there is no need to manage scoped symbolic environments, copy and drop are explicit which simplifies reference counting, and we can easily abstract over multiple parameters or results. Arity offers a more convenient foundation than atomic value types for static type safety analysis.

The intention is to leverage Awelon together with [projectional editing](http://martinfowler.com/bliki/ProjectionalEditing.html) tools. In this context, Awelon language is a purely functional assembly. Programmers can observe and manipulate it directly, but we can also project rich textual or even graphical programming interfaces above it. Because we evaluate by rewriting, it is feasible to also render the outputs of evaluation in the same format. Hence, we can model projections that evaluate in-place. Further, words in the source or output can provide a rich, human-meaningful reference structure suitable for hypertext.

Conventional application models introduce barriers that hinder composition, decomposition, sharing, and reuse of both computations and data. Awelon project aims to lower these barriers, make data and computations more accessible and sharable behind lightweight projections. This requires careful attention in both language and [application models](ApplicationModel.md).

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

Definitions for `zero` and `succ` are left to the dictionary. However, in practice, the definitions will be standardized and runtime-approved to simplify *Acceleration* for natural numbers and arithmetic. Awelon does not support other number types, but *Editable Views* can feasibly be leveraged to support numerical towers at the syntactic layer. 

## Embedded Texts

Awelon has limited native support for embedding texts inline between double quotes such as `"Hello, world!"`. Embedded texts are limited to ASCII, specifically the subset valid in Awelon code (32-126) minus the double quote `"` (34). There are no escape characters, and there is no Unicode support. Semantically, a text represents an ASCII binary list.

        ""      = [null]
        "hello" = [104 "ello" cons]

This limited support for texts is intended for unit test data, lightweight DSLs (such as regular expressions), lookup keys, rendering hints, and similar features. More general, sophisticated text should instead be represented using binary *Secure Hash Resources* (see below) or structured data models.

Definitions for `null` and `cons`, and hence the basic type for a list, are left to the dictionary. In practice, however, the dictionary definitions will be determined based on available runtime accelerations. In this case, acceleration may support lists using arrays under the hood.

Embedded texts are suitable for simple things like test data, comments, and micro-DSLs such as regular expressions. They are not suitable for general use. To work around these limitations, the primary options are:

* interpret, e.g. `"multiple\nlines" lit` to rewrite escapes
* structure, e.g. `["multiple" ["lines" [null] cons] cons] unlines`
* use *Secure Hash Resources* to reference external binary

## Labeled Data (experimental!)

Labeled data is convenient for human meaningful documentation, lightweight extensibility of data models, and commutative structure. Most modern programming languages have built-in support for labeled products and sums - aka records and variants. Awelon provisionally supports labeled data by introducing pairs of symbolic functions of form `:label` and `.label`. Modulo the prefix character, each label must be syntactically valid as a word. Effectively, a record works like this:

        [[A] :a [B] :b [C] :c] .c == [C] [[A] :a [B] :b]
        [A] :a [B] :b == [B] :b [A] :a      (labels commute)

Logically, we operate linearly on abstract row-polymorphic record constructors:

        :a      {R without a} [A] → {R, a=[A]}
        .a      S [{} → {R, a=[A]}] → S [A] [{} → {R}]

A record value `{a=[A],b=[B],c=[C]}` is abstract, never represented syntactically. But the record constructor `[[A]:a [B]:b [C]:c]` can effectively be treated as a record value. Like other functions, the record constructor is subject to ad-hoc composition, abstraction, and factoring. Unlike other functions, we can easily leverage commutativity of labels when refactoring.

For variants, consider that basic sum type `(A+B)` has a Church encoding `∀r.(A→r)→(B→r)→r`. That is, an observer will supply a "handler" for each case, and the value itself selects and applies one handler, dropping the others. For labeled sums, we simply need a labeled product of handlers - `[[OnA] :a [OnB] :b [OnC] :c ...]`. Variant values could have concrete representation like `[.label [Value] case]`.

        [[OnA] :a [OnB] :b] .a [ValA] case == [ValA] OnA
        case = w d w i

This model of labeled data adds no expressiveness to Awelon. A record could be encoded as a trie, a label as a path. This feature could be implemented via careful use of accelerators and editable views. Doing so was my original intention! However, making the labels primitive and records abstract better enforces structure and preserves benefits for labeled data, and simplifies static error analysis and reporting.

That said, I'm reluctant to introduce labels as language primitives. I want to see how well the feature works in practice, and how nicely this data model works in context of accelerators and optimizations, before accepting it as a built-in feature of Awelon. For now, it should be considered an experimental extension.

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

Annotations help developers control, optimize, view, and debug computations. Unlike words, which are mostly user-defined, annotations are given meaning by the runtime system. Annotations are represented as parenthetical words like `(par)` or `(a3)`. Potential useful annotations:

* `(a2)..(a9)` - arity annotations to defer computations
* `(t1)..(t9)` - tuple assertions for output scope control
* `(unit)` - assert empty block
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

A runtime should optimize representations for common data types and functions. The accelerated implementation may be hand optimized and built into the runtime to achieve performance similar to a primitive. As a representative example, a runtime might optimize the representation for natural numbers and basic arithmetic functions, such that they're much more efficient than a naive implementation. A more sophisticated example might involve representations for matrices and functions for linear algebra, enabling use of the GPGPU. Even more sophisticated would be accelerating an "interpreter" for a safe subset of OpenCL, which would effectively embed that subset of OpenCL into the Awelon language.

As trivial examples, runtimes might accelerate the following functions:

           [A]i == A            (inline)         i = [] w a d
        [B][A]w == [A][B]       (swap)           w = [] b a

The runtime will look at the given definitions. Upon recognizing `[] b a`, the runtime may link `w` to an acclerated swap implementation. Whenever `i` appears at the end of a subprogram, we might leverage the tail-call optimization.

Unfortunately, recognition of accelerators is fragile under refactoring and abstraction. For example, it may be that `i = [] w a d` is recognized where the logically equivalent `i = [[]] a a d` is not recognized. We might even tie recognition of accelerators to specific words. Performance should not silently rot due to independent developments of the runtime. A simple convention is to support an annotation like `[code fragment](accel)` that asserts the given fragment of code should be recognized and accelerated, enabling fast failure if the accelerator is ever deprecated.

Accelerators in Awelon replace the conventional use of built-in functions and some uses of FFI (where FFI is motivated for performance only). But development, maintenance, and standardization of accelerators is non-trivial. This is a long-term development path for Awelon systems.

## Stowage

Stowage is a simple idea, summarized by rewrite rules:

        [large value](stow) => [$secureHash]
        [small value](stow) => [small value]
        [large binary](stow) => %secureHash

Stowage allows secure hash resources to be constructed at runtime and removed from working memory until later required. Essentially, this gives us a functional, persistent, garbage collected virtual memory model. Further, these hashes can be used efficiently together with memoization as a basis for incremental computing involving small updates to large, persistent data structures.

What "large value" means is heuristic. But it should be simple to predict, understand, and deterministically reproduce. It may be configurable from a dictionary, perhaps by defining a word like `stow_threshold` to a number of bytes. It may also prove useful to support a few stowage options with variable sizes, such that we can use `(stow_small)` vs. `(stow_large)`.

## Dictionary

Awelon words are defined in a codebase called a "dictionary". 

Proposed representation:

        /prefix1 secureHash1
        /prefix2 secureHash2
        :symbol1 definition1
        :symbol2 definition2
        ~symbol3

A dictionary 'node' is a line-oriented ASCII text, representing an update log. Most lines will define or delete symbols (`:` or `~` respectively), but we may also index a matched prefix to a subtree. Tree nodes are named by secure hash, cf. *Secure Hash Resources*. We strip the matched prefix from symbols in the subtree, hence `:poke` under `/p` becomes `:oke`. For lookup, only the last update for a given symbol or prefix is used. Hence, `/p` will mask prior updates such as `/prod` and `:poke`. We can normalize our dictionary nodes by erasing masked updates and sorting whatever remains. The empty prefix or symbol is permitted.

This representation has characteristics of the LSM-tree, Merkle tree, and radix tree. It supports deeply immutable structure, structure sharing, lazy compaction, scaling beyond local memory or disk, efficient diffs, and lightweight real-time working set updates. The empty prefix `/ secureHash` can model prototype inheritance, stream checkpoints, or resets. By examining past definitions, we can heuristically estimate stability of definitions to optimize caching. 

*Note:* We can represent conventional libraries as a subset of words with a common prefix, e.g. such that `/math_ secureHash` patches in a specific version of a math library. However, for Awelon, my intention is that we'll usually curate and share entire codebases - ensuring all words are versioned, managed, tested together. Instead of libraries, distribution could be modeled via DVCS-inspired mechanisms - pull requests, bug reports, etc..

## Hierarchical Dictionary Structure

Several of Awelon's proposed [application models](ApplicationModel.md) rely on storing data into the dictionary. In this context, the dictionary serves as a filesystem or database with spreadsheet-like characteristics. But with multiple humans and software agents maintaining the data, we introduce several concerns related to name conflicts and information security for data flows. To simplify these issues, Awelon permits hierarchically embedding one dictionary within another. The subordinate dictionary is sandboxed, unable to access its host. But the host can access its subordinates through extended words of form `dictname/foo`. We can also interpret other operations under a hierarchical context:

        d/bar       (use `bar` from dictionary `d`)
        d/42        => d/[41 succ]
        d/[41 succ] => [d/41 d/succ]
        d/"hello"   => d/[104 "ello"]

In the dictionary representation, we simply define the extended symbols. For example, we can can write `:d/bar def` to update the definition for word `bar` in dictionary `d`. We can also use `/d/ secureHash` to logically embed or update an entire dictionary. Of course, writing out `d/42` is only ugly and inefficient if it has the same meaning as `42`. So we permit localizing: an evaluator may rewrite the hierarchical qualifier whenever it does not affect behavior.

## Evaluation

Evaluation of an Awelon program simply rewrites it to an equivalent program, in context of a dictionary. An external agent can presumably observe the result and extract data from the evaluated form, perhaps extend the program and continue. Awelon is a pure language, but any interaction with an external agent provides a potential basis for an effects model. 

Primitives rewrite by simple pattern matching:

            [B][A]a => A[B]         (apply)
            [B][A]b => [[B]A]       (bind)
               [A]c => [A][A]       (copy)
               [A]d =>              (drop)

Words will rewrite to their evaluated definitions. However, words should not rewrite unless doing so leads to further rewrites in the program. That is, if rewriting a word just results in the inlined definition, then we've lost some useful, human-meaningful structure but gained nothing. We'll only rewrite words if we stand to gain something. This constraint is called lazy linking, and it supports various performance and aesthetic goals.

Evaluation can proceed with undefined words. Undefined word represent unknown computations and do not evaluate further. This can be leveraged for some application models, where we can use undefined words to represent future inputs.

Awelon's basic evaluation strategy is simple:

* rewrite outer program
* evaluate before copy 
* evaluate final values

Evaluating the outer program before values gives us the greatest opportunity to drop values or annotate them with memoization or other features. Evaluation before copy resists introduction of rework without introducing need for stateful memoization, and it covers the common case. Final values are reduced because we assume the program as a whole might be copied for use in many locations.

Annotations can greatly affect evaluation strategy. As two examples, `[A](eval)` could enforce evaluation of the value `[A]` before erasing `(eval)`, while `[A](lazy)` could replace "evaluate before copy" with a call-by-need strategy for a specific value. In the latter case, we can extract logical copies using a technique similar to *Named Local Variables* upon render.

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

The primary basis for incremental computing in Awelon is [memoization](https://en.wikipedia.org/wiki/Memoization). Memoization may be implicit for definitions of words or secure hash resources, but requires annotations to work with anonymous data:

        [computation](memo) => [result]

Effectively, memoization involves seeking a representation of the computation in a separate table then replacing it by the result from that table. The exact mechanism might be different, perhaps recording traces of instrumented computations such that traces can be partially reused. There are no strong guarantees: memoization may be probabilistic, table entries may be concurrently expired, and so on. Fortunately, even if memoization works only part of the time, it can result in significant savings for recursive computations on persistent data structures.

To support incremental computing, memoization must be combined with cache-friendly data structures and update patterns. Persistent data structures leveraging stowage - such as finger trees and tries - are especially useful. For example, if we compute a monoidal result for each node in a persistent tree, computing a slightly different tree could reuse most of the memoization effort while use of stowage would reduce costs for comparing data.

## Static Linking

Consider a redirect, `foo = bar`. When we link `foo`, we'll always link `bar`. This is due to the lazy evaluation rule: we won't link `foo` unless doing so also results in rewrites other than a trivial inlining of foo's definition. Hence, we could skip the intermediate rewrite to word `bar` and simply rewrite to bar's definition. Transitively, we can say foo's static link definition is bar's static link definition.

This isn't limited to redirects. Similar analysis can be performed in context of arity annotations and type analysis. It is feasible to optimize static link definitions far beyond simple evaluation. A `[program](link)` annotation might expose runtime link optimizations.

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

In some cases, it might be convenient to erase data that we know we shouldn't use in the future to recover memory resources, yet preserve a placeholder and substructural type information (such as `(nd)` for relevant data). We could represent this pattern by use of a `(trash)` annotation that erases data and replaces it with an error value:

        [A](trash)      => [(error)]
        [A](nd)(trash)  => [(error)](nd)

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

Between blocks and tuple annotations, we scope both inputs and outputs to a computation. But it might be more convenient to also have annotations or types to simply assert how many arguments a block function may observe.

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

### Quota Control

Evaluation of programs requires resources, notably memory and CPU time. Normally, these resources aren't specific to evaluation for any subprogram. However, in practice it is convenient to control evaluation efforts for specific subprograms or at least quickly discover and debug resource errors. For Awelon, it is feasible to annotate a subprogram - a block - with quotas for its evaluation. For example, consider: 

        [Expression][500 :mem 1000 :cpu](quota)d

This should indicate that we should evaluate the given expression within 500 memory units and 1000 cpu time units, perhaps corresponding to kilobytes and milliseconds. After we're done annotating the quota, we can simply drop it. If evaluation halts on quota, a runtime can serialize the partially evaluated expression together with the remaining quota.

*Note:* I haven't deeply contemplated the details for quota control annotations, so the above is just an initial concept. It must be tested and tuned in practice. Additionally, a runtime might control ad-hoc resources such as stowage and memoization.

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

Conditional behavior requires special attention. In Awelon, we use Church-encodings. For example, the basic sum type `(A+B)` is encoded as a function of type `∀r.(A→r)→(B→r)→r`. It is difficult to infer locally that two handler arguments must match the observed type `r`, but we can use annotations to provide hints for type safety analysis:

        (choice) : E [S → S'] [S → S']
        (option) : E [S → S'] [S [V] → S']
        (either) : E [S [A] → S'] [S [B] → S']
        (switch) : E [{} → {a:[S [A] → S'], b:[S [B] → S', ...}]

Annotations can generally augment static type analysis with hints, value sealers, substructural types, multi-stage programming, and so on. 

### Staged and Deferred Typing

Consider the `pick` function from Forth, which copies an element from the stack at a depth based on a given natural number. 

        [Vk]..[V1][V0] k pick == [Vk]..[V1][V0][Vk]

Although `pick` isn't very interesting, it is difficult to provide a simple static type. We could feasibly leverage dependent types, but that would not be simple. However, for `3 pick` it is feasible to compute a simple static type. Trivially, we could define `pick` such that `3 pick` statically expands into a program `[] b b b w c [w a] a`, where the number of sequential `b` operations is based on the specific value `3`. We need only to defer type analysis until after expansion. 

Delay of type analysis until after partial evaluation should be familiar to users of preprocessors, macro systems, or template metaprogramming in more conventional programming languages. The feature is simple, powerful, and convenient. Due to Awelon's local rewriting semantics, we don't require special support for partial evaluations. But we do need some means to describe our intentions and provide feedback in the form of fast failure. I propose two annotations:

        [F](dyn) == [F]     
        [F](stat) == [F]    iff F does not contain (dyn)

A definition of `pick` could have the form `[pick body here]b(dyn)i`. The `(dyn)` annotation simply tells our static type analysis tools to suppress complaints due to incomplete type inference. Obvious type errors should still be reported. Meanwhile, `(stat)` would enable us to scope use of dynamic behavior. For example, this would allow us to suppress use of macros at runtime for a given subprogram.

*Aside:* There are similarities to the `(now)` and `(later)` annotations proposed for multi-stage programming. However, there is no implicit constraint against dynamic behavior, and a significant volume of a dictionary may have dynamic behavior if developers are not careful to scope its use.

### Termination Analysis

Awelon is Turing complete, so we cannot always decide whether a program terminates. However, Awelon is also purely functional, and any non-termination is a form of error. As much as possible, by default, Awelon systems should use static analysis to detect non-termination among other errors.

*Note:* Even for programs that are guaranteed to terminate, they may have exponential costs. Quota control is typically necessary in practice.

### Structural Equivalence

A structural equivalence assertion has a surprising amount of utility.

        [A][B](eq) == [A][B]     iff A,B, structurally equivalent

Using this, we can assert that two key-value structures share the same key comparison or hash functions. We can perform lightweight unit testing. And we can effectively assert shared origins for values if taken together with symbolically scoped value sealers. Although Awelon lacks nominative types, asserting shared origins can serve a similar role.

### Sophisticated Types

For the more sophisticated types - such as GADTs, signatures, existential types, dependent types - we cannot rely on inference or simple annotations. We need a type description language with flexible abstraction and factoring. A viable option:

        [description of type assumptions for stack](type)d

Awelon doesn't specify a type description language, but we're free to use strings or data structures. Static analysis tools should recognize de-facto standards by ad-hoc means, such as a version comment within the type description. 

*Aside:* Type descriptions related to words can generally be moved outside of mainline code, into auxiliary definitions. But it's useful to favor annotations even within type descriptions, to avoid relying entirely on naming conventions. E.g. `foo_type = [[foo][type description](type)]`. 

### Substructural Types

[Substructural types](https://en.wikipedia.org/wiki/Substructural_type_system) constrain against copy and drop operations. They can enable us to reason about protocols and life cycles, and also help enforce optimizations such as in-place updates. 

Use of substructural types has a significant impact on how we express generic algorithms for data structures. For example, the conventional approach to a tree lookup is to logically copy the tree then drop various branches until we reach the desired element. But with linear types, we might instead approach this by [unzipping the tree](https://en.wikipedia.org/wiki/Zipper_%28data_structure%29) to access the target element, then zip it back together when done.

We can feasibly tag individual values with substructural constraints:

* `[A](nc)` - mark value as non-copyable
* `[A](nd)` - mark value as non-droppable
* these attributes are inherited upon bind

        [B](nc)[A]b == [[B](nc)A](nc)

Unfortunately, use of `(nc)` and `(nd)` does not generalize nicely. These substructural constraints are not scoped to a subprogram. Scoping is essential for use-cases where we only wish to constrain a model internally. General use of substructural constraints should rely on sophisticated `(type)` declarations and static analysis.

## Editable Views

Awelon is designed to use a simple technique to support richer programming styles, DSLs, and larger programs: Awelon shifts the rich syntax burden to [projectional editing](http://martinfowler.com/bliki/ProjectionalEditing.html), preferably in the form of *editable views*. By *editable views* I mean to emphasize purely functional, bidirectional, RESTful rewrites between serializable representations.

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

Most editable views should provide an escape for raw Awelon code. For example, the `#42` escape for natural numbers might be generalized to an escape for arbitrary words `#foo` or even full blocks `#[raw awelon code]`. This ensures universal access to Awelon behavior and the dictionaries. Of course, some DSL-inspired views might cleverly constrain subprograms to a subset of Awelon to better enforce performance or correctness.

Ideally, editable views for values should be in normal form, i.e. such that `[3143 -3 decimal]` does not evaluate further and can be constructed by a normal evaluation. This allows us to reuse our views on the evaluated code, and hence rewrite one view into another. Design of views is consequently sensitive to arity annotations and accelerators. Similarly, comments should also be computable, for example:

        // comment  ==  "comment"(a2)d

The arity annotation delays deletion of the comment, and enables us to construct new comments as part of evaluation. Of course, we aren't limited to plain text comments, and we might use comments as rendering hints - for example, see *Named Local Variables* below.

Editable views effectively treat Awelon language as a functional macro-assembly. The view compiles code to Awelon, or decompiles Awelon into code of the view language. The main difference from conventional languages above a bytecode is that the Awelon code is treated as the canonical source, and we're always forced to 'decompile' said code into our language of editing. The indirection provided by the decompiler simplifies issues surrounding whitespace formatting, backwards compatibility, experimental extensions, or modeling preferences per project or user.

Although initial emphasis is plain text views, it is feasible to model richly interactive graphical views involving tables, graphs, canvases, checkboxes, sliders, drop-down menus, and so on. A sophisticated projectional editor could support frames or a zoomable interface where a word's definition may be logically inlined into the current view.

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

### Session Views

It is not difficult for *Editable Views* to operate on multiple definitions as a set. For example, we might take all the definitions prefixed by `foo_` as a set and edit them together as one large text file. Intriguingly, we can also support spreadsheet-like editable views if we assume a set of tabular definitions following a simple convention such as `foo_a1` and `foo_b3`. A few such ideas are discussed as [application models](ApplicationModel.md) for Awelon.

Session views can be made first-class by "defining" session words. Consider:

        my_session = [foo][bar][baz]

Here our session view might involve editing the words `foo` `bar` and `baz`. Additionally, we would edit the definition of `my_session` if we open or close words while editing. A more sophisticated session model might additionally support namespaces and other rendering hints, task lists, recent history, composition, and so on.

### Namespaces

Shorthand for large names is convenient for reading and writing code. One common mechanism for this shorthand is "namespaces". Conventionally, namespaces hide a common prefix for large, fully qualified names. Namespaces ultimately a user experience concern, a natural fit for editable views and projectional editing. 

Support for qualified namespaces is very simple and unambiguous. For example, we comment `import large_prefix as x` then we subsequently resolve `x_foo` to `large_prefix_foo`. But we can also support ambiguous namespaces, e.g. if we `import foo` and `import bar` then a symbol `xyzzy` refers ambiguously to word `foo_xyzzy` or `bar_xyzzy` or plain old `xyzzy`.

In context of editable views, ambiguities must be resolved at edit time. The code saved into the dictionary will use fully qualified names. Resolution of ambiguity can be achieved heuristically (search for defined words, match on types) or interactively (e.g. via code completion, intellisense). Conversely, a development environment could render an ambiguous word using a color or subscript to reduce ambiguity for the human reader. For environments where such mechanisms aren't available, it seems wiser to favor qualified namespaces.

*Aside:* Namespaces are best managed separately from function definitons, e.g. at the *Session View* layer. This separation helps amortize overheads, reduce clutter, and avoid entanglement between definitions and development environment. Namespaces could feasibly be customized per user or project. This observation might generalize to many other rendering hints.

## Arrays and In-place Updates

In-place updates are efficient - they can avoid overheads of persistent data structures related to indirection, allocation, and garbage collection, and they benefit from stronger cache locality. Although usually associated with imperative languages, purely functional languages can safely support in-place updates if they track sharing. For example, we can update an array in-place if that array is not shared; nobody will observe the mutation. Even if the array is shared, we can simply produce an unshared copy then update that - and this will be efficient if updates are batched or a lot more frequent than copies. 

In Awelon, copying is explicit so it is not difficult to track sharing. And although we don't have arrays, a runtime can accelerate lists to use arrays under the hood. 

## Generic Programming

A common example of generic programming is that an `add` function should have appropriate meaning based on the arguments given to it - e.g. natural numbers, rational numbers, or vectors. This requires some model for overloading. My preferences lean towards solutions like Haskell type classes. Consider the Haskell `add`:

        add :: (Num a) => a -> a -> a

Informally, we can interpret this from a perspective of multi-stage programming. The first argument is a value of type `Num a` (a typeclass instance), and the arrow `=>` suggests we receive this argument in an earlier stage. Haskell makes *implicit* this propagation of typeful metadata, treating the set of typeclasses as a global database. But it's entirely feasible to manage a type context explicitly.

Anyhow, Awelon has a story for multi-stage programming via `(now)` and `(later)` (see *Temporal Scope*). But to complete the generic programming story we also need a model for typeful metadata and a convenient model to manage and propagate this context in advance of our normal arguments. Such developments are beyond the scope of this document, but should be an interesting subject for the future.

