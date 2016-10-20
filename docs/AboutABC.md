# Awelon Bytecode

Awelon Bytecode (ABC) provides a computational medium in which ideas, data, and actions can be precisely expressed, shared, and composed. This document describes ABC's idioms and design. For a short summary, see [ABC](ABC.md). 

## Why a New Bytecode?

Awelon project has a vision for human-computer interaction that places users within the environment rather than above or apart from it, where all HCI is modeled as software development and live programming. This vision includes non-conventional [application models](ApplicationModel.md) where humans and software agents operate upon a codebase in a shared environment, and a [linked evaluation model](AboutAO.md) that preserves ad-hoc structure meaningful to humans and software agents.

Most bytecodes aren't designed with such applications in mind.

What sets ABC apart?

ABC is evaluated by pure, local, confluent rewriting. Importantly, ABC does not become entangled with its environment during evaluation. There are no jumps or pointers, no external memory or stack, no FFI, no side-effects. Effects are achieved in multi-agent systems via Awelon's non-conventional [application layer](ApplicationModel.md). An ABC subprogram will simply evaluate into another representation of the same ABC subprogram, much like `6 * 7` evaluates in conventional arithmetic to `42`.

ABC is simple and extensible. There are only four primitive operations `abcd` - apply, bind, copy, drop. ABC programs can be composed by simple concatenation, linked by simple inlining. Extensions are primarily via the [AO link layer](AboutAO.md): any user-defined function whose name is a single UTF-8 codepoint may be used as a bytecode. Extensions will be accelerated for primitive performance by runtimes that recognize them, leading to de-facto standardization of popular ABC extensions.

ABC is serializable and weakly legible. ABC strings are encoded in UTF-8 (mostly the ASCII subset), avoiding control characters, and may be rendered in normal text editors. Data can be embedded within ABC via extensions like `#42` to construct natural numbers plus a little syntactic sugar for texts, and these can also be used in evaluation output when known to a runtime. 

ABC is also designed for use with editable views

ABC is easily parallelized, persisted, and cached. This is an emergent consequence of its evaluation model and serializability. We can serialize an ABC computation, ship it to a remote processor for evaluation, look it up in a database to see if the result is known, or store it to disk to resume later. ABC also leverages serializability for its *value stowage* feature, an simple and robust variation on virtual memory to work with larger than memory tree-structured data.

ABC is easily observed, debugged, and manipulated. This is an emergent consequence of its simple composition, locality of evaluation, and legibility: it is easy to take ABC programs, join them together, and watch them flow together or expand during evaluation like an active substance. With the AO link layer and caching, we can further model remote interactions - spreadsheet-like incremental computing with hypermedia objects. This is important to Awelon project's vision of HCI, modeling computation as a continuous and pervasive but accessible part of the user environment.

ABC is amenable to static analysis, type checking. However, evaluation does not depend on types, and is safe for unchecked evaluation. Because we can defer type checking until after evaluation, we have a lot of implicit support for staged metaprogramming. We can also share bytecode without requiring trust between agents.

## The Bytecode

ABC has only four primitive combinators `abcd`.

        [B][A]a == A[B]         (apply)
        [B][A]b == [[B]A]       (bind)
           [A]c == [A][A]       (copy)
           [A]d ==              (drop)

In addition to these four combinators, we have the `[]` block structure. A primitive ABC program consists of a stream of `[abcd]`. The blocks must be properly balanced, i.e. every `[` has a matching `]` and vice versa. 

Non-primitive ABC program must be reducible to a primitive ABC program by expanding data representations with their Church encodings, inlining linker tokens, eliminating other tokens. Doing so would hurt performance and hinder debugging, but would not affect the observable results. 

The potential reduction to just four primitives makes ABC relatively easy to comprehend. There aren't any surprises, no corner cases to handle. And these primitives are *useful* even for high level code, easy to understand and to implement efficiently.

### Tokens

Tokens have form `{foo}`, a short text wrapped in curly braces that support symbolic extension of ABC. Syntactically, token texts must be valid UTF-8 excluding curly braces, control characters (C0, DEL, C1), and the replacement character. Semantically, tokens are limited to purely functional behavior via local rewriting. Use of tokens falls primarily in two classes:

* tokens with *identity* semantics for performance, debugging
* tokens with *linking* semantics for structured software

Tokens with identity semantics include seals `{:db}{.db}`, gates `{@foo}`, and annotations `{&par}`. Seals provide lightweight symbolic type wrappers, which is convenient for fail-fast behavior and potentially for debugger rendering. Gates are used for active debugging like breakpoints, logging, or profiling and are configured at the runtime. Annotations serve ad-hoc performance and safety purposes as documented by a runtime. Seals, gates, and examples of annotations are described in this document.

Linking replaces a token by more bytecode, acyclically. ABC's primary linking model is [Awelon Object (AO)](AboutAO.md), which introduces tokens of the form `{%word}`, and must be evaluated in context of an AO dictionary that defines the words. AO has a lazy linking model to preserve link structure whenever inlining code linking would not contribute to rewrites beyond mere inlining of definitions.

### Bytecode Extension

Bytecodes may be extended through the link layer. In context of AO, a bytecode sequence `xyz` is effectively shorthand for `{%x}{%y}{%z}`. Reducing common operations to a single character enables a compact representation of programs and data. An AO dictionary might define:

        [B][A]w == [A][B]   (swap);     w = {&a2}[]ba
        [A]i    == A        (inline);   i = []wad
        [B][A]o == [A B]    (compose);  o = {&a2}[ai]bb

Performance of bytecode extension may be supported through runtime-accelerated dictionaries. A consequence is that bytecode extension in practice isn't entirely user-defined, but rather is a collaborative between users and runtime developers.

### Whitespace Formatting

The whitespace characters SP and LF are given identity semantics, equivalent to the empty program. They will have identity semantics, equivalent to the empty program. Essentially, this just permits some lightweight formatting of bytecode for easier reading in a text editor.

### Data Embedding and Extraction

Leveraging the ABC dictionary, we can define eleven operators `#1234567890` such that `#` introduces a new zero value, and each digit performs the Church encoded equivalent of "multiply by ten, add digit". Thus, `#42` would construct a Church encoded 42. 

Leveraging the *accelerated dictionary* concept, a runtime might recognize the encoding for numbers, use a machine word to represent them under the hood, and output numeric results in the same form such that `#6 #7 *` evaluates to `#42`. 

Text embedding is a special case for efficient UTF-8 embedding:

        "text literals are multi-line UTF-8
         they start with character " (32)
         blacklist codepoints:
            C0 (except LF), DEL, C1
            replacement character
         linefeed is special character:
            LF SP   new line, drop SP
            LF LF   same as LF SP LF
            LF ~    terminates text
         no other special characters
        ~

For simplicity, ABC only supports the general purpose multi-line texts rather than attempting to support multiple kinds of text. However, [editable views](EditingAO.md) may support inline texts such as `"hello, world!"`. 

Text has a simple desugaring semantics via bytecode extension:

        "hello" => [[#104] "ello" y]    (y for 'yield')
        ""      => ~

Text desugars as UTF-8 bytes, so a character like `→` (U+2192) will desugar into three bytes (226, 134, 146). Desugaring as a binary is efficient for common use cases (e.g. using texts as keys in a data structure), and encourages developers treat individual characters as substrings (which is the only correct option with unicode).

It is possible to unify semantic structure of natural numbers, texts, [claw command sequences](EditingAO.md), and general coroutines. For example, natural number `#5 == (,,,,)` (yield five times, then return) and `"hello" == (#104, #101, #108, #108, #111)` (yield five times, each time placing a number on the stack, then return) could effectively have the same structure. However, unification is not required. In case of unification, `~` and `#` should have the same behavior of representing a final action (cf. *conditional behavior*, below). 





## ABC Evaluation and Performance

### Basic Evaluation Strategy

ABC evaluation rewrites an ABC program into a different representation of the same program. The goal isn't necessarily reduction to a 'normal' form, but rather to a condition efficient for extracting information, sharing, or usefully performing further evaluations, etc..

The basic evaluation strategy for ABC is: 

* rewrite outer program
* evaluate before copy
* copy lazily, by need
* evaluate final values

By first rewriting the outer program, we have some opportunity to apply annotations or drop values representing conditional paths. By evaluating before copying, we avoid creating rework (it's easy for a runtime to track whether an object is already evaluated). By copying only if there is demand (e.g. via `a` or `b`), we avoid unnecessarily replicating data in the output. Deep evaluation of final values is useful for data extraction and efficient further evaluation.

The basic evaluation strategy may be tuned by annotations.

### Arity Annotations

Arity annotations use rewrite rules for `{&a2}..{&a7}`:

                       [B][A]{&a2} => [B][A]
                    [C][B][A]{&a3} => [C][B][A]
                                   ...
        [G][F][E][D][C][B][A]{&a7} => [G][F][E][D][C][B][A]

It's the *annotation* that has the indicated arity. Arity annotations provide a lightweight form of input buffering, controlling partial evaluations where they aren't efficient or interesting. Arity annotations serve as a foundation for lazy evaluation, fixpoints, and ultimately to control [AO linking](AboutAO.md).

*Aside:* The six arity annotations cover the common range for parameter lists. If a function needs more than five arguments, developers should seriously consider factoring parameter objects. 

### Accelerated Dictionary

An ABC runtime system (interpreter, compiler, etc.) should provide at least one [Awelon Object (AO)](AboutAO.md) dictionary describing functions that it specially recognizes. This runtime dictionary should also document recognized annotations, rewrite rules, and related metadata. Developers will construct their own dictionaries using the runtime's dictionary as a starting point or reference.

Recognized functions are 'accelerated' by substitution of hand-optimized implementations and data representations. Optimization of data representation is the critical feature. That is not something easily achieved by a compiler. Upon recognizing `#1234567890`, an ABC runtime could use machine words to represent construction of natural numbers. By further recognizing arithmetic operations, this representation may be used for calculations.

Other valuable targets for acceleration:

* floating point numbers
* binary and list processing
* key-value records and databases
* key-based variants or sum types
* linear algebra and matrix math
* parallel process networks

Whenever we have accelerated data representations, we should also have an annotation like `{&nat}` for natural numbers to serve as a type assertion or conversion. We might actually want a pair of annotations - one for assertions on representation, and one that will convert representations if necessary.

Recognition of accelerated functions may be fragile, i.e. requiring an exact match of function names and definitions. Ideally, it would be more robust and portable. But efficient recognition of accelerators is also important (especially for an interpreter), and is easiest at the link layer. A runtime might accelerate multiple dictionaries for portability reasons.

### Accelerated Fixpoints and Loops

Fixpoint applies a function with a fixpointed copy of that function in context. This is a useful function for general purpose programming. It's inconvenient to use directly, but can be used to create arbitrary conditional loop constructs. Because ABC eagerly evaluates deep structure, we'll favor a variation of the strict Z-combinator:

        [B][A]z == [B][[A]z] A 

        z = [[c]a[{&a3}ci]bbwi]{&a3}ci

            where
            [B][A]w == [A][B];  w = []ba
            [A]i == A;          i = []wad

        [B][A]z == [B][A][[c]a[{&a3}ci]bbwi]{&a3}ci                 (def z)
                == [B][A][[c]a[{&a3}ci]bbwi]ci                      (arity)
                == [B][A][[c]a[{&a3}ci]bbwi][[c]a[{&a3}ci]bbwi]i    (def c)
                == [B][A][[c]a[{&a3}ci]bbwi][c]a[{&a3}ci]bbwi       (def i)
                == [B][A]c[[c]a[{&a3}ci]bbwi][{&a3}ci]bbwi          (def a)
                == [B][A]c[[[c]a[{&a3}ci]bbwi]{&a3}ci]bwi           (def b)
                == [B][A]c[              z           ]bwi           (def z)
                == [B][A][A][z]bwi                                  (def c)
                == [B][A][[A]z]wi                                   (def b)
                == [B][[A]z][A]i                                    (def w)
                == [B][[A]z]A                                       (def i)

This will likely become *the* de-facto standard fixpoint combinator. It has many convenient properties like being tail-recursive, not replicating `[A]` more than necessary, and supporting recovery of the `z` identifier as a simple rewrite. In practice, we'll *accelerate* this, reducing a dozen steps and two copies to a single step with one logical copy of `[A]`. Other loop structures may be defined in terms of `z` and might be independently accelerated. But `z` is the big one.

### Parallel Evaluation

ABC can easily support parallel rewriting. But parallelism and synchronization has some overhead, so we might favor making it explicit. Use of a `[computation]{&par}` annotation can serve in this role, requesting that a runtime perform the given computation in parallel.

We should be able to move the value containing the computation around or drop it without waiting. If we drop a parallel computation, that should efficiently abort the effort. Copying a parallel computation must wait for evaluation to complete, such that even if we halt on quota and seriazlie the program to disk, we will have avoided creation of rework.

When a runtime's overheads for parallelism are low, it may parallelize at its own discretion. Convenient targets for parallelism are: final values in the output, and the evaluation performed when lazily copying a value.

### Accelerated Process Networks

While use of `{&par}` is useful for simple divide-and-conquer strategies on large data structures, it is not very expressive. For example, it is difficult to model process pipeline parallelism, or scale to 

We cannot make useful observations on partially evaluated results. We cannot model communications or pipeline parallelism. Efficient distributed parallelism can be difficult because we're forced to send the entire computation rather than just some micro-communications between machines.

To address these weaknesses, I propose acceleration of a variant of [Kahn Process Networks (KPNs)](https://en.wikipedia.org/wiki/Kahn_process_networks). 

A process network can be modeled as a set of named processes that read and write to named ports, wires between ports, and a queue of messages on each wire. While it is not the case for KPNs generally, we might permit wires to be bounded buffers with pushback. A constraint is that each port is attached to a single wire, with the same types for input and output. This might be expressed as a DSL of sorts, and may even have a dedicated type safety analysis. 

We can accelerate evaluation of this network. KPNs evaluate deterministically, so we can treat it as rewriting the network description or DSL. However, KPNs evaluate efficiently using processes and queues - even in a distributed system. Our accelerator can leverage that. 

Further, we can accelerate interaction with the KPN while it is still under evaluation - injecting or extracting messages, merging another KPN, etc... Non-monotonic updates may require waiting long enough for the relevant subnetwork to stabilize. Copying a KPN would wait until parallel evaluation completes, but that would be an unusual case.

Intriguingly, modeling KPNs as first class values has much nicer properties than baking KPNs into the programming language. KPNs can be passed around as first-class objects or coroutines, still undergoing parallel evaluation and allowing incremental update. A KPN not only contains processes, it acts as a process. KPNs are also an interesting alternative to monads for an [effects model](KPN_Effects.md).

### Accelerated Linear Algebra

While KPNs are great for most parallel processing, we'll additionally need to accelerate matrix and vector math to take full advantage of GPGPU or SSE based parallelism. This will require a lot of care and consideration, but there are good examples to follow like Haskell's 'accelerate' and (for graphics) 'lambda cube'. 

### Efficient Interpretation

To efficiently interpret an ABC binary, we must leverage an index/cache on the bytecode such that, upon observing the start of a block, token, or text, we skip to the end and potentially bind any interesting information. For tokens, we might also use the index to efficiently link without reading the token. 

Efficient interpretation should be a major focus of an ABC runtime, enough to permit user-controlled compilation. The use of accelerators will likely be critical to efficient interpretation.

### Compilation

Compilation of code is essential if ABC is ever to be performance competitive with more conventional languages. A runtime should support compilation. I tend to prefer predictable performance, so I would like a runtime to provide compilation as an explicit action - i.e. something like `[function]{&jit}`. In a staged evaluation context like [AO](AboutAO.md), or via use of memoization, we can effectively get staged compilation.

*Aside:* Extracting an independent executable is something different from compilation for performance. This might also be worth pursuing, but more so at the [application layer](ApplicationModel.md), and might involve compiling to an intermediate language like C or JavaScript.

### Rewrite Optimizations

ABC's semantics admit many rewrites for performance that would not be performed during the normal course of evaluation. For example:

        []a     =>
        [i]b    =>
        bi      =>  i
        cd      =>
        cw      =>  c

A dictionary can allow more and higher level rewrites, so this feature would generally be achieved in conjunction with [AO](AboutAO.md). Ideally, developers can even suggest their own rewrite rules via the dictionary, which could be checked and applied. But a runtime can at least support rewrites for functions it accelerates.

Unfortunately, rewrite optimizations tend to be ad-hoc and fragile. They are difficult to apply outside of an explicit optimizer pass, and are difficult to prove safe. I would prefer not to rely on them too much. In most cases, developers should aim to make these optimizations explicit as part of evaluating an intermediate representation or DSL.

Rewrite optimizations might be available at runtime via an annotation like `{&opt}`.

### Stowage and Memoization

Stowage and memoization based caching are covered in greater detail in the [Awelon Object (AO)](AboutAO.md) documentation. But the general summary is:

        [large value]{&stow}    =>  [{%resource}]
        [computation]{&memo}    =>  [cache value]

Use of stowage supports larger than memory values and computations, offloading bulky data to disk. It might be understood as a more precise, explicit model of virtual memory. Values smaller than a heuristic threshold will not be moved by stowage.

A memo cache uses a serialized computation (potentially via secure hash) as key into a lookup table. If found, we can replace the computation by the value. Otherwise, we can compute and add it to the table. There may be some heuristic and probabilistic decisions about which keys make it into the table, e.g. based on time/space tradeoffs. Developers are encouraged to batch and buffer computations such that the time/space tradeoff for memoization is probably a good one.

Stowage and memoization work together nicely. Use of stowage can allow efficient serialization of computations involving large data structures or user-defined functions. Use of memoization can reference and share stowed resources without reconstructing them.


## Static Type Safety for ABC

ABC's behavior does not depend on any type judgements - no type driven dispatch or overloading. Hence, ABC may be evaluated without static typing. However, static analysis of type safety can nonetheless offer significant benefits:

* a clean, robust, trustworthy codebase
* type-sensitive projectional editors
* verifiable documentation, informed programmers
* typeful rendering and program extraction
* JIT compilation without dynamic type checks

It is my intention that most ABC codebases be strongly, statically type safe as the default. Explicit bypass of the type system will be supported, cf. dynamic evaluation below.

### A Simple Static Type System

ABC programs can be understood as pure functions operating upon a stack value. For convenience, we can align type descriptions with program stack structure:

        a   : S A (S → S') → S' A
        b   : S A (E A → E') → S (E → E')
        c   : S A → S A A
        d   : S A → S
        [F] : S → S type(F)

Each type list `S B A` is left associative, representing a product type `((S * B) * A)` with the 'top' of our stack on the right so we align with a program of form `S [B] [A]`. The leftmost type `S` effectively represents the remainder of the stack. In ABC, all other values are Church encoded and thus have function types. So in `S B A` the types `B` and `A` must be functions.

These types for ABC's primitives together with simple techniques like type unification, can be leveraged for inference of much larger programs. However, there isn't much opportunity to detect inconsistencies from ABC primitives.

### Type Annotations and Declarations

Tokens provide convenient anchors for static type inference. For example:

* `{&nat}` - argument is embeddable as natural number
* `{&lit}` - argument is embeddable as text literal
* `{&bool}` - argument is a boolean value
* `{&t3}` - argument has form `[[A][B][C]]`
* `{&aff}` - argument may not be copied
* `{&rel}` - argument may not be dropped
* `{:foo}` - wrapped types, unwrap with `{.foo}`

Given these tokens, we can detect many inconsistencies statically or at runtime. For example, it is obvious that `[A]{&aff}c` is an error, as is `[A]{.foo}` or `#42{&lit}` or `[[A][B]]{&t3}`. However, tokens tend to be limited in their expressiveness, oriented on things that are easy to check at runtime.

To support rich, human meaningful type documentation, we should additionally use the [AO layer](AboutAO.md) to attribute type information to subprograms. For example, `word.type` may declare the type of `word` in a manner meaningful both to a type checker and a human. Type descriptions, in this case, would be first-class computable values, and may prove more flexible or extensible 

In any case, the presence of type annotations and declarations can provide a basis for detecting inconsistencies in the ABC program.

### Conditional Behavior

A major concern for any programming language and type system is conditional behavior. Most languages have a dedicated syntax and semantics, e.g. the `if then else` expression. In ABC, our conditional behaviors will instead be Church encoded. For example:

        type Bool = True | False
        [onTrue][onFalse] true  i => onTrue
        [onTrue][onFalse] false i => onFalse
        true  = [di]   
        false = [ad]

In practice, of course, computation of a boolean condition is frequently separated from expression of the `[onTrue][onFalse]` conditional paths. This makes it difficult to locally detect type errors or inconsistencies between `onTrue` or `onFalse` contingent on unification of environment. To detect type errors early, we need to inform our type system that our conditional paths will be evaluated in context of a boolean. We can leverage annotations in this role. For example:

        {&bool}[[onTrue][onFalse]]ai

This subprogram expresses that `[onTrue][onFalse]` will be applied in a context with a boolean at the top of the stack. Our static type checker knows about booleans, and thus can determine the types of `onTrue` and `onFalse` should unify in useful ways, enabling local detection of inconsistent type errors.

I would also like effective support for option and sum values:

        type Opt A = Some A | None
        [onSome][onNone] [A] some i     => [A] onSome
        [onSome][onNone]     none i     =>     onNone

        type Sum A B = Left A | Right B
        [onLeft][onRight] [A] left i    => [A] onLeft
        [onLeft][onRight] [B] right i   => [B] onRight

        left    = [wdwi]b
        right   = [wbad]b
        some    = left
        none    = false

Support for `{&bool}`, `{&opt}`, and `{&sum}` should be sufficient in practice. Generalizing to more than two paths isn't critical.

*Aside:* Conveniently, `{&bool}[[onTrue][onFalse]]ai` is structurally, semantically, and visually similar to an `if then else` expression. It is feasible to provide a more conventional if-then-else view through an editor.

### Structural Scopes

ABC provides a simple mechanism for controlling scope of a computation. Assume we have a function `foo` that takes two inputs and produces three outputs. A structurally scoped application can be represented as:

        [A][B][foo]bb{&t3}i

What we're doing here is binding two arguments to the function, asserting that there are three outputs, then inlining those outputs into our main code. Dynamically, the `{&t3}` annotation, asserting the argument is a tuple of size three, requires a simple bit of reflection. 

        [[A][B][C]]{&t3}    =>  [[A][B][C]]

I propose that most ABC systems should support `{&t0}`..`{&t7}`, the practical range of tuple sizes. If we determine through static analysis that the `{&t3}` annotation will be valid at runtime, the annotation may be eliminated statically. So this annotation is suitable in both static and dynamic contexts.

### Value Sealing for Lightweight Types

ABC introduces two tokens `{:foo}` and `{.foo}` to support symbolic value sealing, to resist accidental access to data. This is supported by a simple rewrite rule: `{:foo}{.foo}` will rewrite to the empty program. The main goal is to force programs to fail fast if they aren't expected to access a particular data structure. In most cases, seals will be used as symbolic wrappers for individual values, e.g. using `[{:foo}]b`. During static type analysis, these type wrappers should serve as a useful type constraint.

### Substructural Types

Substructural types are useful for structuring application behavior independently of syntax or concurrency model. In context of a bytecode that lacks much syntactic structure, these types could greatly simplify reasoning about code. ABC can support substructural types both statically and dynamically. Developers will access substructural types via annotations and a few simple rules:

* `{&rel}` - relevant values, forbid drop (operator `d`)
* `{&aff}` - affine values, forbid copy (operator `c`)
* inherit substructure of bound values (operator `b`)
* relevant and affine together result in linear values
* `{&rel}` and `{&aff}` are commutative and idempotent

Values with substructural types may freely be applied (operator `a`). Substructure is not lifted during partial evaluation, so substructural type does not interact with features like `{&seq}` or `{&par}`.

Static typing requires precomputing this information, i.e. tracking which values are copied and dropped and which are marked relevant or affine, and detecting any conflict statically. This isn't difficult, though I have yet to think up an *aesthetic* set of type annotations that don't make me cringe. Consider:

        c : (A!aff * E) → (A * (A * E))
        d : (A!rel * E) → E

        {&rel} : (A * E) → (A&rel * E)
        {&aff} : (A * E) → (A&aff * E)

In this case, we have a conflict if `A+aff` (a value annotated affine) is used where `A-aff` (a value that is not affine, a copyable value) is needed.

### Accelerator Type Checking

Specialized type checking for accelerators can be effective without becoming overly complicated. For example, we could support specialized type models for accelerated process networks, key-value records, and key-based variants.

### Dynamic Evaluation

Many useful programming styles are difficult to statically typecheck. Example:

        "abcx → ax^2 + bx + c" runPoly

Assume this constructs a program that takes four arguments - a, b, c, x - then computes the specified polynomial. The number of arguments we take depends on polynomial's text value. Providing a static type judgement for `runPoly` is feasible but non-trivial, generally requiring sophisticated dependent types and proofs.

Similar scenarios exist for print formatting and DSLs.

Fortunately, we do not need sophisticated types. The polynomial text is right there. Statically. So, even if we cannot easily provide a static type for `runPoly`, we might be able to provide a simple type for the larger program after we partially evaluate `runPoly`. We only need some means to defer static type analysis until after some partial evaluation. 

For ABC, I propose a `{&dyn}` annotation. Usage:

        "abcx → ax^2 + bx + c" runPoly =>
            "abcx → ax^2 + bx + c" compilePoly {&dyn} i =>
            [polynomial behavior]{&dyn} i =>
            [polynomial behavior]i

At this point we may halt evaluation and pass the program to our static type checker. Dynamic evaluation is considered complete after the `{&dyn}` annotations are eliminated (via `[A]{&dyn} => [A]`). 

If a program contains `{&dyn}` annotations even after partial evaluations, we may call that program dynamically typed. Developers are free to construct dynamically typed software. But where dynamic partial evaluation can complete statically, it can readily be used for macro-like metaprogramming. An ABC software system may thus consist of an ad-hoc mix of fluid and rigid software components.


## Miscellaneous

### Runtime Errors

While static type checking is optimal, runtime type errors are possible with `{&dyn}` or if we do not bother to type check. In addition, developers may perform dynamic assertions, or express partial functions, in ways that a type checker cannot readily handle.

To simplify error reporting and debugging, we'll want to record known errors in the generated program. To accomplish this, we can use an `{&error}` annotation around a bad subprogram. An evaluator can freely inject the error annotation, delimiting the bad code:

        #42{&lit}       =>  [#42{&lit}]{&error}i
        [A]{&aff}c      =>  [[A]{&aff}c]{&error}i
        {:s}d           =>  [[A]{:s}d]{&error}i
        [[A]]{&t2}      =>  [[A]]{&t2}{&error}

Developers may freely specify their own error values:

        "TODO: fix foo yesterday!"{&error}

Use of `{&error}` marks a *value* as erroneous. Observing that value, e.g. with `[A]{&error}i`, results in a stuck computation. Being stuck on an error will not generate further errors. A type checker may treat an error value as having any type.

### Comments

It is not difficult to model comments in ABC. For example:

        "this is my boomstick!
        ~{&a2}{@rem}d

I generally prefer documentation be pushed to associative structure at the AO layer, into words like `foo.doc`, `foo.talk`, `foo.about`, `foo.author`, or `foo.example`. But if we're going to model comments, the `{&a2}` arity annotation and `{@rem}` gate make them more useful and accessible than comments are in most programming languages. The gate enables logging, conditional breakpoints, etc.. The arity annotation resists premature destruction of the comment and enables construction of commented values or programs.

### Gates for Active Debugging

Active debugging describes techniques that provide a view of a computation in progress: breakpoints, logging, animation, tracing, profiling.

* **breakpoints:** provide a frozen view of a computation in progress
* **logging:** provides a local view of a subprogram's dataflow
* **animation:** global logging via breakpoints, big space overhead
* **tracing:** prefix argument with comment to help trace data flows 
* **profiling:** view time/effort spent on specified subcomputations

I propose tokens of form `{@gate}` - called 'gates' - for active debugging. The symbol is user-defined. A gate operates on a single argument, e.g. `[A]{@foo}`. The behavior of a gate is configured at the evaluator. For example: 

* open gates just pass the argument (not debugging)
* closed gates do not evaluate further (breakpoint)
* logging gates record argument to configured log
* trace gates inject a comment into their argument
* profiling gates aggregate performance statistics

It's feasible for a gate to serve a few roles at once. They're effectively user-configured annotations. Gates may have default configurations based on naming conventions. E.g. `{@log:xyzzy}` might print to a log titled `xyzzy`.

An interesting feature is that we can halt on many 'breakpoints' concurrently, different subcomputations stalling at `[A]{@bp}action`. Halting on many breakpoints is convenient for providing a more predictable debug context, and for program animation. 

Active debugging can be performed without hand modification of code, e.g. if we assume automatic rewriting of a codebase to inject appropriate gates. This is probably easiest in context of [AO](AboutAO.md) because we can leverage clearly named subprograms and link structure. 

#### Program Animation

Using breakpoints and a systematic animation strategy, we can animate a program's evaluation:

* When evaluation stalls on breakpoints, take a snapshot.
* Delete breakpoints as determined by animation strategy.
* Repeat until no active breakpoint gates are discovered.

This has a lot of benefits for debugging. We have the whole 'spatial' context of breakpoints, but we also preserve the 'temporal' context of logging. Though, logging and profiling can also be used with program animation, e.g. by recording a set of log messages and profiling statistics per frame.

An animation strategy can be specified many ways:

* always delete rightmost breakpoint (or leftmost)
* delete all current breakpoints to maximize frame size
* focus on breakpoints of given names, hierarchically
* interleave breakpoints of different names
* pseudo-random selection

Animating on breakpoint is much nicer than animating on quota. Individual frames will be deterministic, modulo profiling data. The structure is much more predictable, which simplifies both compression and stable rendering. The animation strategy enables evaluation to be precisely controlled and focused to just the parts we're interested in observing or rendering.

### Testing

For testing purposes, it is frequently useful to assert that two values match.

I'd rather avoid this sort of ad-hoc reflection at runtime. But it could be supported at the AO layer, e.g. by specifying test objects with expected and actual results. If necessary, however, we could introduce some annotations for the role:

* `{&eqv}` - assert two values are structurally equivalent
* `{&beqv}` - assert two values are behaviorally equivalent

Behavioral equivalence might be tested by some ad-hoc combination of static analysis or fuzz testing.

### Garbage Data

Similar to the `{&error}` annotation for error values, developers might want to recycle memory early while keeping the placeholder (especially if that placeholder might have substructural types or associated debug traces). This could be done by introducing annotation `{&trash}` which would replace the data with an error value but preserve substructure and debug data. 

### Lazy Evaluation and Coinductive Data

We can leverage arity annotations to defer computations by constructing a value like `[[B]{&a2}A]`. This has the same type and semantics as `[[B]A]`, but the arity annotation prevents further evaluation. These properties are convenient for representing coinductive data types such as infinite data streams or procedurally generated worlds that otherwise might expand to fill available memory.

Binding extra arguments to force deferred computation isn't always convenient or optimal. So we introduce an annotation to `{&force}` partial evaluation of a deferred computation. Evaluation would proceed though sufficient input is available to clear arity annotations, without actually providing any input.

A weakness of deferred computation is that we create rework upon copy. Developers can use `{&memo}` to memoize and share expensive computations. But memoization has its own costs and doesn't always pay for itself. Sometimes recomputing is cheap enough. I will leave use of memoization to the developer.

*Aside:* There is no `[A]{&lazy}` annotation operating on the evaluation strategy because it would be inconsistent in context of intermediate output or transparent persistence. For example, given `[[B]{&par}]{&lazy}` we cannot distinguish the origin as `[B]{&par}[]b{&lazy}` vs. `[B][{&par}]b{&lazy}`. Use of arity annotations doesn't have this problem.
