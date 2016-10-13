# Awelon Bytecode

Awelon Bytecode (ABC) provides a computational medium in which ideas, data, and actions can be precisely expressed, shared, and composed. This document describes ABC's idioms and design. For a short summary, see [ABC](ABC.md). 

## Why a New Bytecode?

Awelon project has a vision for human-computer interaction that places users within the environment rather than above or apart from it, where all HCI is modeled as software development and live programming. This vision includes non-conventional [application models](ApplicationModel.md) where humans and software agents operate upon a codebase in a shared environment, and a [linked evaluation model](AboutAO.md) that preserves ad-hoc structure meaningful to humans and software agents.

Most bytecodes aren't designed with such applications in mind.

What sets ABC apart?

ABC is evaluated by pure, local, confluent rewriting. Importantly, ABC does not become entangled with its environment during evaluation. There are no jumps or pointers, no external memory or stack, no FFI, no side-effects. Effects are achieved in multi-agent systems via Awelon's non-conventional [application layer](ApplicationModel.md). An ABC subprogram will simply evaluate into another representation of the same ABC subprogram, much like `6 * 7` evaluates in conventional arithmetic to `42`.

ABC is simple and extensible. There are only four primitive operations `abcd` - apply, bind, copy, drop. ABC programs can be composed by simple concatenation, linked by simple inlining. Extensions are trivial via the [AO link layer](AboutAO.md): a user-defined function whose name is a single UTF-8 codepoint may be used as a bytecode. Extensions will be accelerated for primitive performance by runtimes that recognize them, leading to de-facto standardization of popular ABC extensions.

ABC is serializable and weakly legible. ABC strings are encoded in UTF-8 (mostly the ASCII subset), avoiding control characters, and may be rendered in normal text editors. Through extension, we support compact embeddings of data. For example, `#42` may construct a Church encoded natural number forty-two. Texts are embedded via a lightweight syntactic sugar. Through acceleration, we can also embed numbers and texts in the output from evaluation. Use of editable views, even textual versions like [claw](CommandLine.md), can further improve legibility for ABC.

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

Tokens have form `{foo}`, a short text wrapped in curly braces. Tokens enable symbolic extensions to ABC. Semantically, tokens are restricted by the normal rules for ABC: it must be possible to reduce every token to a primitive, purely functional `[abcd]` sequence. Tokens in ABC fall primarily into two groups:

* tokens with *identity* semantics for performance, debugging
* tokens with *linking* semantics for structured development

Tokens with *identity* semantics include seals `{:db}{.db}`, gates `{@foo}`, and annotations `{&par}`. Seals support lightweight symbolic types. Gates are used for active debugging. Annotations serve ad-hoc performance and safety purposes. These are described later in this document. 

ABC's primary linking model is [Awelon Object (AO)](AboutAO.md), which introduces tokens of the form `{%word}` binding to an implicit dictionary. During evaluation, the token is substituted for the word's definition when doing so enables evaluation to proceed. 

*Note:* Tokens are syntactically limited - at most 62 bytes UTF-8 token text between the curly braces, and the text itself may not contain curly braces, C0, DEL, C1, or the replacement character. Tokens should not include much logic or structure internally.

### Bytecode Extension

ABC is extended through the link layer. In context of AO, `xyz` is effectively shorthand for `{%x}{%y}{%z}`. Reducing common operations to a single character enables compact representation of programs. An AO dictionary, thus, might define the following:

        [B][A]w == [A][B]   (swap);     w = {&a2}[]ba
        [A]i    == A        (inline);   i = []wad
        [B][A]o == [A B]    (compose);  o = {&a2}[ai]bb

For performance, this idea should be coupled with the concept of accelerated dictionaries. An interpreter or compiler can provide optimized representations and implementations to support performance critical computations. Because performance requires cooperation with interpreters, bytecode extensions will tend to have some de-facto standardization.

### Data Embedding and Extraction

Leveraging the ABC dictionary, we can define eleven operators `#1234567890` such that `#` introduces a new zero value, and each digit performs the Church encoded equivalent of "multiply by ten, add digit". Thus, `#42` would construct a Church encoded 42. 

Leveraging the *accelerated dictionary* concept, a runtime might recognize the encoding for numbers, use a machine word to represent them under the hood, and output numeric results in the same form such that `#6 #7 *` evaluates to `#42`. 

Text embedding is a special case and needs a syntactic sugar:

        "literals are multi-line UTF-8
         they start with character " (32)
         blacklist characters:
            C0 (except LF), DEL, C1
            replacement character
         linefeed is special character:
            LF SP   new line, drop SP
            LF LF   same as LF SP LF
            LF ~    terminates text
         no other special characters
        ~

I haven't settled on a specific model for embedded text. Primary candidate:

        "hello" => [[#104] "ello" y]    (y for 'yield')
        ""      => #

This representation favors structural unification of natural numbers, texts, [claw command sequences](CommandLine.md), and general coroutines. For example, natural number `#5 == (,,,,)` (yield five times, no action) and `"hello" == (#104, #101, #108, #108, #111)` (yield five times, each time placing a number on the stack). 

### Whitespace Formatting

SP and LF are permitted in ABC. They will have identity semantics, equivalent to the empty program. Essentially, this just permits some lightweight formatting of bytecode for easier reading.

## ABC Evaluation and Performance

### Basic Evaluation Strategy

ABC evaluation rewrites an ABC program into a different representation of the same program. A simple evaluation strategy for ABC is: 

* rewrite outer program
* evaluate before copy 
* evaluate final values
* prioritize annotations

By first rewriting the outer program, we get some opportunity to apply annotations (notably `{&lazy}`) or drop conditional evaluation. By evaluating a value before copying it, we avoid creating rework. When rewriting has completed, we can usefully proceed with evaluating the values because they haven't been dropped.

### Evaluation Control

For performance reasons, developers may wish to guide the evaluation strategy. This may be supported by use of annotations:

* `{&seq}`  - shallow evaluation of argument
* `{&par}`  - parallel shallow evaluation
* `{&lazy}` - lazy evaluation, do not evaluate 
* `{&eval}` - full evaluation of argument
* `{&a2}..{&a7}` - restrict partial evaluation on arity

Use of `{&seq}` and `{&eval}` would correspond to normal evaluation strategies, with `{&seq}` just evaluating the toplevel (same as you'd evaluate upon `i`) and `{&eval}` performing deep evaluation. 

Use of `{&par}` is an explicitly parallel `{&seq}`. Making this explicit is useful because parallelism may have some non-trivial overhead such that we don't want it for the normal case. Dropping a parallel evaluation should cause it to abort, while copying a parallel evaluation may wait until it completes. More expressive parallelism is feasible with *acceleration of process networks* (see below).

Laziness is an important strategy. Though 'lazy' is misleading - it's actually *call by name* in the sense that copying a lazy value does not use a shared cache. Use `{&lazy}` with `{&cache}` (described later) if you need conventional laziness. Laziness is critical for fixpoints and modeling infinite data structures.

Arity annotations are defined by a series of six rewrite rules:

        [B][A]{&a2} => [B][A]
        [C][B][A]{&a3} => [C][B][A]
        ...
        [G][F][E][D][C][B][A]{&a7} => [G][F][E][D][C][B][A]

Arity annotations are effectively a form of lightweight input buffering, and enable developers to guard against partial evaluations when they aren't useful or interesting. For example, the swap function `[]ba` could be applied to a single argument, resulting in `[[A]]a`. But there isn't much benefit in doing so, so we instead use `{&a2}[]ba` to ask for two arguments before continuing. Arity annotations are especially useful in context of [AO](AboutAO.md) because it can help control lazy linking.

### Accelerated Dictionary

An ABC runtime system (interpreter, compiler, etc.) should specify an [Awelon Object (AO)](AboutAO.md) dictionary including functions that it specially recognizes. The runtime dictionary should also document recognized annotations, rewrite rules, and related metadata. Developers will construct their own dictionaries using the runtime dictionary as a starting point or reference.

Recognized functions are 'accelerated' by substitution of hand-optimized implementations and data representations. Optimization of data representation is the critical feature. That is not something easily achieved by a compiler. Upon recognizing `#1234567890`, an ABC runtime could use machine words to represent construction of natural numbers. By further recognizing arithmetic operations, this representation may be used for calculations.

Other valuable targets for acceleration:

* floating point numbers
* binary and list processing
* key-value records and databases
* linear algebra and matrix math
* parallel process networks

Recognition of accelerated functions may be fragile, i.e. requiring an exact match of function names and definitions. Ideally, it would be more robust and portable. But efficient recognition of accelerators is also important (especially for an interpreter), and is easiest at the link layer.

### Accelerated Fixpoints and Loops

Fixpoint applies a function with a fixpointed copy in context.

        [A]Y == [[A]Y] A

Fixpoint is a valuable function for general purpose programming. ABC does not have any built-in loop constructs, nor does it support recursion via the link layer (AO definitions must be acyclic). But use of a fixpoint combinator allows for expression of arbitrary loop constructs. The original `Y` combinator, discovered by Haskell Curry, can easily be translated to ABC:

        (Problematic Fixpoint Candidate)

        Y = [cb]oci

        [A]Y == [A][cb]oci      (def Y)
             == [cb A]ci        (def o)
             == [cb A][cb A]i   (def c)
             == [cb A]cb A      (def i)
             == [cb A][cb A]b A (def c)
             == [[cb A]cb A]  A (def b)
             == [[A]Y] A        (by fourth equality)

However, this fixpoint has two weaknesses. First, the "by fourth equality" step is difficult to perform with normal evaluation and results in *two* copies of `A` that aren't in use. Second, ABC's standard evaluation strategy (which evaluates within functions by default) may result in infinitely more copies if the fixpoint function is part of our output: `[A]Y => [[A]Y]A => [[[A]Y]A]A ...`. To address these weaknesses, I recommend an alternative variation of fixpoint that includes a `{&lazy}` annotation and defers copy:

        (Recommended Fixpoint Candidate)

        [A]Y == [[A]Y]{&lazy} A

        Y = [[c]a[ci]bb{&lazy}wi]ci

        [A]Y == [A][[c]a[ci]bb{&lazy}wi]ci                          (def Y)
             == [A][[c]a[ci]bb{&lazy}wi][[c]a[ci]bb{&lazy}wi]i      (def c)
             == [A][[c]a[ci]bb{&lazy}wi] [c]a[ci]bb{&lazy}wi        (def i)
             == [A]c[[c]a[ci]bb{&lazy}wi]    [ci]bb{&lazy}wi        (def a)
             == [A]c[[[c]a[ci]bb{&lazy}wi]ci]  b{&lazy}wi           (def b)
             == [A]c[          Y            ]  b{&lazy}wi           (def Y)
             == [A][A][Y]b{&lazy}wi                                 (def c)
             == [A][[A]Y]{&lazy}wi                                  (def b)
             == [[A]Y]{&lazy}[A]i                                   (def w)
             == [[A]Y]{&lazy} A                                     (def i)

This requires a few more steps, but avoids an unnecessary copy of `A`, and the definition of `Y` is clearly part of the output. So a simple rewrite step after direct evaluation would recover the fixpoint structure.

Regardless, we'll want to *accelerate* our fixpoint combinator. By recognizing this operation and accelerating it, we can essentially reduce fixpoint to an annotation on a function. A loop becomes a single step operation, and the `Y` is easily preserved in the final output. 

We might also accelerate common loop constructs built with the `Y` combinator, e.g. it is possible to define a useful, pure `while` function over a sum type: `while :: (a + b) → (a → (a + b)) → b`, and this can be augmented with an extra state value (the stack). 

### Accelerated Process Networks

While use of `{&par}` is sufficient for simple divide-and-conquer strategies on large data structures, it is not very expressive. For example, it cannot express communicating processes or pipelines. To support more expressive parallelism, I propose that ABC runtimes should *accelerate* evaluation of a bounded-buffer variant of [Kahn Process Networks (KPNs)](https://en.wikipedia.org/wiki/Kahn_process_networks).

A description of a KPN (processes, wires, messages on wires) can be deterministically evaluated into a new description of the same KPN. This evaluation can be naively represented as a pure function. 

A runtime accelerated evaluator can use more conventional queues and threads internally. Further, it is feasible for an external system to interact with a KPN that is still undergoing evaluation - i.e. inject input, extract output, adding a process or wire. These actions need only wait just long enough to resolve deterministically while allowing parallel evaluation to continue. In each case, we would only be forced to wait *just long enough* to resolve deterministically.

*Aside:* Open KPNs are interesting as a potential [alternative to monads for effectful code](KPN_Effects.md), admitting parallel and concurrent effects, and flexible buffering. They're also a potential alternative to OOP-style objects, since we can model invoking a KPN by injecting some data then extracting results, and passing KPNs around as first-class values.

### Accelerated Linear Algebra

While KPNs are great for most parallel processing, we'll additionally need to accelerate matrix and vector math to take full advantage of GPGPU or SSE based parallelism. This will require a lot of care and consideration, but there are good examples to follow like Haskell's 'accelerate' and (for graphics) 'lambda cube'. 

### Compilation

A runtime can provide a `[function]{&jit}` annotation such that we construct a more efficient representation for evaluation of the function. Taken together with [AO dictionaries](AboutAO.md) and a little caching, we can effectively achieve staged compilation for important words. Effective use of JIT may be limited to cases where it's easy to determine static types.

Compiling an executable independent of an ABC runtime is feasible as a case of *program extraction*. Generally, program extraction might translate an ABC program to a Haskell module, JavaScript object, C function, and so on. Extraction requires a well understood program type to integrate with the context. In case of independent executables, the program type will likely be some variant on monadic IO.

Awelon project favors [application models](ApplicationModel.md) that do not rely on program extraction, so local `{&jit}` compilation more appropriate for basic performance concerns. However, I would like to support both techniques.

### Rewrite Optimizations

ABC's semantics admit rewrites for performance. For example:

        []a     =>
        [i]b    =>
        bi      =>  i
        cd      =>
        cw      =>  c
        ad      =>  wdi         (easier tail calls)

Rewrites might be used to systematically recognize accelerators like `[][]baad => i`. 

Rewrites benefit from accelerators. An interesting possibility is recognizing associative actions, like multiplication of matrices or merging of key-value databases or loop fusions, and rewriting them for performance.

Unfortunately, rewrite optimizations tend to be ad-hoc and fragile. They are difficult to apply to dynamically constructed code, and are difficult to prove safe in the general case. I would prefer not to rely on them too much. In most cases, developers should aim to make optimizations explicit, e.g. construct an intermediate object representing a lazy multiplication of matrices then reorder on run.

### Stowage and Caching

Stowage and caching are covered in greater detail in the [Awelon Object (AO)](AboutAO.md) documentation. But the general summary is:

        [large value]{&stow}    =>  [{%resource}]
        [small value]{&stow}    =>  [small value]
        [computation]{&cache}   =>  [cache value]

Use of stowage supports larger than memory values and computations, offloading bulky data to disk. It might be understood as a more precise, explicit model of virtual memory.

Cache uses a serialized computation as a representation for a value. We'll look up the computation in a table. If found, we replace the computation by its result without performing all the intermediate steps. Otherwise, we perform the computation and heuristically decide whether to add it to the table (based on time/space tradeoff). 

The serialization requirement for caching has some overhead, so developers are encouraged to make suitable 'cache-points' explicit such that the time/space tradeoff is probably a good one. Use of stowage can help ameliorate cache overheads by controlling synchronization costs.

### Lazy Evaluation

By the general evaluation strategy, call-by-need is the default for input to a computation. It can be preserved for outputs, too, via explicit use of a `[computation]{&lazy}` annotation. In AO, lazy evaluations are not implicitly cached, which may be a problem if you might copy a lazy computation. If need to cache lazy evaluations, make it explicit with `{&cache}{&lazy}` (or `{&lazy}{&cache}`, order makes no difference).

Laziness may be used to model infinite data structures - e.g. infinite streams, trees, procedurally generated worlds, etc.. Support for laziness has no semantic impact in ABC, but instead is related to consumption of quotas after the 'toplevel' for a program is evaluated. In practice, I imagine most ABC evaluators shall support laziness.

### Performance Annotations

Many annotations are used for performance:

* `{&seq}` - shallow evaluation of subprogram 
* `{&par}` - parallelize evaluation of subprogram
* `{&eval}` - perform full evaluation of subprogram
* `{&lazy}` - delay evaluation until otherwise requested
* `{&a2}..{&a7}` - arity annotations to control batching
* `{&lit}` - force argument to text literal representation
* `{&nat}` - force argument to natural number representation
* `{&stow}` - move value to link layer, away from working memory
* `{&trash}` - drop data but keep placeholder, substructure
* `{&cache}` - use cached result or add result to cache
* `{&opt}` - simplify and optimize a subprogram 
* `{&jit}` - compile a function for runtime internal use

Use of annotations to control staging and compilation has potential to be very effective in giving developers control of the performance of their code. In general, annotations on representation also support type checking and may be effectively used together with accelerators to squeeze the most performance from a representation.

### Interpretation and Compilation

For fast interpretation, ABC has a few significant weaknesses:

* embedded data can hurt locality of active code
* unknown sizes for tokens, literals, and blocks
* tokens are not statically linked ahead of time

To overcome these weaknesses, we have at least two options.

One option is to rewrite ABC to use an alternative representation of bytecode internally, one that addresses these weaknesses. This might embed data with an address or offset, and bind linker tokens similarly. Direct interpreted performance may improve, though by how much is difficult to determine without profiling. The cost is complexity for translations in both directions, persistence, stowage, and incremental computing.

Another option is to index and cache metadata to efficiently process an ABC string, separately from it. This could be achieved by a hashtable mapping address or offset to the metadata. This has some advantages - flexibility, easier serialization. 

This won't help with code locality, but it could reduce the issues of scanning to the end of a token or literal within a compact binary, and it would support binding of a token to its interpretation.

In context of a JIT compiler, I suspect a fast interpreter is less important than a simple representation. So the index on raw ABC may be the better option unless something about the other bytecode simplifies compilation.

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

*Aside:* Conveniently, `{&bool}[[onTrue][onFalse]]ai` is structurally, semantically, and visually similar to an `if then else` expression. It is feasible to provide specialized presentations for a [claw-like](CommandLine.md) view of conditional expressions in ABC.

### Structural Scopes

ABC provides a simple mechanism for controlling scope of a computation. Assume we have a function `foo` that takes two inputs and produces three outputs. A structurally scoped application can be represented as:

        [A][B][foo]bb{&t3}i

What we're doing here is binding two arguments to the function, asserting that there are three outputs, then inlining those outputs into our main code. Dynamically, the `{&t3}` annotation, asserting the argument is a tuple of size three, requires a simple bit of reflection. 

        [[A][B][C]]{&t3}    =>  [[A][B][C]]

I propose that most ABC systems should support `{&t0}`..`{&t7}`, the practical range of tuple sizes. If we determine through static analysis that the `{&t3}` annotation will be valid at runtime, the annotation may be eliminated statically. So this annotation is suitable in both static and dynamic contexts.

### Value Sealing for Lightweight Types

ABC introduces two tokens `{:foo}` and `{.foo}` to support symbolic value sealing, to resist accidental access to data. This is supported by a simple rewrite rule: `{:foo}{.foo}` will rewrite to the empty program. A program of form `[A]{.foo}` or `{:foo}b` can fail fast. In most cases, seals will be used as symbolic wrappers for individual values, e.g. using `[{:foo}]b`.

During static type analysis, these type wrappers should serve as a useful type constraint. Dynamically, in addition to serving as a fail-fast condition, we might leverage these symbols as hints to guide rendering or debugging.

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

In this case, we have a conflict if `A&aff` (a value annotated affine) is used where `A!aff` (a value that is not affine, a copyable value) is needed.

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

### Runtime Type Errors

While static type checking is optimal, runtime type errors are possible with `{&dyn}` or if we do not bother to type check. In addition, developers may perform dynamic assertions, or express partial functions, in ways that a type checker cannot readily handle.

To simplify error reporting and debugging, we'll want to record known errors in the generated program. To accomplish this, we can use an `{&error}` annotation around a bad subprogram. An evaluator can freely inject the error annotation, delimiting the bad code:

        #42{&lit}       =>  [#42{&lit}]{&error}i
        [A]{&aff}c      =>  [[A]{&aff}c]{&error}i
        {:s}d           =>  [[A]{:s}d]{&error}i
        [[A]]{&t2}      =>  [[A]]{&t2}{&error}

Developers may freely specify their own error values:

        "TODO: fix foo yesterday!"{&error}

Use of `{&error}` marks a *value* as erroneous. Observing that value, e.g. with `[A]{&error}i`, results in a stuck computation. Being stuck on an error will not generate further errors. A type checker may treat an error value as having any type.

### Gates for Active Debugging

Active debugging describes techniques that provide a view of a computation in progress: breakpoints, logging, animation, tracing, profiling.

* **breakpoints:** provide a frozen view of a computation in progress
* **logging:** provides a local view of a subprogram's dataflow
* **animation:** global logging via breakpoints, big space overhead
* **tracing:** backtrack dataflows, mark values with their origin
* **profiling:** view time/effort spent on specified subcomputations

I propose tokens of form `{@gate}` - called 'gates' - for active debugging. The symbol is user-defined. A gate operates on a single argument, e.g. `[A]{@foo}`. The behavior of a gate is configured at the evaluator. For example: 

* open gates just pass the argument (not debugging)
* closed gates do not evaluate further (breakpoint)
* logging gates record argument to configured log
* trace gates add some contagious metadata to argument
* profiling gates aggregate stats for evaluation of arguments

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
