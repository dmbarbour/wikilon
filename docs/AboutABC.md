# Awelon Bytecode

Awelon Bytecode (ABC) provides a computational medium in which ideas, data, and actions can be precisely expressed, shared, and composed. This document describes ABC's idioms and design. For a short summary, see [ABC](ABC.md). 

## Why a New Bytecode?

Awelon project has a vision for human-computer interaction that places users within the environment rather than above or apart from it. This vision includes alternative [application models](ApplicationModel.md) where humans and software agents operate upon a codebase in a shared environment, and a [linked evaluation model](AboutAO.md) that preserves ad-hoc structure meaningful to humans and software agents.

Most bytecodes aren't designed with such applications in mind.

ABC has many unusual features that make it suitable for Awelon project:

* ABC is **easily serialized and rendered**. ABC has a plain-text encoding that directly embeds numbers and literals. Thus, even a simple text editor can provide a useful view for debugging and comprehending code. AO is designed for use with editable views, projectional editors. Sophisticated graphical renderings are feasible.

* ABC **embraces data**. Where most languages shift responsibility for data to an external filesystem or database, ABC prefers to internalize it. Doing so simplifies serialization, persistence, composition, static safety, partial evaluation and staging, transparent procedural generation, and modeling interactive objects. Further, this readily extends to larger-than-memory data via stowage and caching.

* ABC is **evaluated by local rewriting**. ABC subprogram may usefully be evaluated with very little context, e.g. no need to provide arguments, no need to reach a subprogram from a main function. The bytecode is effectively an active material: just place it and watch it flow together. Further, partially evaluated results are useful even if our quota times out. Local rewriting simplifies a lot of Awelon project's HCI visions for computation being continuous, omnipresent, easily observed, composed, and tested.

* ABC is **purely functional**. The rewriting semantics are simple, confluent, and context-free. Computations may be freely be cached or replicated. Modulo performance and quotas, ABC computations have deterministic behavior no matter which runtime evaluates them.

* ABC is **concatenative**. Composition of ABC functions is a simple concatenation of their programs. There is no sophisticated binding to perform, no headers or footers to manipulate. Conversely, decomposition is also straightforward, simply cutting programs apart at well defined fragments.

* ABC is **streamable**. There are no pointers or jumps within the bytecode. Unbounded programs can processed iteratively, with code being removed from memory after it has been processed. This supports metaphors of code as a stream, and use of streaming code to model actions over time.

* ABC is amenable to **type checking** and static analysis at the bytecode level. This simplifies safe sharing and linking, and reduces the need for a staged workflows. We can feel confident and secure in widely distributing ABC.

* ABC is **incremental**. Between purity, serializability, and stowage, support for caching is safe and efficient. Systematic caching - together with some simple software patterns involving stowage - enables incremental computation even for massive data structures.

* ABC supports **symbolic structure** with named relationships, resources, and metadata. This is achieved in context of the [Awelon Object (AO)](AboutAO.md) structure-preserving link and evaluation model, and serves as a basis for RESTful [application models](ApplicationModel.md).

## The Bytecode

See also [ABC](ABC.md) and [minimalist ABC](ABC_Minimalist.md). 

### Primitives

ABC has only four primitive combinators `abcd`.

        [B][A]a == A[B]         (apply)
        [B][A]b == [[B]A]       (bind)
           [A]c == [A][A]       (copy)
           [A]d ==              (drop)

In addition to these four combinators, we have the `[]` block structure. A primitive ABC program consists of a stream of `[abcd]`. The blocks must be properly balanced, i.e. every `[` has a matching `]` and vice versa. 

Non-primitive ABC program must be reducible to a primitive ABC program by expanding data representations with their Church encodings, inlining linker tokens, eliminating other tokens. Doing so would hurt performance and hinder debugging, but would not affect the observable results. 

The potential reduction to just four primitives makes ABC relatively easy to comprehend. There aren't any surprises, no corner cases to handle. And these primitives are *useful* even for high level code, easy to understand and to implement efficiently.

### Data Embedding

ABC's data embedding simplifies data entry, extraction, and debugging by presenting data in forms that humans can comprehend and tools can easily access. Natural numbers use eleven operators `#1234567890`. These are designed such that `#42` will compute a Church encoded 42. 

Text literals will use a multi-line embedded encoding:

        "literals are multi-line UTF-8
         they start with character " (32)
         blacklist characters:
            C0 (except LF), DEL, C1
            surrogate codepoints
            replacement character
         linefeed is special character:
            LF SP   new line, drop SP
            LF LF   same as LF SP LF
            LF ~    terminates text
         no other special characters
        ~

Literals encode a finite sequence of UTF-8 bytes. The chosen Church encoding is designed to unify literals and numbers with a more  general concept of iterators or coroutines. For example, if I assume `(foo,bar,baz)` represents a general sequence in a [claw](CommandLine.md) view, then it will be possible to define things such that the following unification applies:

        #5      == (,,,,)
        #3      == (,,)
        #0      == #

        "hello" == (#104, #101, #108, #108, #111)
        ""      == #

        [yield][done](foo,bar,baz)i == foo [[yield][done](bar,baz)i] yield
        [yield][done]#i             == done

        [A]i == A;  i = [][]baad
        

From this, we might derive that `#` is equivalent to `[ad]`. Sequences may generally have a structure `(foo,bar,baz) = [[foo](bar,baz)s]`, terminating with `[[baz]#s]` for some word `s`. 

### Whitespace

SP and LF are permitted in ABC. They have identity semantics, equivalent to the empty program. Essentially, this just permits some lightweight formatting of byte code for easier reading.

### Tokens

Tokens have form `{foo}`, a short text wrapped in curly braces. Tokens enable symbolic extensions to ABC. Semantically, tokens are restricted by the normal rules for ABC: it must be possible to reduce every token to a primitive, purely functional `[abcd]` sequence. Tokens in ABC fall primarily into two groups:

* tokens with *identity* semantics for performance, debugging
* tokens with *linking* semantics for structured development

Tokens with *identity* semantics include seals `{:db}{.db}`, gates `{@foo}`, and annotations `{&par}`. Seals support lightweight symbolic types. Gates are used for active debugging. Annotations serve ad-hoc performance and safety purposes. These are described later in this document. 

ABC's favored linking model is [Awelon Object (AO)](AboutAO.md), which introduces tokens of the form `{%word}` binding to an implicit dictionary. During evaluation, the token is substituted for the word's definition when doing so enables evaluation to proceed. 

### ABC Dictionary

ABC includes a standard dictionary of opcodes defined in terms of expansion to a primitive ABC string. This dictionary will gradually develop with the motivation to improve:

* performance and optimization
* data entry and extraction

The initial standard dictionary consists only of opcodes `#1234567890`. These support both motivations, and were deemed essential for effective data entry. I might also add inline, swap, and compose. 

        [A]i    == A        (inline)
        [A][B]w == [B][A]   (swap)
        [A][B]o == [A B]    (compose)

        i = []wad
        w = []ba
        o = [wai]bb

Eventually, ABC's extended dictionary will support:

* natural number arithmetic
* binary and list processing
* floating point number models
* linear algebra, matrix math
* process network evaluations
* polymorphic records and variants

The ABC standard dictionary will be carefully curated and vetted, and thus moves very slowly. Fortunately, use of [Awelon Object (AO) dictionaries](AboutAO.md) supports similar benefits without rigorous standardization. Use of runtime built-in AO dictionaries can provide an empirical testbed for potential ABC dictionary extensions.

## ABC Evaluation and Performance

### Evaluation Strategy

A general evaluation strategy for ABC is: 

* rewrite outer program
* evaluate inner values

By first rewriting the outer program, we get the most opportunity to apply annotations or drop conditional evaluations. When rewriting has fully completed, we'll have a stable structure containing code and blocks. We can go through each block and repeat the evaluation strategy.

A runtime has discretion to deviate from this strategy. Evaluation order doesn't affect program semantics.

### Big Step Accelerators

ABC performance is achieved primarily by big-step rewriting with known functions. For example, a runtime can evaluate `i` much more efficiently than its expanded definition `[][]baad`. Hence, we might call `i` an accelerator. 

Big-step rewriting becomes especially valuable when working with Church-encoded data structures that might be given a more efficient representation under the hood. For example, natural numbers constructed with `#1234567890` might be encoded as simple machine words. Upon doing so, we may benefit from accelerating `+` so we can directly add compact machine words rather than expand to a Church encoding. Efficient processing of texts, vectors, binaries, matrices, process networks, etc. is feasible.

Useful functions like `i` and `+` will become part of the ABC standard dictionary, effectively becoming bytecodes. However, the ABC standard dictionary moves very slowly and will have a rigorous vetting process. 

In the mean time, we might achieve similar benefits through the [AO layer](AboutAO.md), e.g. by specifying a small, useful dictionary that will be accelerated by our runtime and enabling developers to derive from that.

### Fork/Join Parallelism

ABC's purity supports a simple form of parallelism: we can evaluate many subexpressions at the same time. However, due to various granularity and cache locality concerns, I imagine ABC evaluators will tend to be sequential by default and only introduce parallelism where requested. A simple expression for parallelism is:

        [computation]{&par}

This computation would then run in parallel with other computations in the outer program. A parallel computation may be dropped (operator `d`) which should abort the effort. An attempt to copy (operator `c`) a parallel computation must generally *wait for it to complete*, to avoid issues of duplicating an incomplete computation in a program that halts on quota. 

### Accelerated Process Networks

While use of `{&par}` is useful for simple divide-and-conquer strategies on large data structures, it is not very expressive. For example, it cannot express communicating processes or pipelines. To support more expressive parallelism, I propose that ABC runtimes should *accelerate* evaluation of a bounded-buffer variant of [Kahn Process Networks (KPNs)](https://en.wikipedia.org/wiki/Kahn_process_networks).

A description of a KPN (processes, wires, messages on wires) can be deterministically evaluated into a new description of the same KPN. This evaluation can be naively represented as a pure function. 

A runtime accelerated evaluator can use more conventional queues and threads internally. Further, it is feasible for an external system to interact with a KPN that is still undergoing evaluation - i.e. inject input, extract output, adding a process or wire. These actions need only wait just long enough to resolve deterministically while allowing parallel evaluation to continue. In each case, we would only be forced to wait *just long enough* to resolve deterministically.

*Aside:* Open KPNs are interesting as a potential [alternative to monads for effectful code](KPN_Effects.md), admitting parallel and concurrent effects, and flexible buffering.

### Accelerated Linear Algebra

While KPNs are great for most parallel processing, we'll additionally need to accelerate matrix and vector math to take full advantage of GPGPU or SSE based parallelism.

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

Rewrites can also be used to systematically recognize big step accelerators. For example, we might rewrite `[][]baad` or `[[]]aad` to `i`. 

Unfortunately, rewrite optimizations tend to be ad-hoc and fragile. They are difficult to apply to dynamically constructed code, and are difficult to prove safe in the general case. So I would prefer not to rely on them, just take advantage where it's easy. If developers need to optimize code, they should make explicit a staged, intermediate language.

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

It is my intention that most ABC codebases be strongly, statically type safe as the default. Some bypasses may be permitted, but would be primarily intended for static computation - cf. macro evaluation, below 

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
* `{&tuple3}` - argument has form `[[A][B][C]]`
* `{&aff}` - argument may not be copied
* `{&rel}` - argument may not be dropped
* `{:foo}` - wrapped types, unwrap with `{.foo}`

Given these tokens, we can detect many inconsistencies statically or at runtime. For example, it is obvious that `[A]{&aff}c` is an error, as is `[A]{.foo}` or `#42{&lit}` or `[[A][B]]{&tuple3}`. However, tokens tend to be limited in their expressiveness, oriented on things that are easy to check at runtime.

To support rich, human meaningful type documentation, we should additionally use the [AO layer](AboutAO.md) to attribute type information to subprograms. For example, `word.type` may declare the type of `word` in a manner meaningful both to a type checker and a human. Type descriptions, in this case, would be first-class computable values, and may prove more flexible or extensible 

In any case, the presence of type annotations and declarations can provide a basis for detecting inconsistencies in the ABC program.

### Typing Conditional Behavior

A major concern for any programming language and type system is conditional behavior. In ABC, our conditional behaviors will instead be Church encoded. For example:

        type Bool = True | False
        [onTrue][onFalse] true  i => onTrue
        [onTrue][onFalse] false i => onFalse
        true  = [di]   
        false = [ad]

In practice, of course, computation of a boolean condition is frequently separated from expression of the `[onTrue][onFalse]` conditional paths. This makes it difficult to locally detect type errors or inconsistencies between `onTrue` or `onFalse` contingent on unification of environment `S` or `S'`. Most languages have dedicated syntax like the `if then else` keywords that make it easy to detect errors. 

For ABC, we can leverage annotations in this role. Consider:

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

        [A][B][foo]bb{&tuple3}i

What we're doing here is binding two arguments to the function, asserting that there are three outputs, then inlining those outputs into our main code. Dynamically, the `{&tuple3}` annotation requires a simple bit of reflection. 

        [[A][B][C]]{&tuple3}    =>  [[A][B][C]]

We might expect an ABC system to support `{&tuple0}..{&tuple7}`. Support for `{&tuple1}` is sufficient, but support for a practical range is convenient.

If we determine through static analysis that the `{&tuple3}` annotation is correct, the dynamic check can be bypassed, the annotation potentially removed entirely. So this annotation is suitable for both static and dynamic contexts.

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

### Macro Evaluation

Many useful programming styles are difficult to statically typecheck. Consider:

        "abcx → ax^2 + bx + c" runPoly

Assume this constructs a program that takes four arguments - a, b, c, x - then computes the specified polynomial. The number of arguments we take depends on polynomial's text value. Providing a type judgement for `runPoly` is feasible but non-trivial, requiring sophisticated dependent types and proofs that are inconvenient to embed in a bytecode.

Similar scenarios exist for print formatting and DSLs.

Fortunately, we do not need sophisticated types in these cases and many others. The polynomial text is right there. Statically. So, even if we cannot provide a type for `runPoly`, we might be able to provide a simple type for the larger program if we partially evaluate `runPoly`. We only need some way to tell our system to defer type analysis until after partial evaluation. This is the province of *macros* in many languages. 

For ABC, I propose introducing a `{&macro}` annotation. Usage:

        "abcx → ax^2 + bx + c" runPoly =>
            "abcx → ax^2 + bx + c" compilePoly {&macro} i =>
            [polynomial behavior]{&macro} i =>
            [polynomial behavior]i

At this point, we may halt evaluation and pass the program to our static simple type checker. Macro evaluation is effectively considered *complete* the moment any value exists to its left, i.e. `[A]{&macro} => [A]`. If after evaluation our program still contains `{&macro}` annotations, we might call that program a macro. In the general case, macros are dynamically typed functions, offering a lightweight escape from the rigid static type system whenever an escape is needed.

## Miscellaneous

### Runtime Type Errors

While static type checking is optimal, runtime type errors are possible with `{&macro}` or if we do not bother to type check. In addition, developers may perform dynamic assertions, or express partial functions, in ways that a type checker cannot readily handle.

To simplify error reporting and debugging, we'll want to record known errors in the generated program. To accomplish this, we can use an `{&error}` annotation around a bad subprogram. An evaluator can freely inject the error annotation, delimiting the bad code:

        #42{&lit}       =>  [#42{&lit}]{&error}i
        [A]{&aff}c      =>  [[A]{&aff}c]{&error}i
        {:s}d           =>  [[A]{:s}d]{&error}i
        [[A]]{&tuple2}  =>  [[A]]{&tuple2}{&error}

Developers can freely specify errors, construct ad-hoc error values.

        "TODO: fix foo yesterday!"{&error}

Error values can be passed around, copied, and dropped like normal values. Error values will not be further evaluated. Evaluation becomes stuck insofar as an attempt is made to observe the error value, e.g. the `{&error}i` pattern. Being stuck on an error does not generate further errors. But the remainder of the computation may still evaluate, so we might report many runtime errors rather than the first one.

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
