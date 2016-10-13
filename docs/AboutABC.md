# Awelon Bytecode

Awelon Bytecode (ABC) provides a computational medium in which ideas, data, and actions can be precisely expressed, shared, and composed. This document describes ABC's idioms and design. For a short summary, see [ABC](ABC.md). 

## Why a New Bytecode?

Awelon project has a vision for human-computer interaction that places users within the environment rather than above or apart from it, where all HCI is modeled as software development and live programming. This vision includes non-conventional [application models](ApplicationModel.md) where humans and software agents operate upon a codebase in a shared environment, and a [linked evaluation model](AboutAO.md) that preserves ad-hoc structure meaningful to humans and software agents.

Most bytecodes aren't designed with such applications in mind.

What sets ABC apart from other bytecodes:

ABC is evaluated by local, confluent rewriting without any unrepresentable terms. Hence, *we may take any ABC subprogram and evaluate it* into another representation of the same program. This is similar to how `2 + 3` evaluates to `5`. 

ABC evaluation is pure, without side-effects. Hence we're free to cache, replicate, or replay any computation for reasons of performance or comprehension. Effects and IO must be handled by external software agents, if at all - e.g. via [process networks](KPN_Effects.md) or RESTful [application models](ApplicationModel.md).





ABC avoids entanglement with its computing environment. It
 We can take any ABC subprogram and evaluate it into another ABC representation of the same subprogram (much like `2 + 3` evaluates to `5`). Further, that representation 



ABC is purely functional


ABC avoids entanglement with its computing environment. There are no pointers, no jumps, no variables, no side-effects. This greatly simplifies problems of serializing, distributing, and persisting computations. The tradeoff is the need for any effects to be carried out by external software agents, e.g. modeling monadic effects or a [process network](KPN_Effects.md) to model input and  IO.

ABC is evaluated by local, confluent, context-free rewriting. 

ABC is concatenative




ABC is concatenative. Composition of functions is easily represented by concatenation of bytecode.



 A 

ABC is serializable as plain UTF-8 text, and hence may be viewed with normal text editors. 

ABC avoids entanglement with its computing environment. 

A first class function or incomplete computation is easily  

ABC is easily serialized, shared, persisted, rendered. Relevantly, ABC avoids entanglement with the environment, and first-class functions within ABC 


* Easily serialized, shared, persisted, and rendered. Relevantly, ABC avoids entanglement with the environment



* ABC is **easily serialized and rendered**. ABC has a plain-text encoding that directly embeds numbers and literals. Thus, even a simple text editor can provide a useful view for debugging and comprehending code. AO is designed for use with editable views, projectional editors. Sophisticated graphical renderings are feasible.

* ABC **embraces data**. Where most languages shift responsibility for data to an external filesystem or database, ABC prefers to internalize it. Doing so simplifies serialization, persistence, composition, static safety, partial evaluation and staging, transparent procedural generation, and modeling interactive objects. Further, this readily extends to larger-than-memory data via stowage and caching.

* ABC is **evaluated by local rewriting**. ABC subprogram may usefully be evaluated with very little context, e.g. no need to provide arguments, no need to reach a subprogram from a main function. The bytecode is effectively an active material: just place it and watch it flow together. Further, partially evaluated results are useful even if our quota times out. Local rewriting simplifies a lot of Awelon project's HCI visions for computation being continuous, omnipresent, easily observed, composed, and tested.

* ABC is **purely functional**. The rewriting semantics are simple, confluent, and context-free. Computations may be freely be cached or replicated. Modulo performance and quotas, ABC computations have deterministic behavior no matter which runtime evaluates them.

* ABC is **simple**

* ABC is **concatenative**. Composition of ABC functions is a simple concatenation of their programs. There is no sophisticated binding to perform, no headers or footers to manipulate. Conversely, decomposition is also straightforward, simply cutting programs apart at well defined fragments.

* ABC is **streamable**. There are no pointers or jumps within the bytecode. Unbounded programs can processed iteratively, with code being removed from memory after it has been processed. This supports metaphors of code as a stream, and use of streaming code to model actions over time.

* ABC is amenable to **type checking** and static analysis at the bytecode level. This simplifies safe sharing and linking, and reduces the need for a staged workflows. We can feel confident and secure in widely distributing ABC.

* ABC is **incremental**. Between purity, serializability, and stowage, support for caching is safe and efficient. Systematic caching - together with some simple software patterns involving stowage - enables incremental computation even for massive data structures.

* ABC supports **symbolic structure** with named relationships, resources, and metadata. This is achieved in context of the [Awelon Object (AO)](AboutAO.md) structure-preserving link and evaluation model, and serves as a basis for RESTful [application models](ApplicationModel.md).

* ABC is **extensible**, via the link layer. Defining new operators is no more difficult than defining new functions. An interpreter or compiler can optimize a common set of functions to get effective performance.

## The Bytecode

See also [ABC](ABC.md). 

### Primitives

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

### Extension

ABC is extended through the link layer. In AO, `xyz` is effectively shorthand for `{%x}{%y}{%z}`. Reducing common operations to a single character enables compact representation of programs. An AO dictionary, thus, might define the following:

        [B][A]w == [A][B]   (swap);     w = {&a2}[]ba
        [A]i    == A        (inline);   i = []wad
        [B][A]o == [A B]    (compose);  o = [ai]bb

This idea is intended to be coupled with the performance idea of accelerated dictionaries, where an evaluator has hand-optimized implementations for a common subset of definitions. This would allow developers to get built-in primitive performance for a growing set of common operations, without a centralized standardization process.

### Data Embedding and Extraction

Leveraging the ABC dictionary, we can define eleven operators `#1234567890` such that `#` introduces a new zero value, and each digit performs the Church encoded equivalent of "multiply by ten, add digit". Thus, `#42` would construct the Church encoded 42. Relevantly, a runtime can recognize a de-facto standard encoding for numbers then also *output* number results using these operators, i.e. such that `#7 #6 *` may evaluate to `#42`.

If we additionally accelerate these operations, our natural number might be represented within a simple machine word under the hood. And we might further accelerate basic arithmetic, and output the data in terms of the same eleven opcodes, such that `#3 #4 +` evaluates to `#7`. 

Text embedding will need a syntactic sugar:

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

I haven't settled on a particular model for literals, though I might favor something like `"hello" => [[#104] "ello" s]`. One general goal is that it should be feasible to unify numbers, texts, and more generic command sequences (cf. [claw](CommandLine.md)) as having a common structure - perhaps an iterator over a sequence. 


### Whitespace Formatting

SP and LF are permitted in ABC. They will have identity semantics, equivalent to the empty program. Essentially, this just permits some lightweight formatting of bytecode for easier reading.

## ABC Evaluation and Performance

ABC evaluation rewrites an ABC program into a different representation of the same program. A general evaluation strategy for ABC is: 

* rewrite outer program
* evaluate inner values

By first rewriting the outer program, we get the most opportunity to apply annotations or drop conditional evaluation. When rewriting has fully completed, we'll have a stable structure containing code and blocks. We can go through each block and repeat the evaluation strategy.

A runtime has discretion to deviate from this strategy. Evaluation order doesn't affect program semantics.

### Accelerated Dictionary

A runtime can optimize an anonymous dictionary of common words, such that they achieve near native performance or use more efficient representations for things like natural numbers. Developers may then derive from this dictionary. Some useful areas to pursue big step acceleration:

* natural number arithmetic
* binary and list processing
* key-value records or databases
* floating point number models
* linear algebra, matrix math
* process network evaluation

Acceleration is achieved in cooperation with the link layer, which we also use to extend the bytecode. Effectively, we just substitute known words with hand-optimized implementations in the interpreter. Acceleration can easily work together with JIT, with acceleration covering the more tricky requirements.

### Fork/Join Parallelism

ABC's purity supports a simple form of parallelism: we can evaluate many subexpressions at the same time. However, due to various granularity and cache locality concerns, I imagine ABC evaluators will tend to be sequential by default and only introduce parallelism where requested. A simple expression for parallelism is:

        [computation]{&par}

This computation would then run in parallel with other computations in the outer program. A parallel computation may be dropped (operator `d`) which should abort the effort. An attempt to copy (operator `c`) a parallel computation must generally *wait for it to complete*, to avoid issues of duplicating an incomplete computation in a program that halts on quota. 

### Accelerated Process Networks

While use of `{&par}` is useful for simple divide-and-conquer strategies on large data structures, it is not very expressive. For example, it cannot express communicating processes or pipelines. To support more expressive parallelism, I propose that ABC runtimes should *accelerate* evaluation of a bounded-buffer variant of [Kahn Process Networks (KPNs)](https://en.wikipedia.org/wiki/Kahn_process_networks).

A description of a KPN (processes, wires, messages on wires) can be deterministically evaluated into a new description of the same KPN. This evaluation can be naively represented as a pure function. 

A runtime accelerated evaluator can use more conventional queues and threads internally. Further, it is feasible for an external system to interact with a KPN that is still undergoing evaluation - i.e. inject input, extract output, adding a process or wire. These actions need only wait just long enough to resolve deterministically while allowing parallel evaluation to continue. In each case, we would only be forced to wait *just long enough* to resolve deterministically.

*Aside:* Open KPNs are interesting as a potential [alternative to monads for effectful code](KPN_Effects.md), admitting parallel and concurrent effects, and flexible buffering. They're also a potential alternative to OOP-style objects, since we can model invoking a KPN by injecting some data then extracting results, and passing KPNs around as first-class values.

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

What we're doing here is binding two arguments to the function, asserting that there are three outputs, then inlining those outputs into our main code. Dynamically, the `{&t3}` tuple annotation requires a simple bit of reflection. 

        [[A][B][C]]{&t3}    =>  [[A][B][C]]

We might expect an ABC system to support `{&t0}..{&t7}`. Support for `{&t1}` is sufficient, but support for a practical range is convenient.

If we determine through static analysis that the `{&t3}` annotation is correct, the dynamic check can be bypassed, the annotation potentially removed entirely. So this annotation is suitable for both static and dynamic contexts.

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

### Arity Annotations

Arity annotations are defined by a set of rewrite rules of form:

        [A][B]{&a2}     == [A][B]
        [A][B][C]{&a3}  == [A][B][C]
        ...

Each annotation simply has the given arity. Arity annotations don't say anything about the surrounding computation. An AO runtime should support at least the practical range `{&a1}`..`{&a7}`. More than seven is generally impractical without a parameter object.

Arity annotations offer a simple way to control evaluation, e.g. to avoid copying or binding before there is need to do so. This is most important in context of [AO](AboutAO.md) where arity annotations can help control lazy linking, waiting until sufficient arguments are available for evaluation to proceed.

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

### Accelerated Fixpoint

Fixpoint is an important function for general purpose programming. It enables ad-hoc loops. It is not difficult to define fixpoint functions in ABC. One simple option is `[cb]ocb` (with `o = [ai]bb, i = [][]baad`).

        [A]y    ==  [[A]y A]  (fixpoint);   y = [cb]ocb

        [A]y    ==  [A][cb]ocb     (def y)
                ==  [cb A]cb       (def o)
                ==  [cb A][cb A]b  (def c)
                ==  [[cb A]cb A]   (def b)
                ==  [[A]y A]       (eqv (step 0, step 2))

However, this definition has the unfortunate characteristic of copying `A` prematurely. We can tune a bit for explicit laziness and to avoid unnecessary copies.


Whatever our choice, our runtime should aim to accelerate use of fixpoint such that unnecessary copies are not performed, we perform the bare minimum of steps, and we recover the fixpoint operator in the final output. Because this will be a common operation in practice.


#### Z combinator - not what I want 
        [Arg][A]Z == [Arg][[A]Z]A

        Z = [[c]a[{&a3}ci]bbwi]{&a3}ci

        [Arg][A]Z 
            == [Arg][A][[c]a[{&a3}ci]bbwi]{&a3}ci                   (def Z)
            == [Arg][A][[c]a[{&a3}ci]bbwi]ci                        (arity)
            == [Arg][A][[c]a[{&a3}ci]bbwi][[c]a[{&a3}ci]bbwi]i      (def c)
            == [Arg][A][[c]a[{&a3}ci]bbwi][c]a[{&a3}ci]bbwi         (def i)
            == [Arg][A]c[[c]a[{&a3}ci]bbwi][{&a3}ci]bbwi            (def a)
            == [Arg][A]c[[[c]a[{&a3}ci]bbwi]{&a3}ci]bwi             (def b)
            == [Arg][A]c[Z]bwi                                      (def Z)
            == [Arg][A][A][Z]bwi                                    (def c)
            == [Arg][A][[A]Z]wi                                     (def b)
            == [Arg][[A]Z][A]i                                      (def w)
            == [Arg][[A]Z]A                                         (def i)

Or we could try to use the `{&lazy}` annotation on the `[[A]Z]` construct.

