# Awelon Bytecode

Awelon Bytecode (ABC) provides a computational medium in which ideas, data, and actions can be precisely expressed, shared, and composed. This document describes ABC's idioms and design. For a short summary, see [ABC](ABC.md). 

## Why a New Bytecode?

Awelon project has a vision for human-computer interaction that places users within the environment rather than above or apart from it. This vision includes alternative [application models](ApplicationModel.md) where humans and software agents operate upon a codebase in a shared environment, and a [linked evaluation model](AboutAO.md) that preserves ad-hoc structure meaningful to humans and software agents.

Most bytecodes aren't designed with such applications in mind.

ABC has many unusual features that make it suitable for Awelon project:

* ABC is **easily serialized and rendered**. ABC has a plain-text encoding that directly embeds numbers and literals. Thus, even a simple text editor can provide a useful view for debugging and comprehending code. AO is designed for use with editable views, projectional editors. Sophisticated graphical renderings are feasible.

* ABC **embraces data**. Where most languages shift responsibility for data to an external filesystem or database, ABC prefers to internalize it. Doing so simplifies serialization, persistence, composition, static safety, partial evaluation and staging, transparent procedural generation, and modeling interactive objects. Further, this readily extends to larger-than-memory data via a stowage model and persistent data structures.

* ABC is **evaluated by local rewriting**. ABC subprogram may usefully be evaluated with very little context, e.g. no need to provide arguments, no need to reach a subprogram from a main function. The bytecode is effectively an active material: just place it and watch it flow together. Further, partially evaluated results are useful even if our quota times out. Local rewriting simplifies a lot of Awelon project's HCI visions for computation being continuous, omnipresent, easily observed, composed, and tested.

* ABC is **purely functional**. The rewriting semantics are simple, confluent, and context-free. Computations may be freely be cached or replicated. Modulo performance and quotas, ABC computations have deterministic behavior no matter which runtime evaluates them.

* ABC is **concatenative**. Composition of ABC functions is a simple concatenation of their programs. There is no sophisticated binding to perform, no headers or footers to manipulate. Conversely, decomposition is also straightforward, simply cutting programs apart at well defined fragments.

* ABC is **streamable**. Unbounded programs can processed iteratively. Old code is forgotten even as new code is introduced. This supports soft, fluid metaphors involving streams of code gradually modifying objects in a context. Human actions can also be modeled as streams of code.

* ABC is **incremental**. Between purely functional semantics, large value stowage, and serializability, caching is safe, efficient, and easy. Systematic caching and a few software patterns enables incremental computation, which is important for Awelon project's application models. 

* ABC is amenable to **type checking** and static analysis at the bytecode level. This simplifies safe sharing and linking, and reduces the need for a staged workflows. We can feel confident and secure in widely distributing ABC.

## The Bytecode

See also [ABC](ABC.md) and [minimalist ABC](ABC_Minimalist.md). 

### Primitives

ABC has only four primitive combinators `abcd`.

        [B][A]a == A[B]         (apply)
        [B][A]b == [[B]A]       (bind)
           [A]c == [A][A]       (copy)
           [A]d ==              (drop)

In addition to these four combinators, we have the `[]` block structure. A primitive ABC program consists of a stream of `[abcd]`. The blocks should be properly balanced, i.e. every `[` has a matching `]` and vice versa. 

Non-primitive ABC program must be reducible to a primitive ABC program by expanding data representations with their Church encodings, inlining linker tokens, eliminating other tokens. Doing so would hurt performance and hinder debugging, but would not affect the observable results. 

The potential reduction to just four primitives makes ABC relatively easy to comprehend. There aren't any surprises, no corner cases to handle. And these primitives are *useful* even for high level code, easy to understand and to implement efficiently.

### Data Embedding

ABC's data embedding simplifies data entry, extraction, and debugging by presenting data in forms that humans can comprehend and tools can easily access. Natural numbers use eleven operators `#1234567890`. These are designed such that `#42` will compute a Church encoded 42. Text literals use an embedded encoding:

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

The chosen Church encoding is suitable to unify literals and numbers with more general iterators and coroutines. For example, if I assume `(foo,bar,baz)` represented a general sequence, then the following semantics would apply:

        #5      == (,,,,)
        #3      == (,,)
        #0      == #

        "hello" == (#104, #101, #108, #108, #111)
        ""      == #

        [B][A](foo,bar,baz)i == foo [[B][A](bar,baz)i] A
        [B][A]#i             == B

        [A]i == A;  i = [][]baad

From this, we might derive that `#` is equivalent to `[di]`. Sequences generally have a structure `(foo,bar,baz) = [[foo](bar,baz)s]`, terminating with `[[baz]#s]` for some `s`. (The elements might be flipped if that leads to a cleaner definition. I still need to derive the definition for `s`.)

*Note:* The syntax `(foo,bar,baz)` is not supported at the ABC layer, but a variation is available in the [claw](CommandLine.md) view.

### Whitespace

SP and LF are permitted in ABC. They have identity semantics, equivalent to the empty program. Essentially, this just permits some lightweight formatting of byte code for easier reading.

### Tokens

Tokens have form `{foo}`, a short text wrapped in curly braces. Tokens enable symbolic extensions to ABC. Semantically, tokens are restricted by the normal rules for ABC: it must be possible to reduce every token to a primitive, purely functional `[abcd]` sequence. Tokens in ABC fall primarily into two groups:

* tokens with *identity* semantics for performance, safety, debugging
* tokens with *linking* semantics for structured development

Tokens with *identity* semantics include seals, gates, and annotations. These are described more thoroughly later in this document. Seals support lightweight types and controlled scope. Gates support active debugging - breakpoints, logging, etc.. Annotations serve ad-hoc purpose but require explicit runtime support. Seals, gates, and annotations are discussed in later sections.

ABC's favored linking model is [Awelon Object (AO)](AboutAO.md), which introduces tokens of the form `{%word}` binding to an implicit dictionary. During evaluation, the token is substituted for the word's definition when doing so enables evaluation to proceed. 

### ABC Dictionary

ABC includes a standard dictionary of opcodes defined in terms of an `[abcd]` string. This dictionary will gradually develop with the motivation to improve:

* performance and optimization
* data entry and extraction

The initial standard dictionary consists only of opcodes `#1234567890`. These support both motivations, and were deemed essential for effective data entry. I might also add inline, swap, and compose. Eventually, ABC might include:

* natural number arithmetic
* binary and list processing
* floating point number models
* linear algebra, matrix math
* polymorphic records and variants
* a Haskell style `runST` monad
* a futures and promises monad

The ABC standard dictionary will be carefully curated and vetted, and thus moves very slowly. Fortunately, use of [Awelon Object (AO) dictionaries](AboutAO.md) supports similar benefits without rigorous standardization. Use of runtime built-in AO dictionaries can provide an empirical testbed for potential ABC dictionary extensions.

## ABC Evaluation and Performance

### Evaluation Strategy

The general evaluation strategy for ABC is:

* evaluate outer program
* prioritize annotations
* evaluate final results

Effectively, ABC evaluation defaults to call-by-need internally. But when the toplevel evaluation halts, we'll run back through the program and recursively evaluate every contained block (unless marked `{&lazy}`). This gives us ample opportunity to apply annotations or drop actions conditionally.

[Awelon Object](AboutAO.md) additionally performs staging, implicitly.

### Big Step Accelerators

ABC performance is achieved primarily by big-step rewriting with known functions. For example, consider the useful inline combinator `i`:

        [A]i == A
        i = [][]baad

We can evaluate `i` much more efficiently than `[][]baad`.

Big-step rewriting becomes especially valuable when working with Church-encoded data structures. For example, if we know some function `+` is equivalent to addition when applied to numbers, then we can efficiently rewrite `#23 #19 +` to `#42`. The runtime/compiler can stick to *compact* representations rather than expanding the Church encoding. Efficient processing of massive texts, vectors, binaries, matrices, etc. is feasible.

Useful functions like `i` and `+` may eventually become part of the ABC standard dictionary, enabling use like bytecodes. However, the ABC standard dictionary moves very slowly and will have a slow vetting process. 

In the mean time, we can achieve similar (albeit less portable) benefits by having a runtime provide a built-in [AO dictionary](AboutAO.md), and perhaps recognize built-ins from other popular runtimes. For example, wikilon runtime might provide `{%i@wikrt}` and `{%+@wikrt}`. This dictionary of built-ins should be subject to normal perusal and export.

*Aside:* Use of built-in functions avoids need for sophisticated recognition algorithms. It is possible than an optimizer could recognize `[][]baad` and replace it by `{%i@wikrt}`, but it is unnecessary to do so.

### Rewrite Optimizations

The basic rewrite rules admit some rewrites for performance. For example, `[]a` - applying identity - can be rewritten to the empty program. We get more useful rewrites when we start working at higher levels. For example, we can also eliminate `[i]b` or rewrite `bi => i`. Loop fusions for collections processing like `[F] map [G] map => [F G] compose map` are viable, assuming the `map` function is sufficiently restricted.

I intend that developers can propose rewrite rules, preferably together with a proof of correctness. Meanwhile, rewrite optimizations are at least available for built-in functions and perhaps a trusted subset of the dictionary.

### Compilation

A runtime can provide a `[function]{&jit}` annotation such that we construct a more efficient representation for evaluation of the function. Taken together with [AO dictionaries](AboutAO.md) and a little caching, we can effectively achieve staged compilation for important words. Effective use of JIT may be limited to cases where it's easy to determine static types.

Compiling an executable independent of an ABC runtime is feasible as a case of *program extraction*. Generally, program extraction might translate an ABC program to a Haskell module, JavaScript object, C function, and so on. Extraction requires a well understood program type to integrate with the context. In case of independent executables, the program type will likely be some variant on monadic IO.

Awelon project favors [application models](ApplicationModel.md) that do not rely on program extraction, so local `{&jit}` compilation more appropriate for basic performance concerns. However, I would like to support both techniques.

### Parallelism and Concurrency

ABC's purity supports a simple form of parallelism: we can evaluate many subexpressions at the same time. This can be made explicit by `[computation]{&par}`. This form of parallelism is useful for many divide-and-conquer use cases. Unfortunately, it is not very expressive and not suitable for all problem domains.

To cover parallelism more generally, ABC can leverage big-step accelerators for a deterministic concurrency model. For example, we might model a system based on Kahn process networks or flow-based programming:

* monadic processes with named ports
* declarative wiring of named ports
* a set of message packets in flight

This system is deterministic given simple constraints:

* only one process may write to each wire
* messages pending on wire are FIFO ordered
* reading a wire implicitly waits for data

*Aside:* Flow-based programming would additionally include a natural number bound for each wire, how many pending messages it can carry before forcing a writer to wait. A bound makes it a lot easier to control memory use and ensure CPU fairness.

A pure function can evaluate the system description to its conclusion, producing a deterministic final state. Our client may then reflect upon this result: extract output messages, inject new input messages, rewire if desired, etc.. The system is easily integrated with external effects models, performing effects in batches based on pending messages. After we modify the system, we can again perform evaluation.

We can take this function and *accelerate* it, e.g. using a runtime built-in. 

Accelerating the evaluation function would enable a runtime to evaluate the system in a natural manner, using queues to buffer communications between components. Further, we could evaluate some of the reflection actions - e.g. for injecting and extracting messages. By doing so, we would preserve the runtime's internal representation of the system for common update actions, enabling subsequent evaluations to proceed with appropriately reduced overheads.

I believe this approach is very promising in its simplicity and utility. Between `{&par}` and an `evalFBP` accelerator, ABC can support a great deal of useful parallelism at the CPU layer. To further cover GPGPU or SSE parallelism requires additional accelerators, e.g. oriented around linear algebra. 

### Stowage and Caching

Stowage and caching are covered in greater detail in the [Awelon Object (AO)](AboutAO.md) documentation. But the general summary is:

        [large value]{&stow}    =>  [{%resource}]
        [small value]{&stow}    =>  [small value]
        [computation]{&cache}   =>  [cache value]

Use of stowage supports larger than memory values and computations, offloading bulky data to disk. It might be understood as a more precise, explicit model of virtual memory.

Cache uses a serialized computation as a representation for a value. We'll look up the computation in a table. If found, we replace the computation by its result without performing all the intermediate steps. Otherwise, we perform the computation and heuristically decide whether to add it to the table (based on time/space tradeoff). 

The serialization requirement for caching has some overhead, so developers are encouraged to make suitable 'cache-points' explicit such that the time/space tradeoff is probably a good one. Use of stowage can help ameliorate cache overheads by controlling synchronization costs.

### Lazy Evaluation

By the general evaluation strategy, call-by-need is the default for inputs to a computation. It can be preserved for outputs, too, via explicit use of a `[computation]{&lazy}` annotation. In AO, lazy evaluations are not implicitly cached, which may be a problem if you copy a lazy computation. If need to cache lazy evaluations, make it explicit with `{&cache}{&lazy}` (or `{&lazy}{&cache}`, order makes no difference).

Laziness may freely be used to model infinite data structures - e.g. infinite streams, trees, etc.. Evaluation of such structures would not terminate in an ABC evaluator that doesn't the `{&lazy}` annotation, but all relevant ABC evaluators will support laziness.

### Performance Annotations

Many annotations are used for performance:

* `{&seq}` - evaluate subprogram immediately (shallow)
* `{&seq*}` - as `{&seq}` but evaluates results too
* `{&par}` - parallelize evaluation of subprogram (shallow)
* `{&par*}` - as `{&par}` but evaluates results too
* `{&lazy}` - call by need evaluation for subprogram
* `{&lit}` - force argument to text literal representation
* `{&nat}` - force argument to natural number representation
* `{&stow}` - move value to link layer, away from working memory
* `{&trash}` - drop data but keep placeholder, substructure
* `{&cache}` - use cached result or add result to cache
* `{&opt}` - simplify and optimize a subprogram 
* `{&jit}` - compile a function for runtime internal use

Use of annotations to control staging and compilation has potential to be very effective in giving developers control of the performance of their code. In general, annotations on representation also support type checking and may be effectively used together with accelerators to squeeze the most performance from a representation.


## ABC Assumptions and Idioms

### Annotations

Annotations are tokens with prefix `&` as in `{&seq}`, `{&lazy}`, or `{&cache}`. Annotations affect the ABC evaluator, tuning evaluation to improve performance or debugging. In general, annotations should not be used for anything that cannot be easily handled by an interpreter at runtime. Annotations not recognized by the runtime will be deleted during evaluation.

For debugging purposes, annotations tend to cause a program to fail fast, e.g. `{&nat}` provides a runtime type assertion that our argument is a natural number, or `{&error}` enables runtimes and developers both to record errors in the output. In some cases, these can be leveraged by static type analysis.

### Weak Substructural Types

Substructural types are very expressive for structuring application behavior independently of syntax. For example, while the common resource pattern `open use* close` can be enforced by a structured syntax or RAII pattern, we could instead model this by constructing a linear handle (with sealed data) upon `open` and destroying it later, in the `close` action. This would give us a lot more flexibility to use the resource within the program.

However, I'm not convinced I won't want an escape for substructure, e.g. to model lossy networks or backtracking. So my proposal is to enforce these types only weakly. The proposal is as follows:

        [A]{&rel} == [A]    (mark relevant)
        [A]{&aff} == [A]    (mark affine)
        [A]{&nss} == [A]    (clear marks)

* a block marked relevant may not be dropped
* a block marked affine may not be copied
* a block both affine and relevant is called 'linear'
* block compositions preserve substructure

We can then model a general copy or drop function that ignores substructure by prefixing with `[] cons {&nss}` (and unwrapping our copies). But developers will be able to reason about substructure except where they explicitly choose to bypass it, and it will be a lot easier to search and find every relevant bypass by searching for clients of the `{&nss}` within a subprogram.

### Structural Scopes

ABC provides a simple mechanism for controlling scope of a computation. Assume we have Function that takes two inputs and produces three outputs on the stack. A structurally scoped application can be represented as:

        [A][B][Function]bb{&tuple3}i

What we're doing here is binding two arguments to the function, asserting that there are three outputs, then inlining those outputs into our main code. Dynamically, the `{&tuple3}` annotation requires a simple bit of reflection and otherwise we'll turn our result into an error value. 

        [[A][B][C]]{&tuple3}    =>  [[A][B][C]]

Like `{&nat}` and `{&lit}`, the `{&tuple3}` annotation is also a simple constraint that can contribute to static type analysis and may be eliminated from code if we can statically prove the behavior.

### Value Sealing

Seals come in pairs with a user-defined symbol - `{:foo}` and `{.foo}`. Seals are defined by simple rewrite semantics: `{:foo}{.foo}` will reduce to the empty program. Frequently, seals will be used as symbolic wrappers for individual values, e.g. using `[{:foo}]b`. Seals serve useful roles as lightweight dynamic type declarations and useful rendering hints for structured data.

### Runtime Error Reporting

To simplify error reporting and debugging, we'll want to record known errors in the generated program. There are a number of ways to achieve this. To accomplish this, we can use an `{&error}` annotation around a bad subprogram. An evaluator can freely inject the error annotation, delimiting the bad code:

        #42{&lit}       =>  [#42{&lit}]{&error}i
        [A]{&aff}c      =>  [[A]{&aff}c]{&error}i
        [A]{:s}d        =>  [[A]{:s}d]{&error}i
        [[A]]{&tuple2}  =>  [[A]]{&tuple2}{&error}

Developers can freely specify errors, construct ad-hoc error values.

        "TODO: fix foo yesterday!"{&error}

Error values can be passed around, copied, and dropped like normal values. Error values will not be further evaluated. Evaluation becomes stuck insofar as an attempt is made to observe the error value, e.g. the `{&error}i` pattern. Being stuck on an error does not generate further errors. But the remainder of the computation may still evaluate, so we might report many runtime errors rather than the first one.

### Gates for Active Debugging

By 'active debugging' I mean breakpoints, logging, and other techniques that give programmers a debug view. For active debugging, ABC uses `{@foo}` tokens. The gate name is a user-defined symbol. The behavior of a gate is configured at the evaluator level. For example:

        [A]{@foo}   =>  [A]                 (if gate open or logging)
        [A]{@foo}   =>  [A]{@foo:123}       (if closed, 123 unique)

An open gate passes the value. A logging gate also passes the value, but first copies the value to a configured log. A closed gate acts a like a better breakpoint and stalls computation locally. A useful pattern is `[{@foo}i]b` to shift evaluation stalls from a producer to a consumer. 

It is possible to stall on multiple instances of a closed gate `{@foo}`. For example, if you map a function containing `{@foo}` over a list containing a hundred values, you might have a hundred stalled computations. We'll give each stalled gate a unique identifier in the output, such that it's easy to locate all `{@foo:*}` activations or delete just a few of them by name.

### Program Animation and History Debugging

When debugging, ideally we want a proper spatial-temporal history. Logs provide a lightweight temporal history, but lose a lot of useful spatial context. Breakpoints provide useful spatial context, but lose valuable temporal contexts. But we can have both!

To get both at the same time, we repeatedly:

* evaluate with closed gates
* take a snapshot of program state
* open some stalled gates

Ultimately, we'll have a series of snapshots representing intermediate evaluation states. Conveniently, these states are *deterministically* structured by the breakpoints, rather than having ad-hoc structure from pausing on a quota. This stable structure should greatly simplify stable rendering.

There are many strategies for choosing which gates to open between frames, depending on how we want to focus our animations. E.g. we could always open the rightmost stalled gate, or open the leftmost and rightmost to burn from both ends, or interleave opening all `{@foo:*}` stalled gates with all `{@bar:*}` gates, or hierarchical strategies, and so on.

### Conditional Behavior

We can Church encode true/false as values:

        [OnFalse][OnTrue] true i  == OnTrue
        [OnFalse][OnTrue] false i == OnFalse

        true  = [ad]
        false = [di]    (= #)

Usefully, we don't actually need an 'if' action. Applying true or false does the job. But this is probably *not* the best approach to conditional behavior - it suffers 'boolean blindness'. Instead, it might be better to model a sum type like: `(a + b)` or `Left a | Right b`. Perhaps something like:

        [A→C][B→C] [A]Left i  == A A→C
        [A→C][B→C] [B]Right i == B B→C

We'll need to see how these techniques work in practice. But as a general rule, the 'stack type' after a conditional behavior should be identical on both path. At least until we're ready to start dealing with dependent types.

### Iteration and Termination

ABC can support ad-hoc loops via a fixpoint combinator. However, for many use cases, fixpoint is unnecessary. Numbers, texts, and sequences enable iteration via simple application.

As a general rule, ABC assumes correct computations will terminate. ABC has no use for an infinite computation. Effects require returning with a request for an effect and a continuation. If a static analysis can prove non-termination for a computation, that's certainly an error.

### Type Safety Analysis

ABC evaluation can proceed with type errors, and the worst that happens is nonsense output. Static typing isn't essential. But static analysis and continuous warnings about likely type errors will almost certainly result in a cleaner codebase and better informed programmers.

Tokens such as `{&nat}`, `{&aff}`, `{&tuple3}`, and `{:s}{.s}` provide a lightweight foundation from which static type inference may proceed. At the AO layer, it is possible to add rich type declarations for words. For example, the word `foo.type` could - by convention - be understood as a type declaration for the word `foo`.

Another useful type assumption is that the program's "stack" should have a stable type despite conditional behavior. We can generally consider the `[A][B][C][D]` structure to the left of a computation as a stack for typing purposes. This assumption would limit dependent type inference, but we could add that back in via type declarations.
