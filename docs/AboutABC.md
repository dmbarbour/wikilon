# Awelon Bytecode

Awelon Bytecode (ABC) provides a computational medium in which ideas, data, and actions can be precisely expressed, shared, and composed. This document describes ABC's idioms and design. For a short summary, see [ABC](ABC.md). 

## Why a New Bytecode?

Awelon project has a vision for human-computer interaction that places users within the environment rather than above or apart from it. I am exploring [alternative application models](ApplicationModel.md) where humans and software agents operate upon a "codebase" that is simultaneously a "database", offering a simple foundation for state. The ability to render, manipulate, compose, decompose, evaluate, and share software artifacts is essential. 

Most bytecodes aren't designed with such applications in mind.

## Distinguishing Features

ABC has many unusual features that make it suitable for Awelon project:

* ABC **internalizes data**. Data is modeled as part of the code rather than eschewing responsibility to a separate filesystem or database. This simplifies composition, validation, partial evaluation, parallel evaluation, staging, and procedural generation of data artifacts. Taken together with caching and linking, it is feasible to work with very 'big' data.

* ABC is **renderable**. ABC has a plain-text encoding that directly embeds numbers and literals. Thus, even a simple text editor can provide a useful view for debugging and comprehending code. Sophisticated *graphical* renderings are feasible via tokens for rendering hints.

* ABC is **evaluated by rewriting**. Any ABC subprogram may usefully be evaluated and rendered. This addresses HCI goals of computation being something continuous and omnipresent, code and data being active artifacts that apply upon contact rather than waiting for a 'main' control flow. It enables immediate, useful examples. Further, it allows us to produce useful outputs even if evaluation halts on quota limits.

* ABC is **purely functional**. The rewriting semantics are simple, confluent, local, and context-free. Computations may be freely be cached, replicated, recomputed. ABC subprograms have predictable behavior even when shared or evaluated in a different system.

* ABC is **concatenative**. Composition of ABC artifacts is as easy to express as concatenating their subprograms. There is no sophisticated binding to perform, no headers or footers to manipulate. Conversely, decomposition is also straightforward - just extract a subprogram.

* ABC is **streamable**. Unbounded programs can processed iteratively with old code being forgotten even as new code is introduced. This supports soft, fluid metaphors for many applications of code - e.g. code flows over objects to modify them, or human actions as streams of code. 

* ABC is **pervasively parallel**. Rewriting means we aren't constrained by a specific control flow, i.e. arbitrary subprograms may evaluate in parallel, and partial outputs from one subprogram might be immediately pipelined as inputs to the next. Pervasive parallelism enables software artifacts to scale to many developers. It also simplifies debugging, since we aren't constrained by control flow to experience a single error in the code per evaluation.

* ABC is amenable to **type checking**, static and dynamic, at the bytecode level. This simplifies safe sharing and linking, and reduces the need for a staged workflow. We can feel confident and secure in widely distributing ABC.

## The Bytecode

See [ABC](ABC.md) and [minimalist ABC](ABC_Minimalist.md) for details. 

But the very short summary:

        (PRIMITIVES)

        [B][A]a == A[B]         (apply)
        [B][A]b == [[B]A]       (bind)
           [A]c == [A][A]       (copy)
           [A]d ==              (drop)
        
        (STANDARD ACCELERATORS)

        [A]i == A               i = [][]baad
        #1234567890             (eleven accelerators)

        (DATA EMBEDDING - Church encoded sequences)

        "literals are multi-line
         indentation based
         LF (10) is special character:
            LF SP   new line, drop SP
            LF LF   same as LF SP LF
            LF ~    terminates text
         no other special characters
         may contain "double quotes"
         capable of embedding ABC
        ~
        
        #42
        #108

        (TOKENS)

        [A]{&annotation}    (performance, safety, misc.)
        [A]{@gate}          (active debugging)
        {:seal}{.seal}      (stream sealing)
        {%word}             (AO dictionary linking)
        {'resource}         (runtime value linking)

## Performance

Performance was sacrificed to make room for those other nice features. Naive interpretation of ABC is unlikely to be performance competitive with Java bytecode or CLR, which have a structure closer to 'the machine'. 

There are a number of methods to ameliorate performance issues with ABC, but many of them are *long term* and will take a while to develop. A solution is to simply use ABC where it's efficient (e.g. producing an SVG data structure) and offload low-level model processing (e.g. rendering of SVG). I aim to gradually expand the scope of the former, e.g. via use of accelerators and annotations to shift subprograms to GPGPU or FPGA computing.

### Accelerators

We can recognize common sequences of bytecode and replace them by another opcode, one that is easier to hand implement or use in further optimizations. For example:

        [B][A]w == [A][B]       w = []ba
        [A]i    == A            i = []wad

ABC shall include a dictionary of standard accelerators to simplify rendering and performance. That dictionary will evolve slowly. Experimental accelerators will generally be internal to our interpreters or compilers.

We might develop accelerators oriented around:

* floating point number models
* linear algebra (vector and matrix math)
* polymorphic records and variants
* a Haskell style `runST` monad
* a futures and promises monad
* DSLs for pipelining computations

The main difficulty with the *accelerators* approach is that it will take a long time to develop. This is a slow start, long term approach to performance. Though, we can at least isolate a few subprograms that *really* benefit from acceleration.

### Performance Annotations

Developers may leverage annotation tokens for performance. Most performance annotations should be treated as *strong* requests: implement or fail fast. This gives developers effective control over performance. However, heuristic or discretionary performance annotations may also be useful.

Performance annotations can do things like:

* `{&par}` - parallelize a subprogram
* `{&seq}` - force evaluation of a subprogram
* `{&asap}` - prioritize a *deep* computation
* `{&lit}` - assert literal type, use runtime's embedded text rep
* `{&nat}` - assert natural number, use runtime's natural number rep
* `{&stow}` - move a large value out of working memory, load it lazily
* `{&load}` - advise that a stowed will soon be needed
* `{&cache}` - use caching implementation for function
* `{&opt}` - simplify and optimize a subprogram 
* `{&jit}` - compile a function 

Use of annotations to control staging and compilation has potential to be very effective in giving developers control of the performance of their code. In general, annotations on representation also support type checking and may be effectively used together with accelerators to squeeze the most performance from a representation.

### Separate Compilation and Linking

ABC readily supports value-layer linking, e.g. by secure hash of the subprogram being linked. It is very convenient and effective to associate `{&jit}` compilations and the like with the same secure hash, such that the runtime has the opportunity to cache compilation efforts and also get easy structure sharing.

For performance, compilation will be essential. Separate compilation and linking is possible by compiling at linker boundaries, i.e. such that when a runtime is asked to 'link' some code it has the opportunity to link either the ABC bytecode or a highly optimized representation from a trusted cache.

## ABC Assumptions and Idioms

This section discusses a few high level properties of ABC's design and context that cannot readily be inferred from the earlier operators or tacit concatenative streamable structure.

### Fast and Loose Reasoning for Termination

ABC favors a philosophy of 'fast and loose reasoning' about termination properties. (cf. [Fast and Loose Reasoning is Morally Correct](http://www.cse.chalmers.se/~nad/publications/danielsson-et-al-popl2006.html), Danielsson, Hughes, et al. POPL 2006.) The idea is that - with respect to optimizations, equational laws, rewriting, loop fusion, parallelization, laziness - we should assume every subprogram terminates (or is intended to). 

To help enforce and remind developers of this philosophy, ABC compilers should perform termination analysis, and long-running programs should be modeled as RDP behaviors or incremental processes (e.g. `μProc.[a→(Proc*b)]`).

ABC is Turing complete, so we can't expect a decision to be reached for every program. However, termination analysis is really about avoiding errors in reasoning. And *most errors aren't subtle*. Accidental non-terminations often can be detected, and many developers will write reusable software such that analysis passes without a 'could not prove' warning.

Termination is a weak property. In practice, we often wish to reason about performance characteristics: latency, memory requirements, resource usage. Ackermann function terminates, but not within reasonable bounds. However, termination is a great start.

### Flexible Safety Analysis

ABC is amenable to static typing, but I haven't decided on any particular types model or inference algorithms. A specific context (such as an AO dictionary) might require code pass certain safety inspections before admitting it, which can provide a simple basis for type safe programming.

An interesting possibility is to model types as structured identity-functions. For example, an integer might be idiomatically represented as `[#+]` (adding zero). And `[vr[A-type]$w[B-type]$lc]` might similarly represent the type for an `(A*B)` pair. Block types could be modeled in terms of composing type assertions. Use of an annotation like `{&type-id} :: ∃a.([a→a]*e)→([a→a]*e)` could both tell our compiler that we have a type description and that it can be wholly eliminated by the optimizer. Dynamic types could feasibly be named by the secure hash of such functions. 

The potential for inference and injecting explicit declarations enables ABC to flexibly approach static type safety.

### Annotations

Annotations are tokens for safety or performance. Annotations are indicated by use of an `&` prefix in the token, e.g. `{&text}`. Annotations must have *identity* semantics - they do not affect the logically observable behavior of a correct program. However, annotations may cause a program to fail, e.g. if the program is incorrect or the annotations are used incorrectly.

The intention is that a runtime should be able to ignore annotations it does not know about, or that are disabled for a particular use case. Some ideas for what annotations might do:

* support parallel or lazy computation
* stow and load values from a backing store
* hints for type or termination checking
* debug output, breakpoints, location tracking
* optimized representations (e.g. compact text) 
* trash data while perserving substructure
* compile or JIT computed blocks of code 
* move computations and data to a GPGPU
* support and enable memoization or caching

For example, an annotation `{&trash}` might indicate that a value will never be observed in the future, enabling its memory to be recycled while leaving a lightweight placeholder. 

Annotations may be paired, e.g. such that a `{&fork}` to parallelize a computation must be paired with a `{&join}` to access its result. Pairing annotations can simplify a runtime implementation, but unfortunately does resist transparency of optimizations. 

Annotations may specify types, i.e. they can be *typed* identity functions. A simple case is that `{&text}` might require a `∀e.(text*e)` argument, and would compact the text value and ensure it serializes as embedded text.

### Value Sealing

Discretionary sealers 'seal' or 'unseal' a value with a symbol.

        {:u} :: (a*e) → ((u:a)*e)     `:` for seal
        {.u} :: ((u:a)*e) → (a*e)     `.` for unseal
        #42{:foo}                     discretionary sealed data

This serves a role similar to type wrappers, simplifying type checking and flexible rendering. However, this isn't very effective for security (at least not by itself). 

*Related:*

Cryptographic sealing for open distributed systems seems feasible. Something like:

        {$:AES} :: ((key * a) * e) → ((sealed a) * e)
        {$&AES} :: (crypto-val * e) → ((sealed a) * e)
        {$.AES} :: ((key * (sealed a)) * e) → (a * e)

However, there are many challenges surrounding cryptographic sealing regarding potential interaction with lazy or parallel futures, value stowage, and garbage collection. And they aren't a good fit for some proposed [application models](ApplicationModel.md) with AO/ABC.

### Gates for Active Debugging

In [minimalist ABC](ABC_Minimalist.md) I proposed use of `{@foo}` gates as a basis for debugging. The behavior of a gate is configured at the runtime level: a 'closed' gate acts as a breakpoint, an 'open' gate just passes data, a 'log' gate will record data to a log, and a 'trace' gate will tag data so we can work out where it's been.

### Value Stowage

A simple 'value stowage' supports larger-than-memory data and computations, separate compilation and dynamic linking, efficient distributed data, etc.. This model is achieved through just two annotations:

        {&stow} :: (v * e) → ((stowed v) * e)
        {&load} :: ((stowed v) * e) → (v * e)

Stowing a large value pushes it into a backing storage, e.g. a filesystem or database. A lightweight placeholder is left in the value's stead. Loading a value moves data in the opposite direction. The backing store will frequently be many orders of magnitude larger than the memory allocated to a computation, and it may use independent garbage collection model.

By modeling tree structures (e.g. a trie, B+ tree, or log-structured merge tree) and stowing tree nodes, it is feasible to represent massive databases as first-class values. Finger-tree ropes can serve as an effective basis for logs and queues. Patterns such as loading code just before inlining it (`{&load}vr$c`) serve as a logical basis for dynamic linking, and might access a compiled and cached representation of the loaded code.

To get the most out of value stowage, it must be used together with a persistence or caching model, such that the data may be reused across many independent computations without recomputing it. Structure sharing might further augment stowage, saving space when a value is computed many times. In a distributed system, stowed values could serve a role similar to hypermedia, enabling code to reference many large values without immediately downloading it. 

A proposed serialization for stowed value placeholders is use of a token:

        {'kf/scope/resourceID}

The prefix `'kf` indicates a stowed value (`'`) with relevant and affine substructural properties (`kf`). The scope helps constrain the search space, and the rest identifies the value. The resourceID should include a secure HMAC if otherwise forgeable (like an incremental number). The limit on token size (255 bytes) should not be an issue in practice, though hierarchically structured resource identifiers like URLs are not viable.

*Note:* Stowage is algebraic. If for some silly reason we `{&stow}{&stow}` a value, we would conversely need to load it twice. However, values smaller than a heuristic threshold don't need to be backed to disk at all. The second `{&stow}` command could instead be represented as a lightweight value wrapper.

## Awelon Bytecode Deflated (ABCD)

I plan to develop an extended bytecode above ABC: ABC Deflated, or ABCD.

Characters ABC doesn't use will be mapped to common, useful, optimizable ABC subprograms. This gives us a form of standard dictionary compression, allowing larger ABC programs to be represented with fewer characters. Further, ABCD subprograms shall generally be accelerated by interpreters, and the operations may allow us to easily infer optimized representations for data structures (e.g. binaries, vectors, matrices, floating point). Ultimately, programs constructed from well-defined subsets of ABC may be compiled to GPGPUs and other devices (cf. Haskell's [accelerate](https://hackage.haskell.org/package/accelerate) package).

ABCD allows ABC to iteratively grow into a high-performance language while preserving ABC's simple semantics. Every ABCD program expands trivially into an ABC program. 

ABCD will greatly improve the performance of ABC interpreters. We gain efficiency by doing more useful work per opcode. If we have opcodes for multiplying matrices, and our program does a lot of that, we'll be spending a greater portion of our time in our pre-compiled matrix multiplication code rather than parsing opcodes and shuffling data. ABCD has great potential to enable high performance *Collections Oriented Programming* (see below). Because interpreters are much simpler to implement or integrate than compilers, ABCD will also improve the accessibility of ABC.

ABCD requires careful standardization to support code distribution (streaming, ABC resources). Fortunately, it is easy to experiment with ABCD-like accelerators within a runtime, assuming some form of cached compilation or JIT. Thus, we can take advantage of this technique long before we standardize, and we can empirically demonstrate the utility of candidate ABCD opcodes. 

Because ABC is encoded in UTF-8, we have sufficient room for ABCD extensions. A few hundred ABCD opcodes should cover a very broad variety of use cases for compression and acceleration.

### ABCD for Collections Oriented Programming

Languages designed for collections oriented programming, such as J, K, APL, or SQL, frequently achieve excellent performance (within their domain) even with an interpreter. The bulky computations dominate the interpreter overheads, and may even allow an interesting degree of parallelism or use of GPU computing. Scalar manipulations, e.g. adding or comparing individual numbers, tend to be much less efficient when interpreted. 

During design and development of Awelon Bytecode, it has been very tempting to pursue collections oriented operations and data types. I ultimately decided against this due to the complexity it would add, e.g. needing to make extra choices about which collections to support (arrays, matrices, lists, streams, relations, etc.) and provide the panoply of operators for manipulating them. The conception of ABCD also contributed significantly to my willingness to kick collections oriented features into the future. 

Simply put, an important subset of ABCD will be collections oriented.

An ABCD interpreter can provide a few under-the-hood data types for compact representations of arrays, matrices, and so on. Where necessary, or where guided by annotation, the interpreter can convert between these compact representations and the more conventional composite of products and sums. A compiler can do similar, of course.

When we know collection values are affine, we can easily implement update functions (e.g. update the 4th element of the array, returning the modified array) using mutation, thus enabling performance properties similar to imperative code. Further, processing of collections can be accelerated, parallelized, and optimized based on high-level knowledge of data structures and the functions involved. Potentially, even serialization might be specialized, e.g. represent a vector of integers as a compact binary string followed by a few ABCD operators to interpret it back into a vector.

ABCD offers a viable path to performant collections oriented programming.

## Meta

Where appropriate, the generic internet media type for ABC should be `application/vnd.org.awelon.abc`. File extension might be `.abc`, though I'd like to avoid maintaining files full of ABC code. Specific, constrained variants of ABC (e.g. restricting tokens) might use a more specific media type.

