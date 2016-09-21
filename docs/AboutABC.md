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

The initial standard dictionary consists only of opcodes `#1234567890`. These support both motivations, and were deemed essential for effective data entry. Eventually, ABC might include:

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

The general evaluation strategy for AO is:

* prioritize annotated strategies
* first evaluate the outer program
* then evaluate within each block

Evaluating the outer program gives us opportunity to apply annotations to blocks, or to drop blocks. Effectively, this strategy gives us call-by-need input, strict output by default, with potential to annotate it into something different.

Because ABC is purely functional and we assume all valid computations terminate, evaluation strategy doesn't have any semantic impact.

### Big Step Accelerators

ABC performance is achieved primarily by big-step rewriting with known functions. For example, consider the an inline function `i`:

        [A]i == A
        i = [][]baad

We can efficiently evaluate `i` much more efficiently than `[][]baad`.

Big-step rewriting becomes especially valuable when working with Church-encoded data structures. For example, if we know some function `+` is equivalent to addition when applied to numbers, then we can efficiently rewrite `#23 #19 +` to `#42`. The runtime/compiler can stick to *compact* representations rather than expanding the Church encoding. Efficient processing of massive texts, vectors, binaries, matrices, etc. is feasible.

Useful functions like `i` and `+` may eventually become part of the ABC standard dictionary, enabling use like bytecodes. However, the ABC standard dictionary moves very slowly and will have a slow vetting process. 

In the mean time, we can achieve similar (albeit less portable) benefits by having each runtime/compiler specify a *built-in* [AO dictionary](AboutAO.md). For example, wikilon runtime might provide `{%i@wikrt}` and `{%+@wikrt}`. This dictionary of built-ins should be subject to normal perusal and export.

*Aside:* Use of built-in functions avoids need for sophisticated recognition algorithms. It is possible than an optimizer could recognize `[][]baad` and replace it by `{%i@wikrt}`, but it is unnecessary to do so.

### Rewrite Optimizations

The basic rewrite rules admit some rewrites for performance. For example, `[]a` - applying identity - can be rewritten to the empty program. 

We get more potential rewrites when we start working at higher levels. For example, we can also eliminate `[i]b` or `[]bi`. Loop fusions for collections processing like `[F] map [G] map == [F G] compose map` are viable if our `map` function is sufficiently restricted.

I intend eventually that developers can propose rewrite rules and simple proofs of correctness for them. Likely, this will occur at the [AO](AboutAO.md) layer, which has both symbolic names room for attributes, metadata, and rewrite strategies. Meanwhile, rewrite optimizations are at least feasible for built-in functions.

### Compilation

A runtime can provide a `[function]{&jit}` annotation such that we construct a more efficient representation for evaluation of the function. Taken together with [AO dictionaries](AboutAO.md) and a little caching, we can effectively achieve staged compilation for important words. Effective use of JIT may be limited to cases where it's easy to determine static types.

Compiling an executable independent of the ABC runtime is feasible as a case of *program extraction*. More generally, program extraction might translate an ABC program to a Haskell module, JavaScript object, or C function. Program extraction requires a well understood program type to integrate the resulting program with its context. In case of independent executables, the program type will likely be some variant on monadic IO.

Awelon project favors [application models](ApplicationModel.md) that do not rely on program extraction, so local `{&jit}` compilation more appropriate for basic performance concerns. However, I would like to support both techniques.

### Parallelism and Concurrency

ABC supports simplistic parallelism: it is easy to evaluate different sub-expressions in parallel. Use of the `{&par}` annotation can make this intention explicit.


The simplest form of parallelism is just to compute one part of a program in parallel with other parts. Use of the `[A]{&par}


A `[A]{&par}` annotation indicates evaluation of `[A]` in parallel with the ongoing parent computation. A parent can parallelize man computations in this manner then do something with the results. This lightweight parallelism is good for many use cases. However, it doesn't enable much communication between parallel threads.

To achieve higher levels of parallelism with communicating tasks, we must:

* model a deterministic, concurrent computation
* accelerate evaluation of this concurrency model 

With this technique, evaluation and communication may proceed in a non-deterministic order without unnecessary bottlnecks, but we 

, but we'll ultimately produce a deterministic result. Non-deterministic identifiers are possible, too.


To cover the more general case, I propose a futures/promises monad with four actions:

* **new**: create a new future promise pair
* **wait**: obtain a future's value
* **fulfill**: 
* **fork**: create a new parallel computation





 multi-threaded Futures/Promises monad, similarly to how Haskell accelerates `runST`. 






, this is ultimately a very weak parallelism model because we cannot express communication between parallel tasks.

 encourage parallel evaluation for a subprogram - i.e. a pool of worker threads could process many `{&par}` requests in parallel. But a lot more parallelism is possible.


Pipeline parallelism requires communication between consumer and producer threads. 

* pipeline parallelism requires multiple communicating 

use of `{&par}` still requires I believe we can do better than that. 

A powerful pattern for parallelism is futures and promises, such that we create

### Explicit Laziness



### Performance Annotations

Annotations can be used for performance in many ways:

* `{&seq}` - evaluate subprogram immediately 
* `{&par}` - parallelize evaluation of subprogram
* `{&lazy}` - delay computation of subprogram
* `{&lit}` - force argument into text literal representation
* `{&nat}` - force argument into natural number representation
* `{&stow}` - move value to link layer, away from working memory
* `{&cache}` - use cached result or add result to cache
* `{&memo}` - same as `{&cache}` but with volatile memory
* `{&opt}` - simplify and optimize a subprogram 
* `{&jit}` - compile a function for runtime internal use

Use of annotations to control staging and compilation has potential to be very effective in giving developers control of the performance of their code. In general, annotations on representation also support type checking and may be effectively used together with accelerators to squeeze the most performance from a representation.

*Note:* Use of `{&lazy}` above may be closer in nature to to call-by-need. If you want something closer to conventional laziness where we cache the result, use `{&memo}{&lazy}`.

## ABC Assumptions and Idioms

### Computations Terminate

ABC is purely functional. Any effects are explicit, e.g. using a free monadic effects models. Modulo debugging, to extract useful output from a computation requires that it terminate properly. Consequently, the assumption is that all computations terminate. With respect to optimizations, loop fusion, parallelization, laziness, and so on we'll simply assume every expression terminates and bypass the whole issue of denotational divergence. If any prover determines that a computations won't terminate, we can report that as an error. 

Of course, ABC is Turing complete. We cannot prove termination in the most general case. But I suspect termination can be proven for most expressions.

*Aside:* Even with a termination guarantee, we'll generally need to run under a quota. Unless we can prove a linear-time guarantee or similar.

### Church Encoded Data

Natural n

        #1234567890             (natural numbers)

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


### Conditional Behavior



### Static Type Safety

To simplify static type safety analysis, we'll generally operate on an assumption: that the program stack should have statically predictable types.


### Iteration and Loops

ABC can support ad-hoc loops via a fixpoint combinator. However, for many use cases, iteration is unnecessary. F ABC numbers and texts support iteration directly:

        








        [A]{&annotation}    (performance, safety, misc.)
        [A]{@gate}          (active debugging)
        {:seal}{.seal}      (stream sealing)
        {%word}             (AO dictionary linking)




### Numbers, Literals, and Sequences

### Runtime Error Reporting




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

