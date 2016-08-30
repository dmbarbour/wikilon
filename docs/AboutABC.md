# Awelon Bytecode

Awelon Bytecode (ABC) is a bedrock component of the Awelon project. ABC consists of a small number of operators, embedded texts, blocks of ABC (first class functions), and ad-hoc symbolic *tokens* that are primarily used for performance annotations, typechecking, debugging, linking, and similar roles. ABCD extends ABC with a standard dictionary of common functions defined in terms of ABC primitive operators, simplifying interpreted performance and compression.

*Note:* ABC is undergoing a *potential* revision, one that simplifies it further. Whether this proceeds will depend on how well it works out in practice. See [the proposed model](ABC_Minimalist.md). This could have a huge impact on some aspects of what is described below, but not so much on the larger ideas.

**Why a new Bytecode?**

Most bytecodes get entangled with the evaluation environment due to use of aliasing, state, pointers, and jumps. While this can aide performance, those bytecodes a poor fit for use cases that involve treating computations, objects, and procedural generation of data as first-class values. Further, they are a poor fit for the 'vision' of Awelon project. 

Awelon project explores a different vision for how humans should interact with computers, one oriented towards augmented reality where humans can exist 'within' a computation rather than existing 'above' the computation and apart from it. For example, a *streamable* code can easily represent user actions. And an [application model](ApplicationModel.md) that is amenable to representation in a codebase makes it easy for humans to control and share their experience. 

**Distinguishing Properties**

* ABC is *streamable*. This means bytecode can be incrementally processed, e.g. to modify a state, then quickly forgotten. In practice, this means the bytecode cannot support backwards 'jumps'. ABC takes this a step further and simply avoids jumps entirely. However, conditional behavior and loops may still be expressed in terms of first-class functions and fixpoint combinators.

* ABC aims to be *weakly legible*, such that a human who knows code can easily develop or debug at the bytecode layer. This weak legibility can potentially be further augmented by techniques like rendering editable views based on program structure (cf. [command language for awelon](CommandLine.md)).

* ABC is purely functional. Computations may freely be cached, replicated, recomputed, and are characterized by values produced. ABC does support a potential escape for this, via tokens. But Awelon project uses tokens compatible with pure computation.

* ABC supports very large values, such that massive databases or filesystems can be modeled as first-class values. This is achieved by a *value stowage* concept - essentially, an explicit approach to virtual memory. This reduces need to integrate external databases or filesystems, which is convenient for purely functional computations.

* ABC will support scalable multi-processor parallelism, guided by annotation. Long term, there may also be support for fine-grained data parallelism, accelerating subprograms via GPGPUs or similar.

* ABC is amenable to static typing. That is, static typing may usefully be performed at a *bytecode* level. This can be convenient. It allows us to mix typechecking with other stages of programming and evaluation in a flexible, ad-hoc manner.

* ABC is concatenative. There are no headers, footers, or 'sections' within the code. We can literally concatenate any two syntactically well formed ABC programs into a third, and semantically it will correspond to a composition of the programs.

## The ABC Stream

ABC is directly encoded using a (mostly) printable subset of UTF-8 characters. The bytecode provides a means to embed texts, first-class subprograms called 'blocks', and tokens. Limited whitespace is permitted - LF and SP (U+10 and U+32). Formally, whitespace encodes the identity behavior, and hence can be injected or eliminated between other operators. An ABC program can be 'streamed' by processing individual operations as we reach them, but most programs will be finite in size regardless.

        [blocks[[are]nestable]]
        #42
        "text begins with a double-quote, and has a block-like format.
         text may extend multiple lines, each continued by LF SP (10 32)
         in which case the LF becomes part of the text. Otherwise, text 
         can be terminated by LF ~ (10 126), which adds no characters.
         ABC text has no need for escapes, other than SP after LF.
        ~
        {tokens}{&annotations}{:sealers}{%aowords}
        vrwlc

The ABC (or ABCD) stream is a sequence of operators, numbers, literals, blocks, and tokens. Effective code additionally requires knowing a few idioms and conventions. Operators are expressed by a single character. Primitive operators are in the ASCII range, as are the most common ABCD accelerators. The larger unicode range provides plenty of space for ABCD to potentially grow. Natural numbers are expressed using eleven dedicated operators `#9876543210`. 

        #42
        #3141 #3 {%decimal}
        #2 #3 {%ratio}

The `#` operator introduces a zero value. The `0..9` digit operators each "multiply by ten, add digit". The intention is to support weakly legible numeric data at the ABC layer. Numbers can be manipulated by related math operators, and more sophisticated number types (like ratios or decimal floating point) can be modeled explicitly. 

Literals embed arbitrary text within ABC, and are understood as syntactic sugar for a much larger program that would construct the same text number by number. Literals are constrained: no C0 (except LF), no C1, no DEL, no surrogates, no replacement character.

        "this is a text
         it may have multiple LF lines
         each LF is indented by SP
         we terminate by LF ~
        ~

Unlike many languages, texts do not have 'escape' characters excepting the `LF SP` sequence. If you want `\n` to mean something other than the two composing characters, you'll need to postprocess the text explicitly. While ABC generalizes for multi-line texts at the ABC layer, [claw](CommandLine.md) and other editable views of ABC will frequently support *inline* text like `"hello"`, so long as it contains no end-quote.

At this time, literals correspond to a *list* of codepoints, e.g. `μL.((N*L)+1)`. I am contemplating an alternative, inspired from *command sequences*. In this alternative, we might have `[B][A]"hello" ap == #104 [[B][A] "ello" ap] A`, enabling flexible iteration without a fixpoint.

Blocks are expressed by square brackets `[]` and may contain an arbitrary subprogram. Blocks effectively serve as the first-class functions of Awelon Bytecode, and serve an essential role in expressing conditional or iterative behavior.

Tokens are expressed by short texts within curly braces `{}`. Tokens enable flexible extensions to bytecode. 

        {&par} {&seq} {&stow} {&trace}  -- annotations
        {%inline} {%ratio} {%decimal}   -- link words (AO dictionaries)
        {'scope/resourceId}             -- link value stowage
        {:map} {:stats} {.stats} {.map} -- discretionary value sealing

Awelon project generally uses tokens in a *purely functional* manner. Annotations, prefixed by `&`, logically have identity type but may serve a valuable role with respect to performance or debugging. Linking words in our AO dictionary is essential for *concise expression* of larger programs. Value stowage can serve a similar role when integrating cached compiled or big data, and potentially can work in large scopes like open distributed systems. Token texts are limited to 255 bytes UTF-8, may not include curly braces or LF, and otherwise share the limitations of literals.


## ABC Behavior Details

### Basic Data Shuffling

ABC provides a minimal set of primitive operators for block-free structure manipulation. The primary structure in ABC is the product (pair), type `(a * b)`.

        l :: (a * (b * c)) → ((a * b) * c)
        r :: ((a * b) * c) → (a * (b * c))
        w :: (a * (b * c)) → (b * (a * c)) 
        z :: (a * (b * (c * d))) → (a * (c * (b * d)))
        v :: a → (a * 1)
        c :: (a * 1) → a

Type `1` is identity for product, called 'unit', and provides static structure.

There are other minimal sets with fewer operators, but this set has some nice symmetry properties. The operators `lzrw` are sufficient for all data shuffling where the rightmost element is sticky, and `v` can displace the rightmost element.

Example encodings:
        
        zw    :: (a * (b * (c * d))) → (c * (a * (b * d))) -- rot3
        vrwlc :: (a * b) → (b * a) -- full swap

In addition to shifting objects around, we can drop or copy values:

        % :: (Droppable x) ⇒ (x * e) → e
        ^ :: (Copyable x) ⇒ (x * e) → x * (x * e)

In ABC, blocks can be tagged with substructural attributes that make them uncopyable or undroppable. But otherwise, all values are copyable and droppable by default. A product or sum may be copied if both element types may be copied.

### Blocks

A block in ABC contains a finite sequence of ABC. Blocks may be constructed as literals, by composition `o`, or by quotation `'`. 

        [vrwlc] :: e → ([(x * y) → (y * x)] * e)
        o :: [x→y] * ([y→z] * e) → ([x→z] * e)
            [def][abc]o = [abcdef]
        ' :: x * e → [s→(x*s)] * e
            #42' = [#42]
            [vrwlc]' = [[vrwlc]]

After construction, a block is applied with the `$` operator:

        $ :: [x→x'] * (x * e) → (x' * e)

Loops are modeled via fixpoint combinators, in particular the strict fixpoint combinator called the Z combinator. A simple Z combinator is `[^'o]o^'o`. A variation suitable for AO's multi-stack environment is `r[^'ol]o^'ol`. But note that loops should always terminate in Awelon project; we'll use higher layers (streaming, RDP) for long running services.

Higher order programming can be modeled as a block that expects a block as an argument. Currying (partial application) can be modeled by combining quotation with composition.

*Aside:* Arguments for `o` were recently swapped to match the most frequent use case (otherwise almost every instance of `o` is preceded by `w`), so there may be some errors in the documentation.

### Numbers

### Text

### Identity

ABC treats two whitespace characters - SP (32) and LF (10) - as identity operators with type `x → x`. Effectively, whitespace in the ABC stream may be ignored. Also, the empty program is equivalent to identity, since it performs no operations on the tacit input.

*Note:* Other whitespaces - e.g. tabs and carriage returns - are not valid ABC operators. If encountered in an ABC stream (outside of text), an error must be raised, as for any other invalid operator.

### Substructure

[Substructural types](http://en.wikipedia.org/wiki/Substructural_type_system) are interesting because they can express structured control-flow or dataflow orthogonally to syntax. They can enforce that protocols and handshakes complete, that resources are released, that promises are kept, that callbacks or continuations execute once and only once. Substructure is also useful for modeling uniqueness or ensuring exclusive write access to a resource.

In ABC, only blocks have substructural type. This is represented by marking an existing block with substructural type:

        k :: ([x→y] * e) → ([x→y]' * e) -- relevant, no drop
        f :: ([x→y] * e) → ([x→y]' * e) -- affine, no copy

These operations are naturally idempotent and commutative.

An affine block may not be copied. A relevant block may not be dropped. A linear block may not be copied or dropped. However, a relevant or linear block may still be applied with `$`.

When blocks are quoted or composed, or a structure containing blocks is quoted, the composite inherits substructural attributes of all the components: 

        [code]f [more]k o = [morecode]kf
        [code]f [more]  o = [morecode]f
        [code]k [more]fl' = [[code]k[more]fl]kf
        [code]f' = [[code]f]f

When a relevant block is copied, both copies are relevant. (*Note:* Technically, only one of the two copies must be relevant. However, it is difficult to explain this in a type system, and difficult to track in a streaming scenario. For simplicity, both copies are relevant.)

*Note:* Substructure may be enforced statically, dynamically, or ignored by a given implementation of ABC. It's essentially a primitive annotation to mitigate the lack of structured syntax and encourage linear programming styles (i.e. avoidance of copy and drop in generic code).

### Conditional Behavior

A sum type, `(a + b)`, represents that we're either right with `b` or left with `a`. Convention is to pun 'right' with 'true', i.e. `(false+true)` ordering. A sum type is the typical result of making an observation, such as comparing two numbers:

        G :: N(x) * (N(y) * e) → ((N(y)*N(x))+(N(x)*N(y)) * e -- y > x
            #4 #2 G -- observes 4 > 2. Returns (N(2)*N(4)) on right.

Technically, `G` is the only observer operator built into ABC. However, it is assumed that invocation of capabilities will often result in sum type observations, e.g. indicating failure vs. success. Sums are also used to model algebraic data structures, e.g. a list can be modeled with `µL.((element*L)+1)`.

Sum types have their own set of data shuffling operators:

        L :: (a + (b + c)) * e → ((a + b) + c) * e
        R :: ((a + b) + c) * e → (a + (b + c)) * e
        W :: (a + (b + c)) * e → (b + (a + c)) * e
        Z :: (a + (b + (c + d))) * e → (a + (c + (b + d))) * e
        V :: a * e → (a + 0) * e
        C :: (a + 0) * e → a * e

Type `0` is identity for sum, called 'void', and corresponds to a vacuous condition. Static analysis may infer types for void to reject inconsistencies even in dead code. After a condition is observed, we can conditionally apply a block:

        ? :: (Droppable b) ⇒ b@[x→x'] * ((x + y) * e) → (x' + y) * e

A block with the 'relevant' substructural property cannot be applied in this manner. Any such block must be applied under all conditions. In addition, we can distribute, factor, and merge sums:

        D :: a * ((b+c) * e) → ((a*b) + (a*c)) * e -- distrib
        F :: ((a*b) + (c*d)) * e → (a+c) * ((b+d) * e) -- partial factor
        M :: (a+a') * e → a * e -- merge

Full factor is modeled by combining partial factor and merge:

        FM :: ((a*b)+(a'*c))*e → a*((b+c)*e) -- full factor; inverse of D

On merge, the types `a` and `a'` must be compatible for future operations. What 'compatibility' requires may be judged in context, knowledge of future operations. 

Sums may also be copied or dropped (with `^` and `%`) assuming both element types may be copied and dropped.

*ASIDE:* A possible type checking technique is to have a 'merged' type that simply validates future operations on both paths. In practice, the partial factor operation can be more difficult to precisely type, because we easily lose precise information about how `(a+c)` and `(b+d)` are on the same side. 

### Partial Functions and Contracts

ABC provides a simple operator for partiality, assertions, and contracts:

        K :: (a + b) * e → b * e

This operator represents a form of divergence: if we're in the left, that's equivalent to a type error and we'll stop the program as quickly and cleanly as possible. Otherwise we're okay. However, ABC is not restricted to runtime detection of this error. Expressing partial functions with `K` enables ABC to infer dependent types and contracts. If `K` cannot be statically proven safe, the programming environment may issue a warning or raise an error.

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

In [minimalist ABC](ABC_Minimalist.md) I proposed use of `{@foo}` gates as a basis for debugging. The behavior of a gate is configured at the runtime level: a 'closed' gate acts as a breakpoint, an 'open' gate just passes data, and a 'trace' gate will 

Our runtime can configure for every gate name

Depending on our runtime configuration, such gates could serve as:

* break

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

