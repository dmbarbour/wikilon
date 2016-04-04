# Awelon Bytecode

Awelon Bytecode (ABC) is a bedrock component of the Awelon project. ABC consists of about forty operators, embedded text, blocks of ABC (to model first class functions), and a [capability secure](http://en.wikipedia.org/wiki/Capability-based_security) model to access external effects. The effects model is further leveraged to support dynamic linking, separate compilation, and performance annotations. 

**Why a new Bytecode?**

Awelon project has several concepts that are not well supported by existing languages or bytecodes. Among these: Awelon is aimed at open distributed systems, which requires special attention to security, serializability, aliasing, and linking. Awelon project includes Reactive Demand Programming (see [AboutRDP](AboutRDP.md)) which benefits from declarative optimizations and equational reasoning at the bytecode level. Awelon project aims to unify the user and programmer experiences, such that objects are readily shared and directly useful as software components, and even user input is modeled as a continuous stream of code that can be mined for tools and user macros.

**Distinguishing Properties**

ABC has many interesting or unusual properties that distinguish it from other bytecodes and programming languages.

* ABC is *streamable*. This means the bytecode can be incrementally executed as it arrives then quickly forgotten. We might stream bytecode to modify a web page, subscribe to a publishing service, or command a robot in real time. The bytecode can manipulate values and/or capability-based APIs. Streamability is essential to the Awelon project vision, to support new forms of tooling. For comparison, conventional bytecodes are designed for the [stored-program computer](http://en.wikipedia.org/wiki/Von_Neumann_architecture#History), and might 'jump back' to model a loop. ABC is streamed, not stored, and ABC does not jump. Loops are modeled instead using [fixpoint combinators](http://en.wikipedia.org/wiki/Fixed-point_combinator#Strict_fixed_point_combinator). 

* Unlike most bytecodes, ABC is *weakly legible*. Natural numbers have an obvious encoding, e.g. `#42` encodes the number forty-two. Text literals can be embedded directly. Basic operators use printable characters, visible in a text editor. Effects are accessed through visually obvious and often human-meaningful tokens between curly braces, e.g. `{foo}` (perhaps guarded by HMAC). In a suitable environment, even richly structured data - meshes, images, matrices, graphs, music notation, etc. - might be [embedded as renderable literal objects](doc/ExtensibleLiteralTypes.md) in ABC. The intention is to simplify learning ABC, debugging, disassembly, modification, and extraction of useful software components.

* ABC is almost purely *functional*. ABC's basic operators have purely functional semantics. ABC does not have a concept for references or aliasing. And ABC's value types are immutable. Further, side-effects are uniformly constrained to respect *spatial idempotence* and *causal commutativity*: invoking the same effect twice with the same argument must be the same as invoking it once, and ordering of effects is controlled only by the computation of arguments. Thus even effectful code allows a lot of nice equational reasoning (and optimizations) similar to pure code. 

* ABC supports a high degree of deterministic parallelism. Any block application (operators `$` or `?`) may be computed in serial or in parallel without changing the result of a valid program. The difficulty isn't one of finding opportunities for parallelism, but rather one of optimizing it. Developers may guide parallelism explicitly by use of annotations, and optimizers may inject parallelism decisions based on heuristics and profiling.

* ABC is multi-paradigm, but in a rather non-conventional sense. ABC can support *functional, imperative, or reactive* programming based on the compiler used and the set of effectful capabilities made available as arguments to the program. Valid optimizations for ABC are the same regardless of paradigm. A single ABC subprogram can often be reused for many different paradigms. 

* ABC is a [*tacit, concatenative* language](http://concatenative.org/wiki/view/Concatenative%20language), similar in nature to Forth, Joy, Factor, and Cat. Concatenating two valid ABC subprogram strings is the same as composing their functions, and it is trivial to extract subprograms into reusable software components. Though, unlike the aforementioned languages, ABC doesn't allow user-defined names (that's left to [higher level languages](AboutAO.md)) and is not stack-based (instead operating on products, sums, and numbers). 

* ABC is *strongly typesafe*, and amenable to static analysis. To the extent safety isn't validated statically, the runtime may enforce it dynamically. It is feasible to typecheck and infer safety for most ABC code, with minimal runtime checks at certain boundaries, e.g. when working with remote systems or side effects. 

* ABC supports [*substructural* typing](http://en.wikipedia.org/wiki/Substructural_type_system), in the form of adding affine (no copy) and relevant (no drop) attributes to blocks. This allows ABC to enforce structured programming behaviors without relying on a structured syntax.

* ABC is highly suitable for *open, distributed programming*. Arbitrary values can be serialized, communicated, and incrementally updated via streaming bytecode. Blocks are easily serialized and can model first class functions, mobile agents, or interactive applications. Capability secure effects and cryptographic value sealing can enable ad-hoc mashups of mutually distrustful subprograms and services. ABC's unusual separate compilation and linking model makes it easy to securely reuse code and data across independent services.

* ABC is highly suitable for staged programming. First class functions in ABC have a simple representation as blocks of ABC code, which makes it easy to recompile a function composed in an earlier programming stage. This is very useful for developing domain specific languages because they can achieve a high level of performance and compilation to native code. 


## The ABC Stream

ABC is represented in a stream of UTF-8 encoded characters. There are no sections, no headers or footers. There is just the stream, potentially unbounded in length. ABC is designed to be visible, printable. Text, blocks, invocations, and the encoding of numbers are at least weakly legible:

        [blocks[[are]nestable]]
        #42
        "text begins with a double-quote, and has a block-like format.
         text may extend multiple lines, each continued by LF SP (10 32)
         in which case the LF becomes part of the text. Otherwise, text 
         can be terminated by LF ~ (10 126), which adds no characters.
         ABC text has no need for escapes, other than SP after LF.
        ~
        {capabilityText}
        vrwlc

This visibility seems useful for didactic purposes and debugging. For similar reasons, ABC supports two whitespace characters (LF (10) and SP (32)) assigning to them the identity function (type `∀x.x→x`), to simplify formatting of ABC for human view.

## ABC Behavior Overview

The ABC stream contains operators, literals (blocks, text, numbers), and invocations.

### Operators

Each operator is represented as a single character, denoting a function. For example, operator `r` denotes the right association function, with type `((a * b) * c) → (a * (b * c))`, and `w` denotes the stack swap operator `(a * (b * c)) → (b * (a * c))`. ABC has a fixed finite alphabet of operators.

ABC operators are applied in sequence to an implicit value external to the ABC stream. This value is typically a composite, constructed of `(a * b)` pairs. This value may represent documents, a stack-based computing environment, or many things. 

ABC operators manipulate a small part of this structure using a stack-like metaphor whereby `(a * (b * (c * ...)))` represents a stack with top three elements `a`, `b`, and `c`. Deep manipulations are modeled by shuffling relevant elements to the top of the stack, manipulating them, then shuffling them back to their proper location. (Idiomatically, ABC paragraphs should start and end with elements in proper location.) *NOTE:* data shuffling can become expensive if ABC is interpreted, but a great deal can be eliminated if ABC is compiled.

Developers can legitimately comprehend juxtaposition as functional composition. The ABC sequence `rw` has type `((a * b) * c) → (b * (a * c))`. Conversely, an ABC program can easily be decomposed into valid subprograms and software components.

### Literals

Blocks and text are the literals of ABC. They require special attention by the ABC reader and result in values added to the stack. Numbers are a pseudo-literal. Literals can be understood as functions that introduce a value.

A block contains a finite sequence of ABC code, and may be understood as a first-class function that simply introduces its value. E.g. `[rw]` has type `x → [((a * b) * c) → (b * (a * c))] * x`. Blocks are the foundation for loops, conditional behavior, and higher order programming. Blocks also support security, protecting interests of both provider (by encapsulating information or authority) and user (by constraining access on apply: `$ :: [x→x']*(x*e)→(x'*e)`).

Numbers use operator `# :: e → (N(0)*e)` to introduce a new zero, then each digit `0-9` has meaning of the form `3 :: N(x)*e → N(10x+3)*e`. Thus, numbers aren't literals, but natural numbers such as `#123` are close enough for legibility. Rational numbers are produced through operations on natural numbers, e.g. `#2#3/*` is two thirds.

Text is shorthand for producing a list of small numbers between 0 and 1114111 (0x10ffff), the Unicode codepoints. A text list has fixpoint type `µL.((c*L)+1)`, where `c` is a codepoint value. Binaries are encoded using a non-conventional base16 text. 

### Tokens

ABC is extensible with tokens, which are expressed by curly braces around a short text such as `{foo}`. There is no way for an ABC program to construct a token, it must simply be part of the bytecode. When a token is reached by an interpreter or compiler, it may be fulfilled by specialized code.

Tokens could feasibly be utilized for modeling side-effects, e.g. to invoke an FFI. However, Awelon project favors use of these tokens in purely functional ways:

* **linking** - with acyclic dependencies and trivial 'inline the referenced bytecode' semantics. Tokens of form `{#secureHashOfBytecode}` are used to link ABC resources in open distributed systems. Tokens of form `{%humanMeaningfulWord}` are used to link definitions within an [AO dictionary](AboutAO.md).

* **annotations** - tokens formally having identity semantics, but may be used to optimize performance, make assertions, provide hints for static analysis, integrate debugging tools, etc.. The important requirement is that it's always valid for a compiler or interpreter to *ignore* an annotation that it doesn't understand.

* **attributes** - comments about code, largely intended for software agents. These have no runtime semantics, instead indicate very ad-hoc stuff like authorship, deprecation, modified dates, indexing hints, rendering hints, tooling hints, etc. for the code. Attributes are represented via dropped blocks of annotations, e.g. of form `[{&author:david}{&deprecated}{&noindex}]%`. See the [application model](ApplicationModel.md) for more.

* **value sealers and unsealers** - a discretionary sealer `{:foo}` has type `(a * e) → (foo:a * e)`, and serves implicitly as a newtype wrapper. To access the sealed value, we can unwrap it with `{.foo}`. Cryptographic sealers are also feasible. See *Value Sealing* below.

Tokens are constrained as follows:

* valid utf-8 text
* no more than 63 bytes
* no control characters (C0, DEL, C1)
* no surrogates (U+D800 to U+DFFF)
* no replacement character (U+FFFD)
* no curly braces `{}`

Tokens may be further constrained within a specific system. Tokens should not receive non-trivial arguments via the token text, though allowing a few flags to tweak behavior is reasonable. For tokens with runtime semantics, arguments can be provided as actual arguments to the token as parameters.

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

ABC's primitive number type is arbitrary precision rationals. In some cases - when the required range and precision or error properties are well understood - a compiler may substitute use of integers, fixpoint, or floating point numbers. 

Natural numbers can be expressed using pseudo-literal constructors in ABC:

        # :: e → N(0) * e
        0 :: N(x) * e → N(10x+0) * e
        1 :: N(x) * e → N(10x+1) * e
        2 :: N(x) * e → N(10x+2) * e
        ...
        9 :: N(x) * e → N(10x+9) * e

After construction, numbers can be manipulated by a few elementary operations: add, multiply, negate, and divmod (to simplify inference of precision and modulus):

        + :: (N(a) * (N(b) * e)) → (N(a+b) * e)
        * :: (N(a) * (N(b) * e)) → (N(a*b) * e)
        - :: (N(a) * e) → (N(0-a) * e)
        Q :: (N(non-zero b) * (N(a) * e)) → (N(r) * (N(q) * e))
            such that q integral, r in (b,0] or [0,b), and qb+r = a

ABC is not rich in math, but accelerators like ABCD can fill the gap. With accelerators, it is feasible to support both floating point computations and high performance vector or matrix math.

### Text

The ABC stream can contain blocks of unicode (UTF-8) text:
        
        "text has a block format 
         it starts with double quote
         it may continue on multiple lines
         each ending with linefeed (10)
         which is escaped by following space
         or terminates text with tilde (126)
        ~

If anything other than space or `~` follows LF, the ABC stream is in error. There are no escape characters except for SP to escape a preceding LF. Semantically, text encodes a list `μText.((codepoint*Text)+1)`. Codepoints are small integers in the range 0..1114111 with an additional constraint to simplify rendering, editing, and processing of embedded texts through other systems:

* no control characters (C0, C1, DEL) except LF
* no surrogate codepoints (U+D800..U+DFFF)
* no replacement character (U+FFFD)

Proper text manipulation demands precise knowledge of the characters (ligatures, combining marks, etc.), and benefits from a more sophisticated representation than a flat list of numbers. However, ABC's simple representation of text is sufficient for its intended role: identifiers, embedded DSLs, etc.

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

### Causal Commutativity and Spatial Idempotence

ABC requires causal commutativity and spatial idempotence for effects models.

Causal commutativity means that there is no ordering relationship unless there is a visible dependency where an output of one behavior is observed or manipulated by the next. This property is valuable for optimizations (cf. [Causal Commutative Arrows and their Optimization](http://haskell.cs.yale.edu/?post_type=publication&p=72) by Hai Liu, Eric Chang, Paul Hudak). 

        conventional commutativity: (ABC does not generally have)
            foo bar = bar foo
        causal commutativity: (ABC requires and assumes)
            [foo] first [bar] second = [bar] second [foo] first
              where first  :: [a→a'] * (a*b) → (a'*b)
                    second :: [b→b'] * (a*(b*c)) → (a*(b'*c))

Spatial idempotence means that, if the same action is performed twice with the same inputs, there is no additional observable impact. This property is also extremely valuable for optimizations, e.g. in content distribution networks. 

        conventional idempotence: (ABC does not generally have)
            foo foo = foo
        spatial idempotence: (ABC requires and assumes)
            [foo] first dup = dup [foo] first [foo] second
              where first  :: [a→a'] * (a*b) → (a'*b)
                    second :: [b→b'] * (a*(b*c)) → (a*(b'*c))
                    dup    :: (x * e) → (x * (x * e))

ABC is designed primarily for reactive demand programming (RDP), which naturally has both spatial idempotence and causal commutativity. By requiring these properties, ABC code can be uniformly optimized without tracking usage context. 

Fortunately, it is not difficult to enforce spatial idempotence and causal commutativity even for imperative programming styles. Primarily, one favors linear objects such that duplicate effects cannot be expressed and effects for each object are serialized. Where necessary, capability secure [oracle machine](http://en.wikipedia.org/wiki/Oracle_machine) idioms allow controlled expression of race conditions.

Spatial idempotence and causal commutativity offer valuable properties for equational reasoning and optimizations. ABC programs can be manipulated in many ways similar to pure functions.

### Fast and Loose Reasoning for Termination

ABC favors a philosophy of 'fast and loose reasoning' about termination properties. (cf. [Fast and Loose Reasoning is Morally Correct](http://www.cse.chalmers.se/~nad/publications/danielsson-et-al-popl2006.html), Danielsson, Hughes, et al. POPL 2006.) The idea is that - with respect to optimizations, equational laws, rewriting, loop fusion, parallelization, laziness - we should assume every subprogram terminates (or is intended to). 

To help enforce and remind developers of this philosophy, ABC compilers should perform termination analysis, and long-running programs should be modeled as RDP behaviors or incremental processes (e.g. `μProc.[a→(Proc*b)]`).

ABC is Turing complete, so we can't expect a decision to be reached for every program. However, termination analysis is really about avoiding errors in reasoning. And *most errors aren't subtle*. Accidental non-terminations often can be detected, and many developers will write reusable software such that analysis passes without a 'could not prove' warning.

Termination is a weak property. In practice, we often wish to reason about performance characteristics: latency, memory requirements, resource usage. Ackermann function terminates, but not within reasonable bounds. However, termination is a great start.

### Flexible Safety Analysis

ABC has an implicit type system with six structural types (pair, sum, unit, void, number, block), substructural types (relevant, affine, expires), and modal types for spatial-temporal attributes (latency, location, sealed values). Termination analysis is recommended to validate fast-and-loose reasoning. ABC further supports inference of dependent types or contracts by use of operator `K`.

However, ABC does not specify any inference algorithms. I hope instead to enable independent evolution of analysis, a growing array of recognizers and strategies to prove safe structures, algorithms, and architectures. 

Instead, ABC has a philosophy:

* prove it right, pass it silently
* prove it wrong, raise an error
* indecision, pass with a warning and check at runtime
* incrementally improve strategies to efficiently decide more programs

Most static languages reject programs unless they're provably correct according to the type system. ABC's more relaxed philosophy accepts (with warning) code that is not provably wrong. This enables analysis to vary between lightweight and heavyweight. 

It also may give ABC a more dynamic feel, especially combined with dependently typed structures. ABC's philosophy is close in nature to [gradual typing](http://en.wikipedia.org/wiki/Gradual_typing), albeit with more inference and less annotation.

### Annotations for Performance and Debugging

Annotations are tokens for debugging or performance. Annotations are indicated by use of an `&` prefix in the token, e.g. in `{&fork}` or `{&≡}`. Annotations must not affect the observable results of a correct program, and an ABC interpreter or compiler should be able to ignore annotations that it does not recognize. However, annotations may cause an incorrect program to fail, and incorrect use of annotations may result in an incorrect program. 

Ideas for what annotations might do:

* support parallel or lazy computation
* stow and load values to a backing store
* hints for type or termination checking
* debug output, breakpoints, location tracking
* optimized representations (e.g. arrays) 
* trash data while perserving substructure
* compile or JIT computed blocks of code 
* move computations and data to a GPGPU
* support and enable memoization or caching

In many cases, use of annotations may be coupled. For example, use of a `{&fork}` annotation to parallelize a computation might be coupled with a `{&join}` annotation to synchronize and access the computed result. Any attempt to access the parallel value without performing the `{&join}` may then be an error. Coupling can greatly simplify implementation by eliminating need for transparent handling of, for example, a parallel pair during an `wrzl` operation.

Eventually, we should have a large set of de-facto standard annotations that are recognized by most runtimes. Meanwhile, each runtime is free to define its own set.

### Spatial-Temporal Types and Capabilities

ABC is designed for RDP, and RDP's design leverages a model of spatial-temporal types in a context of programming overlay networks, heterogeneous computing, and distributed systems. For security reasons, these spatial-temporal types cannot be directly observed or manipulated by ABC primitives. However, they may be influenced or observed by capability invocations.

A 'spatial' type is essentially a description of *where* a value is. This includes physical locations with varying precision - server, client, GPU, FPGA, specific threads. Additionally, virtual or logical locations may be modeled to simplify reasoning about interactions between subprograms, or to model staging or pipelines.

A 'temporal' type is a description of *when* a value can be observed, and might be described as a rational number of seconds from program start. Temporal types are useful to control reactive feedback loops and to understand and manage latencies in distributed systems. Upper bounds - expirations - are also useful. They help model timeout protocols, control distribution, and interact in interesting ways with substructural types.

The space-time model is a matter of convention, with a de-facto standard that is not part of ABC's definition. It is enforced through the capabilities protocols. My current vision has the following characteristics:

1. Adding two numbers, comparing two values, quoting a structure, etc. requires all inputs coexist in space-time. Some delay for adding or comparing numbers may be implicit, but movement is generally explicit. 
2. Temporal manipulations use a 'temporal cursor' concept: rather than delaying values directly, you advance values to a cursor, which can be manipulated independently. (The idea is to maximize idempotence and commutativity of delay operations.)
3. New substructural types may be introduced, notably *expiration* for blocks. A block can be marked for expiration, such that it is an error to apply it after a known point in time. For relevant blocks, it may further be an error to delay it beyond that point. Similarly, blocks may be specific to location.
4. Spatial manipulations are very specific, i.e. each capability representing a *directed edge* between spaces in a connectivity graph. Developers can constrain the connectivity graph in useful ways, e.g. to enforce staging of computation. 
5. For security reasons, reflection on spatial-temporal types is separate from distribution and manipulation. In general, we don't want most code to be sensitive to where it executes.

The intention is that these capabilities are distributed *statically*, i.e. using partial evaluation or a staged model, such that the spatial-temporal information is available at compile-time. Also, while spatial-temporal types and capabilities are intended primarily for RDP, I expect they would be useful for imperative programming.

*NOTE:* Distributed systems must admit the possibility of non-deterministic disruption. In many cases, it can be useful to model distribution capabilities as having the possibility of failure. 

*NOTE:* Physical resources are almost always location specific, and must be manipulated from the right location. The idea is to put code near the resource, rather than to remotely manipulate it.

### Uniqueness Types and Capabilities

Modeling creation of 'new' or 'unique' values requires special consideration in ABC. Due to spatial idempotence, two equivalent requests for a 'new' value must return the same result. Consequently, it is necessary to ensure that each request for a unique value is a unique request.

Linear and affine types are useful in this endeavor. ABC systems can model a 'uniqueness source' - a unique capability that generates unique values on demand. While a uniqueness source cannot be copied (because it would no longer be unique!), it may be *forked* such that there are now two unique uniqueness sources. An operation to construct a unique value will consume the uniqueness source, so it becomes necessary to iterate between forking and construction. 

New unique constructs will typically be one of:

* a sealer/unsealer pair (see Value Sealing, below)
* exclusive capabilities to access a state resource
* a cryptographically secure pseduo-random number generator 

Exclusive state capabilities correspond to conventional allocations, e.g. to `newIORef` in Haskell. For some state models (specifically, those that also respect *causal commutativity*, though this excludes most imperative state models) it may be permissible to share access to the state after obtaining the initially exclusive capabilities. 

In many contexts - live programming, orthogonal persistence, open systems - it is useful to ensure *stability* of unique values. 

Stability is readily achieved using a filepath/URL metaphor: when 'forking' a uniqueness source, we provide an identifier for the child that is unique within the parent. This stabilizes uniqueness with respect to changes in the order children are constructed. If used with a little discipline, significant restructuring of the source code becomes feasible without damaging persistence or relationships assuming stable identity.

### Value Sealing

Value sealing is a simple technique with very wide applications. The setup is simple. We have two capabilities - a 'sealer' and the corresponding 'unsealer'. At runtime, these may be allocated as a pair, using a secure uniqueness source (see above). 

        {:u} :: (a*e) → ((u:a)*e)     `:` for seal
        {.u} :: ((u:a)*e) → (a*e)     `.` for unseal

            #42{:foo}                       discretionary sealed data

Discretionary sealers offer no real protection. However, they serve a useful role in resisting type errors or providing  rendering hints (e.g. using `{:jpeg}` on a text representing a large binary is a pretty strong hint). 

We can also use *cryptographic* value sealing, e.g. protected by AES or ECC encryption. Cryptographic sealers allow us to enforce sealer disciplines even in an open distributed system. The details for cryptographic sealing haven't been hammered out, but a viable option is:

        {$:format}  :: (k*(a*e)) → ($a*e)
        {$.format}  :: (k*($a*e)) → (a*e)
        {$&format}  :: (sealer annotation)

            ["cipherText...\n~]kf{$&aes}    cryptographic sealed data

Here the `$` is serving as a prefix roughly meaning 'secured'. We have a sealer, an unsealer, and a sealed data annotation. The key is provided as an argument when sealing or unsealing (perhaps different keys with PKI). The types of both key and encrypted data may be specific to the format, but should generally include a block to record captured substructural attributes (affine, relevant, linear).

Value sealing is an important companion to object capability security. It provides a basis for [rights amplification](http://erights.org/elib/capability/ode/ode-capabilities.html#rights-amp), whereby you can gain extra authority by possessing both a capability and a sealed value, or by unsealing a value to gain a capability. It can also enforce various modularity patterns even in distributed systems. Value sealing should be considered orthogonal to transport-layer encryption. 

### ABC Resources for Large Values, Separate Compilation, Dynamic Linking

ABC values may be quoted to generate a string of bytecode having type `e → (value * e)`. This bytecode can be given a unique identifier. The value can then be replaced by its identifier until it needs to be loaded into memory. This concept is both simple and versatile. Some roles:

* virtual memory, work with gigabytes in much less memory
* reference values without transmitting them across network
* compression, store big value only once then name it many times
* dynamic linking, load functions shortly before inlining them
* separate compilation, maintain cache of compiled functions

Resource identifiers will in general be local to a runtime environment (potentially a distributed environment). Local resource identifiers have the advantage of using smaller identifiers and supporting precise garbage collection. With a network session, local resource identifiers could be shared with a remote client who may ask for (and optionally cache) the returned value. (Such requests could easily be protected by HMAC.) For ideal caching, a resource identifier should never be reused (unless the same value is constructed again).

It is also feasible to create *global* resource identifiers, using a secure hash of the bytecode. These do make GC difficult. At the moment, there is very little need for global identifiers, but they might become interesting if ABC runtimes are widely used. A service could graduate widely used local resources into global ones.

Within an ABC stream, an ABC resource will be indicated by token. For local resources in Wikilon runtime, I am currently considering something like:

        {'kf/Scope/ResourceID}

The prefix `'kf` is indicating a value resource `'` along with the substructural attributes `kf`. A value without substructural constraints could voluntarily the `kf`. The `Scope` could feasibly identify a server or session or global scope. Some general rules are first that a `Scope/ResourceID` should never be reused for a different value (such that a cached resource is never invalid, though access may expire), and second that resources be unforgeable (such that a hacker cannot guess at identifiers and retrieve security-sensitive information). Use of a 60-80 bit HMAC in a local resource ID is probably sufficient to prevent forgery.

Unlike URLs, we're constrained to 63 bytes for our token strings. Scope and ResourceID have practical size limits, and sophisticated hierarchical structure is not recommended.

Use of ABC resources will be driven by related annotations. I'm proposing `{&stow}` to construct the value resource (and reduce memory pressure) coupled with `{&load}` to subsequently access it, forcing it into local memory. Stowage is *latent*, which is very important for performance: if we repeatedly `{&stow}` and `{&load}` a value within a loop, we'll not get around to serializing our value to an external store (not before our loop terminates). While stowage is local, it seems feasible to graduate a stowed value to ever wider scopes based on profiling network traffic.

Anyhow, between the explicit use of `{&load}` and fast access to its substructural attributes, we can perform generic data plumbing on our value resource *without* loading it.

*Aside:* Originally I had ABC resources for arbitrary code and value resources were the specialization. However, this does not have a good story for how the ABC resources are decided in the first place, leaving that to some unnamed external agent. The current approach has a more complete story, and works well enough with arbitrary code - e.g. `{'/resource}{&load}vr$c` to specify a block resource then immediately inline it.

### Encoding Binary Data in ABC

Programmers often work with binary encoded data, e.g. compressed visual or audio data, secure hashes, ciphertext. I would like the ability to encode MP3 files, texture data, or short video clips as ABC resources. This would allow me to leverage ABC's secure content distribution, caching, partial evaluation, and nearly transparent link model. However, it's valuable that large binaries can be stored and transmitted efficiently.

One simple option is to use a naive encoding of binaries - e.g. base16 - and couple this with a dedicated compression pass for storage or streaming. I propose alphabet `bdfghjkmnpqstxyz` (`a-z` minus vowels `aeiou` and common ABC ops `vrwlc`). This ensures binaries are visually distinct, yet opaque.

        "htkzmfkjkxfbkpmbmgmjkxfbkhkzktkzmffbmgkpmhfbkdkxkjmhftfbkgkzkymg
         kjkgmhkjmhmjmffbkdkhkpmbkpmgkgkpkykmfbkjktkpmhftfbmgkjkhfbkhkzfb
         kjkpmjmgkxkzkhfbmhkjkxmbkzmffbkpkykgkpkhkpkhmjkymhfbmjmhfbktkdkf
         kzmfkjfbkjmhfbkhkzktkzmfkjfbkxkdkmkykdfbkdktkpmdmjkdfyfbjjmhfbkj
         kykpkxfbkdkhfbkxkpkykpkxfbmkkjkykpkdkxftfbmdmjkpmgfbkykzmgmhmfmj
         khfbkjmnkjmfkgkpmhkdmhkpkzkyfbmjktktkdkxkgkzfbktkdkfkzmfkpmgfbky
         kpmgkpfbmjmhfbkdktkpmdmjkpmbfbkjmnfbkjkdfbkgkzkxkxkzkhkzfbkgkzky
         mgkjmdmjkdmhfyfbhhmjkpmgfbkdmjmhkjfbkpmfmjmfkjfbkhkzktkzmffbkpky
         fbmfkjmbmfkjknkjkykhkjmfkpmhfbkpkyfbmkkzktmjmbmhkdmhkjfbmkkjktkp
         mhfbkjmgmgkjfbkgkpktktmjkxfbkhkzktkzmfkjfbkjmjfbkkmjkmkpkdmhfbky
         mjktktkdfbmbkdmfkpkdmhmjmffyfbhjmnkgkjmbmhkjmjmffbmgkpkymhfbkzkg
         kgkdkjkgkdmhfbkgmjmbkpkhkdmhkdmhfbkykzkyfbmbmfkzkpkhkjkymhftfbmg
         mjkymhfbkpkyfbkgmjktmbkdfbmdmjkpfbkzkkkkkpkgkpkdfbkhkjmgkjmfmjky
         mhfbkxkzktktkpmhfbkdkykpkxfbkpkhfbkjmgmhfbktkdkfkzmfmjkxfy
        ~

A compression pass could easily recognize runs of these characters and rewrites to a short header and a bytecount. The format I developed for use within Wikilon is especially optimized for texts, encoding 32 bytes per line, encoding blocks of up to 4096 bytes (128 lines) with only 0.1% overhead. Here's the format I use internally:

        (248) (size) (bytes)
            (size in 0..127): 8..512 contiguous bytes (multiples of 4)
            (size in 128..254): 2..128 lines of 32 bytes with LF SP separators
            (size 255): escape prior (248) byte

Byte `(248)` does not naturally appear in UTF-8 text, and hence does not appear in ABC. The escape is included only to ensure compression is a total function, valid on all bytestrings. This binary compression could be followed by a more conventional compression, e.g. Snappy or the awesome new ZStandard. It is likely that conventional compression is, by itself, sufficient for many use cases.

Efficient *processing* of binaries may further require accelerators like ABCD. We can convert our binary to an actual binary representation for runtime use, and use accelerated list processing functions to index the binary or even (via holding a unique reference) update it in place.

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

