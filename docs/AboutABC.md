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

ABC is represented in a stream of UTF-8 encoded characters. There are no sections, no headers or footers. There is just the stream, potentially unbounded in length. ABC is designed to be visible, printable. Text, blocks, tokens, and the encoding of numbers are at least weakly legible:

        [blocks[[are]nestable]]
        #42
        "text begins with a double-quote, and has a block-like format.
         text may extend multiple lines, each continued by LF SP (10 32)
         in which case the LF becomes part of the text. Otherwise, text 
         can be terminated by LF ~ (10 126), which adds no characters.
         ABC text has no need for escapes, other than SP after LF.
        ~
        {tokens}{&annotations}{:sealers}{%aowords}
        [{&attributes}]%
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

ABC is extensible with tokens, which are expressed by curly braces around a short text such as `{foo}`. Currently, tokens are used for:

* **linking** - with acyclic dependencies and trivial 'inline the referenced bytecode' semantics. Tokens of form `{'kf/scope/resourceId}` are used to link values. Tokens of form `{%humanMeaningfulWord}` are used to link definitions within an [AO dictionary](AboutAO.md).

* **annotations** - tokens starting with `&`, such as `{&par}`, `{&seq}`, `{&stow}`, `{&load}`, `{&text}`. Annotations support performance, debugging, static analysis. Correct use of annotations will have identity semantics, while incorrect use may cause a program to fail. It should be safe for a compiler or interpreter to ignore annotations that it does not recognize.

* **attributes** - annotations without runtime semantics, used instead as metadata about code. Attributes provide hints for indexing, rendering, tooling. Attributes are represented by a dropped block of annotations, e.g. `[{&author:david}{&deprecated}{&noindex}]%`. See the [application model](ApplicationModel.md) for more.

* **value sealers and unsealers** - a discretionary sealer `{:foo}` has type `(a * e) → (foo:a * e)`, and serves implicitly as a newtype wrapper. To access the sealed value, we can unwrap it with `{.foo}`. Cryptographic sealers are also feasible. See *Value Sealing* below.

Like normal ABC bytecode, tokens should have purely functional semantics (at least with respect to optimization). However, this still leaves room for privileged effects - e.g. reflection, backtracking, uniqueness providers. Conventional imperative effects must instead be handled by monadic programming and other purely functional models.

Token texts are constrained as follows:

* forbid control codes (C0, C1, DEL)
* forbid surrogate codes (U+D800..U+DFFF)
* forbid replacement char (U+FFFD)
* forbidsthe curly braces: `{}`
* between 1 and 63 bytes UTF-8

Tokens may be further constrained within a specific codebase, e.g. an AO dictionary. 

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

Annotations are tokens for debugging or performance. Annotations are indicated by use of an `&` prefix in the token, e.g. in `{&lazy}` or `{&join}`. Annotations have *identity* semantics - they do not affect the logically observable behavior of a correct program. However, annotations may cause an incorrect program to fail. And incorrect use of annotations may cause a program to be incorrect. 

The idea is that a runtime should be able to ignore annotations (or sets of related annotations) that it does not recognize and still produce the same result, albeit perhaps less efficiently.

Ideas for what annotations might do:

* support parallel or lazy computation
* stow and load values from a backing store
* hints for type or termination checking
* debug output, breakpoints, location tracking
* optimized representations (e.g. compact text) 
* trash data while perserving substructure
* compile or JIT computed blocks of code 
* move computations and data to a GPGPU
* support and enable memoization or caching

For example, an annotation `{&trash}` might indicate that a value will never be observed in the future, enabling its memory to be recycled while leaving a lightweight placeholder. And `{&trace}` might serialize a value to a debug output buffer, and also trash it.

Correct use of annotations may be paired. For example, with explicit laziness we might use `{&lazy}` to mark a block to generate pending values, then use `{&join}` on pending values to observe them. Explicit `{&join}` is not as convenient as laziness by default, but it does simplify implementation.

Annotations may require specific types. A simple case is that `{&text}` might require a `∀e.(text*e)` argument, and would compact the text value and ensure it serializes as embedded text.

### Parallelism

For scalable parallelism, I propose fork and join annotations together with a simple *process functions* (PF) and asynchronous futures concepts.

        PF i o      [i → (o * (PF i o))]
        {&fork}     ((PF i o) * e) → ((PF i (future o)) * e)
        {&join}     ((future a) * e) → (a * e)

When a forked PF is called, a parallel runtime can immediately return an asynchronous future result and the next forked PF. Synchronization occurs upon attempting to 'join' the future result, and parallelism is achieved by delaying synchronization. (Copying a forked PF may also involve synchronization, but in practice should be a non-issue.) Returning the 'next' PF effectively allows for stateful processes models. Any model of process control signals, message passing, termination, etc. must be represented via the input-output data types. 

Process functions may be implemented with independent heaps, potentially as separate OS level processes, perhaps even distributed across physical nodes. The costs of initializing a process can generally be amortized over a series of calls to the process. A runtime may freely use heuristic pushback to keep fast producers from running too far ahead of slow consumers, i.e. to control memory overhead. PF parallelism can be efficient, scalable, predictable, and should generally be easier to control than Haskell's par/seq parallelism.

*Note:* GPGPU and vector processing parallelism will be developed later, and will use an orthogonal basis. 

*Aside:* Serialization of process functions with parallelism/futures is non-trivial. One must model reconstruction of parallel futures with flexible relationships between processes. 

### Laziness

Explicit laziness might be represented using annotations and futures.

        {&lazy}     ([a → b] * e) → ([a → (future b)] * e)
        {&join}     ((future a) * e) → (a * e)

The join annotation is the same used for parallelism, enabling some opacity regarding the nature of a future. With laziness, any value may be wrapped as a future via `[]{&lazy}$`.

Explicit laziness is much less expressive than implicit laziness. But that isn't a bad thing. There is no need to deal with cyclic values, tied knots, etc.. And modeling infinite data structures with laziness should be discouraged anyway, because ideally we can remove annotations without inviting divergence.

*Note:* Futures, laziness, fork, etc. are algebraic. A `{&lazy}{&lazy}` block should return a `(future (future b))` which must in turn be joined *twice* to access the data. In the general case, there may be intermediate computations between joins. If flag bits are used to represent block attributes, then `[block]{&lazy}{&lazy}` might need to be rewritten as the equivalent function `[[block]{&lazy}vr$c]{&lazy}` to preserve algebraic properties.

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

### Value Stowage

A simple 'value stowage' supports larger-than-memory data and computations, separate compilation and dynamic linking, efficient distributed data, etc.. This model is achieved through just two annotations:

        {&stow} :: (v * e) → ((stowed v) * e)
        {&load} :: ((stowed v) * e) → (v * e)

Stowing a large value pushes it into a backing storage, e.g. a filesystem or database. A lightweight placeholder is left in the value's stead. Loading a value moves data in the opposite direction. The backing store will frequently be many orders of magnitude larger than the memory allocated to a computation, and it may use independent garbage collection model.

By modeling tree structures (e.g. a trie, B+ tree, or log-structured merge tree) and stowing tree nodes, it is feasible to represent massive databases as first-class values. Finger-tree ropes can serve as an effective basis for logs and queues. Patterns such as loading code just before inlining it (`{&load}vr$c`) serve as a logical basis for dynamic linking, and might access a compiled and cached representation of the loaded code.

To get the most out of value stowage, it must be used together with a persistence or caching model, such that the data may be reused across many independent computations without recomputing it. Structure sharing might further augment stowage, saving space when a value is computed many times. In a distributed system, stowed values could serve a role similar to hypermedia, enabling code to reference many large values without immediately downloading it. 

A proposed serialization for stowed value placeholders is use of a token:

        {'kf/scope/resourceID}

The prefix `'kf` indicates a stowed value (`'`) with relevant and affine substructural properties (`kf`). The scope helps constrain the search space, and the rest identifies the value. The resourceID should include a secure HMAC if otherwise forgeable (like an incremental number). The limit on token size (63 bytes) should not be an issue in practice, though hierarchically structured resource identifiers like URLs are not viable.

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

