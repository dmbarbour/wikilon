
# Awelon Language

## Vision and Design Overview

Awelon is a programming language and environment designed to empower individuals to control, create, comprehend, customize, extend, and share computation artifacts.

Major design elements:

* A scalable concrete codebase, represented as a [log-structured merge tree](https://en.wikipedia.org/wiki/Log-structured_merge-tree) over [content addressable storage](https://en.wikipedia.org/wiki/Content-addressable_storage) with [prefix-aligned indexing](https://en.wikipedia.org/wiki/Radix_tree). This data structure can support massive volumes, lazy downloads, incremental synchronizations, lightweight backups, and atomic updates. Further, we can amortize physical storage and network costs within a community via proxy or [content delivery network](https://en.wikipedia.org/wiki/Content_delivery_network).

* A [purely functional](https://en.wikipedia.org/wiki/Purely_functional_programming) language evaluated via [rewriting](https://en.wikipedia.org/wiki/Rewriting) semantics to encode structured data and computations. The intention is to simplify sharing, caching, multi-stage programming, and user ability to inspect and comprehend computation through deterministic replay and rendering. Source, intermediate states, and final values can all use the same tooling.

* Automation is achieved by defining [bots](https://en.wikipedia.org/wiki/Software_agent) in the dictionary. Bots in Awelon are modeled as transactions that repeat indefinitely, implicitly waiting when unproductive. Effects are asynchronous via manipulation of system variables, such as adding a task to a system queue or some data to a network output buffer. This design is fail-safe, resilient, idempotent, securable, extensible, and safe to modify at runtime - which is convenient for live coding, runtime upgrade, and robust administrative process control.

* User interfaces build upon [projectional editing](http://martinfowler.com/bliki/ProjectionalEditing.html) over code and transactional memory. This design allows us to easily extend our interface with additional views and controllers. The relationship between a projection and the underlying data is easily inspected for comprehension. Further, combined with rewriting semantics, we can project not only code and results but every intermediate step for debugging. Conventional GUIs can be supported indirectly by 'editing' a task queue that's handled by a bot shortly after we commit.

The Awelon language is a just one aspect of the Awelon system. Its lightweight syntax and local rewrite semantics are essential in context of Awelon's vision and design goals. In contrast, performance and static type safety were secondary concerns, although still valuable (and addressed below).

## Language Basics

Awelon builds upon a semantic foundation of four concatenative combinators:

        [B][A]a == A[B]         (apply)
        [B][A]b == [[B]A]       (bind)
           [A]c == [A][A]       (copy)
           [A]d ==              (drop)

Besides these primitives, Awelon has specialized support for efficient embedding of data - natural numbers, texts, and binaries. Further, programmers may define a directed acyclic graph of words in a dictionary. Overall, Awelon has the look and feel of a stack-oriented programming language. However, evaluation is based on rewriting. Thus, a program that is shorted some arguments, such as `[A]a`, is not an error - it simply doesn't rewrite further.

The minimal syntax and semantics supports stability, projections, and rewriting. However, for practical use, Awelon further supports *Annotations* indicated by parenthetical words such as `(par)` or `(trace)`. Annotations formally have identity semantics, like whitespace. However, they may influence performance, static analysis, debugging, and rendering. *Accelerators* are an important subset of annotations that request a function be replaced by a built-in equivalent. Accelerators enable an interpreter or compiler to extend Awelon with performance primitives.

## Words

Words identify functions in Awelon. Aside from Awelon's four primitive words, words are user-defined in a key-value database we call a *Dictionary*. 

Syntactically, Awelon words are intended to be URL friendly, visually distinct, sortable, and easily partitioned into prefix-aligned packages and directories. Unicode was rejected both for URLs and because it's easy to have distinct encodings that are visually similar, which hinders debugging. However, it is feasible to leverage *Projectional Editing* to support CJK or iconographic presentations of words and operators. Regex:

        Word = Frag('-'Frag)*
        Frag = [a-z]+(Nat)?
        Nat  = '0' | [1-9][0-9]*

Semantically, a defined word is equivalent to its definition. Definitions must be acyclic. Thus, we could transitively expand any Awelon program into a finite stream of primitives. However, evaluation normally rewrites words lazily, preserving human-meaningful symbols and structure, and avoiding exponential expansion. Because definitions must be acyclic, loops are expressed through a fixpoint combinators (see *Loops*).

An Awelon system may enforce ad-hoc constraints on words and definitions. For example, an integrated development environment might assume that `foo-meta-doc` should define documentation for `foo`. The development environment might complain to the programmers if this word computes a value that it does not recognize as documentation. Similarly, we can simulate package private definitions by warning when `foo-local-*` words are used outside of `foo-*`. In some sense, the Awelon dictionary gives words a *denotation* while the development environment gives words their *connotation*.

*Aside:* Depending on context, it may be convenient to think of words as compressing a program stream, words as hypertext links within a smart filesystem, and words as functions or software components. Unlike conventional PLs, Awelon encourages embedding of data - forums, almanacs, databases, full text of books, etc. - within the dictionary.

## Natural Numbers

Awelon has limited support for natural numbers. Syntactically, natural numbers are represented by regex `Nat = '0' | [1-9][0-9]*`. Natural numbers are modeled as a syntactic sugar or projection over a recursive construction:

        0 = [zero]
        1 = [0 succ]
        2 = [1 succ]
        42 = [41 succ]

Definition of `succ` and `zero` - and hence our model for natural numbers - is in theory left to programmers. For example, we could favor a recursive sum encoding (`type Nat = μN.(1+N)` where `type (A+B) = ∀r.(A→r)→(B→r)→r`). Or we could favor a Church encoding (`type Nat = ∀x.(x→x)→x→x`). Or we could model naturals using an optional list of bits. In practice, this choice will depend on runtime support for *Accelerators*.

Awelon does not provide a rich numeric tower. Instead, it's left to *Projectional Editing* and *Accelerators* to build upon natural numbers. 

## Embedded Texts

Awelon provides limited support for embedding textual information like `"Hello, world!"`. This is motivated for embedded comments, label values, and some lightweight DSLs. Text is modeled as a projection over a recursive construction:

        "" = [null]
        "hello" = [104 "ello" cons]

Definition of `cons` and `null` are left to programmers, and presumably construct a simple list of bytes. Embedded texts are limited to ASCII minus C0, DEL and `"` (bytes 32-33, 35-126). There are no character escape sequences. However, it wouldn't be difficult to support post-process text - something like `"multi-line\ntext" lit` such that `lit` rewrites the `\n` to a linefeed (byte 10). 

*Note:* For large texts or binary data, developers are encouraged to bypass the Awelon parser. The Awelon dictionary provides specialized support for binary references, with the same semantics as embedded texts but without any byte-level constraints.

## Annotations

Annotations are special parenthetical words, such as `(par)` or `(error)`.

Annotations always have the same formal semantics: identity. That is, adding or removing annotations should not affect a correct program's observable behavior. However, within this limitation, annotations are assigned ad-hoc *informal* semantics by the runtime or compiler. For example, `[A](par)` might request parallel evaluation of `[A]`, while `[A](trace)` could output `[A]` to a debug log, and `(error)` would simply not evaluate further and thus cause computation to fail fast. In general, annotations augment performance, safety, debugging, and display of programs. 

Annotations encode *programmer intentions* rather than functional behavior.

Some potential annotations:

* `(a3)` - await at least three arguments on stack
* `(trace)` - print argument to debug console or log
* `(error)` - barrier for progress within a computation
* `(par)` - evaluate argument in parallel, in background
* `(eval)` - evaluate argument before progressing further
* `(lazy)` - defer computation, but share across copies
* `(stow)` - move large values to disk, load on demand
* `(optimize)` - rewrite function for efficient evaluation
* `(jit)` - compile a function for multiple future uses
* `(memo)` - memoize a computation for incremental computing
* `(nat)` - assert argument should be a natural number
* `(type)` - describe type of stack at given location
* `(seal)` - wrap types for safety or modularity
* `(quota)` - impose limits on argument evaluation effort

Awelon does not limit annotations much beyond the need for identity semantics.

## Accelerators

Accelerators are built-in functions, accessed via annotation of a reference implementation. Use of accelerators enables Awelon compilers or interpreters to extend the set of "performance primitives" available to Awelon programmers. For example, Awelon systems should accelerate natural numbers. We might use `(nat)` to indicate that a value should use an optimized representation for natural numbers under the hood. A function to add two natural numbers could be annotated via `[reference impl] (accel-nat-add)`. This tells our interpreter or compiler to replace the reference implementation by the specified built-in operator.

We aren't limited to conventional data types. Carefully designed accelerators can leverage cloud computing or GPGPU resources, making Awelon usable for problem domains like machine learning, and graphics processing. See later section on *High Performance Computing* for an expansion on this.

The main risk with accelerators is diminished portability between runtimes (and versions thereof). Fortunately, because acceleration is always driven by explicit annotations with reference implementations, there is very little risk of silent performance degradation or semantic drift. It's easy to detect and inform developers when an accelerator is not recognized or doesn't mean what we think it should mean.

*Aside:* When bootstrapping the language or developing new accelerators, we might temporarily accept acceleration without validation of the reference implementation. For example `["todo! nat add" (error)] (accel-nat-add)` might work until we're ready to provide the reference implementation.

## Dictionary

Awelon words are defined in a codebase called a "dictionary". A dictionary is essentially a key-value database, associating words to definitions. To support Awelon project's various goals, Awelon specifies a standard dictionary representation with convenient properties for import/export, versioning, sharing, scaling, etc.. Legibility is also a goal, to simplify debugging or inference of implementation. Awelon dictionaries can feasibly scale to many gigabytes or terabytes, support distributed representation, and feasibly integrate with block-chains.

The proposed representation is simple:

        /prefix1 secureHash1
        /prefix2 secureHash2
        :symbol1 definition1
        :symbol2 definition2
        ~symbol3

Each dictionary 'node' is represented by line-oriented ASCII text, with one line per entry, representing the update log. Each line uses a character prefix to indicate a definition (`:`), deletion (`~`), or indirection (`/`). Indirection uses radix indexing and identifies a binary node by secure hash. The prefix is removed from deeper nodes, so `:poke` moved into `/p` would become `:oke`. Entries are logically applied in order, as an update log. On lookup, it's sufficient to apply the final entry matching a symbol (whether it's `/p` or `:poke`). We will usually normalize a dictionary by removing masked entries then sorting.

This design gives us a [persistent](https://en.wikipedia.org/wiki/Persistent_data_structure) [log-structured merge](https://en.wikipedia.org/wiki/Log-structured_merge-tree) [radix tree](https://en.wikipedia.org/wiki/Radix_tree) over [content-addressed storage](https://en.wikipedia.org/wiki/Content-addressable_storage). It's feasible to share structure and storage with similar dictionaries. When synchronizing, we can incrementally download just the difference in dictionary nodes. Updates to deep tree nodes are buffered near the root, supporting lightweight updates and implicit working sets. In case of 'live' dictionaries, it is feasible to stream updates over a network and occasionally checkpoint via `/ secureHash`. Also, we can inspect a dictionary node or stream via conventional ASCII processing tools.

*Note:* Dictionaries nodes do not directly support comments. Instead, developers will use associative words (`foo-meta-doc` as documentation for `foo`) or embed descriptions within definitions (`"comment" (a2) d`). This ensures metadata is preserved independently of dictionary indexing, and is accessible for further computation or abstraction.

### Secure Hash Resources and Binary Large Objects

Besides use in the `/prefix secureHash` dictionary tree nodes, Awelon dictionaries may embed arbitrary binary large objects via `%secureHash` or oversized Awelon definitions via `$secureHash`. 
        
        :my-binary-large-object %secureHashOfBinary
        :my-oversized-function $secureHashOfDefinition

Use of secure hashes gives us many nice properties: immutable, acyclic, cacheable, securable, provider-independent, self-authorizing, self-authenticating, structure sharing, automatic naming, uniformly sized. In some contexts, such as synchronizing with a remote dictionary, we might download the unknown hashes lazily. Support for binary large objects is convenient for importing external data resources - image and sound data, CVS databases, and so on - while maintaining exact versions and snapshots. 

The favored secure hash is currently the 320-bit [BLAKE2b](https://blake2.net/) algorithm, represented as 64 characters in a variant [base32](https://en.wikipedia.org/wiki/Base32) alphabet.

        Base32 Alphabet: bcdfghjklmnpqrstBCDFGHJKLMNPQRST
            encoding 0..31 respectively

        Example hashes, chained from "test":

        rmqJNQQmpNmKlkRtsbjnjdmbLQdpKqNlndkNKKpnGDLkmtQLPNgBBQTRrJgjdhdl
        cctqFDRNPkprCkMhKbsTDnfqCFTfSHlTfhBMLHmhGkmgJkrBblNTtQhgkQGQbffF
        bKHFQfbHrdkGsLmGhGNqDBdfbPhnjJQjNmjmgHmMntStsNgtmdqmngNnNFllcrNb

*Security Notes:* The secure hash is essentially a bearer token, identifying and authorizing access to the binary resource. Any lookup must be careful to resist timing attacks that could iteratively discover stored hashes. Further, we might wish to salt sensitive data with entropy fields (or comments) to resist brute-force 'does data with this hash exist?' attacks. 

### Software Distributions and Packages

Awelon does not optimize for package-based software distribution. Instead, I encourage developers to favor holistic dictionary distribution models, taking inspiration from community wikis or github pull-requests. Many technical advantages attributed to packages - sharing, incremental compilation, download only what we need, etc. - can be adequately achieved via secure hash resources and caching. Holistic distribution can simplify problems related to package version configuration management and dependency hell. Socially, it also shifts control from package providers to dictionary users, who can freely extend or adjust the code and share it with their chosen communities.

However, Awelon systems can represent package-based software distribution by aligning packages with word prefixes. For example, a one-line entry `/packagename- secureHash` can install or update a specific version for an entire package. This might be suitable in cases where packages involve special licenses or subscriptions. *Namespaces* can be supported via projectional editing to mitigate verbosity from hierarchical names. For dynamic systems, developers can arrange for a bot to synchronize packages from a trusted source (see *Bots, Effects, and Applications*).

## Loops

Awelon disfavors recursive definitions. Instead, we use fixpoint combinators:

        [X][F]z == [[F]z]F
        z = [[(a3) c i] b (eq-z) [c] a b w i](a3) c i

        assuming:
            [def of foo](eq-foo) == [foo]
            [B][A]w == [A][B]       w = (a2) [] b a
               [A]i == A            i = [] w a d

Other loop combinators can be built upon `z`. For example, we can develop a `foreach` function that processes a list or stream. We could similarly preserve the name `foreach` in partially evaluated loops via `foreach = [(eq-foreach) ...]z`. Use of the *Named Locals* projection might help when defining new loop combinators.

*Aside:* I've frequently contemplated support for recursive word definitions. However, there are some persuasive arguments for avoiding them: inline all the things semantics, programs as finite sequences rather than graphs, no need for an external reference concept to explain or construct loops. Further, after we develop a library of loop combinators and common collections types, most use cases would be covered.

## Stowage

By annotation, large values could be moved from memory to environment:

        [large value](stow)    => [stow-id]
        [small value](stow)    => [small value]

Here `stow-id` must be a word representing `large value`. The stowage word should also be much smaller than the value. Further, it should be *stable* in the sense that the the same word tends to correspond to the same value or location even after minor changes to a computation. Stability is valuable for reusable memoization and also simplifies stable layout when rendering live computed views.

Stowage can serve as a controlled variation of [virtual memory](https://en.wikipedia.org/wiki/Virtual_memory) enabling larger than memory computations. Stowage also interacts nicely with *Memoization*, reducing large value comparisons to simple word version comparisons. Most importantly for Awelon's goals, stowage supports *progressive disclosure* when rendering large computed values in a user-interface. Without stowage, we have an inconvenient distinction between source data versus computed values.

The main weakness of stowage: it is not obvious how long to remember stowed values after performing a computation that produces them. This would depend very much on our evaluation context. We might need to model evaluation 'sessions' which can track `(stow)` and `(trace)` outputs. 

## Evaluation

Evaluation will rewrite an Awelon program to an equivalent Awelon program. In context of annotations like `(stow)` or `(trace)`, we might produce auxiliary outputs, but not in a way that can be observed within the Awelon computation. Awelon is a pure language, but effects will be modeled explicitly in some limited contexts (cf. *Bots, Effects, and Applications*). 

Primitives rewrite by simple pattern matching:

            [B][A]a => A[B]         (apply)
            [B][A]b => [[B]A]       (bind)
               [A]c => [A][A]       (copy)
               [A]d =>              (drop)

Words rewrite to their evaluated definitions. Under normal conditions, words will rewrite based on available input and this can be controlled via *Arity Annotations* (see below). The motive to defer rewriting is to retain human-meaningful symbols, structure, and projected views for evaluated results. Also, it avoids exponential expansion, and simplifies partial evaluation.

Awelon does not specify an evaluation strategy. That is, neither strictness nor laziness are part of Awelon's semantics. Program developers should guide evaluation through annotations such as `(par)`, `(lazy)`, and `(eval)` where it's important.

### Arity Annotations

Arity annotations are useful for Awelon, and have simple rewrite rules:

        [B][A](a2) == [B][A]
        [C][B][A](a3) == [C][B][A]
        ...

Arity annotations are useful to control rewriting and partial evaluation. For example, consider a swap function `w = [] b a` vs `w = (a2) [] b a`. In the former case, we might evaluate `[A]w => [[A]]a`. However, this isn't particularly useful to a human or an optimizer, so we might favor the latter where `[A]w` would not evaluate further because `w` is still lacking an operand. Arity annotations also support call-by-need computation. For example, `[[A](a2)F]` has the same observable behavior and type of `[[A]F]`, but the former will defer computation until another operand is supplied.

*Note:* Positional arguments do not scale nicely for human usability. Beyond three arguments, developers should favor aggregation of data into structures, at least for public APIs.

## Garbage Collection

Awelon has explicit copy and drop, and garbage collection is not a strong requirement. However, copy is a very common operation in Awelon. Thus, for performance, it can be convenient to either use tracing GC or reference counting. Awelon cannot form cyclic value structures, which simplifies GC.

## Memoization

We can support [memoization](https://en.wikipedia.org/wiki/Memoization) via annotations.

For example, `[Function](memo2)` might express that we should memoize the function together with its next two arguments. The runtime would find or create a memoization table specific to the function. Of course, memoization does impose significant overheads, especially in context of parallel or lazy arguments. Thus, it must be applied carefully or performance will suffer. 

Awelon can weakly support incremental computing via caching of a word's evaluated definition. However, full support for incremental computing requires use of explicit memoization together with cache-friendly patterns: compositional views over persistent data structures, use of stowage for shared tree nodes, etc..

## Error Reporting

Expected errors should instead be modeled as explicit return values, usually via sum types. This allows the errors to be handled by the function's client, rather than halting computation. However, for errors without recovery, we might use `(error)` annotations, which act as an explicitly undefined words and do not rewrite further.

## Debugging

Awelon debugging is based largely around replay. Because Awelon evaluates by local rewriting, it's easy to render (or even animate) the intermediate steps required to reach the current state. We also don't need breakpoints to introspect intermediate states. However, debugging by logging is also convenient, and we can support this via `(trace)` annotations, perhaps using `(trace-logname)` to allow easier filtering.

In context of effectful code (per *Bots, Effects, and Applications*), the challenges of debugging stateful behavior is ameliorated by transactional memory, the simple application model, and the ability to simulate execution of command sequences in a variety of sample environments.

## Static Typing and Safety Analysis

Awelon doesn't depend on static types insofar as there is no type-driven dispatch or overloading. However, the language does imply a simple structural type model. If programmers can discover errors earlier by static analysis, that's a good thing. Awelon's stack-like environment can easily be typed as a tuple, and values as functions. Record constructors are typed using row polymorphism. Types for our primitive operations:

        a           ((s * x) * (s → s')) → (s' * x)
        b           ((s * x) * ((e * x) → e')) → (s * (e → e'))
        c           (s * x) → ((s * x) * x)
        d           (s * x) → s
        [F]         s → (s * type(F))

Type annotations can be expressed using Awelon annotations. For simple cases, we can use specific type annotations like `(nat)` or `(bool)`, or encode a simple type within the annotation symbol. However, in general we will need a parameterized annotation, like `[Type Descriptor](type)d`. This enables flexible abstraction and composition of type descriptions. In context of the Awelon dictionary, we might also favor a naming convention where `foo-meta-type` should describe the type of `foo`.

*TODO:* I think Awelon would greatly benefit from a lightweight, dynamically enforceable stack notation.

### Opaque and Nominative Data Types

Awelon can simulate nominative data types via paired symbolic annotations:

        (seal-foo)      (s * x) → (s * foo:x)
        (unseal-foo)    (s * foo:x) → (s * x)

This would resist accidental access to data, and provide a better debugging experience by attaching symbolic tags to values. To support *opaque* data types, we additionally constrain where sealers are used, e.g. restricting use of `(seal-foo)` and `(unseal-foo)` to the `foo-*` volume of the dictionary. Enforcing this constraint is trival, and it would effectively give us package-private data types. Opaque data types can serve as a second-class approximation of [abstract data types](https://en.wikipedia.org/wiki/Abstract_data_type).

We should combine this with a restriction that `foo-local-*` words should only be used from `foo-*`, so we can implement package-private utility functions to work with our package-private data types.

*Aside:* These annotations operate on the top value of the stack. But we could have a variation that seals the entire stack type, to help guard stack 'frames'. Perhaps `(stack-seal-foo)` and `(stack-unseal-foo)`. 

### Multi-Stage Programming

Multi-stage programming (MSP) is about robust, type-safe control of *when* a compution occurs. This is useful for predictable performance. For Awelon, we might leverage annotations to express programmer intentions and assumptions. Consider paired annotations:

        [A](step-foo) => [A]
        [F](stage-foo) => [F]   iff [F] does not contain (step-foo)

A step is complete when it has a value. A stage is complete when all steps in that stage are complete. These annotations allow us to describe some second-class staging assumptions and assert that a stage should be complete. In context of static safety analysis, we could warn the developer whenever we cannot easily prove at compile-time that a stage is complete. To effectively use MSP will further require abstractions and projections.

### Static Computation and Deferred Typing

In context of compile-time metaprogramming (input macros, templates, embedded DSLs, etc.) it can be convenient to defer static type analysis until after partial evaluation with some static program inputs. This would allow "lightweight" dependent types that only depend on static values. 

In a conventional language, we might use a distinct API-level syntax for static parameters vs runtime parameters - such that invokations are like `foo<bar>(baz,qux)`. In Awelon, we must introduce an annotation like `[A](static) => [A]` to insist that `[A]` can be computed statically or depends only on parameters explicitly marked static within the caller (giving us a simple contagion model). This is a lot easier to check than type information, and is a lot weaker than full multi-stage programming.

### Advanced Types and Gradual Typing in Awelon

Ideally, we can ensure type-safe indexing of arrays, support 'unboxed' data types on stacks and in collections, protect uniqueness of memory references for efficient in-place mutation, ensure termination or big-O performance, track units for scientific computing, and guarantee the expected types are exchanged in an interaction between computations.

Support for all this will require a "sufficiently advanced" type system with features like phantom types, existential types, generalized algebraic data types, session types, indexed types, dependent types, types tracking termination assumptions, and so on. Developing these features is certainly a long-term project for Awelon systems.

In the mean time, we should enable programmers to work with partially type-safe codebases. The overall strategy I'm favoring for static safety in Awelon is *Gradual Typing*. 

## Structural Equivalence

Annotations might assert two functions are structurally equivalent:

        [A][B](eq) => [A][B]     iff A and B are structurally equivalent

A motivating case for structural equivalence assertions is merging two sorted data structures. Our assumption is that the key-comparison function is equivalent. With `(eq)`, we could represent this assertion. This could also be useful for lightweight unit testing, or for verifying our assumptions when defining projections.

Behavioral equivalence is not something we can generally test in a Turing complete language. But structural equivalence could include limited forms of behavioral equivalence comparisons.

## Projectional Editing

Awelon's simple syntax must be augmented by [projectional editing](http://martinfowler.com/bliki/ProjectionalEditing.html) techniques to support richer programming interfaces, DSLs, namespaces, application models, and larger programs. As a simple example, we could develop a numeric tower:

        #42         == Awelon's natural 42
        42          == [#42 int]
        -7          == [[7 int] negate]
        3.141       == [3141 -3 decimal]
        -0.0070     == [-70 -4 decimal]
        2.998e8     == [2998 5 decimal]
        -4/6        == [-4 #6 rational]

In this example, I build one view upon another, but we also have a comprehensible representation in without the view. For example, if our projectional editor lacks support for rationals, users would still see the `[-4 #6 rational]` representation. This is convenenient for iterative development of extensible views. Further, if carefully designed our data views be normal forms (evaluate to themselves) such that we can also render `-7` as an *output* from a computation. This may involve careful use of arity annotations. Besides numeric towers, projections can support command sequences, infix operators, lists and records, embedded comments via `"comment" (a2) d`, and other ad-hoc syntax extensions.

Ideally, projections must be designed in coordination with definitions and *Accelerators* to ensure the same projections can be reused after rewriting evaluations. A consistent projection for input, output, and intermediate computations contributes to Awelon's vision for user interfaces. This supports user comprehension and control, enabling users to trace displayed data to its source or extend processing with further computations.

Beyond textual projections, graphical projections are feasible - forms with sliders, checkboxes, and buttons. A color picker widget for a color value, a date picker widget for a date value. By developing some specialized projections, we can effectively provide an application user-interface. We can project over multiple dictionary definitions, perhaps modeling a spreadsheet or worksheet. See *Bots, Effects, and Applications*.

### Infix and Prefix Operators

Infix and prefix expression of operations can be convenient insofar as they reduce explicit nesting and contribute to concision, legibility, and familiarity. They may be worth pursuing in sophisticated textual projections over Awelon code. For projections over singular operators, we have three options:

        A + B == [A] [B] plus           (infix)
          + B ==     [B] plus           (prefix)
          +   ==         plus           (postfix)

In context of Awelon, I favor prefix and trivial postfix operators rather than true infix. Prefix operators don't affect anything we see before the operator and thus don't interfere with a *Named Local Variables* projection. We can do a lot with prefix operations, such as a lightweight projection over lists:

        , B == [B] cons
        .   == [null] cons
        [1,2,3.] == [1 [2 [3 [null] cons] cons] cons]

The cost of introducing prefix or infix operators is eventual need to deal with precedence and associativity, and to occasionally work around it. We could favor parentheses for precedence and escape our annotations (e.g. `#(trace)`). Or we could just require explicit blocks and inlining. 

*Note:* Awelon cannot overload words or operators. In some cases, a context-dependent projection might be appropriate. But *Generic Programming* requires separate attention.

### Named Local Variables

We can implement let and lambda expressions as an editable projection. 

Consider a lightweight syntax where `\x` indicates we "pop" a value from the stack and assign it to a local variable `x`, scoped to the remainder of our current definition or block (modulo shadowing). Thus `[\x EXPR]` serves the same role as `(λx.EXPR)`, while `[X] \x BODY` serves the same role as `let x = [X] in BODY`. We can represent this via bidirectional rewriting:

        \x EXPR == (var-x) T(x,EXPR) 
          assuming `x` represents a value
        
        T(x,E) | E does not contain x       == d E
        T(x,x)                              == 
        T(x,[E])                            == [T(x,E)] b
        T(x,F G)                            
            | only F contains x             == T(x,F) G
            | only G contains x             == [F] a T(x,G)
            | F and G contain x             == c [T(x,F)] a T(x,G)

This projection is robust and independent of user definitions. 

Unfortunately, it is not optimal for conditional behaviors where use of a variable in an if-then block would result in closures. This could eventually be solved by a more cohesive projection with built-in knowledge of common conditionals. Meanwhile, we should optimize by hand.

Despite caveats, local variables are a convenient projection in cases where tacit programming becomes a mess of data-shuffling, or for larger programs where the user will have difficulty tracking the stack.

*Aside:* If we want explicit `let` or `where` clauses, we could feasibly develop further projections over structures such as `[\x EXPR] [X] where`, starting from lambdas. 

### Namespaces

It is not difficult to recognize comments declaring qualified namespaces.

        using long-prefix as lp; lp-foo
            becomes
        "using long-prefix as lp"(a2)d long-prefix-foo

However, I don't recommend this! Namespace declarations in code too easily become a form of boiler-plate (especially for one-liner functions). They also interact awkwardly with copy-paste. Further, namespaces fixated in code don't allow for much user customization.

Instead, I propose to move namespaces to the editor layer: an editor can track a user's set of preferred nicknames together with other user-model data like a clipboard. Users can manage this set, perhaps switch between packages based on current project. It's up to each user and the editor's feature set. Further, it blends nicely into richer views - namespaces are essentially a very limited form of user-defined, pluggable view.

*Aside:* I'm interested in use of color as an alternative to prefixes, e.g. such that `html-div` is written in a different color from `math-div`. This would give us a more concise notation.

## Object Oriented Awelon

Ignoring static analysis, we can easily model objects in a functional language:

        type Object = Request → (Response * Object)

It's just a function, albeit often with a fixpoint closure for stateful objects. 

However, in context of static type safety, we usually want the response to depend on the request. This can be achieved by dependent types. But dependent types are more sophisticated than I would prefer to deal with in many cases. An alternative is to specialize type analysis for a `Request` model where requests include static method labels like `"draw-to-canvas"` as the top element on the stack. This essentially supports a 'lightweight' dependent type system where we only depend on static information.

Beyond type safety, we mostly need projections common object invocation and constructor patterns. This would easily build upon the static method labels, e.g. with `!draw-to-canvas` representing a method call to the object on the top of our stack, or `x!draw-to-canvas` representing a linear method call in context of *Named Local Variables* (a convenient equivalent to `x !draw-to-canvas \x`). 

Between fixpoint closures, lightweight dependent types, projections, and accelerators, Awelon can support most object-oriented patterns. We can further introduce *Command Sequences* to model shared objects, and objects that could interact with a network or other external device. 

Intriguingly, an `[Object]` can support *Projectional Editing* at two layers. First, we can project over the object constructor - the `[Object]` string - like we do for any other value. Second, we build projections above the *interface* to the object, i.e. invoking `"draw-to-canvas"` to produce a view, or invoking some methods that rewrite the object to represent user input events. 

## Labeled Data

Labeled data types, such as records and variants, are a staple of conventional programming languages. I've frequently contemplated built-in support for labeled data in Awelon. However, projections and accelerators should be adequate to the task without requiring new primitives.

Rather than a concrete trie or association list, I propose to model records as specialized objects that accept a specialized request. For example, we could use a model of the form:

        [[A] "a" rset "b" [B] rset ...](rec)

In this case, `rset` might be modeled a function that takes *three* arguments: a label-value pair `"a"` and `[A]`, followed by the request. The request will generally contain a fresh record and the label we're searching for or replacing. Each `rset` operation would handle and modify the request, so our final output is a modified form of the initial request, containing a copy of the record, less the extracted element (if any). A lightweight textual projection could render `"a" rset` as `:a` or similar. The `(rec)` annotation would provide hints for static typing and accelerated representation.

Labeled variants are functions that extract and apply a 'handler' from a record. So this might be represented as `[[A] "a" case]` with `case` selecting the handler.

Importantly, by avoiding use of a trie, association list, or other concrete representation, we greatly simplify reasoning about the observable properties of our record, which optimizations an accelerator may perform (such as reordering of labels) without affecting observable behavior, and our ability to abstract or compose records.

## Arrays and In-Place Mutation

In Awelon, arrays could be modeled as an accelerated list representation, or alternatively as an accelerated object similar to records. We could have accelerated functions to access or edit the array at some offset. Naively, editing the array at some offset involves copying the array with the edit in place. However, if an array has only one memory reference, we could save ourselves some trouble and instead mutate in-place.

This is related to the concept of linear type systems and uniqueness types. However, this could be applied even in a dynamic system, implicitly performing copy-on-write for shared arrays. With a few annotations, we could express our assumptions and stabilize performance.

Support for in-place mutation of arrays would be valuable for a variety of data structures (like hashtables) and algorithms (like union-find), and could also enhance initial construction performance for a trie or rope where we use arrays of children. We also benefit from in-place mutation of tuples or records, albeit to a lesser degree.

*Aside:* Accelerators could theoretically use a [persistent array](https://en.wikipedia.org/wiki/Persistent_array) implementation. But I believe we should keep arrays simple, and discourage huge arrays. Developers can explicitly model a persistent data structure when they want one, to better interact with stowage, memoization, parallelism, and laziness.

## Ephemeron Tables

To model a system of variables in a purely functional system naively involves representing variable references as natural numbers, and variable state with a hashmap. Unfortunately, this naive approach swiftly encounters a performance problem: we cannot garbage-collect the variable state.

To solve this, it is feasible to support [ephemeron tables](https://en.wikipedia.org/wiki/Ephemeron) as an accelerated object with specialized type safety properties. Ephemeron tables should be value objects that accept requests to allocate, read, write, and perhaps compare opaque 'variables'. For more declarative styles, we might allocate associative variables 'between' other variables.

However, variables references remain tied to the table in which they were initially allocated, or future versions via further allocations, writes, or copying. Attempting to use a variable reference with a different table should result in a type error (preferably static). Under this access restriction, it becomes safe to garbage-collect an entry in the table if all references to it are dropped.

*Aside:* Ephemeron tables should also support efficient mutations, e.g. leveraging linearity where feasible. Linear ephemeron tables would allow Awelon to perform well for imperative or OOP models and simulations, while conveniently retaining a purely functional semantics.

## Pattern Matching

Awelon does not have built-in support for pattern matching. However, it is feasible to model first-class patterns - and [parser combinators](https://en.wikipedia.org/wiki/Parser_combinator) in general - as simple compositions of first-class functions of type `source → (error + success)`. The error value could carry information about a partial match to improve efficiency of further efforts. 

First-class patterns are expressive and extensible. The challenge is to develop some convenient and efficient projections that correspond to decomposing lists or records, handling variants, and support for pattern guards and active patterns. The weakness of first-class patterns is that we won't have much compiler optimization for pattern matching - we could only rely on partial evaluation or acceleration.

*TODO:* Concrete examples, as they're developed.

## Command Sequences

A purely functional language can model interactive programs as functions that return either a command-continuation pair or a final result. The caller can interpret commands and provide results to the continuation, which is represents another interactive program. This creates an interaction loop. For interaction with the real-world, a runtime system can interpret a well defined API of commands.

Command sequences are a lightweight projection to hide use of continuations and support imperative expression and procedural abstractions for interactive programs. A suitable projection is simple:

        X ; Y ; Z == [X] [Y;Z] cseq == X [Y [Z] cseq] cseq

Here, `cseq` must be the last operation within a subprogram for the projection to apply. We can understand `;` as a unary prefix operator (`; Y == [Y] cseq`), because `X` is untouched. For data plumbing between commands, I assume this projection is applied above *Named Local Variables*. Thus, variables 

For procedural abstractions (including conditionals and loops), we assume any command `X` may transparently evaluate to a command sequence `X1 ; X2`. This requires *associativity*. That is, `X1 [X2] cseq [Y] cseq` must have the same observable behavior as `X1 [X2 [Y] cseq] cseq`. Further, the command sequence that merely returns its input should be an identity element, allowing abstraction of no-op behaviors. That is, `return;Y` and `Y;return` and `Y` should also share the same observable behavior.

Performance is a critical concern. In context of procedural abstractions, it is not unusual to construct deep procedural "call stacks". For example, if `Z` calls `Y` calls `X` then our call stack might include `[X2] cseq [Y2] cseq [Z2] cseq`. A direct composition of continuations can reconstruct the call stack, such as `[X2 [Y2] cseq [Z2] cseq]`. However, this touches `[Z2]` for every operation in `X`, resulting in a cost per operation that is linear in call stack depth. This is very bad! Instead, we want constant-time overheads for continuation management. Thus, we should construct a continuation `[X2 [Y2 [Z2] cseq] cseq]`. Achieving this requires an intermediate representation.

A minimum viable implementation:

        return = [none]
        yield  = [[null] some]
        cseq = w [i] [w [cons] b b [some] b] if

        Where:
            [L][R]none == L             none = w a d
            [L][R][X]some == [X]R       some = w b a d
            [C][L][R]if == [L][R]C      if = [] b b a i
            [F]i == F                   i = [] w a d
            [A][B]w == [B][A]           w = [] b a

In this case, we assume a command is either `return` or `yield`, with the actual return value or command simply represented by the remainder of our stack. The intermediate representation for the continuation is a simple list. We would process this list to produce an efficient continuation. However, exposure of this list is an abstraction leak that may hinder acceleration or static safety analysis. A better implementation might directly encode a list fold into a yielded continuation constructor.

In context of command sequence, we also have command models. A command model is a set of `yield` commands, each with with a type and documentation about intended consequences. Static safety analysis for command sequences would greatly benefit from a specialized extension that leverages static command labels like `"read"` vs `"fork"`. With this, a command model's type can be represented as a simple record type, albeit with a different header or annotation. 

*Note:* Command sequences can be understood as an free monad. However, I'm intentionally avoiding the monad abstraction or branding for Awelon, as it's historically been a tripping hazard for FP newcomers.

## Constraint Logic Programming

Constraint and logic programming are convenient styles for some problems, albeit terribly inefficient for the general use case. We can leverage *Command Sequences* to interleave expression of a constraint model with processing into an acceptable return value. However, we'll need concurrency to mitigate our rigid sequence ordering. A preliminary command model:

        type CC v a -- concurrent constraints
        new     : CC v (v a)
        write   : v a -> a -> CC v ()
        read    : v a -> CC v a
        fork    : CC v () -> CC v ()
        amb     : CC v a -> CC v a -> CC v a
        fail    : CC v a

Single-assignment variables and fork support deterministic concurrency, allowing us to defer reads until information is available. The amb operator models a non-deterministic choice, and provides our search space. A computation may explicitly fail, indicating that requirements are not met. A computation will implicitly fail if it writes to an assigned variable, enters a datalock on read, or if a forked subcomputation fails. 

Use of amb, fork, and single-assignment variables is quite expressive. For example, we can model a channel as a linked list with an unassigned variable at tail position. Writing a shared channel would involve amb to choose between writing vs reading the tail variable, iterating forward after read. The resulting channel would model all possible input permutations, including interactive ones between threads. Of course, searching all permutations is expensive. We could benefit from design patterns and command model extensions for guided search to hide irrelevant permutations.

        assoc   : v a -> v b -> CC v (v c)
        cost    : Nat -> CC v ()

One extension that I find intriguing introduces associative variables for ad-hoc metadata, enabling an extensible dialog around a variable rather than within one. Another useful extension introduces a cost model so programmers may preferentially guide search towards a subset of solutions.

This command model will ultimately be interpreted by a pure function, perhaps of form `runCC : forall v . CC v a -> Stream of a`. Efficient implementation requires *Ephemeron Tables* to eliminate old variables as we evaluate. The order of results and efficiency of this computation will depend on search heuristics within `runCC`. Relevantly, we cannot observe race conditions between threads. We can parallelize the promising search paths, but the first solution computed is not necessarily the first reported.

## Generic Programming

Awelon directly supports [parametric polymorphism](https://en.wikipedia.org/wiki/Parametric_polymorphism), which serves as a simplistic form of [generic programming](https://en.wikipedia.org/wiki/Generic_programming). Together with *Multi Stage Programming* for static parameters, we could get quite far with parametric polymorphism. However, parametric polymorphism can be verbose and awkward for many use cases.

*Constraint Logic Programming* offers an intriguing foundation for generic programming. Generic programming is essentially programming in terms of requirements, and constraints are as direct a representation of requirements as it's feasible to achieve without ad-hoc reflection. However, we would need to develop a layer above constraint programming to easily track shared registries and component metadata.

## High Performance Computing

*High Performance Computing* (HPC) requires taking advantage of GPGPU and cloud computation resources. This is important for a variety of problem domains - machine learning, image recognition, graphics processing, physics simulations, scientific computing, and so on.

For Awelon, we approach HPC via *Acceleration*. This constrains us to purely functional computations - deterministic, confined, and independent of physical configuration. Although this may hinder some use cases, it does reduce setup overheads and ensure the computations are repeatable, cacheable, sharable, and verifiable.

### Accelerated Vector Processing

To utilize GPGPU hardware from Awelon, we can model an abstract remote processor specialized for structured binaries. For example, we might push two binaries representing floating point matrices, ask the processor to multiply them, then request the result as another binary. 

In Awelon, we could model this remote processor as a purely functional object (see *Object Oriented Awelon*). Importantly, we can accelerate an object if we arrange for the same accelerated handler function to be used for every request with a hidden internal state. Pseudocode: 

        type Object = Request -> (Object * Response)

        process :: State -> Request -> (Object * Response)
        process st req = 
            let (st',resp) = ... a pure computation ...
            ((process st'), resp)

Here, `process` would be accelerated, and the `State` type (beyond the initial state) is hidden from external observers, which allows our accelerator to compute alternative state values, assuming observable behavior is preserved.

For accelerated vector processing, we must use requests and state model that are easy to implement on a GPGPU. Most of our design effort would be developing this set of requests.

However, one especially important consideration is our ability to register some code with the remote processor, such that we can invoke it later. By registering code ahead of time, we benefit in two ways: we can invoke the code many times without repeating the load effort, and our accelerated implementation of the processor may compile and cache code for the physical hardware.

*Aside:* We can extend our processor to a network of communicating processors with streaming binaries, using a model like *Kahn Process Networks* to preserve observable determinism. This would allow partitioning of a streaming computation across multiple GPGPUs. We might also benefit from an abstract processor for low-level bit-banging.

### Kahn Process Networks

[Kahn Process Networks (KPNs)](https://en.wikipedia.org/wiki/Kahn_process_networks) model deterministic, distributed computation. KPNs are excellent for event stream processing, task parallelism, and cloud computing. KPNs are monotonic, so it's feasible to interactively add input and extract output while the computation is ongoing in the background. 

A KPN process could be developed against a command model. Here, we use second-class port and process names instead of first-class channels and processes in order to simplify connectivity and abstraction.

        type KPN a -- command model for KPNs
        read : Port -> KPN Msg
        write : Port -> Msg -> KPN ()
        wire : Port -> Port -> (Msg -> Msg) -> KPN ()
        fork : ProcName -> KPN () -> KPN ()
        type Port = Outer PortName | Inner (ProcName * PortName)

We can fork child processes and declaratively wire inputs to outputs. The process may read or write ports that haven't been wired. In classic KPNs, read is the synchronous operation: it waits for data if none is available. We could extend this API with bounded buffers (so writes can wait), logical time (so reads can time out), and perhaps logical copy and drop of ports (for performance).

When we're done describing our KPN, we implement an accelerated object to "run" the KPN. The API for this might be simple like `runKPN : KPN () -> Object` (cf *Accelerated Vector Processing*). This object should accept read and write requests for the outer KPN ports, enabling pure code to interact with a deterministic process network computing in the background. The accelerated implementation can leverage threads and shared queues with in-place mutations, insofar as linearity is respected (cf *Arrays and In-Place Mutation*).

A significant challenge for KPNs is developing an adequate static safety analysis. Ideally, we want distinct message types per port, to statically forbid reading and writing of wired ports, and perhaps some variation on session types to ensure interactions won't get stuck. Like command models and object interface types, this could benefit from specialized annotations and type descriptors. Minimally, we could tag KPN process descriptions with `(kpn)`. 

## Bots, Effects, and Applications

Awelon applications are modeled as bots that publish projections to users.

### Bot Definition

An Awelon bot process is a deterministic transaction, repeated indefinitely.

Process coordination is implicit: Deterministic repetition of a read-only or failed transaction is obviously unproductive. Thus, we can improve system efficiency by waiting for a change among the observed variables. For example, a stream processing bot might voluntarily abort if an input queue is empty or output queue is full. Compared to locks and signals, this provides a robust means to wait for arbitrary conditions, and makes it relatively easy to preserve system consistency.

Preliminary API in pseudo-Haskell:

        type DebugName = String -- for debugging!
        type TX v e a -- command model for bots
        type Env v = ... -- model of host system
        type Bot = forall v e a . Env v -> TX v e a
        type EQ a = a -> a -> Bool -- conservative equality

        new     : DebugName -> TX v e (v a)
        assoc   : v a -> v b -> TX v e (v c)
        read    : v a -> TX v e (Maybe a)
        write   : v a -> (Maybe a) -> TX v e ()
        try     : TX v e a -> TX v e' (Either e a)
        abort   : e -> TX v e a
        fork    : DebugName -> TX v e a -> TX v e' ()

We would construct *Command Sequences* that perform these operations. We can manipulate variables via `read` and `write` operations. The `new` operation creates fresh unassigned variables, while `assoc` supports declarative, associative variables for modeling relationships. The `try` and `abort` operations support hierarchical transactions with strong exception safety, which simplifies partial failure and graceful degradation. The `abort` value may carry ad-hoc information about why the transaction failed. The `try` operation only handles `abort` - it would not catch a runtime type error or reading an unassigned variable.

The `fork` operation specifies a 'one-off' operation to attempt in unspecified order upon commit. This is intended for use in read-fork patterns, where the operation may be repeated until a variable observed by the parent transaction changes. With recursive use of read-fork, we can form a tree with read-write loops at the leaves, expand a single bot into multiple component bots. This supports task-concurrency, multi-stage programming, and convenient packaging of behaviors. Use of assoc is convenient for declaration of variables from a read-fork transaction. *Note:* If fork is used from a read-write transaction, I propose to abort the transaction and warn the programmer.

Bots are installed by simply defining `app-*` words in the dictionary. This ensures that the set of active behaviors in an Awelon system is easily discovered, managed, and extended. In the general case, we can modify bot definitions at runtime, which supports live programming, continuous deployment, and system administration. 

### Effectful Environment

Bots operate upon a collection of environment variables, `forall v . Env v`. 

Effects, such as network access, are achieved through manipulation of specific environment variables. For example, the environment may include a system task queue. After the write is committed, the system may process the request. A response is written back into the environment - a location would usually be specified in the request. This would be accessed by a future bot transaction. Hence, effects are always asynchronous.

A typical environment might support network access, a pseudo-filesystem or database, reflection over the dictionary, a shared registry so bots can publish 'services' for other bots, and so on. Application state - anything that should survive transactions - must be recorded in this environment. We don't have true 'private' state per se because that would hinder ad-hoc extension, auditing, and debugging. Instead, we partition a 'home' directory from a pseudo-filesystem for each bot, and optionally use security patterns to restrict distrusted bots.

For performance and maintenance reasons, this environment is *ephemeral*. That is, we may logically or physically 'reset' the system to use a fresh `forall v. Env v` environment. Assuming variables are ephemeral avoids costs related to serialization, and allows us to preserve nice properties like linear references or parallel background computation. Further, resetting is a convenient administrative solution to entering a bad state that wasn't caught by aborting a transaction.

Of course, some parts of the environment may *reflect* over durable storage, similar to how writes over a memory-mapped file are durably backed. In these cases, we might model synchronization to durable storage as a separate request.

Security is a relevant concern. Fortunately, the `forall v` constraint supports strong [capability security](https://en.wikipedia.org/wiki/Capability-based_security) for bot definitions: a bot is limited to the provided environment, and thus may easily be confined or sandboxed by simply wrapping the bot to intercept the environment. Type safety can be enforced using the same mechanisms as *Ephemeron Tables*. 

### Time Dependent Behavior

Assume we have a current time environment variable that contains a timestamp. Logically, we have a system clock that is blindly writing this variable. In concrete terms, it might be lazily computed when read. Either conflict analysis must be precise for blind writes, or we must simplify analysis by sharing the timestamp for concurrent transactions.

Interaction between a system clock and transaction-based process control can be awkward. To avoid a busy wait loop, we must not read time as part of a 'stable' transaction. A read-fork transaction must not read system time. A read-abort behavior should abort *before* reading current time to avoid immediate retry. Developers must work with time like any other variable having a high rate of change.

Waiting on time is a common requirement. Although we have transactional process control, that doesn't work for external network resources or human interactions. To wait for time, my idea is to write a future value to the current time variable. This would cause the transaction to wait until at least the specified time before committing. Thus, we can represent timeouts or network polling.

### User Interfaces

In general, bots may add some 'services' to a shared registry. Some of these services would be intended for other bots, but we would also register services that the user will interact with to obtain a projection over the environment. This supports tight integration between a projection and bot-created variables, active interfaces with notifications, and a simple security model insofar as administrators constrain bots and bots constrain lesser users.

Users will be able to interact with multiple projections, e.g. across multiple widgets or windows. These projections may be provided by multiple different bots. However, they should all use the same transaction. That is, users should be able to perform updates in multiple windows then 'commit' them all together. Further, even before committing, every time we perform an edit we should be able to reactively observe the change in every other window that depends on the modified variables. This supports lightweight, robust extension of user interfaces with alternative views, controllers, macros, etc. by adding new bots. It also simplifies debugging - a debugger interface is essentially just another UI extension.

Dictionary access is achieved via reflection through the effectful environment.

Of course, there will be a lot of scaffolding and bootstrapping required before we reach the level where we can define all interfaces via bots via Awleon codebase. Meanwhile, we must develop ad-hoc specialized development environments, which mostly focus on projections over the dictionary.

### Separate Compilation

Awelon's bots and user-interfaces can be compiled separately insofar as *reflection* features are not used. Of course, the compiler must provide a lightweight runtime that implements the 'system' implicit to the effectful environment. But it is quite feasible to develop bots that don't use reflection and can be separately compiled and installed in the manner of conventional servers or end user applications.

