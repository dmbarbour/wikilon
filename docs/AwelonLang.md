
# Awelon Language

## Vision and Design Overview

Awelon is a programming language and environment designed to empower individuals to control, create, comprehend, customize, extend, and share computation artifacts.

Major design elements:

* A scalable concrete environment, represented as a [log-structured merge tree](https://en.wikipedia.org/wiki/Log-structured_merge-tree) over [content addressable storage](https://en.wikipedia.org/wiki/Content-addressable_storage) with [prefix-aligned indexing](https://en.wikipedia.org/wiki/Radix_tree). This filesystem-like structure can support massive volumes, lazy downloads, incremental synchronizations, lightweight backups, atomic updates, and prefix-aligned sharing. Further, we can amortize physical storage and network costs within a community via proxy or [CDN](https://en.wikipedia.org/wiki/Content_delivery_network).

* A [purely functional](https://en.wikipedia.org/wiki/Purely_functional_programming) language evaluated under [rewriting](https://en.wikipedia.org/wiki/Rewriting) semantics to encode structured data and computations. The intention is to simplify sharing, caching, multi-stage programming, and user ability to inspect and comprehend computation through deterministic replay and rendering intermediate and final terms. Static analysis is possible but not required by the language.

* User interfaces leverage [projectional editing](http://martinfowler.com/bliki/ProjectionalEditing.html). Users can graphically manipulate code or data through tables, graphs, forms, flow charts, maps, and sheet music. The editor should be extensible with user-defined widgets. Computed terms can use the same view widgets as source, and live data should result in live displays. Because all user actions are modeled as edits, we can uniformly support undo and transactional macro-edits. 

* Automation is achieved by defining [bots](https://en.wikipedia.org/wiki/Software_agent). Bots can access the network and manipulate the Awelon environment. Concretely, bots are modeled as transactions that repeat indefinitely, implicitly waiting when unproductive, operating on system variables for auxiliary state and effects. This design is fail-safe, resilient, idempotent, securable, extensible, and safe to modify at runtime - which further supports live coding, runtime upgrade, and robust process control.

Applications are modeled as users and bots operating in the shared environment. Users view and manipulate the environment through suitable projections. We can extend our projectional editor with widgets specific to an application's data model, enabling ad-hoc front-end GUIs. User requests or tasks are written into the environment where a bot would see them. Bots would operate on structured data. Frequently, we'll want to model the shared context as a first-class value.

However, we may also model conventional web applications via bots interacting with the network. We might eventually bootstrap our projectional editors as web applications. And even without bot support, we can model many useful applications - REPLs, spreadsheets, forums, calculuators, image processing, simulators, document editors. 

## Language Basics

Awelon builds upon a foundation of four primitive concatenative combinators:

        [B][A]a == A[B]         (apply)
        [B][A]b == [[B]A]       (bind)
           [A]c == [A][A]       (copy)
           [A]d ==              (drop)

These few primitives form a Turing complete computation language. Programmers will further define a dictionary of words, which are trivially equivalent to their inline definitions. Awelon also has limited support for embedded data - natural numbers, texts, and binary large objects - with implicit definitions (see below). Awelon evaluates by rewriting, but can usefully be understood as a purely functional [stack-based programming language](https://en.wikipedia.org/wiki/Stack-oriented_programming) with special handling in case of stack underflow (a program `[A]a` simply won't evaluate further). 

An Awelon compiler or interpreter will further recognize a set of *Annotations*, represented by parenthetical words such as `(par)`, `(trace)`, `(nat)`, or `(error)`. Unlike user-defined words, annotations always have the same formal meaning: identity. That is, adding or removing annotations should not affect a program's formal behavior. But it can affect a lot of informal behaviors - evaluation strategy, memoization, data representation, quotas, guidance for JIT compilation, static analysis, fail-fast assertions, debugger integration, rendering hints, and so on. Annotations can express *programmer intentions*.

*Accelerators* are a critical subset of performance annotations. Accelerators enable Awelon systems to be extended with new performance primitives - floating point arithmetic, arrays, SIMD vector processing, etc.. The premise is that we can replace a naive Awelon model by an equivalent compiler or interpreter supported built-in. For example, lists can be considered a model of arrays. An `(array)` annotation could tell our compiler to represent a list as a compact array under-the-hood. Then use of `[list-index](accel-array-index)` can use an efficient offet-based index to the list represented as an array.

Awelon's lightweight syntax is intended for use with *Projectional Editing*. We can render a program as a rich user-interface. After we evaluate by rewriting, we can render the result using the same projections. We can also partially evaluate and render intermediate states. Without projections, Awelon's syntax does not scale nicely, though it's adequate for one-liner queries or simple debugging REPLs.

## Words

Words (excepting the few key words) are the user-definable unit in Awelon. Syntactically, a word is a sequence of lower-case alphanumerics and hyphens, starting with an alpha. Regex: `[a-z][a-z0-9-]*`. The meaning of a word is a trivial equivalence to its definition. Valid definitions must be block-balanced (no unmatched `[` or `]` block delimiters) and acyclic (see *Loops*).

An Awelon system may enforce some ad-hoc structure on definitions. For example, a development environment might associate `foo-meta-doc` with `foo` and complain when `*-meta-doc` words do not evaluate to a recognizable documentation type. To simulate package-local definitions, we might reject use of `foo-local-*` outside scope of `foo-*`. A projectional editor could provide special rendering for structured words, e.g. enabling use of the CJK character set or iconography. We could use `tag-*` words for reverse lookups, and reserve `ref-*` words for Awelon's process model. It's the responsibility of Awelon system designers to ensure these constraints never become onerous.

*Aside:* Awelon does not have built-in support namespaces. However, in context of projectional editing, we can use hyphenated prefixes like `foo-*` as ad-hoc namespaces. See section on *Namespaces*, below. 

## Natural Numbers

Awelon has limited support for natural numbers. Syntactically, natural numbers are represented by regex `0 | [1-9][0-9]*`. Semantically, natural numbers are Awelon words with an automatic definition.

        0 = [zero]
        1 = [0 succ]
        2 = [1 succ]
        42 = [41 succ]

Definition of `succ` and `zero` - and hence our model for natural numbers - is in theory left to our developers. For example, we could select between a recursive sum encoding (`type Nat = μN.(1+N)` where `type (A+B) = ∀r.(A→r)→(B→r)→r`). Or we could favor the traditional Church encoding (`type Nat = ∀x.(x→x)→x→x`). Or we could use model a list of booleans and define `succ` appropriately. In practice, however, this is not a decision of regular programmers. Instead, support from *Accelerators* will inevitably determine the favored model.

Awelon does not provide a rich numeric tower. Instead, it's left to *Projectional Editing* and *Accelerators* to build upon natural numbers.

## Embedded Texts

Awelon has limited support for embedding texts inline between double quotes such as `"Hello, world!"`. Semantically, embedded texts are special words defined automatically by template:

        "" = [null]
        "hello" = [104 "ello" cons]

That is, texts are simply an ASCII-encoded list of bytes. Like natural numbers, `null` and `cons` must be defined in the dictionary, and *Accelerators* will determine the de-facto standard definitions.

Embedded texts are limited to ASCII minus control characters and double-quote. There are no escape sequences! However, it is not difficult to wrap a text with processors to rewrite with user-defined escapes, e.g. `["hello\nmulti-line\nworld" literal]` might evaluate to a binary with line-feeds in place of each `\n` sequence. Similarly, we could support base64 binary representations. Working with multi-line texts and inline binaries can be made more convenient with support of *Projectional Editing*. 

*Note:* For larger texts and binaries, developers are encouraged to leverage *Secure Hash Resources* at the dictionary layer. For even larger ones, a rope-like data structure with binary blocks may be appropriate. Embedded texts are intended for labels, test data, inline comments, one-liner micro-DSLs, etc..

## Annotations

Annotations are special parenthetical words, such as `(par)` or `(error)`.

Annotations always have the same formal semantics: identity. That is, adding or removing annotations should not affect a correct program's observable behavior. However, within this limitation, annotations are assigned ad-hoc *informal* semantics by the runtime or compiler. For example, `[A](par)` can request parallel evaluation of `[A]`, `(trace)` could print to a debug console, and evaluating `(error)` can cause computations to fail fast. In general, annotations augment performance, safety, debugging, and display of programs. They encode any *programmer intentions* other than functional behavior.

Some potential annotations:

* `(a3)` - await at least three arguments on stack
* `(trace)` - print argument to debug console or log
* `(error)` - prevent progress within a computation
* `(par)` - evaluate argument in parallel, in background
* `(eval)` - evaluate argument before progressing further
* `(stow)` - move large values to disk, load on demand
* `(optimize)` - rewrite function for efficient evaluation
* `(jit)` - compile a function for multiple future uses
* `(memo)` - memoize a computation for incremental computing
* `(nat)` - assert argument should be a natural number
* `(box)` - specify runtime representation is a reference
* `(type)` - describe type of stack at given location
* `(seal)` - wrap types for safety and modularity
* `(quota)` - impose limits on argument evaluation effort

Awelon does not limit annotations much beyond the need for identity semantics.

## Accelerators

Accelerators are built-in functions, accessed via annotation of a reference implementation. Use of accelerators enables Awelon compilers or interpreters to extend the set of "performance primitives" available to Awelon programmers. For example, Awelon systems should accelerate natural numbers. We might use `(nat)` to indicate that a value should use an optimized representation for natural numbers under the hood. The function to add two natural numbers could be annotated via `[reference impl] (accel-nat-add)`, which we subsequently inline. This tells our interpreter or compiler to replace the reference implementation by the specified built-in operator.

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

        [X][F]z == [X][[F]z]F
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

Words simply rewrite to their evaluated definitions. However, this rewrite should be lazy, deferred if it does not result in progress of the computation. The motive for lazy rewriting of words is to retain human-meaningful symbols and structure in the evaluated results, enabling humans to grok the result and supporting graphical projections of the results using the same *Projectional Editing* models. By 'progress' we certainly need more than inlining a word's definition. Preferably, rewriting a word results in at least one value available for further computation.

Evaluation of loops should be tail-call optimized where feasible. In context of Awelon, we might implement this by recognizing common `a d` sequences at the end of a function, or at least building tail-calls into accelerators. However, working with relatively large stacks also shouldn't be a problem for Awelon programs.

Developers may guide evaluation strategy through annotations. For example, we might use `(par)` to request parallel computation of a value. We might use `[F](jit)` to request compilation for a function or loop body. We could use `(lazy)` and `(eval)` to recommend call-by-need or eager evaluation. By making performance annotations explicit, we can also avoid silent performance degradation by raising a warning or error when the annotation cannot be implemented.

*Aside:* Many languages have experimented with user-defined rewrite rules. The typical example is equivalent to `[F] map [G] map == [F G] map`. It is feasible to extend Awelon with rewrite rules, driven by annotations, leaving the burden of proof to the developers. However, I'm inclined to discourage this because such optimizations are fragile to abstraction and rules-set extension. Instead, I would suggest constructing an intermediate DSL as a data type, then rewriting it via function.

### Arity Annotations

Arity annotations are useful for Awelon, and have simple rewrite rules:

        [B][A](a2) == [B][A]
        [C][B][A](a3) == [C][B][A]
        ...

Arity annotations can be used to help control rewriting and partial evaluation. For example, consider a swap function `w = (a2) [] b a`. If our runtime has a naive view of 'progress', we might rewrite `[A]w => [[A]]a`, which is not useful progress. With the arity annotation, `[A]w` does not evaluate further, instead `[B][A]w` evaluates directly to `[A][B]`. 

Arity annotations are also useful for modeling codata. For example, `[[A](a2)F]` has the observable behavior and type of `[[A]F]`, but the former defers computation until the result is required.

### Garbage Collection

Awelon has explicit copy and drop, so garbage collection is not a strong requirement. However, copy is not uncommon, so we'll probably want to avoid deep-copies. Thus, evaluation will likely involve reference counting or tracing garbage collection. 

## Memoization

Annotations can easily indicate [memoization](https://en.wikipedia.org/wiki/Memoization).

For example, `[Function](memo2)` might express that we should memoize the function together with its next two arguments. The runtime would find or create a memoization table specific to the function, likely specialized for the expected argument type. Of course, memoization does impose some overheads, so it must be applied carefully or performance will suffer. Effective incremental computing requires use of memoization together with cache-friendly patterns: compositional views over persistent data structures, stable *Stowage* identifiers, etc..

*Aside:* Awelon systems are purely functional, but memoization over time-series data can model many stateful applications.

## Error Reporting

Expected errors should instead be modeled as explicit return values, usually via sum types. This allows the errors to be handled by the function's client, rather than halting computation. However, for errors without recovery, we might use `(error)` annotations, which act as an explicitly undefined words and do not rewrite further.

## Debugging

Awelon debugging is based largely around replay. Because Awelon evaluates by local rewriting, it's easy to render (or even animate) the intermediate steps required to reach the current state. We also don't need breakpoints to introspect intermediate states. However, debugging by logging is also convenient, and we can support this via `(trace)` annotations, perhaps using `(trace-logname)` to allow easier filtering.

In context of effectful code (per *Bots, Effects, and Applications*), the challenges of debugging stateful behavior is ameliorated by transactional memory, the simple application model, the free monad, and the abstract variable type. It is feasible to continuously test behavior under a variety of purely functional simulations.

## Static Typing and Safety Analysis

Awelon doesn't depend on static types insofar as there is no type-driven dispatch or overloading. However, the language does imply a simple structural type model. If programmers can discover errors earlier by static analysis, that's a good thing. Awelon's stack-like environment can easily be typed as a tuple, and values as functions. Record constructors are typed using row polymorphism. Types for our primitive operations:

        a           ((s * x) * (s → s')) → (s' * x)
        b           ((s * x) * ((e * x) → e')) → (s * (e → e'))
        c           (s * x) → ((s * x) * x)
        d           (s * x) → s
        [F]         s → (s * type(F))

Type annotations can be expressed using Awelon annotations. For simple cases, we can use specific type annotations like `(nat)` or `(bool)`, or encode a simple type within the annotation symbol. However, in general we would use a parameterized annotation like `[Type Descriptor](type)d`. This allows us to abstract and compose type descriptions. In context of the Awelon dictionary, we might also follow a naming convention where `foo-meta-type` should describe the type of `foo`.

### Opaque and Nominative Data Types

Implementation-hiding modularity in functional programming languages is frequently based around [opaque data types](https://en.wikipedia.org/wiki/Opaque_data_type) as a second-class approximation of [abstract data types](https://en.wikipedia.org/wiki/Abstract_data_type). Direct access to data representation is confined to an easily controlled volume of code. Code outside this privileged volume must interact with the data through a limited interface. This allows developers to enforce structural invariants, provide smart constructors that validate data, etc.. A prerequisite for opaque data types is [nominative types](https://en.wikipedia.org/wiki/Nominal_type_system), which allow us to restrict use of otherwise structurally equivalent types based on type names.

Awelon can support nominative data types via paired symbolic annotations:

        (seal-foo)      (s * x) → (s * foo:x)
        (unseal-foo)    (s * foo:x) → (s * x)

We can then impose a simple rule: that `(seal-foo)` and `(unseal-foo)` may be used in source only from words that match `foo-*`. This rule is trivially enforcable by linter. By enforcing it, we confine direct access to our sealed data to the `foo-*` volume of our codebase, which aligns conveniently with Awelon's ad-hoc packages. We can further enforce that `foo-local-*` words may only be directly used from `foo-*`, to support package-private helper functions.

*Aside:* We might also support sealing entire stacks. This would be convenient for debugging stack errors and restricting arity.

### Partial Typing and Escape Hatches

Unfortunately, many concerns and design patterns are difficult to type: heterogeneous collections, type-safe indexing of arrays, physical unit tracking (kilometers vs miles vs liters), big-O performance, static allocations and in-place update for arrays, interpreting strings into functions, typed ASTs for DSLs, resource discovery and adaptation, modeling objects as closures where each method has a suitable return type. 

To address these requires a "sufficiently advanced" type system with support for phantom types, indexed types, generalized algebraic data types, dependent types, linear types, existential types, performance and latency types, and so on. However, support for expression, validation, and inference of advanced types will certainly be a long-term project. Further, there is a cost to expressing and validating these types that may be unsuitable for fluid programming contexts like rapid prototyping.

As a design philosophy, Awelon systems may encourage use of types but should make it easy to work partially type-safe codebases or partial validation of declared types. And for common cases like interpreting a static DSL string into a function, it should be convenient to defer typing until after the function is produced.

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

In this example, I build one view upon another, but we also have a comprehensible representation in without the view. For example, if our projectional editor lacks support for rationals, users would still see the `[-4 #6 rational]` representation. This is convenenient for iterative development of extensible views. Further, if carefully designed our data views be normal forms (evaluate to themselves) such that we can also render `-7` as an *output* from a computation. This may involve careful use of arity annotations.

Besides numeric towers, projections could feasibly support lists and matrices, monadic programming, pattern matching, list comprehensions, and other features. Problem specific languages can frequently be modeled as data-structures that we evaluate statically. Embedded comments can also be represented, e.g. as a view of `"comment"(a2)d`.

Besides textual views, projectional editors can support graphical editor-viewer widgets. For example, to edit a color or date-time, we might provide a color-picker or calendar widget. Intriguingly, we can support multiple widgets for a given volume of code, with an edit in one reactively affecting the others. Further, we can feasibly support *specialized* view or editor widgets for an application's data model - thus providing Awelon's version of a GUI application front end.

Our projectional editors may further support views of the dictionary, not just views of a specific program. Ad-hoc sessions could perhaps be encoded as `[foo][bar][baz]`, loading all three definitions into an editor. But we can also use structured sessions, e.g. render `foo-c1`, `foo-b3`, etc. as a spreadsheet `foo` with each cell as a separate word. A REPL or notebook application might use a word per line, implicitly continuing the prior line (e.g. `:repl-3 repl-2 command3`). 

### Named Local Variables

Although tacit programming styles are suitable for many problems, they make an unnecessary chore of sophisticated data shuffling. Fortunately, we can support conventional let and lambda expressions as a projection. Consider a lightweight syntax where `\x` indicates we'll "pop" a value from the stack and assign it to a local variable `x`, scoped to the remainder of our current definition or block (modulo shadowing). Thus `[\x EXPR]` becomes equivalent to `(λx.EXPR)`, while `[X] \x BODY` effectively simulates `let x = [X] in BODY`. We can represent this via bidirectional rewriting:

        \x EXPR == (var-x) T(x,EXPR) 
          assuming `x` represents a value
        
        T(x,E) | E does not contain x       == d E
        T(x,x)                              == 
        T(x,[E])                            == [T(x,E)] b
        T(x,F G)                            
            | only F contains x             == T(x,F) G
            | only G contains x             == [F] a T(x,G)
            | F and G contain x             == c [T(x,F)] a T(x,G)

This design is robust and independent of user definitions. However, it isn't optimal for conditional behaviors where we select one branch to apply. In those cases, we might produce unnecessary closures. So we may need to extend our rewrite rules for a few known special cases, or rely upon a compiler and optimizer to eliminate unnecessary closures, or simply hand-optimize those cases. We might also tweak the algorithm `T` to work nicely with partial programs, and partial extractions. Regardless, use of named locals are convenient for use cases involving sophisticated data shuffling or ad-hoc closures.

*Aside:* Named locals were, at least to me, the first convincing proof that sufficiently advanced *Projectional Editing* is a viable alternative to sophisticated built-in syntax.

### Namespaces

It is not difficult to recognize comments declaring qualified namespaces.

        using long-prefix as lp; lp-foo
            becomes
        "using long-prefix as lp"(a2)d long-prefix-foo

However, I don't recommend this! Namespace declarations in code too easily become a form of boiler-plate (especially for one-liner functions). They also interact awkwardly with copy-paste. Further, namespaces fixated in code don't allow for much user customization.

Instead, I propose to move namespaces to the editor layer: an editor can track a user's set of preferred nicknames together with other user-model data like a clipboard. Users can manage this set, perhaps switch between packages based on current project. It's up to each user and the editor's feature set. Further, it blends nicely into richer views - namespaces are essentially a very limited form of user-defined, pluggable view.

*Aside:* I'm interested in use of color as an alternative to prefixes, e.g. such that `html-div` is written in a different color from `math-div`. This would give us a more concise notation.

## Monadic Programming

The [monad pattern](https://en.wikipedia.org/wiki/Monad_%28functional_programming%29) is convenient for explicit modeling of effects, exceptions, backtracking, lightweight APIs or DSLs, and multi-stage programs. However, this pattern requires syntactic support, lest it devolve into an illegible mess of deeply nested functions. Consider a lightweight candidate projection for monadic command sequences:

        X; Y; Z == X [Y;Z] cseq == X [Y [Z] cseq] cseq

This projection flattens nesting, enabling direct expression of program logic. The command sequence combinator `cseq` binds our continuation `[Y;Z]` to the result of computing `X`. To complete our monad, we must define a pure `return` such that `return [Y] cseq` locally rewrites to `Y`. To fit with Awelon's tacit stack programming style, we return the data stack rather than an explicit value. This projection conveniently layers with *Named Local Variables* such that `Z` can access variables defined at `X` or `Y`. Hence, we get conventional variable scoping with no extra effort.

Of course, this projection conveniently supports only one monad. I propose a variant of the *operational monad*, which is adaptable and extensible through the operation type, and supports local definition of effect handlers. See also [Operational Monad Tutorial](https://apfelmus.nfshost.com/articles/operational-monad.html) by Heinrich Apfelmus or [Free and Freer Monads](http://okmij.org/ftp/Computation/free-monad.html) by Oleg Kiselyov. With the operational monad, one monad is sufficient for all monadic programming use cases.

In Awelon, we can model the operational monad by defining `yield` as our only alternative to `return`. Before we yield, we can place a request continuation on the data stack where our caller knows to find it. This provides an opportunity for intervention or interaction between computations. Modulo type safety, an implementation of this idea is quite simple:

        return = [none]
        yield = [[null] some]
        cseq = w [i] [w [cons] b b [some] b] if

        Assuming:
            [L][R]none == L             none = w a d
            [L][R][X]some == [X]R       some = w b a d
            [C][L][R]if == [L][R]C      if = [] b b a i
            [F]i == F                   i = [] w a d
            [A][B]w == [B][A]           w = [] b a

Normally, `yield` will be encapsulated within the definition of basic operations like `[Var] [Val] write`. Thus, in normal code, only `return` is explicitly visible, and would usually appear only as the final step of a command sequence. The continuation when yielding is captured into a reverse-ordered list. This intermediate representation simplifies deep procedural abstractions which easily result in patterns like `yield [X] cseq [Y] cseq [Z] cseq`. By processing these continuations in reverse order, we can construct the performance and projection friendly `[X [Y [Z] cseq] cseq]`.

The biggest remaining challenge for monadic programming in Awelon is static type safety. Without fancy dependent types, or at least generalized algebraic data types, it's difficult to validate safety of request-response patterns where a response type should upon the request (e.g. where type of a variable read depends on the variable). Eventually, hopefully, Awelon systems will support sufficiently advanced types. But in the interim, we'll either accept risk of runtime type errors or model operations such that we have simple homogeneous types.

## High Performance Computing

Effective support for High Performance Computing (HPC) is essential for many problem domains: machine learning, graphics processing, physics simulations, and so on. Minimally, we should support both task-parallel cloud computing and data-parallel GPGPU computing. 

This section describes how we might support HPC in Awelon.

### Kahn Process Networks

[Kahn Process Networks (KPNs)](https://en.wikipedia.org/wiki/Kahn_process_networks) are model for deterministic, distributed computation. KPNs are excellent for event stream processing, task parallelism, and cloud computing. KPNs are monotonic - we can add an input without invalidating prior outputs. Because KPN behavior is independent of the process scheduler, accelerators can use opportunistic scheduling and buffering based on physical data races. KPNs offer a tremendous boost in expressiveness compared to `(par)` annotations. 

I propose we model KPNs monadically. The monad can abstract process behavior and encapsulate state, and we can focus *Acceleration* on just a few monadic operators. A preliminary monadic API sketch:

        read : Port a -> KPN a
        write : Port a -> a -> KPN ()
        wire : Port a -> Port b -> (a -> b) -> KPN ()
        fork : ProcName -> KPN () -> KPN ()
        type Port a = Outer PortName | Inner (ProcName * PortName)

This API uses second-class ports with external wiring to control connectivity. A process is only able to reference its external ports and those of its immediate `fork` children. We could easily extend this API with bounded buffers and temporal semantics to simplify memory control and reactive behavior. I'm hand-waving over type safety here - initial accelerators could use dynamic types if needed.

Acceleration of a `eval-kpn` function might then be implemented by instantiating an interpreter-intrinsic KPN "object" with a distinct method for each operator. This object may use queues and threads under the hood, or even distributed nodes if the scale is sufficient, so long as the observed results match the reference implementation. We can also leverage the monotonicity of KPNs to evaluate them incrementally, awaiting first output on a given port.

*Aside:* KPNs do not admit network disruption. But they're pure, so we can recompute if needed, and perhaps maintain checkpoints for a long-running computation.

### Accelerators for Data Parallelism

A simple approach to data parallelism is to push structured binary data and operations to a specialized remote processor. When we're done computing, we can fetch the binary result. In Awelon, we can model this communication as a monadic API. Insofar as the remote computation is deterministic and confined, we can *Accelerate* evaluation, perhaps leveraging an available GPGPU or a CPU with SIMD extensions.

The main difficulty is developing a suitable model of the remote processor (the set of operations, data types, stacks or registers or channels, etc.). This is essentially a language design problem. We may seek inspiration from linear algebra, collections-oriented languages like APL and J, or perhaps Haskell's [accelerate](http://hackage.haskell.org/package/accelerate) or [lambda cube](http://lambdacube3d.com/) packages. Certainly, we're more likely to pay for those communication overheads if small operations correspond to expensive computations.

Intriguingly, we could extend this idea to communicating with *multiple* processors, and orchestrating deterministic communications between them. This would make it possible to partition a large or streaming data computation across multiple GPGPUs without transitioning large volumes of data through the Awelon processor.

## Arrays and In-Place Mutation

Awelon accelerators could represent lists as arrays. For unique arrays - where we have only one reference - we could feasibly accelerate indexing operators to mutate the array in place. This idea can extend to tuples and records, too. It is feasible to statically track uniqueness and typefully enforce these nice performance properties. It's also feasible to just use reference-counted arrays and dynamically optimize if we know we have a single reference.

Alternatively, we could adapt and accelerate Haskell's [ST monad](http://hackage.haskell.org/package/base-4.12.0.0/docs/Control-Monad-ST.html), accelerating a `run-st` handler over a small set of monadic operators. ST has an advantage of being explicit, but it's also unfriendly to parallelism or incremental input. 

I favor uniqueness types over ST, but the latter might be an easier start. Either way we approach it, effective support for arrays and in-place mutation would be convenient for performance of many algorithms like union-find, hash-based duplicate detection, or optimizing initial construction of a search tree.

## Labeled Data and Records

Labeled data types, such as records and variants, are a staple of most programming languages. I've frequently contemplated built-in support for labeled data in Awelon. However, editable views and accelerators should be adequate to the task without requiring new primitives.

Naively, a record could be encoded as an association list, and a variant as a function that selects and applies a handler from an association list. However, the specific concrete choice of "association list" can be awkward for many use cases, as would other specific choices like "trie". 

I propose instead that Awelon favors record *constructors* - i.e. we use `[[A] "a" put [B] "b" put ...](record)`. The block represents a function writing to an opaque record representation (perhaps a trie). The `(record)` annotation then indicates that this function should have the type of a simple record constructor, and informs our runtime to use an accelerated record representation. This preserves commutative behavior and error semantics of `put` operations, and simplifies functional abstraction of records. Record constructors are also easy to recognize and manipulate through a projectional editor.

## Data Representation

Precise control over data representation can improve performance by reducing indirection and heap memory pressure. For example, we might represent a tuple `[[T1][T2][T3]]` as three values on our stack, or represent a known-to-be-small natural number as a 32-bit word.

For Awelon, it is feasible to control data representations through annotations. However, it is not trivial to do so. We'd also need to track these data representations and adapt behavior of functions based on representations provided. Fortunately, most of the costs can be shifted to the compiler, while programmers mostly use annotations to declaratively assert or convert representations.

Support for precise data representation is a long-term goal for Awelon systems. It is of relatively low priority. Short term, it may be sufficient to specialize arrays of numbers or binary structures. 

## Multi-Stage Programming

Multi-stage programming (MSP) is about explicit, robust control of *when* a compution occurs. This is useful for predictable performance. For Awelon, we can leverage annotations to express programmer intentions and assumptions. Consider paired annotations:

        [A](step-foo) => [A]
        [F](stage-foo) => [F]   iff [F] does not contain (step-foo)

A step is complete when it has a value. A stage is complete when all steps in that stage are complete. These annotations allow us to describe our staging assumptions and assert that a stage should be complete, but do not directly implement any software patterns to simplify staging. We might additionaly leverage monadic programming or intermediate DSLs to support staging.

*Aside:* In addition to controlling computation, MSP may benefit from control over just-in-time compilation (JIT). This could be supported via `[F](jit)` annotations. Controlling JIT can improve performance stability compared to the ad-hoc tracing JIT seen in conventional languages.

## Generic Programming

Awelon supports polymorphism. We can implement lists once for many data types, or an operational monad for many operation types. This supports a weak form of generic programming. However, Awelon does not support overloading of symbols. For example, we cannot have one `add` symbol that automatically selects the appropriate function based on whether the arguments are natural numbers, floating point, or matrices.

This could feasibly be mitigated by *Multi-Stage Programming*: we could develop a stage that propagates type information and other static metadata. This would allow `add` to select the appropriate function based on context. Projectional editors could further help, making it more convenient to work with `add<T>` functions where `T` represents our static metadata.

## Bots, Effects, and Applications

An Awelon bot process is a deterministic transaction, repeated indefinitely.

Process coordination is implicit. Deterministic repetition of a read-only or failed transaction is obviously unproductive. Thus, we can improve system efficiency by waiting for a change among the observed variables. For example, a stream processing bot might voluntarily abort if an input queue is empty or output queue is full. Compared to locks and signals, this provides a robust means to wait for arbitrary conditions, and makes it relatively easy to preserve system consistency.

Preliminary API in pseudo-Haskell:

        type DebugName = String -- for debugging!
        type TX v e a -- opaque, monadic
        type Env v = ... -- model of host system
        type Bot = forall v e a . Env v -> TX v e a
        type EQ a = a -> a -> Bool -- conservative equality

        new     : DebugName -> TX v e (v a)
        read    : v a -> TX v e (Maybe a)
        write   : v a -> Maybe a -> TX v e ()
        modify  : (Maybe a -> Maybe a) -> v a -> TX v e ()
        try     : TX v e a -> TX v e' (Either e a)
        abort   : e -> TX v e a
        fork    : DebugName -> TX v e a -> TX v e' ()

I assume the monadic API is manipulated through a suitable projection so isn't too different from conventional imperative programming. We can manipulate variables via imperative `read` and `write` operations. The `modify` operation is basically a composed read-write, but could be optimized to reduce concurrency conflicts. The `try` and `abort` operations support hierarchical transactions with strong exception safety, which simplifies partial failure and graceful degradation. The `new` operation creates fresh variables. Variables have an explicit unassigned state via the optional type, so we can manage memory manually (much like Awelon's copy/drop). We can also leverage the unassigned state where it's useful.

The `fork` operation specifies a one-off operation to attempt after we commit. This is intended for use in read-fork patterns, where we can cache the set of forks and repeat those rather than recomputing the parent in each step. Hence, we form a stable read-fork tree with read-write loops at the leaves. One bot can represent many bots. This supports task-concurrency, multi-stage programming, and convenient packaging of behavior.

It is feasible to extend this API with invariant assertions, publish-subscribe, cached views, and other features. However, it's probably best to get the current API working and usable before we extend it.

### The Extended Environment

Bots operate upon a collection of environment variables, `forall v . Env v`. 

Effects, such as network access, are achieved through manipulation of these variables. For example, the environment may include a system task queue. After a transaction writes to this queue and commits, the system could process the requests. Acknowledgements and responses can be written back into the environment (the request might specify where) for subsequent access by the system. Besides system task queues and network access, an environment could support reflective access to the dictionary, filesystem, registries for publishing services, and more. (*Aside:* We might represent the filesystem as a constrained volume of a dictionary.)

This extended environment should be *ephemeral*. That is, variable data is lost if we logically or physically reset the system. This is convenient for performance insofar as it allows variables to contain lazy or parallel computations that would be difficult to serialize. It also simplifies administrative control, providing a convenient recovery from bad states that that weren't caught by transactions. However, some parts of the environment - dictionary, filesystem, etc. - may be backed by durable storage. For those cases, our environment should support explicit sync requests.

The `forall v` constraint means bot definitions must not contain or directly observe variables. This enables us to easily test bots by evaluating them in a simulated environment, or confine a distrusted behavior to a restricted environment. For contexts like stowage, memoization, and distributed computing, I propose to serialize our opaque variable references using words like `[ref-1123-debugname-hmac]`. The ephemeral [HMAC](https://en.wikipedia.org/wiki/HMAC) suffix (unique per reset) can ensure cryptographic security guarantees even in context of reflection and code distribution. We can easily reserve `ref-*` words for this purpose, warning if they're discovered in the Awelon dictionary.

### Installing Bots

Bots will be "installed" by defining `app-*` words in the dictionary. This ensures that the set of installed bots is easily discovered and managed. We can modify bot definitions at runtime, which also gives us live programming, continuous deployment, and system administration in terms of manipulating the dictionary.

For reasons of simplicity, idempotence, and extensibility, these `app-*` bots all receive the same `Env v` environment value. We can simulate bot private state by arranging for bots to use different volumes of a filesystem or registry. For distrusted bots behaviors, we should confine them explicitly as part of the installer's definition, perhaps writing `:app-pkg [pkg-bot] [local-pkg-sandbox-cfg] sandbox`.

*Aside:* Although live programming is an important aspect of Awelon's vision, special cases exist where static separate compilation is more appropriate. For those cases, we can develop bots that don't reflect on the dictionary. Such bots can be separately compiled.

### User Interfaces

Awelon systems model user interfaces in terms of projectional editing. The most obvious is projections over the dictionary, which give us spreadsheet-like behaviors and live coding for background bots. But more generally, we can model projections over the extended environment, enabling users to view and manipulate variables. This can support behaviors similar to conventional GUIs, where pressing a button might add an event to a queue.

Projecting over transactional memory offers many benefits. When we push a button, rather than perform form-validation up front, it's easier to simply 'abort' with an error message. We could even evaluate the button in advance, and disable it if it would abort, continuously rendering the current error message. We can support edits across multiple projections, reactively update views based on shared variables, then submit edits together in one transaction. We can extend user interfaces by adding more projections, no need to combine related views or actions into one window. It's feasible to detect conflicts when multiple users operate on shared variables, and to handle conflicts before committing. 

In a multi-user environment, most users will have restricted access. It's convenient if user authority is represented as a set of projections, such that users have exactly the authority they're presented with (after they've been authenticated). To support this, bots can publish projections into the environment for users to access based on their authorities/roles/etc.. This allows administrators to indirectly manage users and extend user-interfaces via bots, and naturally supports 'push' notifications by publishing relevant projections.

