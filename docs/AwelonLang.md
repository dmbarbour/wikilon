
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

Annotations always have the same formal semantics: identity. That is, adding or removing annotations should not affect a program's formal behavior. However, within this limitation, annotations are assigned ad-hoc *informal* semantics by the runtime or compiler. For example, `[A](par)` can request parallel evaluation of `[A]`, `(trace)` could print to a debug console, and observing `(error)` can cause evaluation to fail fast. In general, annotations augment performance, safety, debugging, and display of programs. They encode any *programmer intentions* other than functional behavior.

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
* `(type)` - describe type of stack at given location
* `(seal-foo)` - symbolic types for safety and modularity
* `(quota)` - impose limits on argument evaluation effort

Awelon does not limit annotations much beyond the need for identity semantics. 

## Accelerators

Accelerators are built-in functions, accessed via annotation of a reference implementation. Use of accelerators enables Awelon compilers or interpreters to extend the set of "performance primitives" available to Awelon programmers. For example, Awelon systems should accelerate natural numbers. We might use `(nat)` to indicate that a value should use an optimized representation for natural numbers under the hood. The function to add two natural numbers could be annotated via `[reference impl] (accel-nat-add)`, which we subsequently inline. This tells our interpreter or compiler to replace the reference implementation by the specified built-in operator.

Because acceleration is always driven by explicit annotations, our runtime can easily inform developers when the accelerator is not recognized or is scheduled for future deprecation. Thus, there is no risk of silent performance degradation. Further, the dynamic binding costs can be very low: an interpreter might use `(accel-nat-add)` without looking at the reference implementation, instead leaving responsibility for validation to a separate static analysis tool. 

The main risk with accelerators is diminished portability between runtimes. Fortunately, because we can validate the reference implementations, there is very little risk of silent divergence. And in practice, the more popular runtimes will form a de-facto standard.

*Aside:* Early in development, it may be inconvenient to provide a reference implementation. Awelon systems can take a practical stance here, e.g. accepting `["todo! fp mul"(error)](accel-fp-mul)` as a transitional scaffolding. 

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

However, Awelon systems can represent package-based software distribution by aligning packages with word prefixes. For example, a one-line entry `/packagename- secureHash` can install or update a specific version for an entire package. This might be suitable in cases where packages involve special licenses or subscriptions. *Namespaces* can be supported via projectional editing to mitigate verbosity from hierarchical names. For dynamic systems, developers can arrange for a bot to synchronize packages from a trusted source (see *Bots and Effects*).

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

Evaluation will rewrite an Awelon program to an equivalent Awelon program. In context of annotations like `(stow)` or `(trace)`, we might produce auxiliary outputs, but not in a way that can be observed within the Awelon computation. Awelon is a pure language, but effects will be modeled explicitly in some limited contexts (cf. *Bots and Effects*). 

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

Awelon does not have a strong dependency on a garbage collector. The extent to which a runtime requires garbage-collection mostly depends on how we implement the copy operator (`c`). If we deep-copy our values, then drop (`d`) can also free the memory. If we shallow-copy a value reference, then drop can only erase the reference and recovering memory would depend on a garbage collector. Between these extremes, a runtime might use reference counting GC - conveniently, there is no risk of forming a reference cycle.

## Memoization

Annotations can easily indicate [memoization](https://en.wikipedia.org/wiki/Memoization).

For example, `[Function](memo2)` might express that we should memoize the function together with its next two arguments. The runtime would find or create a memoization table specific to the function, likely specialized for the expected argument type. Of course, memoization does impose some overheads, so it must be applied carefully or performance will suffer. Effective incremental computing requires use of memoization together with cache-friendly patterns: compositional views over persistent data structures, stable *Stowage* identifiers, etc..

*Aside:* Awelon systems are purely functional, but memoization over time-series data can model many stateful applications.

## Error Reporting

Expected errors should instead be modeled as explicit return values, usually via sum types. This allows the errors to be handled by the function's client, rather than halting computation. However, for errors without recovery, we might use `(error)` annotations, which act as an explicitly undefined words and do not rewrite further.

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

We can then impose a simple rule: that `(seal-foo)` and `(unseal-foo)` may be used directly only from words that match `foo-*`. This rule is trivially enforcable by linter or compiler. By enforcing it, we confine direct access to our sealed data to the `foo-*` volume of our codebase. This aligns conveniently with Awelon's ad-hoc packages. We can further enforce that `foo-local-*` words may only be directly used from `foo-*`, to support package-private types and functions.

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

Although tacit programming styles are suitable for many problems, they make an unnecessary chore of sophisticated data shuffling. Fortunately, we can support conventional let and lambda expressions as a projection. Consider a lightweight syntax where `\x` indicates we'll "pop" a value from the stack and assign it to a local variable `x`, scoped to the remainder of our current program (definition or block) modulo shadowing. Thus `[\x EXPR]` becomes equivalent to `(λx.EXPR)`, while `[X] \x BODY` effectively simulates `let x = [X] in BODY`. We can represent this via bidirectional rewriting:

        \x EXPR == (var-x) T(x,EXPR) 
          assuming `x` represents a value
        
        T(x,E) | E does not contain x       == d E
        T(x,x)                              == 
        T(x,[E])                            == [T(x,E)] b
        T(x,F G)                            
            | only F contains x             == T(x,F) G
            | only G contains x             == [F] a T(x,G)
            | F and G contain x             == c [T(x,F)] a T(x,G)

This design is robust and independent of user definitions. However, it isn't optimal for conditional behaviors where we select one branch to apply. In those cases, we might produce unnecessary closures. So we may need to extend our rewrite rules for a few known special cases, or rely upon a compiler and optimizer to eliminate unnecessary closures, or simply hand-optimize those cases. Regardless, use of named locals are convenient for use cases involving sophisticated data shuffling or ad-hoc closures.

*Aside:* Named locals were, at least to me, the first convincing proof that *Projectional Editing* is a viable alternative to sophisticated built-in syntax. They also demonstrate that projections might be usefully understood as lightweight compiler/decompilers that may inject a few annotations to support decompilation. That said, I hope and intend for most views be simpler and more localized.

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

This projection flattens nesting, enabling direct expression of program logic. The command sequence combinator `cseq` binds our continuation `[Y;Z]` to the result of computing `X`. To complete our monad, we must define a pure `return` such that `return [Y] cseq` locally rewrites to `Y`. To fit with Awelon's tacit stack programming style, we return the data stack rather than an explicit value. This projection conveniently layers with local variables such that `Z` has access to variables defined at `X` or `Y`. 

Of course, this projection conveniently supports only one monad. I propose a variant of the *operational monad*, which is adaptable and extensible through the operation type. See also [Operational Monad Tutorial](https://apfelmus.nfshost.com/articles/operational-monad.html) by Heinrich Apfelmus or [Free and Freer Monads](http://okmij.org/ftp/Computation/free-monad.html) by Oleg Kiselyov. With the operational monad, one monad is sufficient for all monadic programming use cases.

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
            
In this implementation, final `return` versus intermediate `yield` are distinguished via optional continuation. Deep procedural abstractions often result in `yield [X] cseq [Y] cseq [Z] cseq` patterns. The resulting continuation is represented by a reverse-ordered list `[[Z] [[Y] [[X] [null] cons] cons] cons]`. We can fold this list to `[X [Y [Z] cseq] cseq]`. (For contrast, if we start with `yield = [[return] some]`, we're likely to end with `[return [X] cseq [Y] cseq [Z] cseq]`, which is unfriendly for projections and performance.)

The biggest challenge for monadic programming in Awelon is static type safety. Without fancy dependent types or generalized algebraic data types, it's difficult to validate request-response patterns where the response type depends upon the request. Eventually, hopefully, Awelon systems will support sufficiently advanced types. But in the interim, we must either accept risk of runtime type errors or model operations such that we have simple homogeneous types.

## Labeled Data and Records

Labeled data types, such as records and variants, are a staple of most programming languages. I've frequently contemplated built-in support for labeled data in Awelon. However, editable views and accelerators should be adequate to the task without requiring new primitives.

Naively, a record could be encoded as an association list, and a variant as a function that selects and applies a handler from an association list. However, the specific concrete choice of "association list" can be awkward for many use cases, as would other specific choices like "trie". 

I propose instead that Awelon favors record *constructors* - i.e. we use `[[A] "a" put [B] "b" put ...](record)`. The block represents a function writing to an opaque record representation (perhaps a trie). The `(record)` annotation then indicates that this function should have the type of a simple record constructor, and informs our runtime to use an accelerated record representation. This preserves commutative behavior and error semantics of `put` operations, and simplifies functional abstraction of records. Record constructors are also easy to recognize and manipulate through a projectional editor.

## Arrays and In-Place Mutation

Awelon doesn't have an array data type. But between annotations and accelerators, we can impose an array representation for some lists, such that we can access data in near-constant time. With further support from our runtime and annotations, it is feasible to track uniqueness of our memory reference such that *modifying* the array can be implemented as an efficient in-place mutation. (This might be understood as a garbage-collector optimization: GC the old value and allocate the new with the modification in place.) Arrays are the obvious low-hanging fruit, but similar benefits might be achieved for records.

Of course, arrays won't replace use of persistent data structures. The latter are more flexible and have nicer properties at large scales (e.g. relating to memoization, stowage, or progressive disclosure). I would recommend arrays where they're relatively small (like leaves of a rope) or when we want a tight in-place update loop (like a union-find algorithm).

## Data Representation

Precise control over data representation can improve performance by reducing indirection and heap memory pressure. For example, we might represent a tuple `[[T1][T2][T3]]` as three values on our stack, or represent a known-to-be-small natural number as a 32-bit word.

For Awelon, it is feasible to control data representations through annotations. However, it is not trivial to do so. We'd also need to track these data representations and adapt behavior of functions based on representations provided. Fortunately, most of the costs can be shifted to the compiler, while programmers mostly use annotations to declaratively assert or convert representations.

Support for precise data representation is a long-term goal for Awelon systems. It is of relatively low priority. Short term, it may be sufficient to specialize arrays of numbers or binary structures. 

## Multi-Stage Programming

Multi-stage programming (MSP) is about explicit, robust control of *when* a compution occurs. This is useful for predictable performance. For Awelon, we can leverage annotations to express programmer intentions and assumptions. Consider paired annotations:

        [A](step-foo) => [A]
        [F](stage-foo) => [F]   iff [F] does not contain (step-foo)

The `(step-foo)` annotations indicate which values must be computed to complete stage `foo`. That is, a stage of computation is complete when no steps required for that stage remain. The `(stage-foo)` annotation effectively asserts that a stage has completed. Stages can complete implicitly even if not asserted. For precision work, developers can work with several named stages. These annotations can guide partial evaluation optimizers, and inform developers when their assumptions are violated. It is feasible to track stages and requirements in the type system. 

Annotations by themselves are insufficient to make MSP convenient to express. We'll additionally want to develop design patterns for convenient staging. For example, read-fork patterns for *Bots and Effects* make it easy to expresss incremental staged processes whose behavior may depend on a relatively stable configuration or plugins registry.

*Aside:* In addition to controlling computation, MSP may benefit from control over just-in-time compilation (JIT). This could be supported via `[F](jit)` annotations. Controlling JIT can improve performance stability.

## Generic Programming

Awelon supports polymorphism. We can implement lists once for many data types, or an operational monad for many operation types. This supports a weak form of generic programming. However, Awelon does not support overloading of symbols. For example, we cannot have one `add` symbol that automatically selects the appropriate function based on whether the arguments are natural numbers, floating point, or matrices.

This could feasibly be mitigated by *Multi-Stage Programming*: we could develop a stage that propagates type information and other static metadata. This would allow `add` to select the appropriate function based on context. Projectional editors could further help, making it more convenient to work with `add<T>` functions where `T` represents our static metadata.

## Bots and Effects

An Awelon bot process is a deterministic transaction, repeated indefinitely.

Process coordination is implicit. Deterministic repetition of a read-only or failed transaction is obviously unproductive. Thus, we can improve system efficiency by waiting for a change among the observed variables. For example, a stream processing bot might voluntarily abort if an input queue is empty or output queue is full. Compared to locks and signals, this provides a robust means to wait for arbitrary conditions, and makes it relatively easy to preserve system consistency.

Preliminary API:

        type TX v e a -- opaque, monadic
        type Env v = ... -- model of host system
        type Bot = forall v e a . Env v -> TX v e a
        type TaskName = String -- for debugging!

        new     : TX v e (v a)
        read    : v a -> TX v e (Maybe a)
        write   : v a -> Maybe a -> TX v e ()
        try     : TX v e a -> TX v e' (Either e a)
        abort   : e -> TX v e a
        fork    : TaskName -> TX v e a -> TX v e' ()

I assume the monadic API is manipulated through a suitable projection so isn't too different from conventional imperative programming. We can manipulate variables via imperative `read` and `write` operations. The `try` and `abort` operations support hierarchical transactions with strong exception safety, which simplifies partial failure and graceful degradation. The `alloc` operation creates fresh variables. To avoid strong requirements for garbage-collection, the above API includes an unassigned state for variables via an optional value type. 

The `fork` operation specifies a one-off operation to attempt after we commit. When repeating a read-fork transaction, we can cache the forked operations and repeat them until an observed variable has changed. Recursively, this can gives us an incremental read-fork tree with reactive read-write loops at the leaves. Use of fork supports task-concurrency and multi-stage programming.

Bots operate upon a set of environment variables `Env v`. Effects, such as network access, are achieved through manipulation of these variables, which are shared with the system and other bots. For example, we might have a system task queue. After we add tasks to this queue then commit, the system may process the requests. We might provide a filesystem-inspired state resource where bots can record state or collaborate. Reflective access to the dictionary might also be provided through this environment. Further, by publishing projections over this environment where users can discover them, we can model ad-hoc effectful, reactive application front-ends and real-time notifications.

To simplify user access, control, and discovery, I propose that bots are installed by simply defining `app-*` words. Reading the bot definition is implicitly part of each repeated transaction. Thus, any modification to `app-*` definitions can be applied immediately. This supports live-coding application models, continuous deployment, runtime software upgrades, and administrative control over system behavior. All bots operate on the same `Env v` environment, but we can easily confine and secure a bot by wrapping its behavior definition. Thus, we can safely install distrusted bots or applications by sandboxing them, or simulate bot behaviors via purely functional environments for automated testing.

In context of stowage, memoization, and debugging, we often serialize variables. To keep this simple, secure, precise, and compatible with Awelon tooling (parsers, type analysis, projectional editors, etc.), I propose to reserve the volume of `ref-*` words for this purpose. Using a reference word within the normal dictionary would be treated as an error, enforcing the `forall v` constraint. We then simply encode variables as `[ref-1123]`, or perhaps a symbolic name for our initial environment variables. 

Ultimately, we have a cycle. The Awelon dictionary defines bots in `app-*`. The bots define projections. A subset of projections can edit the dictionary through reflection. By editing the dictionary, we can add, remove, or modify our applications. The resulting system has many nice properties for control, customization, extension, liveness, sharing, security, etc.. Of course, to support bootstrapping and debugging, we'll inevitably define several projections and other tools outside this cycle.

