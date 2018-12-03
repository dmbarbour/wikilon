
# Awelon Language

## Vision and Design Sketch

TLDR: spreadsheets at scale with bots at border

I want control over my computation environment. Given the opportunity, I would curate my sources, copy databases for personal use and archival, own my data processing algorithms, freely replay and trace computations to their sources to support comprehension, extend and customize my environment with new data or data processing, and selectively share my extensions and customizations with my community.

Other people also want to control my computation environment: to push advertising and propaganda, to manage and sell my private data, to hide the algorithms behind obfuscation and EULAs, to restrict unpurchased extension, to cultivate a dependency upon a corporate provider. There are several economic and political incentives that pull holistic software architecture design in a direction that marginalizes user control. But that isn't the direction I want to go, at least not as the default experience.

Awelon is designed for user or community control of environments. Elements:

* The Awelon environment - the *Dictionary* - is represented as a [persistent tree structure](https://en.wikipedia.org/wiki/Persistent_data_structure) over [content addressable storage](https://en.wikipedia.org/wiki/Content-addressable_storage). This supports huge volumes, lazy downloads, incremental synchronization, lightweight backups, atomic updates, and (via proxy or [CDN](https://en.wikipedia.org/wiki/Content_delivery_network)) amortizing physical storage and network costs for shared volumes.

* The Awelon language is [purely functional](https://en.wikipedia.org/wiki/Purely_functional_programming) with local [rewriting](https://en.wikipedia.org/wiki/Rewriting) evaluation. This language is used for structured data and data processing, including binding of data into computed views. We can cache and [compute incrementally](https://en.wikipedia.org/wiki/Incremental_computing) over live data. We can safely replay computations. Intermediate computation states can be written in the same language as initial source and final results, which simplifies tooling and debugging. We can automate testing and global consistency analysis.

* The Awelon user interface is based on [projectional editing](http://martinfowler.com/bliki/ProjectionalEditing.html). We can leverage flexible graphical projections - tables, graphs, sheet music, flow charts, etc. - for editing code and data. Critically, due to rewriting evaluation, we can use the same projections to render computed values or intermediate debug steps. In context of live data, we can display live computed values like a dashboard or spreadsheets.

* An active Awelon environment defines [bots](https://en.wikipedia.org/wiki/Software_agent) to model stateful or effectful behavior. Awelon's bots are modeled as transactions that repeat indefinitely, awaiting a relevant state change if unproductive. Users can control bots through the projectional editor, via manipulation of bot definitions or state, allowing for [live coding](https://en.wikipedia.org/wiki/Live_coding).

The idea of projectional editing can be stretched a little: macro-editing by invoking transactional scripts via command shell or form submission, uploading a file or camera stream as a user edit action, collaborative editing by sharing user intention or activity through auxiliary variables in the environment. But the essential structure should remain: Effectful user actions are represented as edits. Through edits, users control bots. Bots control everything else.

The [application models](ApplicationModel.md) document describes useful patterns for modeling applications in the Awelon system via projections and bots. This document focuses on Awelon language and dictionary.

## Language Basics

Awelon has four basic concatenative combinators with confluent rewrite rules:

        [B][A]a == A[B]         (apply)
        [B][A]b == [[B]A]       (bind)
           [A]c == [A][A]       (copy)
           [A]d ==              (drop)

Awelon additionally has limited support for data - natural numbers, embedded texts, binary large objects, which are treated as a syntactic sugar. Beyond these few primitives, programmers may define words in a dictionary. Evaluation proceeds by rewriting according to primitive combinators and rewriting words to their definitions when doing so contributes to further progress. Hence, the result of evaluation of an Awelon program is another Awelon program, equivalent to the input - but potentially simplified.

An Awelon runtime will also recognize a set of *Annotations*, represented by parenthetical words. For example, `[A](par)` might request parallel evaluation for the expression `A`, while `[F](trace)` might indicate that `[F]` should be written to a debugging log or console. Annotations formally have identity semantics: that is, ignoring or erasing annotations must not affect the internally observable result of a computation. However, annotations may affect external observations - parallelism, memoization, evaluation order, precise JIT control, static analysis, assertions, quotas, debugging, rendering in projectional editors, and so on.

An important class of performance annotations is *Accelerators*. Accelerators enable Awelon to leverage efficient representations under-the-hood, such as representing certain lists as arrays. Similarly, we can substitute accelerated functions with built-in functions - for example, to access or split an array at a given offset. Formally, Awelon only has first-class functions (in `[]` brackets), and developers must use [Church encodings](https://en.wikipedia.org/wiki/Church_encoding) or variants thereof to represent data. However, by leveraging accelerators, Awelon may use performance primitives comparable to more conventional languages.

## Words

Words are the user-definable unit in Awelon. Syntactically, a word is a sequence of lower-case alphanumerics and hyphens, starting with an alpha. Regex: `[a-z][a-z0-9-]*`. The formal meaning of a word is a trivial equivalence to its definition. Evaluation will lazily substitute a word by its definition.

Valid definitions must be acyclic, allowing for a simple inline expansion of all definitions (see *Loops*). Further, definitions must be block balanced - no unmatched `[` or `]` block delimiters. 

Environments may enforce ad-hoc constraints, reserving words or restricting definitions. For example, a linter might complain if a `*-meta-doc` word does not evaluate to an expected documentation type. To support local definitions, a linter could warn when words of form `foo-local-*` are referenced from outside `foo-*`.

## Natural Numbers

Awelon has limited support for natural numbers. Syntactically, natural numbers are represented by regex `0 | [1-9][0-9]*`. Semantically, natural numbers are Awelon words with an automatic definition.

        0 = [zero]
        1 = [0 succ]
        2 = [1 succ]
        42 = [41 succ]

Definition of `succ` and `zero` - and hence our model for natural numbers - is in theory left to our developers. For example, we could select between a recursive sum encoding (`type Nat = μN.(1+N)` where `type (A+B) = ∀r.(A→r)→(B→r)→r`) or a Church encoding (`type Nat = ∀x.(x→x)→x→x`). In practice, however, runtime support for *Accelerators* determines which model will be favored. And the performance for natural numbers should be primitive. For more sophisticated number types, it's feasible to build a tower of numbers via *Editable Views*.

*Note:* I've frequently considered a hard-coded interpretation, e.g. the Church encoding `N = [(c a)^N d]`, to guarantee stable meaning across all Awelon dictionaries. But this hardly seems worthwhile in context of the many other data types we will build above natural numbers.

## Embedded Texts

Awelon has limited support for embedding texts inline between double quotes such as `"Hello, world!"`. Semantically, embedded texts are special words defined automatically by template:

        "" = [null]
        "hello" = [104 "ello" cons]

That is, texts are simply an ASCII-encoded list of bytes. Like natural numbers, `null` and `cons` must be defined in the dictionary, and *Accelerators* will determine the de-facto standard definitions.

Embedded texts are limited to ASCII minus control characters and double-quote. There are no escape characters! However, it's not difficult to wrap texts with processing to handle user-defined escapes, e.g. `["hello\nmulti-line\nworld" literal]` might evaluate to a binary with line-feeds in place of each `\n` sequence, and a standard processing functions could easily be accelerated and supported in *Editable Views*. 

*Note:* For large texts and binaries, developers are encouraged to leverage *Secure Hash Resources* at the dictionary layer, and rope-like data structures where appropriate. Embedded texts are intended for labels, test data, inline comments, one-liner micro-DSLs, etc..

## Annotations

Annotations are special parenthetical words, such as `(par)` or `(error)`.

Annotations always have the same formal semantics: identity. That is, adding annotations to a program must not affect its formal behavior. However, within this limitation, annotations are assigned ad-hoc *informal* semantics by the runtime or compiler. For example, `[A](par)` can request parallel evaluation of `[A]`, `(trace)` could print to a debug console, and observing `(error)` can cause evaluation to fail fast. In general, annotations augment performance, safety, debugging, and display of programs. They encode any *programmer intentions* other than functional behavior.

Some potential annotations:

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
* `(quota)` - impose limits on argument evaluation effort
* `(static)` - assert value is constant within a definition

Awelon does not constrain annotations beyond requirement for identity semantics.

## Accelerators

Accelerators are built-in functions with reference implementations, accessed by annotation. Use of accelerators enables Awelon compilers or interpreters to extend the set of "performance primitives" available to Awelon programmers.

For example, most Awelon systems will accelerate natural numbers. We might use `(nat)` to indicate that a value should use an optimized representation for natural numbers under the hood. A function to add two natural numbers might be annotated as `[reference impl] (accel-nat-add)`, enabling our interpreter to symbolically substitute the built-in function. Validation of the given reference implementation doesn't need to be performed by the interpreter, but should be performed by a linter or other static analysis tool.

Because acceleration is explicitly accessed by annotation, our runtime can easily inform programmers if an accelerator is not recognized or has been scheduled for future deprecation. Thus, performance with accelerators can be predictable, stable, and will not silently degrade.

The cost of accelerators is that they complicate our runtimes and interfere with portability. Thus, acceleration should be used sparingly, targeting a few widely useful types and functions. We can also accelerate evaluation of a few carefully selected DSLs. For example, accelerate linear algebra or a safe subset of OpenCL to effectively leverage SIMD instructions or GPGPU hardware. But performance of most code should be left to a compiler.

## Dictionary

Awelon words are defined in a codebase called a "dictionary". A dictionary is essentially a key-value database, associating words to definitions. To support Awelon project's various goals, Awelon specifies a standard dictionary representation with convenient properties for import/export, versioning, sharing, scaling, etc.. Legibility is also a goal, to simplify debugging or inference of implementation. Awelon dictionaries can feasibly scale to many gigabytes or terabytes, support distributed representation, and feasibly integrate with block-chains.

The proposed representation:

        /prefix1 secureHash1
        /prefix2 secureHash2
        :symbol1 definition1
        :symbol2 definition2
        ~symbol3

A dictionary 'node' is represented by dense, line-oriented ASCII text, representing an update log. Each line will define or delete a symbol (`:` or `~` respectively), or index another node (via `/`). Within indexed nodes, we strip the prefix. Hence, `:poke` under `/p` becomes `:oke`. For lookup, only the last update for a symbol or prefix is used. Hence, `/p` will mask all prior entries with prefix `p`, including `/prod` or `~prince`. We can normalize a dictionary node by erasing masked entries then sorting whatever remains. Normalization is valuable to maximize structure sharing between similar dictionaries.

Using secure hashes allows dictionaries to be deeply immutable, persistent data structures. This greatly simplifies versioning and structure sharing. The update log can be allowed to accumulate in a root node then be propagated in batches towards the index nodes, which allows for efficient update and implicit working sets (a log-structured merge-tree). The empty prefix (`/ secureHash`) is valid, and can be used to represent a dictionary prototype or checkpoint.

*Note:* The dictionary does not permit comments. That sort of metadata must be embedded within the dictionary, using either associated symbols (such as `foo-meta-todo`) or embedding within definitions (like `"comment"(a2)d`). This permits metadata to be preserved and indexed like everything else.

### Secure Hash Resources

Awelon dictionaries use secure hashes as identifiers for binary large objects. This has several nice properties: immutable, acyclic, cacheable, securable, provider-independent, self-authorizing, self-authenticating, implicitly structure sharing, automatically named, uniformly sized. Besides use in `/prefix secureHash` dictionary tree nodes, Awelon dictionaries may embed arbitrary binary resources via `%secureHash` or oversized Awelon definitions via `$secureHash`.
        
        :my-binary-large-object %secureHashOfBinary
        :my-oversized-function $secureHashOfDefinition

Binary resources allow us to embed images, meshes, textures, sounds, and other ad-hoc binary data in our codebase without resorting to awkward base64 encodings or other techniques. References to external definitions, meanwhile, are mostly relevant for optimizing the dictionary representation, ensuring a predictable worst-case size for dictionary index nodes. *Note:* Secure hash resource references are extensions at the dictionary layer, and are not part of Awelon code.

Support for binary resources in this manner ameliorates much awkwardness when embedding bulk data within Awelon code. A binary resource is treated similar to an embedded text, a list of bytes, albeit with fewer limitations. We can easily embed images, for example. Large definitions, meanwhile, may work together with *Stowage* to support large structured data.

Specifically, the current secure hash proposal is the 320-bit [BLAKE2b](https://blake2.net/) algorithm encoding the hash using 64 characters in a variant [base32](https://en.wikipedia.org/wiki/Base32) alphabet.

        Base32 Alphabet: bcdfghjklmnpqrstBCDFGHJKLMNPQRST
            encoding 0..31 respectively

        Example hashes, chained from "test":

        rmqJNQQmpNmKlkRtsbjnjdmbLQdpKqNlndkNKKpnGDLkmtQLPNgBBQTRrJgjdhdl
        cctqFDRNPkprCkMhKbsTDnfqCFTfSHlTfhBMLHmhGkmgJkrBblNTtQhgkQGQbffF
        bKHFQfbHrdkGsLmGhGNqDBdfbPhnjJQjNmjmgHmMntStsNgtmdqmngNnNFllcrNb

The BLAKE2b algorithm could be replaced by another, simply rewriting the entire dictionary, if ever it proves inadequate. The proposed base32 alphabet is chosen to avoid accidental offense with pronounceable words.

*Security Notes:* A secure hash should be treated as a bearer token authorizing access to the associated resource. However, it's important that the system must not *leak* this authority. In particular, we should guard against timing attacks to discover stored secure hashes. Further, there is an attack of the form "does data with this secure hash exist?" where the  attacker might request millions of hashes to discover, for example, an unknown phone number within an otherwise predictable template. This can be resisted by including an entropy field within sensitive data. Finally, to work with untrusted content distribution services, we could encrypt data using the original hash, and lookup using a hash of hash.

### Software Packaging and Distribution

Awelon is designed for distribution of entire dictionaries. An advantage of whole-dictionary distribution is that everything can be curated, tested, known to work nicely together. There is no "version hell" of package maintenance. Awelon dictionaries can be very large, but *lazy download* of secure hash resources can enable working with dictionaries that contain more data than we use.

Packages remain useful, e.g. for access control, commercial reasons, or real-time update of data packages. For these cases, we can conveniently align package names with a word prefix. This enables a one-line install or update `/packagename- secureHash`, and works nicely with static enforcement of local definitions and value sealers. Like conventional software package systems, we would likely develop a community package registry.

However, Awelon doesn't optimize for packages. We must use the full `packagename-` prefix for both internal and external references. Forking or renaming a package will require rewriting those internal package references - fortunately, that's a simple linear-time operation. Verbosity and noise in the user-interface from repeated package prefixes can be ameliorated via *Editable Views*.

## Stowage

By annotation, large values could be moved from memory to environment:

        [large value](stow)    => [stow-id]
        [small value](stow)    => [small value]

Here `stow-id` must be a word representing `large value`. The stowage word should also be much smaller than the value. Further, it should be *stable* in the sense that the the same word tends to correspond to the same value or location even after minor changes to a computation. Stability is valuable for reusable memoization and also simplifies stable layout when rendering live computed views.

Stowage can serve as a controlled variation of [virtual memory](https://en.wikipedia.org/wiki/Virtual_memory) enabling larger than memory computations. Stowage also interacts nicely with *Memoization*, reducing large value comparisons to simple word version comparisons. Most importantly for Awelon's goals, stowage supports *progressive disclosure* when rendering large computed values in a user-interface. Without stowage, we have an inconvenient distinction between source data versus computed values.

The main weakness of stowage: it is not locally obvious how long to remember stowed values after performing the computation that produces them. Viable solutions: report stowage together with an evaluated result, model stowage as an element of a stateful compute session so we can use the session life-cycle, or use a naming scheme that tracks origin such as `foo-stow-id` so we know to recompute `foo` if the value has been forgotten.

## Evaluation

Evaluation will rewrite an Awelon program to an equivalent Awelon program. In context of annotations like `(stow)` or `(memo)` or `(trace)`, we might supply a few auxiliary outputs. Awelon is a pure language, but interactions with external agents provides a basis for effects.

Primitives rewrite by simple pattern matching:

            [B][A]a => A[B]         (apply)
            [B][A]b => [[B]A]       (bind)
               [A]c => [A][A]       (copy)
               [A]d =>              (drop)

Words rewrite to their evaluated (and potentially optimized) definitions. However, this rewrite should be *lazy* in the sense that a rewrite is avoided when it does not contribute to further progress. The motive is to retain human-meaningful symbols and structure within the evaluated result, which may be flexibly rendered using *Editable Views*. 

Evaluation strategy is unspecified. The default may be ad-hoc and heuristic. Annotations may guide the strategy. Valid, terminating computations will always converge on the same final result regardless of strategy. If we halt early, due to quota or error, evaluation strategy and other optimizations might be exposed.

### Arity Annotations

Arity annotations are useful for Awelon, and have simple rewrite rules:

        [B][A](a2) == [B][A]
        [C][B][A](a3) == [C][B][A]
        ...

These annotations can be used to defer linking of words where a partial evaluation isn't useful. For example, consider a swap function `w = (a2) [] b a`. Ignoring the arity annotation, we'd rewrite `[A]w => [[A]]a`, which isn't useful progress. With the arity annotation, `[A]w` does not evaluate further, but `[B][A]w` evaluates directly to `[A][B]`. Arity annotations are also useful for modeling codata. For example, `[[A](a2)F]` has the observable behavior as `[[A]F]`, but the former defers computation until the result is required.

## Loops

Awelon disfavors recursive definitions. Instead, use fixpoint combinators:

        [X][F]z == [X][[F]z]F
        z = [[(a3) c i] b (eq-z) [c] a b w i](a3) c i

        assuming:
            [def of foo](eq-foo) == [foo]
            [B][A]w == [A][B]       w = (a2) [] b a
               [A]i == A            i = [] w a d

Other loop combinators can be built upon `z`. For example, we can develop a `foreach` function that processes a list or stream. The most common loop combinators may be accelerated for performance.

*Aside:* I've contemplated allowing recursion many times. Unfortunately, recursion isn't very helpful in-the-small where our loop combinators can do the job. In-the-large, recursion could allow us to model interactive applications in a second-class manner, but would also entangle all things and hinder sharing.

## Memoization

Annotations can easily indicate [memoization](https://en.wikipedia.org/wiki/Memoization).

For example, `[Function](memo2)` might express that we should memoize the function together with its next two arguments. The runtime would find or create a memoization table specific to the function, likely specialized for the expected argument type. Of course, memoization does impose some overheads, so it must be applied carefully or performance will suffer. Effective incremental computing requires use of memoization together with cache-friendly patterns: compositional views over persistent data structures, stable *Stowage* identifiers, etc..

*Aside:* Awelon systems are purely functional, but memoization over time-series data can model many stateful applications.

## Error Reporting

We can represent runtime errors or assertion failures by simply introducing an `(error)` annotation that acts as an explicitly undefined word, unable to be further rewritten. Then, we can define words such as `divide-by-zero = (error)` to create explicit, named errors that should never rewrite. In these cases, errors halt evaluation. We can also encode lazy error values, which only become an error when observed, e.g. `[divide-by-zero]`. 

For expected or continuable errors, such as a parse or constraint error that might lead to falling back and searching an alternative path, error values should be modeled explicitly in a function's return data type. Awelon doesn't have a built-in exception system, but it's feasible to model an exception monad.

## Static Typing

Awelon doesn't depend on static types insofar as there is no type-driven dispatch or overloading. However, the language does imply a simple static type model. And if programmers can discover errors earlier by static analysis, that's a good thing. Awelon's stack-like environment can easily be typed as a tuple, and values as functions. Record constructors are typed using row polymorphism. Types for our primitive operations:

        a           ((s * x) * (s → s')) → (s' * x)
        b           ((s * x) * ((e * x) → e')) → (s * (e → e'))
        c           (s * x) → ((s * x) * x)
        d           (s * x) → s
        [F]         s → (s * type(F))

Type annotations can be expressed using Awelon annotations, we only need some conventions. Obviously, we can use specific annotations such as `(nat)` or `(bool)` for the most common types. Lightweight annotations could encode simple arities or stack notations, e.g. `[F](t21)` might simply assert `F` receives two arguments and outputs one. For ad-hoc sophisticated or precise types, we might require a type argument `[Type Descriptor](type)d`. We can also, feasibly, assign names to types or variables via annotations to simplify debugging.

Unfortunately, simple static type systems are sometimes too simplistic and restrictive. For any consistent type system, we'll always have safe programs that cannot be typed. For example, the `pick` function from Forth isn't amenable to typing without sophisticated dependent types:

        [Vk]..[V1][V0] k pick == [Vk]..[V1][V0][Vk]

In this context, we could develop a series of functions like `pick2nd` and `pick3rd`, at cost of much boiler-plate. Or we could try to defer typing until after we've specialized on the first parameter, treating `pick` as a dynamically typed macro. Intention to defer type checking might be indicated by annotation, e.g. adding a `(dyn)` comment to the subprogram with `[A](dyn) => [A]` behavior.

*Note:* Besides static types, termination analysis is also useful. As a purely functional language, non-termination or divergence is always an error for Awelon programs.

### Opaque and Abstract Data Types

Implementation-hiding modularity in functional programming languages is frequently based around [opaque data types](https://en.wikipedia.org/wiki/Opaque_data_type) as a simplified approximation of [abstract data types](https://en.wikipedia.org/wiki/Abstract_data_type). Direct access to data representation is confined to a controlled volume of code. External code is limited to a subset of provided interfaces. Those interfaces enforce invariants, control coupling, partition programming tasks, and isolate bugs.

For Awelon, we can support opaque data types via seal-unseal annotations:

        (seal-foo)      (s * x) → (s * foo:x)
        (unseal-foo)    (s * foo:x) → (s * x)

A sealer serves as a symbolic type wrapper, to resist accidental access to data representation. But we can (via static analysis or linter) also enforce a simple rule: that these annotations are only directly accessible from words with a matching `foo-` prefix. Given this additional constraint, our sealers can isolate direct data access to a volume of code, support smart constructors, etc.. This should work nicely with similar constraints limiting external access to `foo-local-*`, and aligns nicely with dictionary packages.

## Structural Equivalence

Annotations can assert two functions are the same, structurally:

        [A][B](eq) => [A][B]     iff A and B are structurally equivalent

Structural equivalence assertions are certainly convenient for lightweight unit testing. But the motivating use case is merging sorted data structures. Efficient merge requires knowing whether the two structures are sorted using the same comparison function. If we couple the sort function with the collection, we can use `(eq)` to verify our assumption.

*Aside:* Behavioral equivalence is not something we can generally test in a Turing complete language. But structural equivalence could include limited forms of behavioral equivalence comparisons.

## Editable Views

Awelon's simple syntax must be augmented by [projectional editing](http://martinfowler.com/bliki/ProjectionalEditing.html) techniques to support richer programming interfaces, DSLs, namespaces, application models, and larger programs. As a simple example, we could support a numeric tower:

        #42         == (Awelon's 42)
        42          == [#42 int]
        -7          == [7 int-neg]
        3.141       == [3141 -3 decimal]
        -0.0070     == [-70 -4 decimal]
        2.998e8     == [2998 5 decimal]
        -4/6        == [-4 #6 rational]

This builds one view upon another, which is convenient for extending views. If our view left out rational numbers, we'd still render a sensible `[-4 #6 rational]`. Relative to built-in number support, there is some storage overhead - but it's relatively minor at larger scales (and compresses well). Besides numeric towers, editable views could feasibly support lists and matrices, continuation-passing style, Haskell-inspired do-notation, generators with yield, and other features. Problem specific languages can frequently be modeled as data-structures that we evaluate statically. Embedded comments can also be represented, e.g. `("comment")` via `"comment"(a2)d`.

A projectional editor can display multiple definitions at once, allowing users to work on related code. If we wish, we could explicitly represent edit sessions within the dictionary, such that `[foo][bar][baz]` is rendered and edited as a file that displays all three definitions. We could also use prefix-oriented sessions, e.g. such that all words under `foo-*` are displayed as a spreadsheet or worksheet. (See [application models](ApplicationModel.md).)

### Namespaces

It is not difficult to recognize a comment as declaring a qualified namespace.

        using long-prefix as lp; lp-foo
            becomes
        "using long-prefix as lp"(a2)d long-prefix-foo

However, namespace declarations too easily become a form of boiler-plate. It's inefficient to specify them at the scope of an individual function's definition, and even for a small edit session. I believe it wiser to shift namespaces to the editor layer. A user could personalize namespaces as part of the user model, or switch between packages of namespaces based on choice or context. This also transitions naturally to DSL support packages - essentially, a namespace is a simplistic DSL view.

If our editor is capable of color, we could use color to distinguish namespaces. For example, `html-div` and `math-div` might simply be written as `div` in distinct colors, with a namespace legend off to the side.

### Named Local Variables

We can leverage editable views to model named local variables, like lambdas or let expressions. For example, consider adapting Kitten programming language's syntax for local vars:

        7 -> X; EXPR            let in
        [-> X; EXPR]            lambda

We can then extract `X` from our expression by simple algorithm:

        EXPR == X T(X,EXPR) for value X

        T(X,E) | E does not contain X       => d E
        T(X,X)                              =>
        T(X,[E])                            => [T(X,E)] b
        T(X,F G)                            
            | only F contains X             => T(X,F) G
            | only G contains X             => [F] a T(X,G)
            | F and G contain X             => c [T(X,F)] a T(X,G)

For performance, we could optimize static conditionals to avoid copying:

        T(X,[F][T]if) => [T(X,F)][T(X,T)]if

However, I would prefer to have a separate optimizer erase static closures.

Variable names could be recorded using a comment or annotation. Example:

        -> x; EXPR
            becomes
        "\X"(a2)d T(X,EXPR)
            or perhaps
        (var-x) T(X,EXPR)

*Aside:* For me, named local variables were the first convincing proof-of-concept that *Editable Views* can serve adequately (although awkwardly) as an alternative to a rich built-in syntax. However, I hope to avoid sophisticated rewrites in most view. Editable views are a nicer fit for hierarchical data constructors, upon which we might build DSLs.

## Labeled Data and Records

Labeled data types, such as records and variants, are a staple of most programming languages. I've frequently contemplated built-in support for labeled data in Awelon. However, editable views and accelerators should be adequate to the task without requiring new primitives.

Naively, a record could be encoded as an association list, and a variant as a function that selects and applies a handler from an association list. However, the specific concrete choice of "association list" can be awkward for many use cases, as would other specific choices like "trie". 

I propose instead that Awelon favors record *constructors* - i.e. we use `[[A] "a" put [B] "b" put ...](record)`. The block represents a function writing to an opaque record representation (perhaps a trie). The `(record)` annotation then indicates that this function should have the type of a simple record constructor, and informs our runtime to use an accelerated record representation. This preserves commutative behavior and error semantics of `put` operations, and simplifies functional abstraction of records. Record constructors are also easy to recognize and manipulate through a projectional editor.

## Arrays and In-Place Mutation

Awelon doesn't have an array data type. But between annotations and accelerators, we can impose an array representation for some lists, such that we can access data in near-constant time. 

In context of a purely functional language, *modifying* an array is naively O(N) - we must copy the array with the modification applied to the copy. However, there is a way to bypass this: if we only have one reference to our array, then logically we can simultaneously construct the new array and garbage-collect the old array. It is feasible to optimize this into an in-place update.

The challenge is knowing or ensuring that there is only one reference. For this, we could try dynamic tracking, or we could favor explicit annotations and static analysis. I favor explicit because it results in a more robust, predictable performance, warns developers if assumptions are violated, and avoids runtime overheads involved with tracking which values are shared. Further, in-place mutation of arrays is most valuable in context of tight loop algorithms - union-find, sorting, etc..

In addition to arrays, we might also develop in-place mutation for records. 

*Note:* Persistent data structures are more flexible than arrays. Besides preserving prior values, they play nicely with sparse data, stowage, memoization, and progressive disclosure. I would strongly recommend use of a persistent data structure unless in-place mutation is essential for performance.

## Generic Programming

A weakness of Awelon is lack of implicit support for generic programming. For example, we cannot implicitly overload an `add` word based on argument type. We'll instead use a separate `nat-add` vs `int-add` vs `matrix-add`. Implicit generic programming can entangle too easily with static type inference, and require commitment to a specific model of static types - which I would prefer to avoid for Awelon. 

But it is feasible to explicitly model generic programs as polymorphic programs, providing the appropriate `[int-add]` as an argument or part of a record. And we could do this ahead of time, if we model a multi-stage program or assume deep constant propagation in our optimizer. If we have records of methods representing our types, we could use `(eq)` to represent equivalence assumptions. 

It should be possible to design monads or editable views that track context and make generic programming relatively convenient. However, I haven't worked out the details. Meanwhile, I would focus on building useful things from a few, simple data types.

## Bots and Effects

An Awelon bot process is modeled as a deterministic transaction repeated indefinitely, but implicitly waiting when repetition is obviously unproductive. These implicit waits support process coordination, replacing locks, signals, polling. For example, repeating an aborted transaction is obviously unproductive. So a stream-processing bot might abort when the input channel is empty or the output channel is full, to implicitly wait for these conditions to change.

A preliminary API:

        type TX v e a -- opaque, monadic
        alloc   : a -> TX v e (v a)
        read    : v a -> TX v e a
        write   : v a -> a -> TX v e ()
        try     : TX v e a -> TX v e' (Either e a)
        abort   : e -> TX v e a
        fork    : TX v e a -> TX v e' ()

        type Bot = forall v e a . Env v -> TX v e a

Here, `try` and `abort` support hierarchical transactions, exceptions, graceful degradation. Meanwhile, `fork` supports task-parallel divide-and-conquer tactics: after we commit, forked transactions are applied repeatedly, but only until the parent must be recomputed.

Bots operate on an `Env` type, which models an abstract host environment. The environment provides system variables, likely hidden behind `args -> TX err ret` methods. Effects will be modeled as asynchronous interaction with these system variables. For example, network access might be requested through a system task queue. After we commit, our host will observe the request, establish the network channels, and respond through the requested variable or object. Beyond network access effects, an environment can provide filesystem-like state or reflective access to the Awelon dictionary. For security, we can control effects by wrapping a distrusted bot's definition to restrict direct access to the environment.

In Awelon systems, bots will be 'installed' by defining them at an easily controlled location, perhaps `bot-*`. Users can securely control a distrusted bot definition by wrapping them, restricting direct access to the host environment. The bot definition is implicitly read at the start of each transaction, and any change is applied immediately, which supports live coding. To support durability and debugging, we might record variables within the dictionary, with variables represented as a symbolic reference `[ref-1123]` to a corresponding location `mem-1123`. I would reserve `ref-*` and `mem-*` words for this purpose, forbidding their use in normal Awelon code.

Bots provide return arbitrary status values for debugging and task management. We can easily display the current status for every bot based on `e` or `a` for those that halt successfully, and also for badly defined bots (e.g. parse errors, type errors, quota limits).

