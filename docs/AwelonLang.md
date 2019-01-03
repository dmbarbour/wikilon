
# Awelon Language

## Vision and Design Overview

Awelon is a programming language and environment designed to empower individuals to create, customize, comprehend, control, and share computation artifacts.

Major design elements:

* A scalable concrete environment, represented as a [log-structured merge tree](https://en.wikipedia.org/wiki/Log-structured_merge-tree) over [content addressable storage](https://en.wikipedia.org/wiki/Content-addressable_storage) with [prefix-aligned indexing](https://en.wikipedia.org/wiki/Radix_tree). This filesystem-like structure can support massive volumes, lazy downloads, incremental synchronizations, lightweight backups, atomic updates, and prefix-aligned sharing. Further, we can amortize physical storage and network costs within a community via proxy or [CDN](https://en.wikipedia.org/wiki/Content_delivery_network).

* A [purely functional](https://en.wikipedia.org/wiki/Purely_functional_programming) language evaluated under [term rewriting](https://en.wikipedia.org/wiki/Rewriting) semantics to encode structured data and computations. The intention is to simplify sharing, caching, multi-stage programming, and user ability to inspect and comprehend computation through deterministic replay and rendering intermediate and final terms. Static analysis is possible but not required by the language.

* User interfaces leverage [projectional editing](http://martinfowler.com/bliki/ProjectionalEditing.html). Users can graphically manipulate code or data through tables, graphs, forms, flow charts, maps, and sheet music. The editor should be extensible with user-defined widgets. Computed terms can use the same view widgets as source, and live data should result in live displays. Because all user actions are modeled as edits, we can uniformly support undo and transactional macro-edits. 

* Automation is achieved by defining [bots](https://en.wikipedia.org/wiki/Software_agent). Bots can access the network and manipulate the Awelon environment. Concretely, bots are modeled as transactions that repeat indefinitely, implicitly waiting when unproductive, operating on system variables for auxiliary state and effects. This design is fail-safe, resilient, idempotent, securable, extensible, and safe to modify at runtime - which further supports live coding, runtime upgrade, and robust process control.

Applications are modeled as users and bots operating in the shared environment. Users view and manipulate the environment through suitable projections. We can extend our projectional editor with widgets specific to an application's data model, enabling ad-hoc front-end GUIs. User requests or tasks are written into the environment where a bot would see them. Bots would operate on structured data. Frequently, we'll want to model the shared context as a first-class value.

However, we may also model conventional web applications via bots interacting with the network. We might eventually bootstrap our projectional editors as web applications. And even without bot support, we can model many useful applications - REPLs, spreadsheets, forums, calculuators, image processing, simulators, document editors. 

## Language Basics

Awelon defines a few primitive combinators as keywords:

        [B][A]a == A[B]         (apply)
        [B][A]b == [[B]A]       (bind)
           [A]c == [A][A]       (copy)
           [A]d ==              (drop)

These few primitives, together with first-class blocks of code in square brackets, form a Turing complete computation language. Awelon also has limited support for embedded data - natural numbers, texts, binary large objects (see below). Programmers are able to define their own words, which trivially rewrite to their definitions as needed for progress. Evaluation will simply rewrite a program's representation to an equivalent representation. For example, assuming appropriate definitions, `6 7 multiply` should evaluate to `42`. Awelon's syntax is extremely simple, very close to an AST, both to simplify rewriting evaluation and integration with projectional editors. The concatenative inline structure should also prove convenient for streaming code, REPLs, and one-liners.

An Awelon runtime or compiler will recognize a set of *Annotations*, represented by parenthetical words. Annotations have formal identity semantics: adding or removing annotations should not affect a program's meaning. But annotations may influence evaluation strategy, memoization, runtime representations, static analysis, assertions, quotas, debugging, rendering, and so on. For example, `(par)` could ask our compiler to parallelize a computation, `(trace)` could print a stack element to a log for debugging. Annotations may generally be understood as expressing *programmer intentions*. 

An essential class of performance annotations is *Accelerators*. Accelerators are how Awelon compilers or interpreters extend the language with new performance primitive types and operators. For example, a runtime could recognize `(array)` to represent a list as an array under the hood, then use `[list-index](accel-array-index)` to replace a naive `list-index` implementation by an built-in array indexing operator. We might also use `(assert-array)` to insist the representation underlying a list should already be `(array)`. If we enforce linearity, it is feasible to support in-place updates to an array. Careful design and use of accelerators enables Awelon to get by with just one primary data-type: the first-class function.

## Words

Words are the user-definable unit in Awelon. Syntactically, a word is a sequence of lower-case alphanumerics and hyphens, starting with an alpha. Regex: `[a-z][a-z0-9-]*`. The formal meaning of a word is a trivial equivalence to its definition. Evaluation will lazily substitute a word by its definition.

Valid definitions must be acyclic, allowing for a simple inline expansion of all definitions (see *Loops*). Further, definitions must be block balanced - no unmatched `[` or `]` block delimiters. 

Awelon environments may enforce some ad-hoc structure on usage of words in the dictionary. For example, to support module local definitions, we might reject code where `foo-local-*` is used outside `foo-*`. When looking for documentation for a word, we might heuristically check `*-meta-doc`. We could reserve `tag-*` and `todo-*` words for reverse lookups. A projectional editor might even render some words using CJK characters or iconography. Essentially, the environmental context provides an implicit connotation for many words, but should not be overly restrictive.

## Natural Numbers

Awelon has limited support for natural numbers. Syntactically, natural numbers are represented by regex `0 | [1-9][0-9]*`. Semantically, natural numbers are Awelon words with an automatic definition.

        0 = [zero]
        1 = [0 succ]
        2 = [1 succ]
        42 = [41 succ]

Definition of `succ` and `zero` - and hence our model for natural numbers - is in theory left to our developers. For example, we could select between a recursive sum encoding (`type Nat = μN.(1+N)` where `type (A+B) = ∀r.(A→r)→(B→r)→r`) or a Church encoding (`type Nat = ∀x.(x→x)→x→x`). In practice, however, *Accelerators* will determine which model will be favored, because nobody is going to accept unary processing of numbers at runtime. A `(nat)` annotation could direct rewriting from `[41 succ]` to `42`. 

Awelon will rely on *Projectional Editing* and *Accelerators* to support a numerical tower above natural numbers. Fully modeling the foibles of floating-point numbers will be non-trivial, but necessary to take advantage of floating point arithmetic units. 

## Embedded Texts

Awelon has limited support for embedding texts inline between double quotes such as `"Hello, world!"`. Semantically, embedded texts are special words defined automatically by template:

        "" = [null]
        "hello" = [104 "ello" cons]

That is, texts are simply an ASCII-encoded list of bytes. Like natural numbers, `null` and `cons` must be defined in the dictionary, and *Accelerators* will determine the de-facto standard definitions.

Embedded texts are limited to ASCII minus control characters and double-quote. There are no escape sequences! However, it is not difficult to wrap a text with processors to rewrite with user-defined escapes, e.g. `["hello\nmulti-line\nworld" literal]` might evaluate to a binary with line-feeds in place of each `\n` sequence. Similarly, we could support base64 binary representations. Working with multi-line texts and inline binaries can be made more convenient with support of *Projectional Editing*. 

*Note:* For larger texts and binaries, developers are encouraged to leverage *Secure Hash Resources* at the dictionary layer. For even larger ones, a rope-like data structure with binary blocks may be appropriate. Embedded texts are intended for labels, test data, inline comments, one-liner micro-DSLs, etc..

## Annotations

Annotations are special parenthetical words, such as `(par)` or `(error)`.

Annotations always have the same formal semantics: identity. That is, adding annotations to a program should not affect its formal behavior. However, within this limitation, annotations are assigned ad-hoc *informal* semantics by the runtime or compiler. For example, `[A](par)` can request parallel evaluation of `[A]`, `(trace)` could print to a debug console, and observing `(error)` can cause evaluation to fail fast. In general, annotations augment performance, safety, debugging, and display of programs. They encode any *programmer intentions* other than functional behavior.

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
* `(quota)` - impose limits on argument evaluation effort
* `(static)` - assert value is constant in scope of definition

Awelon does not constrain annotations beyond requirement for identity semantics.

## Accelerators

Accelerators are built-in functions with reference implementations, accessed by annotation. Use of accelerators enables Awelon compilers or interpreters to extend the set of "performance primitives" available to Awelon programmers.

For example, Awelon systems should accelerate natural numbers. We might use `(nat)` to indicate that a value should use an optimized representation for natural numbers under the hood. The function to add two natural numbers could be annotated via `[reference impl] (accel-nat-add)`, which we subsequently inline. This tells our interpreter or compiler to replace the reference implementation by the specified built-in operator.

Because acceleration is driven by symbolic annotations, our runtime can easily inform developers when an accelerator is not recognized or is scheduled for deprecation. There is no risk of silent performance degradation. The dynamic binding costs are very low: a simple dictionary lookup. Acceleration is simple, stable, and robust.

Reference implementations can be validated by a separate process. Early in development, it can be inconvenient to provide a reference implementation. Awelon systems enable a practical stance here: we can waive validation for new accelerators - e.g. `["todo! fp mul"(error)](accel-fp-mul)` might provide access to a built-in floating point multiply operator even before we've properly modeled floating point numbers. This can support transitional scaffolding of our system.

The main cost of accelerators is portability: code that relies on a set of accelerators will not be performance-portable to a runtime that doesn't also provide those accelerators. To mitigate this cost, we should standardize accelerators and favor acceleration of models that are stable and useful across many problem domains, such as arithmetic and linear algebra.

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

The BLAKE2b algorithm could be replaced by another, simply rewriting the entire dictionary, if ever it proves inadequate. The proposed base32 alphabet skips all the vowels to avoid accidental offense with pronounceable words.

*Security Notes:* A secure hash should be treated as a bearer token authorizing access to the associated resource. However, it's important that the system must not *leak* this authority. In particular, we should guard against timing attacks to discover stored secure hashes. Further, there is an attack of the form "does data with this secure hash exist?" where the  attacker might request millions of hashes to discover, for example, an unknown phone number within an otherwise predictable template. This can be resisted by including an entropy field within sensitive data. Finally, to work with untrusted content distribution services, we could encrypt data using the original hash, and lookup using a hash of hash.

### Software Packaging and Distribution

Awelon is designed for distribution of entire dictionaries. An advantage of whole-dictionary distribution is that everything can be curated, tested, known to work nicely together. There is no discovery phase of downloading packages or searching an environment's class path. There is no version hell of package configurations. Of course, the cost is that we might have a much larger dictionary than we require for a given program. But this concern is mitigated by *lazy download* of secure-hash resources.

However, packages remain a useful software distribution and maintenance concept. For example, we might wish to sell a package, or synchronize and mirror a selected volume of our dictionary with a shared community. For these cases, we align package names with a hyphenated prefix `packagename-`. This is rather ad-hoc. Avoiding name conflicts is left as a problem for the community. Installation, removal, and update of package becomes a one-liner in the dictionary: `/packagename- secureHash`. Packages distributed in this manner would still support structure sharing with prior versions and across similar dictionaries.

Awelon is not optimized for packages. Packages must use the full `packagename-` prefix internally. But we can support namespaces via the *Projectional Editing* environment. 

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

Developers will guide evaluation strategy through annotations. For example, we might use `(par)` to request parallel computation of a value. We might use `[F](jit)` to request compilation for a function or loop body. We could use `(lazy)` and `(eval)` to recommend call-by-need or eager evaluation. Further, annotations may result in certain rewrites that cannot be performed by language primitives. For example, `(nat)` could rewrite `[41 succ]` to the equivalent `42`. 

*Aside:* We can feasibly support user-defined optimizations similar to GHC Haskell's rewrite rules, activated by annotations. The typical example is stream fusion: `[F] map [G] map == [F G] map`. The burden of correctness - preserving identity semantics of annotations - would then be shifted to our developers and debuggers. However, ad-hoc rewrites are not robust to abstraction or modularity. Instead, I would favor developing a DSL data structure for stream processing, and an optimizer-compiler function for the DSL. Between rewriting and `(jit)`, Awelon is very friendly to DSLs.

### Arity Annotations

Arity annotations are useful for Awelon, and have simple rewrite rules:

        [B][A](a2) == [B][A]
        [C][B][A](a3) == [C][B][A]
        ...

Arity annotations can be used to help control rewriting and partial evaluation. For example, consider a swap function `w = (a2) [] b a`. If our runtime has a naive view of 'progress', we might rewrite `[A]w => [[A]]a`, which is not useful progress. With the arity annotation, `[A]w` does not evaluate further, instead `[B][A]w` evaluates directly to `[A][B]`. 

Arity annotations are also useful for modeling codata. For example, `[[A](a2)F]` has the observable behavior and type of `[[A]F]`, but the former defers computation until the result is required.

## Loops

Awelon disfavors recursive definitions. Instead, we use fixpoint combinators:

        [X][F]z == [X][[F]z]F
        z = [[(a3) c i] b (eq-z) [c] a b w i](a3) c i

        assuming:
            [def of foo](eq-foo) == [foo]
            [B][A]w == [A][B]       w = (a2) [] b a
               [A]i == A            i = [] w a d

Other loop combinators can be built upon `z`. For example, we can develop a `foreach` function that processes a list or stream. The most common loop combinators may be accelerated for performance.

*Aside:* I've contemplated allowing recursion many times. Unfortunately, recursion isn't very helpful in-the-small where common loop combinators can do the job. And in-the-large recursion is just a recipe for entangling code with the environment, hindering sharing and comprehension, which is not congruent with Awelon's goals. Further, I hope that we'll gradually elevate Awelon code above explicit loops, instead favoring collections-processing combinators or DSLs. Thus, recursion is rejected.

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

*Note:* Besides static types, termination analysis is also useful. As a purely functional language, non-termination or divergence is an error for Awelon functions.

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

## Projectional Editing

Awelon's simple syntax must be augmented by [projectional editing](http://martinfowler.com/bliki/ProjectionalEditing.html) techniques to support richer programming interfaces, DSLs, namespaces, application models, and larger programs. As a simple example, we could develop a numeric tower:

        #42         == (Awelon's natural 42)
        42          == [#42 int]
        -7          == [7 int-neg]
        3.141       == [3141 -3 decimal]
        -0.0070     == [-70 -4 decimal]
        2.998e8     == [2998 5 decimal]
        -4/6        == [-4 #6 rational]

In this example, I build one view upon another, but we also have a comprehensible representation in without the view. For example, if our projectional editor lacks support for rationals, users would still see the `[-4 #6 rational]` representation. This is convenenient for iterative development of extensible views. Further, if carefully designed our views such as `[7 int-neg]` should be normal forms such that we can also render `-7` as an *output* from a computation. This may involve careful use of arity annotations.

Besides numeric towers, projections could feasibly support lists and matrices, monadic programming, and other features. Problem specific languages can frequently be modeled as data-structures that we evaluate statically. Embedded comments can also be represented, e.g. as a view of `"comment"(a2)d`.

Besides textual views, projectional editors can support graphical editor-viewer widgets. For example, to edit a color or date-time, we might provide a color-picker or calendar widget. Intriguingly, we can support multiple widgets for a given volume of code, with an edit in one reactively affecting the others. Further, we can feasibly support *specialized* view or editor widgets for an application's data model - thus providing Awelon's version of a GUI application front end.

Our projectional editors may further support views of the dictionary, not just views of a specific program. Ad-hoc sessions could perhaps be encoded as `[foo][bar][baz]`, loading all three definitions into an editor. But we can also use structured sessions, e.g. render `foo-c1`, `foo-b3`, etc. as a spreadsheet `foo` with each cell as a separate word. A REPL or notebook application might use a word per line, implicitly continuing the prior line (e.g. `:repl-3 repl-2 command3`). 

### Namespaces

It is not difficult to recognize a comment as declaring a qualified namespace.

        using long-prefix as lp; lp-foo
            becomes
        "using long-prefix as lp"(a2)d long-prefix-foo

However, I don't recommend this! Namespace declarations in code too easily become a form of boiler-plate (especially for one-liner functions). They also interact awkwardly with copy-paste. Further, namespaces fixated in code don't allow for much user customization.

Instead, I propose to move namespaces to the editor layer: an editor can track a user's set of preferred nicknames together with other user-model data like a clipboard. Users can manage this set, perhaps switch between packages based on current project. It's up to each user and the editor's feature set. Further, it blends nicely into richer views - namespaces are essentially a very limited form of user-defined, pluggable view.

*Aside:* I'm interested in use of color as an alternative to prefixes, e.g. such that `html-div` is written in a different color from `math-div`. This would give us a more concise notation.

### Named Local Variables

Although tacit programming styles are suitable for many problems, they make an unnecessary chore of sophisticated data shuffling. Fortunately, we can support conventional let and lambda expressions as a projection. 

Consider a lightweight syntax where `\x` indicates we'll pop a value from the stack and assign it to a local variable `x`. The 'scope' is simply the remainder of the current definition or block. Thus `[\x EXPR]` becomes an equivalent to `(λx.EXPR)`, while `[X] \x BODY` corresponds to `let x = [X] in BODY`. We might represent this via bidirectional rewriting:

        \x EXPR == (local-x) T(x,EXPR) 
          assuming `x` represents a value
        
        T(x,E) | E does not contain x       => d E
        T(x,x)                              => 
        T(x,[E])                            => [T(x,E)] b
        T(x,F G)                            
            | only F contains x             => T(x,F) G
            | only G contains x             => [F] a T(x,G)
            | F and G contain x             => c [T(x,F)] a T(x,G)

This design isn't optimal for all cases. Especially not for conditional behaviors: for `[F][T]if` we know we'll apply either `F` or `T` in the current stack, so a more efficient translation is `T(x, [F] [G] if) => [T(x,F)] [T(x,G)] if`. To ameliorate this, we can either add some specialized rules for known conditional constructs, or develop a compiler optimization to eliminate unnecessary closures.

*Aside:* For me, support for named locals were the first convincing proof that *Projectional Editing* can be a viable alternative to sophisticated built-in syntax.

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

Awelon lacks implicit support for generic programming. For example, we cannot implicitly overload a central `add` word based on argument type. We'll instead use a separate `nat-add` vs `int-add` vs `matrix-add`. If we want a generic `add` word that works for many types, we'll need to explicitly model the typeful context through our program, and leverage partial or multi-stage evaluation for performance.

It seems feasible to develop editable views and a monadic type to make tracking the type context mostly implicit. And in the absence of built-in type system support, we might rely more heavily on static staging with deferred type analysis. However, the solution is not obvious and I have not yet worked through the details. I hope to get back to this feature later.

*Aside:* Between polymorphism and a standard set of collections, generic programming is not essential. But it's sometimes convenient.

## Bots and Effects

An Awelon 'bot' process is modeled as a deterministic transaction, repeated indefinitely. Repetition will implicitly wait when repetition would obviously be unproductive. This implicit wait supports process coordination, replacing locks, signals, polling. For example, a stream processing bot can abort when the input channel is empty or the output channel is full. An aborted transaction is obviously unproductive, so the bot would implicitly wait for the observed condition to change.

Preliminary API:

        type TX v e a -- opaque, monadic
        type Env v = ... -- model of host system
        type Bot = forall v e a . Env v -> TX v e a
        type TaskName = String -- for debugging!

        alloc   : TX v e (v a)
        read    : v a -> TX v e (Maybe a)
        write   : v a -> Maybe a -> TX v e ()
        try     : TX v e a -> TX v e' (Either e a)
        abort   : e -> TX v e a
        fork    : TaskName -> TX v e a -> TX v e' ()

Bots operate upon a provided environment of opaque variables `forall v. Env v`. This environment models the host system. Effects, such as network access or reflection on the dictionary, are achieved asynchronously through manipulation of system variables. For example, we can write requests to a system task queue. After the transaction commits successfully, the system will process the task queue to execute the tasks and write appropriate acknowledgements or responses. The bot may observe the inputs via subsequent transaction. The environment model is ad-hoc and should be carefully documented. 

Because the environment is abstract via the `forall v` constraint, bots must not have any built-in variable references. We can control a bot's access to system variables and effects or simulate an environment. This is convenient for automatic testing, and working with distrusted bot behaviors.

To support user discovery and control of bots, I propose to use the `bot-*` volume of the dictionary. Each bot is uniquely identified as `bot-botname`, and installing a new bot involves simply adding its definition. Users can directly manipulate bot definitions, with changes being applied immediately. The user should also have easy access a task-manager UI, which presents each bots together with useful metrics or records like CPU usage and a recent history of commit-abort return values.

Transactions manipulate variables via imperative `read` and `write` operations. New variables can be requested via `alloc`. Because garbage collection is awkward for simulated or sandboxed environments, I propose to explicitly model the unassigned `None` state for variables. (We can still use garbage collection as a fallback or to debug memory leaks.) The `try` and `abort` operations give us hierarchical transactions with strong exception safety for partial failure and graceful degradation. In context of stowage, memoization, or checkpoints, variables will be concretely represented as `[ref-1123]`. To preserve the `forall v` restriction, we can simply reserve the entire `ref-*` volume of words and warn when a reference word is used in the normal dictionary.

To support divide-and-conquer tactics, task parallelism, and staged programming, I introduce the `fork` operation, which specifies an operation to perform after we commit. When we repeat a deterministic read-fork transaction over stable variables, we'll compute the same set of forked tasks. We can simply repeat those tasks until the variables change. Thus, with `fork`, our bot becomes a hierarchical tree of read-fork transactions with read-write process loops for leaves. (Stabilizing this tree is left to the developer. For an unstable mix of read-fork-write, we could simply fail the transaction and raise an error.)

The cost of this model is occasional unproductive work and rework. But developers can keep that under control via design patterns. The benefits of this model for liveness, extensibility, securability, and resilience make it an excellent fit for Awelon's goals.

