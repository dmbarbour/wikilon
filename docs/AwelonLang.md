
# Awelon Language

Awelon is a Turing complete, purely functional language based on concatenative combinators with confluent rewrite rules. Awelon uses four primitive combinators:

        [B][A]a == A[B]         (apply)
        [B][A]b == [[B]A]       (bind)
           [A]c == [A][A]       (copy)
           [A]d ==              (drop)

Awelon additionally has limited support for data - natural numbers, embedded texts, and large binaries. Beyond these few primitives, programmers may define words in a dictionary. Evaluation proceeds by rewriting according to primitive combinators and lazily rewriting words to their definitions when doing so permits further progress. Hence, the result of evaluation is an Awelon program equivalent to the input.

Those `[]` square brackets represent first-class functions and contain Awelon code. Values in Awelon are always formally first-class functions, frequently using [Church encodings](https://en.wikipedia.org/wiki/Church_encoding) or [Scott encodings](https://en.wikipedia.org/wiki/Mogensen%E2%80%93Scott_encoding). However, effective Awelon compilers or interpreters should recognize and optimize common functions and value types. This is a concept of software *Accleration* to support efficient use of CPU and memory, extending the set of language *performance primitives* relative to a reference implementation. Acceleration for collections-oriented operations, such as matrix multiplication and linear algebra, can feasibly leverage SIMD instructions or GPGPU.

A runtime will also recognize a set of *Annotations*, represented by parenthetical words. For example, `[A](par)` might request parallel evaluation for the expression `A`, or `[F](accel)` might indicate that `F` should be recognized and accelerated. Annotations have identity semantics. Ignoring them won't affect observations within the program. However, external observers will be affected. Annotations serve roles in debugging and guiding performance.

By itself, Awelon is a simplistic language - a purely functional assembly. 

Being purely functional, all data must be represented within the Awelon program. Hence, the Awelon *dictionary* doubles as a database or smart filesystem, and is designed for easy update, sharing, and integration. The intention is to leverage [projectional editing tools](http://martinfowler.com/bliki/ProjectionalEditing.html) to render Awelon programs and data with a rich structural or graphical syntax. Because Awelon evaluates by rewriting, projections designed for source code can generally also render evaluated results or intermediate states. Hence, computations may be viewed as self-rewriting user interfaces. The purpose of Awelon language is to use this idea to develop new [application and data models](ApplicationModel.md) that are accessible, sharable, and composable by end users.

*Note:* I'm contemplating a few variations of Awelon. See [Immutable Awelon](ImmutableAwelon.md) and [Awelon with Modules](AwelonFML.md).

## Words

Words are the user-definable unit for Awelon code. Syntactically, a user-definable word has regex `[a-z][a-z0-9-]*`. That is, a word consists of lower case alphanumerics and hyphens, and starts with an alpha.

The formal meaning of a word is a trivial rewriting to its definition, a function encoded in Awelon. Definitions must have acyclic dependencies (see *Loops*), must be block-balanced (no unmatched `[` or `]`). In addition to formal semantics, words may have informal connotations in context of a system or environment. For example, `foo-meta-doc` may define documentation associated implicitly with `foo`. 

In context of an Awelon system, words may be further restricted by static analysis. For example, a linter could easily express a policy that words of form `foo-local-*` are only directly referenced from words of similar form `foo-*`, representing private functions. Or a policy that `foo-type`, if defined, must evaluate to a recognizable representation of a function type, which is then verified against the definition of `foo`. Such constraints can provide rich structure within the Awelon system.

## Natural Numbers

Awelon has limited support for natural numbers. Syntactically, natural numbers are represented by regex `0 | [1-9][0-9]*` wherever a word may appear. Semantically, natural numbers are Awelon words with an automatic definition.

        0 = [zero]
        1 = [0 succ]
        2 = [1 succ]
        42 = [41 succ]

Definition of `succ` and `zero` - and hence our model for natural numbers - is in theory left to our developers. For example, we could select between a recursive sum encoding (`type Nat = μN.(1+N)` where `type (A+B) = ∀r.(A→r)→(B→r)→r`) or a Church encoding (`type Nat = ∀x.(x→x)→x→x`). In practice, runtime support for *Acceleration* determines which model will be favored. For more sophisticated number types, it's feasible to build a tower of numbers via *Editable Views*.

*Note:* I've frequently considered a hard-coded interpretation, e.g. the Church encoding `N = [(c a)^N d]`, to guarantee stable meaning across all Awelon dictionaries. But this hardly seems worthwhile in context of the many other data types we will build above natural numbers.

## Embedded Texts

Awelon has limited support for embedding texts inline between double quotes such as `"Hello, world!"`. Semantically, embedded texts are special words defined automatically by template:

        "" = [null]
        "hello" = [104 "ello" cons]

That is, texts are simply an ASCII-encoded list of bytes. Like natural numbers, `null` and `cons` must be defined in the dictionary, and *Acceleration* determines the de-facto standard. 

Embedded texts are limited to ASCII minus control characters. There are no built-in escape characters, and the double quote also is forbidden. Although this is very limited, it's sufficient for lightweight DSLs, labels, basic test data, comments, rendering hints. We can build above the basic text using *Editable Views* - for example, `["hello\nmulti-line\nworld" literal]` could evaluate to a binary with line-feeds in place of the `\n` sequence. For large texts or ad-hoc binaries, developers are encouraged to favor *Binary Resources* at the dictionary layer rather than awkwardly embedded text.

## Annotations

Annotations are special parenthetical words, such as `(par)` or `(error)`.

Annotations always have the same formal semantics: identity. That is, adding annotations to a program must not affect its formal behavior. However, within this limitation, annotations are assigned ad-hoc *informal* semantics by the runtime or compiler. For example, `[A](par)` can request parallel evaluation of `[A]`, while observing `(error)` can cause evaluation to fail fast and simplify debugging. In general, annotations augment performance, safety, debugging, and display of programs. They encode any *programmer intentions* other than functional behavior.

Some potential annotations:

* `(trace)` - print argument to debug console or log
* `(error)` - prevent progress within a computation
* `(par)` - evaluate argument in parallel, in background
* `(eval)` - evaluate argument before progressing further
* `(stow)` - move large values to disk, load on demand
* `(accel)` - assert software acceleration of a function
* `(optimize)` - rewrite function for efficient evaluation
* `(jit)` - compile a function for multiple future uses
* `(memo)` - memoize a computation for incremental computing
* `(nat)` - assert argument should be a natural number
* `(type)` - describe type of stack at given location
* `(quota)` - impose limits on argument evaluation effort
* `(static)` - assert value is constant within a definition

Awelon does not constrain annotations beyond requirement for identity semantics.

## Acceleration

We can improve performance of software by replacing slow implementations of common operations or data types with fast ones, requiring only that behavior is preserved. I call this *acceleration*, in general, alluding to hardware acceleration but permitting software acceleration. In Awelon, we could recognize common models (such as natural numbers) and operations upon them (such as adding or multiplying). 

In practice, acceleration requires annotations such as `(nat)` and `[reference impl](accel)`. Annotations make assumptions explicit, resulting in robust, predictable performance that neither degrades silently nor improves magically. They also simplify static analysis of programs to ensure all uses of accelerators are safe.

Besides natural numbers, acceleration can feasibly be applied to integers, floating point, lists as arrays, records, linear algebra, a pure subset of OpenCL, Kahn process networks, etc.. It's important to develop a few models with a relatively high return on investment. Effectively, a choice of accelerators becomes a set of *performance primitives* for Awelon.

## Dictionary

Awelon words are defined in a codebase called a "dictionary". A dictionary is essentially a key-value database, associating words to definitions. To support Awelon project's various goals, Awelon specifies a standard dictionary representation with convenient properties for import/export, versioning, sharing, scaling, etc.. Legibility is also a goal, to simplify debugging or inference of implementation. Awelon dictionaries can feasibly scale to many gigabytes or terabytes, and support distributed representation, like a variant file-system.

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

*Security Note:* A secure hash should be treated as a bearer token authorizing access to the associated resource. However, it's important that the system must not *leak* this authority. In particular, we should guard against timing attacks to discover stored secure hashes. Further, there is an attack of the form "does data with this secure hash exist?" where the  attacker might request millions of hashes to discover, for example, a partially known phone number within an otherwise predictable template. This attack can be resisted by including an entropy field (or comment) with random data together with the sensitive data.

*Security Note 2:* To work with untrusted content distribution services, we can easily use half our secure hash for lookup and the other half as a symmetric encryption key. 

### Software Packaging and Distribution

Awelon is designed for distribution of entire dictionaries. An advantage of whole-dictionary distribution is that everything can be curated, tested, known to work nicely together. There is no "version hell" of package maintenance. Awelon dictionaries can be very large, but *lazy download* of secure hash resources can enable working with dictionaries that contain more data han we use.

However, packages remain useful for access control, sale, or real-time update.

In those cases, we might associate a package with a registered prefix `packagename-*`. This allows for convenient one-line update or install `/packagename- secureHash`, easily enforcable local definitions via `packagename-local-*`, and package protected *Opaque Data Types* (via `(seal-packagename)` and `(unseal-packagename)`). A simple registry can resist name conflicts. A package could be installed and maintained manually, or automatically given a URL, authorization, and authentication requirements (perhaps included within the client dictionary as `local-install-packagename`).

To resist version hell, I would recommend packages are mostly provided through community-curated distributions so all the versions of packages are known to work together. Also see [Haskell Stackage snapshots](https://www.stackage.org/).

## Stowage

Large scale computations frequently work with data that doesn't fit all at once into memory. These days, operating systems help address the issue using *virtual memory* where volumes of the address space are offloaded until accessed. Purely functional languages can take virtual memory a step further due to the absence of mutation, collapsing identical volumes of data to support structure sharing. We can make this explicit using a `(stow)` annotation:

        [large value](stow)    => [stow-id]
        [small value](stow)    => [small value]

Here, `stow-id` is a word whose definition is `large value`. Effectively, we're allocating new words to extend a dictionary, or creating a supplementary dictionary. An evaluator that supports stowage will simply produce these words as an extra output. Words can be reused when the same value is stored. This is a 'safe' form of dictionary mutation, being monotonic in nature.

Importantly, stowage can work nicely with caching and memoization of computations. Stowage identifiers offer a second-class form of reference equality that Awelon otherwise lacks. Stowage words can also serve as natural volumes for progressive disclosure when rendering. A disadvantage is that stowage identifiers will often need to be garbage collected.

## Evaluation

Evaluation will rewrite an Awelon program to an equivalent Awelon program. In context of annotations like `(stow)` or `(memo)` or `(trace)`, we might supply a few auxiliary outputs. Awelon is a pure language, but interactions with external agents provides a basis for effects.

Primitives rewrite by simple pattern matching:

            [B][A]a => A[B]         (apply)
            [B][A]b => [[B]A]       (bind)
               [A]c => [A][A]       (copy)
               [A]d =>              (drop)

Words rewrite to their evaluated (and potentially optimized) definitions. However, this rewrite should be *lazy* in the sense that a rewrite is avoided when it does not contribute to further progress. The motive is to retain human-meaningful symbols and structure within the evaluated result, which may be flexibly rendered via *Editable Views*. We may also support nouns - words defined by a single first-class value - such as `true = [a d]` or `unit = [(error)]` - which can be bound like values `true [] b == [true]`. An undefined word is equivalent to the trivial loop, like `:foo foo`, and does not rewrite further. 

Evaluation strategy is unspecified. The default may be a heuristic mix of lazy, eager, and parallel. Annotations may guide the strategy. Valid, terminating computations will always converge on the same final result regardless of strategy. If we halt early, which is possible with a quota or error, evaluation details and optimizations might be exposed.

### Arity Annotations

Arity annotations are useful for Awelon, and have simple rewrite rules:

        [B][A](a2) == [B][A]
        [C][B][A](a3) == [C][B][A]
        ...

These annotations can be used to defer linking of words where a partial evaluation isn't useful. For example, consider a swap function `w = (a2) [] b a`. Ignoring the arity annotation, we'd rewrite `[A]w => [[A]]a`, which isn't useful progress. With the arity annotation, `[A]w` does not evaluate further, but `[B][A]w` evaluates directly to `[A][B]`. Arity annotations are also useful for modeling codata. For example, `[[A](a2)F]` has the observable behavior as `[[A]F]`, but the former defers computation until the result is required.

## Loops

Lacking recursive definitions, we express loops using fixpoint combinators:

        [X][F]z == [X][[F]z]F
        z = [[(a3) c i] b (eq-z) [c] a b w i](a3) c i

        assuming:
            [def of foo](eq-foo) == [foo]
            [B][A]w == [A][B]       w = (a2) [] b a
               [A]i == A            i = [] w a d

Other loop combinators can be built upon `z`. For example, we can develop a `foreach` function that processes a list or stream. The most common loop combinators may be accelerated for performance.

*Aside:* I've frequently contemplated allowing recursive definitions in Awelon. Recursion is certainly more convenient than fixpoint combinators, although loop combinators mostly avoid the requirement. However, recursion entangles behavior semantics with a dictionary, and it does not occur naturally from refactoring lower level code.

## Memoization

Annotations can easily indicate [memoization](https://en.wikipedia.org/wiki/Memoization).

        [computation](memo) => [result]

Memoization involves searching for an existing record of the computation, or writing one if it does not exist. The idea is to trade space for time. Of course, there are also significant overheads for storage and lookup, so this should be applied carefully.

For effective incremental computing, we must use memoization together with cache-friendly patterns: compositional views over persistent data structures. We also need stable *Stowage* identifiers - allocating new names would hinder memoization.

## Error Reporting

We can represent runtime errors or assertion failures by simply introducing an `(error)` annotation that acts as an explicitly undefined word, unable to be further rewritten. Then, we can define words such as `divide-by-zero = (error)` to create explicit, named errors that should never rewrite. In these cases, errors halt evaluation. We can also encode lazy error values, which only become an error when observed, e.g. `[divide-by-zero]`. 

For expected or continuable errors, such as a parse or constraint error that might lead to falling back and searching an alternative path, error values should be modeled explicitly in a function's return data type. Awelon doesn't have a built-in exception system, but it's feasible to model an exception monad.

## Static Typing

Awelon doesn't depend on types: there is no type-driven dispatch or overloading. However, the language implies a simple static type model. If users can discover errors earlier by using static type analysis, that's always a good thing. The stack-like environment can be typed as a tuple, and values as functions. Record constructors are typed using row polymorphism. Types for our primitive operations:

        a           ((s * x) * (s → s')) → (s' * x)
        b           ((s * x) * ((e * x) → e')) → (s * (e → e'))
        c           (s * x) → ((s * x) * x)
        d           (s * x) → s
        [F]         s → (s * type(F))

Type annotations can be expressed using Awelon annotations, we only need some conventions. Obviously, we can use specific annotations such as `(nat)` or `(bool)` for the most common types. Lightweight annotations could encode simple arities or stack notations, e.g. `[F](t21)` might simply assert `F` receives two arguments and outputs one. For ad-hoc sophisticated or precise types, we might require a type argument `[Type Descriptor](type)d`. We can also, feasibly, assign names to types or variables via annotations to simplify debugging.

Unfortunately, simple static type systems are sometimes too simplistic and restrictive. For any consistent type system, we'll always have safe programs that cannot be typed. For example, the `pick` function from Forth isn't amenable to typing without sophisticated dependent types:

        [Vk]..[V1][V0] k pick == [Vk]..[V1][V0][Vk]

In this context, we could develop a series of functions like `pick2nd` and `pick3rd`, at cost of much boiler-plate. Or we could try to defer static typing until after we've specialized on the first parameter, treating `pick` as a macro. Intention to defer type checking can be indicated by annotation, e.g. adding a `(dyn)` comment to the subprogram with `[A](dyn) => [A]` behavior.

*Note:* Besides static types, termination analysis is also useful. As a purely functional language, non-termination or divergence is always an error for Awelon programs.

### Opaque Data Types

Implementation-hiding modularity in functional programming languages is frequently based around [opaque data types](https://en.wikipedia.org/wiki/Opaque_data_type) as a simplified approximation of [abstract data types](https://en.wikipedia.org/wiki/Abstract_data_type). Direct access to data representation is confined to a controlled volume of code. External code is limited to a subset of provided interfaces. Those interfaces enforce invariants, control coupling, partition programming tasks, and isolate bugs.

For Awelon, we can support opaque data types via value sealer annotations:

        (seal-foo)      (s * x) → (s * foo:x)
        (unseal-foo)    (s * foo:x) → (s * x)

By themselves, these annotations serve as symbolic type wrappers, akin to `newtype` in Haskell, resisting accidental access to representation. To protect opaque data types, we further constrain direct access to these annotations to a codebase prefix: `(seal-foo)` and `(unseal-foo)` are only permitted in source definitions of words starting with `foo-`. This is trivially enforced by linter, and works well together with a policy for private functions, for example that `foo-local-*` may only be directly used from other words of form `foo-*`. A hyphenated prefix would then serve as a dictionary package or module, with ad-hoc opaque data types and hidden functions.

## Structural Equivalence

Annotations can assert two functions are the same, structurally:

        [A][B](eq) => [A][B]     iff A and B are structurally equivalent

Structural equivalence assertions are certainly convenient for lightweight unit testing. But the motivating use case is merging sorted data structures. Efficient merge requires knowing whether the two structures are sorted using the same comparison function. If we couple the sort function with the collection, we can use `(eq)` to verify our assumption.

*Aside:* Behavioral equivalence is not something we can generally test in a Turing complete language. But structural equivalence could include limited forms of behavioral equivalence comparisons.

## Editable Views

Awelon's simple syntax must be augmented by [projectional editing](http://martinfowler.com/bliki/ProjectionalEditing.html) techniques to support richer programming interfaces, DSLs, namespaces, application models, and larger programs. As a simple example, we could support a numeric tower:

        #42         == (Awelon's 42)
        42          == [#42 #0 integer]
        -7          == [#0 #7 integer]
        3.141       == [3141 -3 decimal]
        -0.0070     == [-70 -4 decimal]
        2.998e8     == [2998 5 decimal]
        -4/6        == [-4 #6 rational]

This builds one view upon another, which is convenient for extending views. If our view left out rational numbers, we'd still render a sensible `[-4 #6 rational]`. Relative to built-in number support, there is some storage overhead - but it's relatively minor at larger scales (and compresses well). Besides numeric towers, editable views could feasibly support lists and matrices, continuation-passing style, Haskell-inspired do-notation, generators with yield, and other features. Problem specific languages can frequently be modeled as data-structures that we evaluate statically. Comments can easily be supported, e.g. `// comment == "comment"(a2)d`. Qualified namespaces are easy to support, e.g. such that `long-prefix-foo` can be abbreviated as `lp-foo`. It is feasible for projections to leverage color, such that `html-div` vs. `math-div` both render as `div` but in different colors, or other graphical expression of meaning.

Although our initial emphasis is plain text views, the eventual goal is to support richly interactive graphical views involving tables, graphs, canvases, music sheets, images, and so on. And for larger scales, although we could simply use huge definitions, we can also project edit sessions that view and edit multiple words together. For example, we might have `my-session = [foo][bar][baz]` so we can 'open' the session then edit those three words together. Or we might edit an prefix of words, perhaps using a spreadsheet-like view. A zoomable user interface is viable, allowing developers to drill into the definition of any component word. See also proposed [application models](ApplicationModel.md) for Awelon.

### Named Local Variables

We can leverage editable views to model named local variables, like lambdas or let expressions. For example, consider adapting Kitten programming language's syntax for local vars:

        7 -> X; EXPR            let-in equivalent
        [-> X; EXPR]            lambda equivalent

We can extract `X` from our expression by simple algorithm:

        EXPR == X T(X,EXPR) for value X

        T(X,E) | E does not contain X       => d E
        T(X,X)                              =>
        T(X,[E])                            => [T(X,E)] b
        T(X,F G)                            
            | only F contains X             => T(X,F) G
            | only G contains X             => [F] a T(X,G)
            | F and G contain X             => c [T(X,F)] a T(X,G)

For performance, we can optimize static conditionals to avoid copying:

        T(X,[F][T]if) => [T(X,F)][T(X,T)]if

Variable names could be recorded using comments or annotations. Example:

        -> x; EXPR
            becomes
        (var-x) T(x,EXPR)

With annotations, `[X](var-x)` might tag a value to simplify debugging.

Named local variables offer a useful proof-of-concept for *Editable Views* as a viable alternative to built-in syntax features. But I believe that most views will be projections of data constructors. Sophisticated whole-program rewrites like named local variables would be the exception, not the rule.

## Arrays

Awelon doesn't have an array data type. But use of annotations and accelerators can impose an array representation for some lists, such that we can access data in near-constant time. In context of a purely functional language, *modifying* an array is naively O(N) - copy the array with the modification in place. 

However, when we know we hold a unique reference to an array's representation, a runtime could modify the representation in-place without violating observable purity. This requires tracking whether we have more than one reference to an array, either dynamically or statically. Awelon's explicit copy operator makes dynamic tracking feasible, so we could flexibly use in-place mutation normally with copy-on-write for shared arrays.

*Note:* Use of arrays is often better replaced by persistent data structures, such as finger-trees or int-maps. Persistent data structures are more flexible in their application, and integrate more readily with *Stowage* for larger-than-memory data. However, arrays do offer performance benefits if used well, so there are trade-offs to consider.

## Labeled Data

Labeled data types (such as records and variants) are weakly commutative, human meaningful, and extensible in comparison to spatially structured data (such as `(A*B)` pairs and `(A+B)` sums). Awelon does not provide built-in support for labeled data. However, it isn't difficult to encode records as association lists, for example. Between annotations, acceleration, and editable views, it should be feasible to implement labeled data within Awelon.

An intriguing possibility is to work with *record constructor* functions, abstracting the actual record representation. A simple record constructor might look like `[[B] "b" put [A] "a" put ...]`. This preserves commutative structure within the record, permits flexible abstraction of records, and works conveniently with editable views. Updates would build the record, modify it, then rebuild the constructor - but this could be optimized away via accelerators.

## Generic Programming in Awelon

A weakness of Awelon is lack of built-in support for generic programming. For example, we cannot implicitly overload an `add` word to use different functions for different types, such as natural numbers versus matrices. We can use explicit overloads, but such mechanisms are often syntactically awkward and difficult to integrate with type systems. Deferred typing and projectional editing should help, but we still require a model with concrete constructors and predictable behavior to project above.

My intuition is to generalize generic programming as a constraint or search problem. For example, the choice of which `add` function to use is based on constraints in future input and result types, which may be provided later. It seems feasible to develop a monad with an implicit environment of constraints, then evaluate a monad to a program result at compile-time, i.e. staged metaprogramming. But I have not verified this intuition in practice.

