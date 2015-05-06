# Awelon Object Language (AO)

Awelon Object (AO) language directly uses [Awelon Bytecode (ABC)](AboutABC.md) as a foundation for functional programming. Simple conventions to establish two useful features:

* ability to link between **words** within a **dictionary**
* support for structured editing and EDSLs within a word

Words provide the basis of modularity and linking. The dictionary replaces conventional models for sharing and packaging code. Support for structured editing and DSLs arises from how word definitions are typed. In the long run, most development should occur through structure editors, but developers always have the option to operate directly on bytecode.

## AO Definitions and DSLs

Every AO word has a purely functional meaning. However, rather than *directly* defining functional behavior using bytecode, definitions construct and intermediate structured value `v` and a function `[v→[a→b]]` such that the structure may trivially be compiled into the word's function `[a→b]`. 

        type Def a b = ∃v.∀e. e → ([v→[a→b]]*(v*e))

The intermediate structured value `v` provides a foundation for DSLs. A structure editor computes and renders this value, supports direct manipulations on it. The value serves as a syntax while the `[v→[a→b]]` function becomes a language descriptor. In the trivial cases, the language may be `[]` (identity, in which case `v` is a block) or `[v'c]` in which case we're quoting and exporting `v` directly as data. In the more general case, we can this model for modules hides some information from the users.

Some possible definitions:

        [rwrwzwlwl][]
        [rw {%swap} wl][]
        #12345[v'c]
        #1#2l[{%fooLang}]

*Syntactically, AO definitions use raw ABC.* 

An unmodified, basic ABC parser can read AO definitions. Words are linked using `{%word}` tokens. Dependencies between words must be acyclic. In addition to word tokens, AO permits annotations and discretionary sealers/unsealers. 

Any word in an AO dictionary may be 'compiled' to independent ABC by transitively inlining the definition for each `{%word}` token and following it by `$vr$c` to compile then inline its functional meaning at runtime. However, we'll want to apply a lot of optimizations, such as precompiling and optimizing each word incrementally.

The weakness of AO is that these definitions are not very readable in raw form. A structure editor is essential to provide a good programming experience.

## AO Dictionaries

An AO dictionary is an associative array of words to definitions with two primary structural constraints:

* dependencies between words are acyclic
* no word in the dictionary is undefined

These structural constraints ensure it is possible to compile a word by transitively inlining its dependencies. Additional constraints, such as ensuring definitions are well typed, are sensible and should be enforced for some curated dictionaries. 

Dictionaries are always *complete*. There are no external dependencies. 

Open source communities can create and curate massive, general purpose dictionaries containing millions of words. These words would include both generic functions, useful data structures, documentation, discussion, tests, examples. They would also include very special purpose utilities and specific objects - clipart, fonts, widgets, game music, interactive fictions, entire open source projects. Private groups can fork an appropriately licensed open source dictionary. DVCS-like mechanisms - forking, merging, patching, cherry-picking, pull-requests - allow sharing between active dictionaries. 

Hosting thousands of forks and historical versions can be efficient via structure sharing. Storage is relatively cheap anyway! There is no need for separate packages or libraries. The DVCS sharing mechanisms replace package managers. When distributing a project, we might filter a much larger dictionary down to the essential subset of words.

However, not all dictionaries need to be large. For sharing or distributing a project, it is trivial to filter a dictionary to just the necessary subset of words.

## AO Dictionary Import/Export

AO doesn't specify how a system should represent dictionaries internally. However, I do define a simple import/export format for sharing dictionaries through a filesystem or HTTP service:

        @swap [rwrwzwlwl][]
        @swapd [rw {%swap} wl][]
        @swapd.doc "x y z -- y x z
          swap just under the top stack element
          assuming typical (stack*ext) environment
         ~[v'c]

This is a flat format suitable for simple text files and streams. Each word definition starts at `@` on a new line, the word runs to the next SP or LF, followed by the definition for that word using ABC. LF characters within the definition are escaped by SP, exactly as we do within ABC text. Two additional constraints:

* words are defined after their dependencies
* words are defined exactly once, no updates

For example, it would be an error if `swapd` above was defined before `swap`, or if `swap` was later redefined. These two constraints simplify validation of the primary structural constraints on dictionaries, and enable fast processing of a dictionary.

For HTTP, we should include an appropriate `Content-Type` header. Since I own `awelon.org`, I'll using the following:

        Content-Type: text/vnd.org.awelon.aodict

I'm using the text class because the `aodict` format is still a "source code" and is at least marginally human readable.

Every Awelon project system should support this simple format, though may apply constraints on received dictionaries (type safety, structure of words, alignment of function types with word suffixes, etc.). Other ad-hoc formats, such as YAML or JSON or even direct encoding in ABC, may additionally be supported.

## AO Dictionary Applications

A **dictionary application** is an application that has no dependencies outside of an dictionary, and whose only state consists of reflectively viewing or updating the same dictionary. 

A dictionary application might be understood as a (potentially mutable) view of the dictionary. While dictionary applications must ultimately be bootstrapped by services outside a dictionary, it is possible and preferable that the majority of dictionary app code be expressed within the same dictionary.

Updates to the dictionary must use dataflows similar to spreadsheets. Spreadsheets, iPython notebooks, and web applications are viable bases for developing dictionary applications.

## AO Design Motivations

One motive for AO is to experiment with programming at social scales:

* wiki-like programming experience
* refactoring impacts hundreds of projects
* integration testing easy as unit testing
* entire applications are software components
* abundant usage examples for didactic purposes 
* learn a word once then use in many projects

Another motive is to bootstrap the Awelon project. Awelon project requires good support for visual DSLs and live programming, such that users can have representations of behavior that are meaningful and directly modifiable. AO supports this via structure editors together with binding [AVMs](NetworkModel.md) to dictionaries so updates provide immediate feedback on runtime behavior.

AO also admits a few interesting possibilities.

Dictionaries are easily used with a genetic programming metaphor: words are genes, definitions are alleles, dictionary is DNA. It is feasible to model entire populations of related dictionaries when performing a program search. This seems much more difficult to perform with most languages and packaging models.

Disassembly is possible by analyzing an ABC stream against a popular dictionary.

## Weaknesses of AO

AO is purely functional, acyclic, tacit concatenative, and has very simple base types. Fixpoint combinators are necessary to express loopy computations. Recursive types are necessary to express collections. A lot of data plumbing code becomes a form of syntactic noise. Effects are usually be expressed asynchronously, through a [network and messaging model](NetworkModel.md) instead of straight-line threaded code. 

The underlying ABC representation is low level from a human standpoint but high-level from a machine standpoint. Thus, AO requires a development environment for effective use, and a compiler for effective performance.

AO programming can feel like puzzle solving - stimulating but exhausting. 

Fortunately, as data types, DSLs, and structure editors are implemented, casual development should become much easier. There is no reason a function couldn't be expressed using conventional mathematical notations. Even if a procedure uses asynchronous operations under the hood, a little syntactic sugar could abstract the asynchronous bits into a monad.

## AO Naming Conventions

The naming of words in an AO dictionary has zero impact on the *intrinsic, purely functional* meaning of a word. However, names certainly have connotations and conventions within a community. Conventions will be established, such as: `foo.doc` provides human-meaningful documentation for the word `foo`. Widely accepted conventions become ad-hoc standards.

I believe there is much power and utility in convention. However, conventions should be carefully developed to avoid becoming rigid or restrictive. So far, I have the following suggestions:

* *plurality:* e.g. if a dictionary has one `main`, it should have many. This suggests conventions should be oriented around prefixes or suffixes rather than specific words.
* *synonyms:* synonyms are easy to express, e.g. using `[{%word}][]`, so there is no need for naming conventions to be sophisticated. If one definition fulfills multiple roles, we can easily define multiple synonyms.
* *shallow:* deep hierarchies become [walled gardens](http://en.wikipedia.org/wiki/Closed_platform) that are difficult to explore or access for reuse, and are overkill for disambiguation. So, keep it shallow where feasible.

AO doesn't support namespaces directly. Every word must be fully written in those bytecode tokens. However, structure editors may hide common prefixes or suffixes to prevent large names from becoming noise. Use of color is also viable, to help developers visually distinguish the origins of words.

Old Content
============

## IDE

There is no Integrated Development Environment (IDE) for AO at the moment, but I believe a specialized IDE could greatly mitigate AO's weaknesses:

* style and color should replace rendering of frequent prefixes or suffixes
* automatic word completion, fuzzy search sensitive to type and context
* hyperlinking or zooming to quickly access definitions and documentation
* automatic visualization of values or types, reducing mental burden
* graphical manipulation of rendered structures to simplify data shuffling
* automatic animation highlighting changes in structure across words in def
* automated refactoring support; discover similar code in other projects
* live programming; continuous feedback of value over change in definition
* development-time program search for data shuffling and other glue code

I envision such an IDE being developed as a wiki. AO's word-based module/function structure is very amenable to wiki based development. In the absence of such an IDE, AO has a steep learning curve and is not very human-friendly. I believe the quality of the AO programming experience will depend heavily upon available tools. 

## Composition is First Principle

Composition is a primary principle of Awelon project. Composition means we can combine values using a small set of simple, uniform operators, with algebraic closure and other nice compositional (i.e. invariant or inductive) properties. Awelon bytecode and AO, however, only offer composition at the low level. To achieve composition at higher layers, developers must favor modeling problem domains and solution elements with value types that are themselves compositional. Some compositional types include:

* documents
* diagrams
* geometries
* relations/tables
* matrices
* grammars
* constraint models
* rulebooks
* scene-graphs
* streams
* widgets

In addition, we can have compositional mechanisms to surgically access and manipulate deep structure of a compositional model, such as [lenses](http://www.cis.upenn.edu/~bcpierce/papers/lenses-etapsslides.pdf) or [zippers](http://en.wikibooks.org/wiki/Haskell/Zippers).

There are many non-compositional models that are common in mainstream programming, such as records and ad-hoc objects. Even lists are at best weakly compositional (combining lists is too expensive). Developers can model these objects in AO, but it isn't recommended. Even if it initially seems a little awkward, moderately inefficient, or distorted in-the-small, finding ways to express problems and solutions compositionally is usually very rewarding in the long run and in-the-large.
