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

*Syntactically, AO definitions use a subset of ABC.* 

An unmodified, basic ABC parser can read AO definitions. But not all of ABC is valid for AO. The set of tokens is constrained to word dependencies, annotations, and discretionary sealers/unsealers. Words and tokens and embedded texts are further constained. See below.

Any word in an AO dictionary may be 'compiled' to independent ABC by transitively inlining the definition for each `{%word}` token and following it by `$vr$c` to compile then inline its functional meaning at runtime. However, we'll want to apply a lot of optimizations, such as precompiling and optimizing each word incrementally.

The weakness of AO is that these definitions are not very readable in raw form. A structure editor is essential to provide a good programming experience.

## AO Dictionaries

An AO dictionary is an associative array of words to definitions.

A healthy dictionary has the following characteristics:

* all words are defined 
* dependencies are acyclic
* all definitions compile
* definitions pass checks

Checks may include linters, static typechecks, automatic testing, termination analysis, abstract interpretation, and validation of properties asserted by annotions (such as structural or behavioral equivalences, commutativity, associativity, type declarations). Some constraints may be context specific, e.g. limiting which tokens, word structures, or texts are permitted (e.g. to simplify interaction with web services and browsers), or requiring certain words have a specific type. When developing AO dictionaries, a suite of automatic checks should be the default so that content remains clean or developers are at least aware of any health issues.

A dictionary has no external dependencies. 

To include work developed by another group involves copying that work into your own dictionary, perhaps with a few simple renames to avoid conflict. This gives developers control, freedom to refactor the code, further develop it, or discard the aspects they don't need. This is very robust for stable snapshots and version management. AO favors DVCS-based mechanisms, e.g. like you see in [github](https://github.com/), for sharing and distributing code - fork, push, pull, pull requests, shared issue tracking. 

AO dictionaries also directly include the textures, game maps, SVG clipart and so on - i.e. there are no external file dependencies. Including data this way is very convenient for distribution and sharing, partial evaluation, forking and versioning, procedural generation, testing, refactoring and abstraction. Further, in context of AO, it is feasible to model the tools for structured editing of this content directly within the dictionaries.

Ultimately, communities may curate dictionaries at scales of many gigabytes of algorithms, information, articles, presentations, games. Gigabytes are cheap, and will only get cheaper. Much of this will be open source and free software (which should be the default), but where necessary we can easily track licensing and copyright information. One goal of Awelon project is to explore development at large scales. Large dictionaries can be a powerful resource to explore and exploit. Cross-project refactorings and integration tests are very simple when they're all together. Words learned can easily be brought into any new project. There are interesting opportunities for variation testing and genetic programming at the whole dictionary level. 

Of course, it's trivial to extract a minimal subset of words and their dependencies.

### Transitory Undefined Words

A dictionary in a transitory state of development have a few undefined or incompletely defined words. These can serve a useful role in the development context: a development environment can recognize these holes, infer their types and behaviors from usage contexts and unit tests, and help developers find an implementation. Or, at the very least, this simplifies top-down development styles where some words are left undefined.

The quantity of undefined words should be commensurate with ongoing development. A dictionary with too many undefined words, or very old undefined words, is still unhealthy.

## AO Dictionary Import/Export

AO doesn't specify how a system should represent dictionaries internally. However, I do define a simple import/export format for sharing dictionaries through a filesystem or HTTP service:

        @swap [rwrwzwlwl][]
        @swapd [rw {%swap} wl][]
        @swapd.doc "x y z -- y x z
         swap just under the top stack element
         assuming typical (stack*ext) environment
        ~[v'c]

This is a flat format suitable for simple text files and streams. Each word definition starts at `@` at the beginning of a line, followed by the word (up to an SP or LF, which is dropped) then followed by the definition in ABC. This is unambiguous due to the design of ABC. 

There are two additional structural constraints:

* words are listed before use
* words are listed only once

These constraints guard against cycles and simplify efficient processing of the dictionary. We can process and compile each word in this file as we encounter it. Undefined words must still be listed before use, but may trivially be represented by use of an empty definition. (Thus, we can import or export dictionaries that are in the middle of development.)

For HTTP, I'm using the following header and Internet media type:

        Content-Type: text/vnd.org.awelon.aodict

I think every Awelon project system will probably want to support this simple aodict format together with ABC. This will be the primary import/export format for dictionaries, and also offers a simple representation to compress large ABC programs.

## Subset of ABC

For reasons that should be obvious, a word cannot contain curly braces, spaces, and line feeds. But AO further restricts words to nicely fit URLs, HTML, command line interfaces, English documentation. Currently the heuristics for valid words are:

* ASCII if alphabetical, numeral, or in -._~!$'*+=:@
* UTF8 except for C1, surrogates, replacement char
* must not start with a digit or +-. followed by a digit
* must not end with a . (dot or period)
* no empty words or enormous words. 1..64 bytes UTF-8.

The other permitted tokens - annotations, and discretionary sealers or unsealers - must also be valid words after the prefix. This simplifies abstraction of them and similar interaction with texts and other contexts. If developers want spaces in words as an alternative to hyphens, they are permitted to use the non-breaking space (U+00A0).

Embedded texts are also constrained. Valid AO texts cannot contain control characters (C0, DEL, or C1) except for LF. AO texts cannot use surrogate codepoints, and cannot use the replacement character. To contain LF, it is followed by SP as normal for ABC. Constraining texts simplifies interaction with HTML forms (e.g. no need to worry about whether CRLF in text is intentional) and helps ensure meaning is more visible. 

Binary data should use ABC base16, i.e. base16 using alphabet `bdfghjkmnpqstxyz` to remain visible (this is easily compressed). While binary isn't encouraged for use in dictionaries (it's very opaque), it might occasionally be useful for converting static image or sound data (since AO doesn't allow external file resources).

## AO Dictionary Applications

A **dictionary application** is an application that has no dependencies outside of an dictionary, and whose only state consists of reflectively viewing or updating the same dictionary. 

A dictionary application might be understood as a (potentially mutable) view of the dictionary. While dictionary applications must ultimately be bootstrapped by services outside a dictionary, it is possible and preferable that the majority of dictionary app code be expressed within the same dictionary.

Updates to the dictionary must use dataflows similar to spreadsheets. Spreadsheets, iPython notebooks, and web applications are viable inspirations for developing dictionary applications.

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

Developing AO code in a plain text editor is possible but will never be comfortable. AO requires a structure editor to shine. Every definition has the intermediate structure `v` and a compiler `[v→[a→b]]`. But it is difficult to see or manipulate the structure of `v` when it is represented in plain text bytecode, unless it's a very simple structure.

AO does not support namespaces directly. However, as dictionaries scale, keeping rendered names short will be appreciated by developers (long names are noisy). The current idea is to push this logic into the interactive editor, e.g. rendering a short word but using a different style (e.g. a color or a prefix/suffix icon) based on the hidden parts of the name. A simple legend could be rendered in the corner of the screen, or when focused on the word.

The purely functional nature of AO requires explicit modeling for effects. At the moment, the favored model is [abstract machines on an abstract network](NetworkModel.md), i.e. where in each step a machine's function receives a message and the current state then generates a list of output messages and an updated state. This machine model and network is easily implemented using pure functions, thus enabling whole networks to be modeled for integration tests. However, effects might instead be modeled monadically for some subprograms.

Words in AO are acyclic. Name-based recursion is not possible. This greatly simplifies compilation, since we can always inline every word. But it does complicate loops, which require fixpoint combinators to express. Most developers find fixpoint combinators mind-boggling, even after using them for a while. Though, as many higher level loop combinators are developed, this becomes much less an issue.

As data types, DSLs, and structure editors are implemented, casual development should become much easier. However, it will take a while to get there. 
 
## AO Naming Conventions

The naming of words in an AO dictionary has no impact on the intrinsic, purely functional meaning of a word. However, names certainly have connotations and conventions within a community. Conventions will be established, such as: `foo.doc` provides human-meaningful documentation for the word `foo`. And these conventions will solidify as we integrate them into development environments, e.g. if we automatically link related words and documentation.

I'd prefer to keep names of things relatively shallow. However, we can make use of redirects, e.g. the definition `[{%foo}][]` essentially redirects to the definition of foo. Also, plurality should be the default, e.g. rather than naming the root word for an application `main` (thereby limiting a dictionary to one app), we name it after the project.

AO doesn't support namespaces directly. Every word must be fully written in those bytecode tokens. However, structure editors may hide common prefixes or suffixes to prevent large names from becoming noise. Use of color is also viable, to help developers visually distinguish the origins of words.

## Conversion to ABC Resources

ABC resources already follow AO's definition structure; the difference is the use of version-specific secure hashes to identify resources instead of human-meaningful tokens. To naively convert an AO definition of word to an ABC resource:

* convert dependencies; every `{%bar}` dependency becomes `{#barResourceId}`
* prepend definition of word with an origin annotation, e.g. `{&@foo}`.
* resourceId is SHA3-256 of bytecode, base16 alphabet `bdfghjkmnpqstxyz`

The injected `{&@foo}` annotations are to simplify round-trip conversion, debugging, claw-code views, etc.. In case of incomplete definitions, they also serve a useful role in naming the holes indicated by `e`. However, this annotation could easily be dropped in a more sophisticated conversion, e.g. one involving optimization or partial evaluation.

Conversion will diverge or fail for an unhealthy dictionary having cyclic definitions.

## Historical Note

The name 'Awelon Object' was initially applied to a Forth-like language suitable for direct keyboard input by humans. Even then AO was a very thin linker and macro layer above Awelon Bytecode with the same dictionary concept (albeit, without support for structured definitions). However, the current form of AO is a better fit for the historical connotations of 'object code'.

There is still a valuable role for an efficient Forth-like language, e.g. for input into a console or command line. I'm still dreaming up a good name for this, but do see the [command line](CommandLine.md) documentation.

Old Content 
============

(for the old Forth-like AO variant)

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
