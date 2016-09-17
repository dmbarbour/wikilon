# Awelon Object (AO)

Awelon Object (AO) isn't an independent language, rather it is a linking, distribution, and security model for [Awelon Bytecode (ABC)](AboutABC.md). AO defines tokens of the form `{%word}` and `{%word@dict}` and how they interact with dictionary objects. Dictionaries are the basic unit of linking and sharing in AO. 

A dictionary consists of a set of words, with a definition for each word. The definitions are encoded in ABC, but may use `{%word}` tokens to link another word defined in the same dictionary, or `{%word@source}` to link a word as defined in the dictionary specified by a set of `source.attribute` words. Semantically, linking a word means inlining its definition. Dependencies between definitions must form a directed acyclic graph, such that inlining all the things will have a finite expansion. (ABC loops can be represented by anonymous fixpoint combinators.)

These dictionaries enable efficient sharing of objects and code. Very importantly, dictionaries also carry ad-hoc associative metadata. For example, given `word@source` a system may look up `word.doc@source` and `word.type@source`, or even `word.author@source` or `word.example@source`. This associative metadata enables an environment to render and explain code effectively, and may help humans comprehend it.

## Dictionary Representation

AO defines a standard **.ao** file format, suitable for import and export. 

A trivial example dictionary:

        @swap []ba
        @swapd [{%swap}]a
        @swapd.doc "[C][B][A] -- [B][C][A]
         swap just under the top stack element
         assuming typical (stack*ext) environment
        ~

Each definition has the form `@word definition`, starting at the beginning of a line. It's a simple SP (32) between the word and definition. A definition continues until the end of a file or until the next definition starts. There is no risk of ambiguity: ABC does not and will not use `@` for anything.

Use file suffix **.ao**, or `text/vnd.org.awelon.aodict` in context of an HTTP transfer.

## Editing AO

Dictionaries are optimally manipulated through projectional editor systems. The Forth-like [claw](CommandLine.md) view is suitable for a minimal text-only input, and is a fair bit more legible than AO. But in general, only projectional editors can make effective use of [Awelon's application models](ApplicationModel.md). 

An interesting possibility for filesystem integration is to use a *FUSE* (Filesystem in Userspace) view, perhaps operating via the web service. If done properly, this could simplify integration with emacs, vim, and other nice text editors.

Editing a **.ao** file by hand, or even a set of linked files, is feasible. But it's also a chore. Outside of early development (e.g. bootstrapping), I wouldn't recommend it to anyone.

## Linking AO

A `{%word}` token links within the current dictionary. To link between dictionaries, we use `{%word@source}`, and we must also define `{%source}` within the current dictionary. For example:

        @source "file" "foo.ao"

A source is defined by a `"Tag" [Value]` pair, which may be computed. The tag will be a short text that tells our linker how to interpret the value. The value will frequently just be another text - e.g. a URL, filename, secure hash. Structured values are possible, and we may want them for multi-homing, security models, composition, or computation. The interpretation is ultimately ad-hoc, subject to extension and de-facto standardization. 

*Aside:* Computing sources introduces some interesting possibilities like switch-driven dependencies. Modeling sources via words also simplifies hand-switching.

### Dictionary Inheritance

AO supports inheritance by recognizing `parent` as a special link source.

        @parent "local" "d/foo"

When a `{%word}` is not defined locally, the linker may search the parent. However, the parent's words are treated as if they were defined locally, such that any local definitions will override those in the parent. If a word is not defined in the parent, search may proceed to the grandparent and much deeper ancestors. 

*Aside:* While this may seem limited to single inheritance, that really just depends on how our 'parent' source is defined. If a linker provides means to describe a composition of sources, then multiple inheritance immediately becomes possible. It might even be useful, e.g. model dictionary-level genetic programming via pseudo-randomized composition of sources.

### Link Layer Metaprogramming

It is feasible to leverage the AO link layer for flexible metaprogramming. We only need to introduce a source model that *computes* a dictionary as a first-class ABC value rather than *links* an external resource.

### Immutable or Monotonic Dictionaries

Linking of mutable things creates a lot of challenges for synchronization, update propagation, caching, etc.. So, whenever possible, we should favor linking of immutable dictionaries (e.g. identified by secure hash), or monotonic dictionaries (where we add new definitions, maybe GC old ones, but never reuse a name).

With immutable dictionaries, dictionary inheritance provides a relatively simple and effective means to logically 'edit' dictionaries.

## Static and Dynamic Linking



Because ABC is pure and uses local rewriting based evaluation, the worst that happens when linking fails is that a `{%word@source}` remains unevaluated, and we'll need to track link scopes at runtime.

## Constraints on Words and Definitions

A word is a

Words are minimally constrained to be relatively friendly in context of URLs, English text delimiters, HTML, [claw code](CommandLine.md), and AO dictionaries/streams. Tokens are constrained for purity, portability, and easy processing. Texts are constrained to avoid conversion errors (e.g. HTML CRLF conversions, or UTF-8 vs. UTF-16). 

Summary of constraints:

* words are limited as follows:
 * ASCII if alphabetical, numeral, or in -._~!'*+:
 * UTF-8 excepting C1, surrogates, replacement char
 * must not start with numeral-like regex `[+-.]*[0-9]` 
 * must not start or end with . or : (period or colon)
 * no empty or enormous words, 1..30 bytes UTF-8.
* tokens are limited to:
 * word dependencies (`{%dupd}{%swap}`)
 * value sealing (`{:foo} {.foo}`)
 * annotations (`{&seq}{&jit}`)
 * gates for active debugging (`{@foo}`)
 * gates, seals, annotations are words (modulo prefix)
* texts are limited to:
 * exclude C0 (except LF), DEL, C1
 * exclude surrogate codepoints U+D800..U+DFFF
 * exclude replacement char U+FFFD

Words may be further limited in context of a given system or application model, e.g. unicode normalization and case folding to reduce ambiguity, and forbidding unicode spaces and separators. However, I'd prefer to avoid more sophisticated rules at this layer.


## Development Idioms

### DVCS Based Distribution

PLs today widely use package based distribution models. A package of code includes data types and functions, and may depend on other packages of code, all independently developed and versioned. Package based distribution introduces many complications: version conflicts, configuration management, external configuration languages. These complications hinder efforts to refactor, abstract, test, live-code, model application state in code, etc. across multiple packages. 

AO favors a far more simple and robust technique: a dictionary contains all dependencies, all words defined. DVCS-based forks and merges are readily applied: developers can fork public dictionaries, update as needed, validate changes, and cherry-pick functions or updates to push back upstream atomically. Instead of packaged libraries, every dictionary is a wholistic ecosystem. 

*Aside:* An interesting possibility is genetic programming. Words serve as genes, definitions as alleles. Fitness can be based on typechecking, unit tests, and score annotations within each dictionary. Take a population of dictionaries and use word-level random merges (and rare mutations) to model sexual recombination. This would be expensive, but reasonably straightforward.

### Leveraging Undefined Words

A dictionary in a transitory state of development will frequently have a few undefined words. These can serve a useful role in the development context: a development environment can recognize undefined words as 'holes'. The type for a hole may be inferred from usage. A good developoment environment should help developers find implementations of this type. The number of undefined words should generally be limited based on the amount of active development, and only certain approaches to development will use them (e.g. top-down).

### Regarding Large Definitions

Definitions can potentially grow very large, especially when containing embedded texts or with dictionary applications. However, huge definitions are not recommended, as they may hinder incremental computation, reuse, memory management, and developer comprehension.

A very large text might be better broken into smaller texts - e.g. per chapter, paragraph, or other meaningful fragment. A large binary modeled via text might better be divided into 'pages', such that persistent structure and edits can be modeled can reusing most pages. Sophisticated definitions consisting of many components (e.g. more than ten to twenty elements) might be better factored into smaller fragments that can be documented and understood incrementally.

At the moment, I'm not suggesting hard caps for definition sizes. But soft caps - warnings, quota limits - may serve to discourage oversized definitions. 
