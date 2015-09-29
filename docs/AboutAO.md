# Awelon Object Language (AO)

Awelon Object (AO) language directly uses [Awelon Bytecode (ABC)](AboutABC.md) as a foundation for functional programming. 

AO is very simple. The primary structure is a dictionary: a set of (word,definition) pairs. Definitions are encoded directly in bytecode, using `{%word}` tokens to indicate acyclic dependencies on other words. The formal semantics of a `{%word}` token is trivially to inline the definition of the identified word (though an implementation of AO is free to use more conventional linking). 

Human developers operate on high level editable views of the bytecode. For example, the code `2/3 mul` might correspond to the much larger bytecode `#2{%integer}#3{%integer}{%ratio}{%mul}`. The [simplest views](CommandLine.md) will provide a Forth-like programming experience. But this technique is readily extensible to support structured programming (e.g. while-do, if-then-else) and more (e.g. color pickers, sliders, canvases, tables, matrices). 

AO dictionaries, like filesystems, have potential to grow into vast ecosystems. Wikis, forums, REPL sessions, documents, interactive fictions, spreadsheets, and other applications can be cleverly encoded in a dictionary. See below for specific examples. Of course, unlike filesystems, AO dictionaries benefit from standard semantics, types, abstraction, refactoring.

AO defines a standard import/export **.ao** file format.

## AO Dictionaries

An AO dictionary is a set of (word,definition) pairs. 

A healthy dictionary has the following characteristics:

* all words are defined 
* dependencies are acyclic
* definitions are well typed

Though 'well typed' isn't fully defined for Awelon Bytecode because I don't want to limit use of dependent types, abstract interpretation, termination or totality analysis, linters, and so on. But if a dictionary obviously has some problems, according to whatever analyses we perform, we should hear about it.

### Import/Export

A dictionary can be encoded as one large, flat file. An example file:

        @swap rwrwzwlwl
        @swapd rw {%swap} wl
        @swapd.doc "x y z -- y x z
         swap just under the top stack element
         assuming typical (stack*ext) environment
        ~

This is format suitable for simple text files and streams. Each word definition starts at `@` at the beginning of a line, followed by the word, followed by whitespace (SP or LF) then the definition in ABC. This is unambiguous: ABC never includes `@` at the beginning of a line. Whitespace around the definition is trimmed. 

There are two additional structural constraints:

* well defined words are listed before use
* words are listed no more than once

These constraints are to simplify stream-processing of a large dictionary, e.g. compiling or typechecking as we go. When we encounter an unlisted word, we know that the word is not well defined (i.e. it's either undefined or part of a cycle). Because words are listed at most once, we don't need to deal with edits and recomputation.

Use file suffix **.ao**, or `text/vnd.org.awelon.aodict` in context of an HTTP transfer. 

### DVCS Based Distribution

PLs today widely use package based distribution models. A package of code includes data types and functions, and may depend on other packages of code, all independently developed and versioned. Package based distribution introduces many complications: version conflicts, configuration management, external configuration languages. This also makes for difficult refactoring, e.g. you cannot rename a function exported from a package without modifying a non-local set of external packages. 

AO favors a simple and robust technique: a dictionary contains all dependencies, all words defined. DVCS-based forks and merges are readily applied: developers can fork public dictionaries, update as needed, validate changes, and cherry-pick functions or updates to push back upstream atomically. Instead of package versions, we always have wholistic ecosystems.

*Aside:* An interesting possibility is genetic programming. Words can serve as genes, definitions as alleles. Based on random word-level merges and swaps, we can get something akin to sexual recombination. This is feasible because dictionaries contain all the relevant code... much like DNA.

### Transitory Undefined Words

A dictionary in a transitory state of development will frequently have a few undefined words. These can serve a useful role in the development context: a development environment can recognize these as 'holes'. The types of a hole can be inferred from usage contexts and unit tests. The environment could help developers find an implementation. Or, at the very least, the temporary presence of undefined words will simplify top-down development. Developers should always be aware of missing words, though. Undefined words are not for 'import' from an external package.

## Constraints on Words and Definitions

Words are constrained to be friendly in context of URLs, English text delimiters, and HTML. Tokens are constrained for purity and portability. Texts are constrained to avoid conversion errors (e.g. HTML CRLF conversions, or UTF-8 vs. UTF-16). 

Summary of constraints:

* words are limited to:
 * ASCII if alphabetical, numeral, or in -._~!$'*+:@
 * other UTF-8 except for C1, surrogates, replacement char
 * must not start with a digit or +-. followed by a digit
 * must not end with a . (dot or period)
 * no empty words or enormous words. 1..64 bytes UTF-8.
* tokens are limited to:
 * word dependencies (`{%dupd}{%swap}`)
 * discretionary value sealer (`{:foo}`)
 * discretionary value unsealer (`{.foo}`)
 * annotations (`{&static}{&copyable}`)
 * token after prefix must be valid as a word
* texts are limited to:
 * exclude C0 (except LF), DEL, C1
 * exclude surrogate codepoints U+D800..U+DFFF
 * exclude replacement char U+FFFD

## Dictionary Applications

A dictionary application is any application whose state is hosted by a healthy dictionary. Consequently, dictionary application state is easily imported, exported, and refactored to discover and abstract common patterns. Dictionary applications may utilize:

* conventions for names, types, code structure 
* indexes, caches, compilers for efficiency
* external services to provide editable views

A simple forum might be represented using threads with structure:

        @forum:Foo {&forum}(describe and initializes forum)
        @thread:1 {%forum:Foo}(adds thread OP to forum)
        @thread:2 {%thread:1}(adds content to thread)
        ...

A reverse token lookup index would find children of a given thread. We compute and cache intermediate thread type or state after each post. A web server could render and present this content for the user. Well designed dictionary applications will further optimize for potential reuse and refactoring. For example, we might separate post content into separate `post` words, such that every post can be treated as an independent function. 

Conveniently, append-only structures as suggested for forums are also a decent fit for command REPLs (with persistent sessions), iPython-like documents, quests (with managed character sheets and game boards), interactive fictions, etc.. where we don't really want updates to 'propagate' through a dictionary. For spreadsheet-like or wiki-like apps, or stateful object concepts, we might more frequently edit existing words. 

Dictionary applications have potential to be experienced as live coding or live programming, assuming we provide zero-button, low-latency feedback. Use of dictionary apps for live coding is a motivating aspect for Awelon Object's design.
