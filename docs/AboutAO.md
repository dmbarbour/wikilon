# Awelon Object Language (AO)

Awelon Object (AO) language directly uses [Awelon Bytecode (ABC)](AboutABC.md) as a foundation for functional programming. 

AO is very simple. The primary structure is a dictionary: a set of `@word definition` pairs. Definitions are encoded directly in bytecode, using `{%word}` tokens to indicate acyclic dependencies on other words. The formal semantics of a `{%word}` token is trivially to inline the definition of the identified word (though an implementation of AO is free to use more conventional linking). 

Human developers operate on high level editable views of the bytecode. For example, the code `2/3 mul` might correspond to the much larger bytecode `#2{%integer}#3{%integer}{%ratio}{%mul}`. The [simplest views](CommandLine.md) of AO provide a Forth-like programming experience. But this technique is readily extensible to support structured programming (e.g. while-do, if-then-else) and more (e.g. color pickers, sliders, canvases, tables, matrices).

AO dictionaries, like filesystems, have potential to grow into vast ecosystems. Wikis, forums, REPL sessions, documents, interactive fictions, spreadsheets, and other applications can be cleverly encoded in a dictionary. See below for specific examples. 


AO also defines a standard import/export **.ao** file format.

## AO Dictionaries

An AO dictionary is an associative array of words to definitions.

A healthy dictionary has the following characteristics:

* all words are defined 
* dependencies are acyclic
* definitions are well typed

Though 'well typed' isn't fully defined for Awelon Bytecode because I don't want to limit use of dependent types, abstract interpretation, termination or totality analysis, linters, and so on. But if a dictionary obviously has some problems, according to whatever analyses we perform, we should hear about it.

### Transitory Undefined Words

A dictionary in a transitory state of development will frequently have a few undefined words. These can serve a useful role in the development context: a development environment can recognize these as 'holes'. The types of a hole can be inferred from usage contexts and unit tests. The environment could help developers find an implementation. Or, at the very least, the temporary presence of undefined words will simplify top-down development. 

Developers should always be aware of missing words, though, e.g. via listing them in warnings. Undefined words are not for 'import' from an external package.

### Dictionary Applications


A "dictionary application" encodes all essential state in the dictionary. I hope to see dictionaries with gigabytes of content. Unlike filesystems, AO dictionaries benefit from standardized semantics, abstraction, refactoring.


For example, a forum or REPL session might model threads as linked lists of `@thread:childId {%thread:parentId}{%post:childId}` definitions, up to some annotated roots. And a spreadsheet might use words like `foo$A1` to indicate a particular cell's definition.

A 'dictionary application' is an application that encodes all its essential state in terms of editable views of the dictionary, perhaps with specialized indexing and caching.



## Words and Definitions

AO definitions use a subset of ABC. Tokens are constrained for purity and portability. Texts are constrained to avoid conversion errors (e.g. across HTML forms with the whacky CRLF conversions, or between UTF-8 and UTF-16). Words are also constrained to be friendly in context of URLs, English text delimiters, and HTML.

Summary of constraints:

* words are limited to:
 * ASCII if alphabetical, numeral, or in -._~!$'*+=:@
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

## AO Dictionary Import/Export

A dictionary can be encoded as one large, flat file. An example file:

        @swap rwrwzwlwl
        @swapd rw {%swap} wl
        @swapd.doc "x y z -- y x z
         swap just under the top stack element
         assuming typical (stack*ext) environment
        ~

This is a flat format suitable for simple text files and streams. Each word definition starts at `@` at the beginning of a line, followed by the word (up to an SP or LF, which is dropped) then followed by the definition in ABC. This is unambiguous: ABC never includes `@` at the beginning of a line.

There are two additional structural constraints:

* well defined words are listed before use
* words are listed no more than once

These constraints are to simplify stream-processing of a large dictionary, e.g. compiling or typechecking as we go. When we encounter an unlisted word, we know that the word is not well defined (i.e. it's either undefined or part of a cycle). Because words are listed at most once, we don't need to deal with edits and recomputation.

Use file suffix **.ao**, or `text/vnd.org.awelon.aodict` in context of an HTTP transfer. 
