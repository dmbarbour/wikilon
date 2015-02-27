
# Background

I love what [embedded literal objects](EmbeddedLiteralObjects.md) aim to accomplish. 

But embedded literals lack *stable identity*, i.e. I'm forced to refer to them in awkward terms such as "the third literal number in function foo". I might get away with this for local edits by a single user. But, in Wikilon, where we need to serialize updates between between client and server and potentially to multiple cooperating users, the lack of convenient identity is a bigger problem than I want to suffer. 

## Extensible Syntax

So, let's consider an alternative.

Back around 2007, I determined useful constraints for extensible syntax: 

* syntax should be specified in very little space
* syntax should be opaque across module boundaries

The first point reduces boiler-plate and abstraction issues that arise if syntax is not cheaply indicated. The second point prevents accidental coupling between modules to the syntax or text with which they're described, and reduces friction between extensible syntax and refactoring.

The coupling of syntax to the module boundary is a good fit for the *stable identity* requirement. In case of Wikilon, a module is a word/page. The resulting syntax can be straightforward to use. In concrete terms, we might export a set or stream of definitions using something like:
        
        #ABC
        :swap rwrwzwlwl
        :.rw rw
        :.wl wl
        :dup r^zlwl
        #AO
        :swapd .rw swap .wl
        :rot swapd swap

This particular format might support multi-line content by escaping each LF with a following SP, and could be extended with new operations via the first character of any line. Anyhow, presumably a system would start with comprehension of at least one bootstrap language (perhaps just AO) then allow new parser functions to be defined by compiling to that form. But this isn't the final form!

## Structured Definitions 

The prior section for Extensible Syntax is still missing a lot of desirable features. For example:

* no generic means to recognize, rename, or refactor words
* it is non-trivial to develop generic editors and rendering
* cannot generically refactor and compress large definitions
* not a close fit to Awelon project's code-as-material metaphor

*We can do better!*

Instead of an extensible syntax at the character level, we represent every word as a pair: a *structured value* and a *compiler function*. The compiler function is simply a pure function that takes the structured value and returns a block. Trivially, the simplest compiler function is *identity*, if the structure itself is a block. The structured value is represented in ABC, but with access to the dictionary via `{%foo}` tokens. The result, for simple code, is very similar to the (now deprecated!) Awelon Object (AO) code:

        #{&AO}
        :swap [rwrwzwlwl]
        :dup [r^zlwl]
        :swapd [rw {%swap} wl]
        :rot [{%swapd} {%swap}]
        :dupd [rw {%dup} wl]
        :over [{%dupd} {%swap}]

These tokens are noisy and ugly. Numbers and short text literals will be even uglier! Fortunately, in a structured editor, it should be entirely feasible to present this ABC code in a pretty format for reading and editing, including basic texts and numbers. Usefully, we have conflated words and blocks from the perspective of our compilers and structured editors. Words don't need special attention.

Now we have a lot of freedom for other languages. Arbitrary values may be built from numbers, products, sums, text, etc.. Structured editors may then render this structure and edit it. Functions to manipulate structure are just plain functions - i.e. macros are easily modeled within the dictionary. It is feasible to model *streams of user input* operating on an ad-hoc structure that defines a word, thus bringing me closer to my Awelon project goals.

Very large dictionary words then benefit from ABC refectoring, compression, and structure sharing techniques, allowing those same tools to be applied to both the dictionary and any virtual machines.

## Interactive Editors

Okay, we have extensible structured-syntax, and potential for very-large lazily loaded values (if we pursue them). The question, then, is how to create nice interactive applets above the dictionary, e.g. for level editors and image canvases and spreadsheets and math notations and so on. The structure editor design does not permit associating UX with languages, but does allow associating it with *structure*, which should be even better. We can easily leverage `{:fooType}` discretionary sealers in a conventional manner (together with a little reflection) to support interactive structured editing!

## Uniform Support for Ambiguity

Usefully, this ABC content is easily and uniformly extensible with the `(|)` characters for ambiguous choice. Ambiguity is feasible both in both the definition structure and within any opaque blocks. I think we can go really far with this without requiring any special attentions. 

