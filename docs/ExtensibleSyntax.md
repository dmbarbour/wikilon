
I love what [embedded literal objects](EmbeddedLiteralObjects.md) aim to accomplish. 

But embedded literals lack *stable identity*, i.e. I'm forced to refer to them in awkward terms such as "the third literal number in function foo". I might get away with this for local edits by a single user. But, in Wikilon, where we need to serialize updates between between client and server and potentially to multiple cooperating users, the lack of convenient identity is a bigger problem than I want to suffer. 

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

This particular format might support multi-line content by escaping each LF with a following SP, and could be extended with new operations via the first character of any line. Anyhow, presumably a system would start with comprehension of at least one bootstrap language (perhaps just AO, perhaps just ABC) then allow new parser functions to be defined. 

The role of a parser function would be to take text input and generate a function. We don't need an AST. We can just skip straight to composing a first-class function. We do need some ability to take a word like "swap" and get back the ABC block `[rwrwzwlwl]`. Acquiring a definition given a word might be modeled as a monadic effect. This has many nice properties:

* easily track dependencies between words (unlike dictionary passing)
* use words so obtained at parse time (unlike generating AST or AO code)
* purely functional parse and result (unlike capability-based effects)
* extensible with other effects, e.g. region annotations for error reports
* easy composition of other language parsers, i.e. create language with macros

Okay, so extensible syntax can potentially be nice, but it's still text-oriented. Getting back to my original point, I still want the interactive graphical models associated with embedded literal objects. I believe this can be accomplished as a simple extension of the 'extensible syntax' concept. In addition to impelementing a parser, the language could implement other utilities such as rendering to an interactive UI.

Thus, a language function must perhaps be extensible with multiple 'methods', or alternatively generate a structure that has multiple functions (a parser and other stuff). I haven't hammered out the fine details yet, such as a suitable type for the language words. But the broad idea seems solid: a language doesn't define just a syntax and parser, it defines a larger user experience. In this case, that user experience is confined to the boundary of one module, since Awelon project specifies the larger architecture.

Using language functions, we might be able to edit graphical data and objects, but also peek under the hood at any time to the serialized representations. And, besides stable identity from coupling to the words layer, this has some other advantages over embedded literal objects: in particular, the language may be upgraded independently of the modules that use it.

I think this is a very promising direction for Wikilon.

