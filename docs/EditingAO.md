
# Editable Views for AO

[Awelon Object (AO)](AboutAO.md) code can be edited by humans, but is not optimized for that use. AO is still a bytecode, designed to be unambiguous and easy to process. Instead, the expectation is that humans will manipulate AO through an editable view - an efficient and aesthetic presentation of code that may be edited in place, where the edits can be translated back into bytecode. 

This idea is close in nature to [projectional editing](http://martinfowler.com/bliki/ProjectionalEditing.html), with an AO dictionary as the storage representation. Ideally, the same dictionary would support user-defined projections and editing utilities.

I'll start with a concrete editable view - claw - and generalize from there. 

## Command Language for Awelon (Claw)

Claw is a Forth-like editable view of AO optimized for short one-liner programs or REPL-like scenarios. Claw focuses on lightweight support for numbers and small texts. Like Forth, Claw doesn't scale nicely beyond a dozen tokens (words, atoms, numbers, small texts). But we can construct larger applications from smaller words.

Claw optimizes for:

* AO dictionary words `foo swap bar`
* number data `42 -7 2/3 3.141 2.998e8`
* inline texts `"hello, world!"`

Additionally, claw supports `[blocks]`, `{tokens}`, and command sequences `(foo,bar,baz)`. 

Claw is universal in the sense that *any* ABC code may be viewed and edited through Claw. Words that would be ambiguous with Claw features are simply presented as tokens - e.g. `{%42}`. However, Claw is most useful for editing code developed through the Claw view because it injects some extra words to deal with construction of numbers and text postprocessing.

# Command Language for Awelon (claw)

Claw is a lightweight, editable view for [Awelon Object (AO) code](AboutAO.md), which in turn uses a subset of [Awelon Bytecode (ABC)](ABC.md). The main purpose of Claw is to support easy use of [dictionary based REPLs and shells](ApplicationModel.md) - expression of 'one line' programs that still do something useful. However, Claw will also double as a primary program format until visual variants are available.

The desiderata for claw is efficient, concise, aesthetic entry of:

* dictionary words `swap inc mul`
* number data `42 -7 2/3 3.141`
* short inline texts `"hello, world!"`
* structured programs and data

To optimize in these areas, claw requires escapes for multi-line texts, bytecode, and arbitrary tokens. Additionally, while claw can be used to edit and view arbitrary ABC, anything other than code desugared from claw will be a mess of escapes.

## Words

A word is the default parse. For example, `swap` will be understood as `{%swap}`. Avoiding the `{%}` for every word does a surprising amount to make claw code more legible and easier to use than raw AO.

## Claw Numbers

Awelon Bytecode has built in support for *natural* numbers, e.g. `#42`. But I assume we'll want to model (and accelerate, eventually) a variety of numbers: negative integers, decimal numbers, ratios. Toward this end, I use the following rules:

        42          \#42 int
        -27         27 negate
        2/3         2 3 ratio
        -4/5        4 5 ratio 
        3.141       3141 3 decimal
        -2.7        -27 1 decimal
        3.0e6       3.0 6 exp10

To express natural numbers directly, rather than naturals wrapped as integers, requires the escaped form `\#42`.

*Aside:* If there is sufficient demand, I may introduce specialized rendering options, like `0x3f` → `\#63 hex`.

## Claw Literals 

Claw optimizes for inline literals:

        "hello, world"          "
                                 hello, world
                                ~ lit

Inline literals are restricted to exclude LF (10) or double-quote (34). Ideally, they should be relatively short - larger literals should be represented by separate words. Inline literals desugar into multi-line literals followed by the word `lit` to permit character conversions. 

Multi-line literals are prefixed by `"` and an initial empty line. Lines with text must be indented by an SP character. (Empty lines don't need to be indented.)

        some commands "
         This is an example multi-line
         literal with "double quotes".
        ~ more commands

Other than the slight tweak to the prefix, Claw multi-line literals have the same structure as ABC literals.

### Claw Command Sequences

A lightweight syntax for sequences provides an effective basis for structured programs and data. They might be used for monadic code, DSLs, streams, effects models. Or just to construct a list. Claw will use a general sugar that can be used with any sequence semantics:

        (foo,bar,baz)       [[foo] (bar,baz) seq]
        (baz)               [[baz] eseq seq]

Note that `eseq` is our empty sequence, and that `()` is a sequence containing a single identity command. 

*Notes:* An alternative I've contemplated is `[foo,bar,baz]` as a representation of `[[bar,baz] after foo]`, in a continuation passing style. However, this locks us into a specific semantics. And use of 

## Embedding Bytecode

Tokens are represented directly in Claw, e.g. `{%foo}` in Claw will be represented directly by `{%foo}` in the bytecode. AO words desugar to the appropriate token, so `foo` desugars to `{%foo}` (assuming the empty namespace). Blocks are represented directly but contain claw code rather than raw bytecode. Thus, `[foo] bar` will desugar to `[{%foo}]{%bar}`.

ABC bytecodes don't need any special effort: the words `a` `b` `c` and `d` simply refer to our four primitive bytecodes. In Claw, these will be expanded into one word per opcode.

Whitespace formatting bytecode (LF or SP) may be preserved by following each LF or SP in claw with an extra SP. Use of whitespace can be used to force a separation between claw views.

### Claw Comments - Rejected

Claw could easily support inline C-style comments. A simple pattern of introducing a text then dropping it would be sufficient. However, it isn't clear to me that we *should* do so. 

        // this is a line comment

            could (but does not) desugar to
            
        "this is a line comment
        ~d

The problems with inline comments are multifarious. Inline comments bulk up code that would be better explained, understood, and documented by factoring it into smaller components. Inline comments are second class, difficult to access or abstract. As documentation, they're weak because we cannot integrate graphical or interactive examples or flexible type setting. 

For these reasons, Claw eschews inline comments in favor of separate documentation attributes, e.g. using words like `foo.doc` to document word `foo`. 

## Future Extensions

### Visual Claw?

Assuming an appropriate editor, I would be interested in recognizing *form*-based presentation and widgets. For example, perhaps `30/100 slider` is recognized and rendered as a slider widget. Graphs, diagrams, canvases, tables, trees, checkboxes, selection lists, radio buttons, and a multitude of other stateful widgets could feasibly be integrated and rendered.

However, it might be wiser to shift this up to the [dictionary level](ApplicationModel.md), e.g. rendering and direct manipulation of dictionary 'objects'. This would enable shared state and structure, flexible integration of eventful widgets via command patterns, and richer rendering attributes.

In either case, techniques such as 'cloning' a subprogram and editing it could provide a basis for form input, and structured editing would provide a basis for structured programming. 
