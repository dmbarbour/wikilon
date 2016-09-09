
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

## Numbers

Awelon Bytecode has built in support for *natural* numbers, e.g. `#42`. But I assume we'll want to model (and accelerate, eventually) a variety of numbers: negative integers, decimal numbers, ratios. Toward this end, I use the following rules:

        42          \#42 int
        -27         27 negate
        2/3         2 3 ratio
        -4/5        4 5 ratio 
        3.141       3141 3 decimal
        -2.7        -27 1 decimal
        3.0e6       3.0 6 exp10

To express natural numbers directly, rather than naturals as integers, requires the escaped form `\#42`. 

## Literals 

Claw optimizes for inline literals:

        "hello, world"          \"
                                 hello, world
                                ~ lit

Inline literals must be valid literals and additionally may not contain LF (10) or the double-quote character (34). They should also be relatively short. The recommendation is to move large literals into their own definitions. Inline literals are followed by the word `lit`, which may perform character conversions.

Multi-line literals can also be expressed. 

        some commands \"
         This is an example multi-line
         literal with "double quotes".
        ~ more commands

We'll escape the literal to make our intentions obvious, then text begins on the next line to guard alignment.

### Claw Sequences

A lightweight syntax for sequences provides an effective basis for structured programs and data. They might be used for monadic code, DSLs, streams, effects models. Or just to construct a list. Because [minimalist ABC](ABC_Minimalist.md) went to some effort to unify command sequences with literals and natural numbers, I may just stick to that model.

        (foo,bar,baz)       [[foo](bar,baz) \s]
        (baz)               [[baz]\# \s]

        [B][A](foo,bar,baz)i    ==  foo [[B][A](bar,baz)i] A
        [B][A]#i                ==  B

Monadic sequences are viable if we assume each action returns another (possibly empty) sequence. Note that `()` is a sequence with one element. The empty sequence is `\#`, same as the zero natural.

*Aside:* I haven't decided whether to also leverage the `{}` curly braces for a sequencing sugar. 

## Bytecode, Tokens, and Blocks

After an escape `\`, we may express a sequence of opcodes (like `\baad`) or a single token `\{foo}`. Claw doesn't optimize for arbitrary tokens, because we can shove that problem up to the dictionary (i.e. create a word just for the token). A special exception for this is gates:

        @foo        \{@foo}

Representation of gates is optimized because, in practice, they aren't something we really want to abstract behind a dictionary word. Instead, we'll add them to code that we're in the process of debugging, then later remove them from that code.

Claw blocks are just blocks containing claw code. For example, `[foo]` in claw will result in `[{%foo}]` in AO. In context of [minimalist ABC](ABC_Minimalist.md), it was deemed that a word `block` merely for placement would just confuse things.

### Claw Comments - Rejected

Claw could easily support inline C-style comments. A simple pattern of introducing a text then dropping it would be sufficient. However, it isn't clear to me that we *should* do so. 

        // this is a line comment

            could (but does not) desugar to
            
        "this is a line comment
        ~d

The problems with inline comments are multifarious. Inline comments bulk up code that would be better explained, understood, and documented by factoring it into smaller components. Inline comments are second class, difficult to access or abstract. As documentation, they're weak because we cannot integrate graphical or interactive examples or flexible type setting. 

For these reasons, Claw eschews inline comments in favor of separate documentation attributes, e.g. using words like `foo.doc` to document word `foo`. 

### Claw Namespaces

Namespaces, used well, lead to concise, readable code. But namespace management risks becoming boiler-plate. And code under a namespace is context-sensitive - i.e. the code cannot as readily be factored because the namespace context must be reconstructed. For claw, I've decided to accept a compromise: a single namespace for a volume of code.

        #foo: -7 bar        (fully desugars to)
        [{&_foo:_}]d#7{%foo:int}{%foo:negate}{%foo:bar}

Our namespace is simply added as a prefix to every word in our volume of code, including any implicit words. When the namespace is expressed within a block of code, that namespace extends only to the end of that block. Otherwise, it extends to the end of the current program.

If a word cannot be expressed within a given namespace (e.g. `foo:9` would be confused with number `9`, and `bar` is not in namespace `foo:`) then we'll use the token expansion instead: `\{%foo:9} \{%bar}`.

## Claw Semantics and Round Tripping

Claw code is a simple syntactic sugar and editable view. The entire semantics of claw code is its unambiguous expansion into AO bytecode. Further, this is expansion reversible: a parser can recover structure and present it to the user for editing. Claw can represent any AO bytecode. However, code not written for the Claw view will tend to be full of ugly escapes. Thus, in practice, Claw only supports round-tripping. 

Claw does not, in general, preserve spacing and newlines. 

## Claw Shell and Effects Model

See [Wikilon's application model](ApplicationModel.md). 

Claw is a good fit for command pattern applications, such as a REPL or shell. Also for iPython-notebook scenarios. Given an appropriate software agent for integrating effects, modeling a Claw 'shell' that reflects on our dictionary or interacts with external services is entirely feasible.

Command sequences make Claw much more effective at these tasks, i.e. we can directly model a series of commands between which we may return to interact with an interpreter or render content.

## Proposals for Future Extensions

### Visual Claw 

Assuming an appropriate editor, I would be interested in recognizing *form*-based presentation and widgets, e.g. `30/100 slider` is recognized and rendered as a slider widget. Graphs, diagrams, canvases, tables, trees, checkboxes, selection lists, radio buttons, and a multitude of other stateful widgets could feasibly be integrated this way. 

If necessary, visual code might be shifted to the dictionary layer, e.g. such that a 'slider' is an embedded rendering of a specific word. This would simplify rendering attributes, update models, security models, aliasing, and [direct manipulation](https://en.wikipedia.org/wiki/Direct_manipulation_interface) of [dictionary applications](ApplicationModel.md). Use of buttons and eventful widgets become viable via use of command pattern. The cost is that I'll need to use more sophisticated 'objects' in the dictionary just to render visually, and a lot more lookups would be performed for rendering.

