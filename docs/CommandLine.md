
# Command Language for Awelon (claw)

Awelon project and Wikilon would benefit from effective command line interfaces, e.g. as a REPL and shell. In context of Wikilon, providing console-like services through HTML forms or JavaScript should not be difficult. The underlying [Awelon Object (AO)](AboutAO.md) bytecode is unsuitable for this application in its raw form.

Desirable features for a command line interface:

* numbers and inline texts `42 2/3 3.141 "foo"`
* trivial access to words, e.g. `swap inc mul`
* escaped access to underlying language `\vrwlc \{&anno}`
* namespace model for multiple environments `#foo: 1 2 3`
* blocks for higher order operations `1 [2 mul] 53 repeat`
* terse encoding of tuples, lists, sequences `{1,2,3}`

Command Language for Awelon (claw) is a syntactic sugar and editable view for AO. Claw semantics is a trivial expansion into AO bytecode, and this expansion is reversible, such that we may view and edit AO bytecode as claw code.

Claw is simple, flexible, and extensible. Though intended primarily for command line interfaces, claw may be leveraged for conventional document interfaces and block-structured programming by recognizing sequences like `[cond] [body] while_do_`. Given a structure editor, claw might further be extended with interactive forms - sliders, color pickers, checkboxes, etc. - e.g. by recognizing sequences like `30 slider` or `255 0 0 rgbcolor`. 

## Claw Code

General points to help guide the design:

* need effective support for rational and decimal numbers
* transparent, simple, predictable, direct AO translation
* easy to preserve claw sesssions into an AO dictionary 
* round tripping, ability to recover claw code from AO
* implicitly bound to AO dictionary for definitions and data
* concatenative, streamable, composable, purely functional
* support DSLs and alternative programming environments

### Rationals, Decimals, Integers, and Literals

I want support for ratios and decimals (e.g. `2/3` and `3.141`) so I can at least use claw as a simple calculator. Scientific E notation would also be useful in some cases, e.g. working with physical equations. As of 2015 June, Awelon Bytecode no longer directly supports rational numbers, so I shall need to model numbers more explicitly. A simple, promising option is to treat each as a sugar:

        2/3     desugars to     2 3 ratio
        3.141   desugars to     3141 3 decimal
        6.02e23 desugars to     6.02 23 exp10

For round-tripping, the form shall be preserved, e.g. we don't lose the zeroes in `1.000`, and `4/6` would be distinct from `2/3` (before processing by `ratio`). Expansions may involve multiple steps. For example, the `6.02` for the scientific E notation expands as a decimal number. We may generalize this idea for integral and literal values:

        42      desugars to     \#42 int
        "foo"   desugars to     \"foo
                                 ~ lit

This approach to literal values is easily extensible with new notations. I would like to explore vectors, matrices, association lists, and other formats. However, for the moment I'll focus on just the five forms indicated above: ratio, decimal, exp10, integer, literal. 

#### Claw Multi-Line Literals and Alternatives

As a universal view, Claw provides a syntax for multi-line literals. 

        \"This is an example
         \multi-line literal
         \with "double quotes".
        ~

Claw uses `LF SP* \` to escape an LF and `LF SP* ~` to terminate (where ABC respectively uses `LF SP` and `LF ~`). This allows Claw to be more flexibly formatted and indented. It also ensures that blank lines of text are visible, and that an incomplete text literal is easily recognized. 

However, this is not very aesthetically pleasing (at least to my eyes), and it would be a pain to write in a console or REPL. Multi-line literals simply, fundamentally, aren't a nice fit for a command line interface. Where large literals are necessary, my recommendation is instead to define them using a separate word and access them by name. We can provide a specialized view for reading and editing text-mostly definitions. 

Inline literals are sufficient for most command line use cases, including labels like `"foo"`, test strings like `"Hello, World!"`, micro-DSLs like regular expressions. While Claw doesn't have built-in character escapes, it isn't difficult to define a function to rewrite substrings like `"\n"` and `"\q"` to model escapes.

### Claw Blocks

An important role of words integer and literal is to place each value at an appropriate location in the environment, e.g. onto a stack. We'll need a similar feature for blocks:

        [foo]   desugars to     \[foo] block

The escaped form of a block still contains claw code but does not assume any placement word. Mostly, this is necessary so we can reliably represent blocks that were not placed at the default location.

### Claw Sequences

A command language sequence is a sequence of commands (even `42` is a command). A simple use case is to construct a sequence of values. For example, `{1,2,3}` is might construct a list of three numbers. Parsimonious expression for a list of values is convenient for matrices, graphs, tables, and more. In the more general use case, sequences are a potential basis for monadic DSLs and block-structured programming.

The semantics for claw sequences is:

        {foo,bar,baz}   desugars to
        [\[foo] cmd \[bar] cmd \[baz] cmd] cmdseq

This treats both commands and sequences as first-class values. The commands are individual blocks, while the sequence is represented by a block that operates sequentially and uniformly upon each command. This has a nice property: composition of sequence blocks is equivalent to concatenation of sequences. However, we could easily rewrite to alternative representations (e.g. with `cmd = \lV` we would push each command onto a list). The recommended definition is `cmd = \lw^z$`, which allows us to provide a command handler.

        handler  :: (command * st) → st
        sequence :: (st * (handler * 1)) → (st * (handler * 1))
        cmd      :: (command * (st * (handler * 1))) → (st * (handler * 1)))
        cmd = \lw^z$

Claw additionally supports an explicit *sequence escape* syntax, enabling injection of arbitrary code into a sequence block. This is currently expressed by prefixing a command by `/`. For example:

        {/foobar, baz}  desugars to
        \[foobar \[baz] cmd] cmdseq

The motivation for sequence escapes is convenient syntactic abstraction, for example extracting subsequence `foo,bar` into a separate word `foobar`. Syntactic abstraction is an important property for Awelon project, though I encourage developers to favor composable semantic abstractions (such that `foo,bar` may be composed in a type-dependent way into a `foobar` command, no escapes required).

*Note:* `{}` is a sequence with one empty command. The empty command sequence is `{/}`.

### Claw Attributes

An attribute is an annotation *about* nearby code. Essentially, it's a comment, though not necessarily for a human. Examples of this sort of thing: todos, deprecations, licensing, authoring, quotas, categories, keywords, enabling or disabling full-text search, suggested view, extra typechecker options, guides for indexing, guides for robotic maintenance. 

Because they have no meaning within our code, we wrap attributes into a block then it away, e.g. `[{&deprecated}]%`. For expressing attributes in Claw code, I will to use parentheses:

        (license:BSD3 author:dmbarbour category:math)       desugars to
        [{&license:BSD3}{&author:dmbarbour}{&category:math}]%

The annotations in this context are presented as words. Commas, within an attributes list, are ignored as whitespace.

*Aside:* We don't optimize presentation of annotations in normal code because they are easily abstracted behind words. So we just escape those normally. This is not the case for attributes, which are not abstracted.

### Claw Namespaces

It's useful to have *short* words when writing Claw code. However, all the short words will tend to be monopolized for a given purpose. If we develop a different environment that needs a new meaning for these words, we'll want namespaces so we can now use our words as the short ones.

So claw code will support namespace attributes, indicated with the `#` prefix. 

        #foo: 42 bar        desugars fully to
        [{&ns:foo:}]%#42{%foo:integer}{%foo:bar}

When Claw sees a singleton attribute, it will scan for an updated namespace and use this to guide the render (or, conversely, the desugar). A namespace attribute is small and simple enough to avoid becoming too much boiler-plate. 

*Aside:* namespaces make our Claw code mildly context sensitive. Our programmers must be aware of namespaces to understand the code, or to copy/paste it into a new context. This critical need for awareness is why I don't permit Claw namespaces to be buried among a bunch of other attributes and assign a visible prefix.

Due to the nature of Claw code as a trivial expansion, we can only have one attribute in any given region of code. The advantage is that we won't have namespaces appearing like a bunch of boiler-plate. The disadvantage is that we are forced to define every word in our new namespace. This could be made tolerable with some automatic tooling. Because it's often convenient to just have a particular namespace for a subprogram, *a namespace expressed within a block of code will extend only to the end of that block*.

A weakness of namespaces is that they can hurt refactoring. Developers must be careful to not move code into a different namespace, e.g. via copy and paste. We could solve this by performing copy-paste on the AO/ABC expansion instead of the claw code, and perhaps record the namespace associated with the selection. Operating at the AO or ABC layers doesn't have any issues with context.

*Note:* A namespace string must be empty, or a valid word if taken by itself.

### Claw Semantics and Round Tripping

Claw code is essentially a simple syntactic sugar above AO. The entire semantics of claw code is its trivial expansion into AO. Further, this is reversible: a parser can recover structure and present it to the user for editing. The main weakness is that such a parser will be oriented around a common set of words (integer, literal, block, ratio, decimal, exp10). This effectively restricts us to round-tripping, i.e. we can recover useful structure only from bytecode generated by expanding claw code. But this is an acceptable limitation.

In general, spacing and newlines are not preserved.

## Claw Shell and Effects Model

See [Wikilon's application model](ApplicationModel.md). Claw is a good fit for command pattern applications, such as a REPL or shell. Also for iPython-notebook scenarios. Given an appropriate software agent for integrating effects, modeling a Claw 'shell' that reflects on our dictionary or interacts with external services is entirely feasible.

## Multi-Media Claw 

Assuming an appropriate editor, I would be interested in recognizing *form*-based presentation and widgets, e.g. `30/100 slider` would be recognized and presented as a slider widget. Graphs, diagrams, checkboxes, radio buttons, and more could be modeled this way. This might be considered a dialect of Claw.
