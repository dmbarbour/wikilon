
# Command Language for Awelon (claw)

Awelon project and Wikilon will benefit from effective command line interfaces, e.g. for [modeling a REPL and shell](ApplicationModel.md). In context of Wikilon, providing console-like services through HTML forms or JavaScript should not be difficult. The underlying [Awelon Object (AO)](AboutAO.md) bytecode is unsuitable for this application in its raw form.

Desirable features for a command line interface:

* numbers and inline texts `42 2/3 3.141 "foo"`
* trivial access to words, e.g. `swap inc mul`
* escaped access to underlying language `\vrwlc \{&anno}`
* namespace model for multiple environments `#foo: 1 2 3`
* blocks for higher order operations `1 [2 mul] 53 repeat`
* terse encoding of tuples, lists, tables

Command Language for Awelon (claw) is a syntactic sugar and editable view for AO. Claw semantics is a trivial expansion into AO bytecode, and this expansion is reversible, such that we may view and edit AO bytecode as claw code. While claw is optimized for one-liner programs, it is usable for page-structured programming.

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
        "foo"   desugars to     \"
                                 foo
                                ~ lit

This approach to literal values is easily extensible with new notations. I would like to explore vectors, matrices, association lists, and other formats. However, for the moment I'll focus on just the five forms indicated above: ratio, decimal, exp10, integer, literal. 

#### Multi-Line Literals

Inline literals are sufficient for most command line use cases, e.g. labels like `"foo"`, test strings like `"Hello, World!"`, and micro-DSLs like regular expressions. Claw doesn't have built-in character escapes, and it isn't difficult to define your `literal` function to rewrite substrings like `"\n"`, `"\q"`, and `"\\"` to model escapes of newlines and double-quotes and escapes.

Nonetheless, claw provides a usable syntax for multi-line embedded text:

        some commands \"
        
         This is an example multi-line
         literal with "double quotes".
        
        ~ more commands

        (author:Cicero date:BCE45 lang:latin) \"
        
         Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do
         eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut 
         enim ad minim veniam, quis nostrud exercitation ullamco laboris
         nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor
         in reprehenderit in voluptate velit esse cillum dolore eu fugiat 
         nulla pariatur. Excepteur sint occaecat cupidatat non proident, 
         sunt in culpa qui officia deserunt mollit anim id est laborum.
        
        ~

Our text begins on the line after `\"`. Each line of claw text is preceded with `LF SP` and the text terminates with `LF ~`. For convenience, `LF LF` is treated as `LF SP LF` so developers don't need to indent empty lines. Other than SP, LF, or ~, any character following LF is an error. For aesthetics, our text is padded with one empty line before and after. One empty line of padding is trimmed from each side when parsing the claw text. The padding is optional unless you actually need the empty lines.

No default escapes are used within the text body. However, claw is generally limited to the same character set as AO dictionaries. Claw forbids control characters (except LF), DEL, surrogates, and the replacement character. If developers need more than this, they may encode it and functionally rewrite the text afterwards.

I hope for claw to be tolerable as a syntax for text markup, typography, or literate programming. These multi-line literals are a good start: it is lightweight and doesn't require ugly escapes. The weakness is that multi-line text cannot be flexibly indented, which may be awkward in some cases. We might mitigate this functionally or via abstraction of text into separate definitions. But, if this doesn't work out, we might need a more structured view for those roles.

### Claw Blocks

An important role of words integer and literal is to place each value at an appropriate location in the environment, e.g. onto a stack. We'll need a similar feature for blocks:

        [foo]   desugars to     \[foo] block

The escaped form of a block still contains claw code but does not assume any placement word. Mostly, this is necessary so we can reliably represent blocks that were not placed at the default location.

### (experimental) Claw Sequences

See [command sequences](CommandSequences.md).

### Claw Attributes

An attribute is an annotation *about* nearby code. Essentially, it's a comment, though not necessarily for a human. Examples of this sort of thing: todos, deprecations, licensing, authoring, quotas, categories, keywords, enabling or disabling full-text search, suggested view, extra typechecker options, guides for indexing, guides for robotic maintenance. 

Because they have no meaning within our code, we wrap attributes into a block, then throw it away, e.g. `[{&deprecated}]%`. For expressing attributes in Claw code, I will to use parentheses:

        (license:BSD3 author:dmbarbour category:math)       desugars to
        [{&license:BSD3}{&author:dmbarbour}{&category:math}]%

The annotations in this context are presented as words. Commas, within an attributes list, are ignored as whitespace.

*Aside:* We don't optimize presentation of annotations in normal code because they are easily abstracted behind words. So we just escape those normally. This is not the case for attributes, which are not abstracted.

### Claw Namespaces

It's convenient to have *short* words when writing code. Short words will tend to be monopolized for a given purpose. We'll eventually want to assign a different meaning to the same short words for use in another context. The conventional mechanism in programming languages to handle this issue is *namespaces*, an ability to contextually access names without fully writing them. 

With claw, we have three constraints that resist conventional namespacing techniques. First, claw is an editable view, a syntactic sugar. Its semantics is a local expansion into bytecode. Hence, claw should not peek at the AO dictionary to help resolve which words belong in which namespace. Second, claw uses names even for expressing numbers, e.g. `2/3` expands to `2 3 ratio`, and we might want namespaces for ratios. Third, claw is intended for use on a command-line. Conventional namespaces are designed for page-oriented programming. A few lines of namespace boiler-plate near the top of a page aren't a huge deal when we follow that a hundred lines of code. But a few lines of boiler-plate will prove irritating in context of a command line. 

My current approach to namespaces involves use of a `#` prefix to set a namespace attribute for a volume of code. 

        #foo: 42 bar        desugars fully to
        [{&ns_foo:_}]%#42{%foo:int}{%foo:bar}

*Note:* Words accessed via namespaces must be valid. For example, we cannot access `foo:9` under namespace `#foo:` because `9` is not a word. The empty string is a valid namespace.

An extension I'm exploring is to use prefixes e.g. such that `f/` becomes shorthand for `foo:`. This would be more advantageous for long words, and it could mitigate the need to develop redirects by allowing multiple namespaces to be used together. This might be expressed as `#f/foo:` with a variant namespace attribute, perhaps `[{&ns_foo:_f}]%`. This could mitigate the weakness of using only one namespace.

When Claw sees a namespace attribute, it will use this to guide the rendering and desugaring. This gives us mild form of context-sensitive syntax. A namespace in a block only lasts to the end of that block. A namespace attribute is hopefully small enough to avoid becoming too much boiler-plate. However, the limit of *one* namespace for a volume of code can be annoying. It requires adding a bunch of redirects to access words from a different namespace.

It is also feasible to separate namespaces from the code, e.g. providing a set of prefixes via the user profile.


### Claw Semantics and Round Tripping

Claw code is essentially a simple syntactic sugar above AO. The entire semantics of claw code is its trivial expansion into AO. Further, this is reversible: a parser can recover structure and present it to the user for editing. The main weakness is that such a parser will be oriented around a common set of words (integer, literal, block, ratio, decimal, exp10). This effectively restricts us to round-tripping, i.e. we can recover useful structure only from bytecode generated by expanding claw code. But this is an acceptable limitation.

In general, spacing and newlines are not preserved.

## Claw Shell and Effects Model

See [Wikilon's application model](ApplicationModel.md). Claw is a good fit for command pattern applications, such as a REPL or shell. Also for iPython-notebook scenarios. Given an appropriate software agent for integrating effects, modeling a Claw 'shell' that reflects on our dictionary or interacts with external services is entirely feasible.

## Multi-Media Claw 

Assuming an appropriate editor, I would be interested in recognizing *form*-based presentation and widgets, e.g. `30/100 slider` would be recognized and presented as a slider widget. Graphs, diagrams, checkboxes, radio buttons, and more could be modeled this way. This might be considered a dialect of Claw.
