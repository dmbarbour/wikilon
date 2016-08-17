
# Command Language for Awelon (claw)

Awelon project and Wikilon will benefit from effective command line interfaces, e.g. for [modeling a REPL and shell](ApplicationModel.md). In context of Wikilon, providing console-like services through HTML forms or JavaScript should not be difficult. The underlying [Awelon Object (AO)](AboutAO.md) bytecode is unsuitable for this application in its raw form.

Desirable features for a command line interface:

* numbers and inline texts `42 2/3 3.141 "foo"`
* trivial access to words, e.g. `swap inc mul`
* escaped access to underlying language `\vrwlc \{&anno}`
* namespace model for multiple environments `#foo: 1 2 3`
* blocks for higher order operations `1 [2 mul] 53 repeat`
* terse encoding of tuples, lists, tables

Command Language for Awelon (claw) is a syntactic sugar and editable view for AO. Claw semantics is a trivial expansion into AO bytecode, and this expansion is reversible, such that we may view and edit AO bytecode as claw code. 

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

Inline literals are sufficient for most command line use cases, e.g. labels like `"foo"`, test strings like `"Hello, World!"`, and micro-DSLs like regular expressions. Claw doesn't have built-in character escapes, and it isn't difficult to define your `lit` function to statically rewrite substrings like `"\n"`, `"\q"`, and `"\\"` to model escapes of newlines and double-quotes and escapes.

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
        
        ~ lit

Our text begins on the line after `\"`. Each line of claw text is preceded with `LF SP` and the text terminates with `LF ~`. For convenience, `LF LF` is treated as `LF SP LF` so developers don't need to indent empty lines. Other than SP, LF, or ~, any character following LF is an error. For aesthetics, our text is padded with one empty line before and after. One empty line of padding is trimmed from each side when parsing the claw text. (The padding is optional, unless you actually need empty lines around the text.)

No default escapes are used within the text body. However, claw is generally limited to the same character set as AO dictionaries, forbidding control characters (except LF), DEL, surrogates, and the replacement character. If developers need more than this, they may encode it and functionally rewrite the text afterwards.

I hope for claw to be tolerable as a syntax for text markup, typography, or literate programming. These multi-line literals are a good start: it is lightweight and doesn't require ugly escapes. The weakness is that multi-line text cannot be flexibly indented, which may be awkward in some cases. We might mitigate this functionally or via abstraction of text into separate definitions. But, if this doesn't work out, we might need a more structured view for those roles.

### Claw Blocks

An important role of words integer and literal is to place each value at an appropriate location in the environment, e.g. onto a stack. We'll need a similar feature for blocks:

        [foo]   desugars to     \[foo] block

The escaped form of a block still contains claw code but does not assume any placement word. Mostly, this is necessary so we can reliably represent blocks that were not placed at the default location.

### Claw Command Sequences

A lightweight and aesthetically pleasant syntax for *command sequences* can greatly augment Claw's expressiveness, enabling:

* concise representation for streaming data
* monadic programming, effects models and DSLs
* incremental computing, cooperative threading

The proposed sugar uses explicit continuation passing style:

        [foo, bar, baz, qux] desugars to:
        [\[\[\[qux] after baz] after bar] after foo]

A command sequence is a block that receives a continuation stack, potentially extends it with `after`, then does some work for the first command in the sequence. The comma might informally be understood as a *yield* action, enabling an intermediate return step after performing each command. The intermediate return enables our caller to extract or inject data, model effects, etc.. Streaming data can be trivially expressed as command sequences, e.g. `[1,2,3,4,5]`, with the caller simply extracting a single value after each step.

Command sequences are transparently subject to abstraction, procedural construction, and modeling both finite and infinite streams of data or actions. Inlining or 'joining' a command sequence is valid and behaves as one would expect assuming simple stack-based implementations of `after`. 

### Claw Attributes

An attribute is an annotation *about* nearby code. Essentially, it's a comment, though not generally intended for a human. Examples of this sort of thing: todos, deprecations, licensing, authoring, quotas, categories, keywords, enabling or disabling full-text search, recommendations for viewing and rendering code, extra typechecker options, guides for indexing, guides for robotic maintenance. 

For attributes in Claw code, I will to use parentheses:

        (license:BSD3 author:dmbarbour category:math)       desugars to
        [{&license:BSD3}{&author:dmbarbour}{&category:math}]%

Dropping the block ensures the annotations have no runtime behavior. Use of annotation symbols simplifies development of reverse lookup indices, and avoids the issue of creating link dependencies. A flat list of symbols keeps it simple, while allowing limited structure (like non-dependency relationships between words) to be expressed if necessary.

*Aside:* Normal annotations must still be expressed as `\{&annotation}` in Claw. But we can abstract annotations behind words like any other code. Attributes need special attention because, like source comments, they cannot be abstracted.

### Claw Source Comments? (Not yet.)

Conventional C-style comments could trivially be represented in Claw via dropping embedded texts. 

        "This is a comment.
        ~%

Such text could be presented using conventional C-style or Haskell style comments.

However, I'm reluctant to support comments within source code. My intuition is that separating comments from source - e.g. into separate words like `foo.doc` and `foo.talk` to describe `foo` - will make the data contained in the comments more accessible for a lot of interesting use cases. Further, I would indirectly encourage factoring code into smaller functions that can be documented independently.

### Claw Namespaces

Claw currently provides a simplistic namespace model: a region of code will have a single namespace, which may be specified as `#prefix`. The namespace implicitly becomes a prefix for all words in the Claw expansion, including implicit words.

        #foo: 42 bar        expands to bytecode
        [{&_foo:_}]%#42{%foo:int}{%foo:bar}

An empty prefix is valid and is also the default namespace.

The goal of namespaces is to support exploration of alternative program models within a single dictionary. This requires alternative definitions for data plumbing and utility words. 

Claw uses namespace attributes to guide rendering and expansion. Some words cannot be expressed in a given namespace. For example, under `#foo:` the word `bar` cannot be expressed because it lacks a `foo:` prefix, and `foo:9` cannot be expressed because `9` doesn't parse as a valid word. In these cases, the claw view is forced to use the token expansion, e.g. `\{%bar}` or `\{%foo:9}`. 

A namespace attribute within a block is limited to the remaining scope of that block. In case of command sequences, namespaces also impact the commas. So developers will need to be aware of this.

There is a proposal for *Qualified Namespaces*, below, that I'm still contemplating. I'm not convinced it's a good idea, even if it could be useful in some limited contexts. (There is a trade-off between concision and sophistication of context.)

## Claw Semantics and Round Tripping

Claw code is a simple syntactic sugar and editable view. The entire semantics of claw code is its unambiguous expansion into AO bytecode. Further, this is expansion reversible: a parser can recover structure and present it to the user for editing. Claw can represent any AO bytecode. However, code not written for the Claw view will tend to be full of ugly escapes. Thus, in practice, Claw only supports round-tripping. 

Claw does not, in general, preserve spacing and newlines. 

## Claw Shell and Effects Model

See [Wikilon's application model](ApplicationModel.md). 

Claw is a good fit for command pattern applications, such as a REPL or shell. Also for iPython-notebook scenarios. Given an appropriate software agent for integrating effects, modeling a Claw 'shell' that reflects on our dictionary or interacts with external services is entirely feasible.

Command sequences make Claw much more effective at these tasks, i.e. we can directly model a series of commands between which we may return to interact with an interpreter or render content.

## Proposals for Future Extensions

### Visual Claw 

Assuming an appropriate editor, I would be interested in recognizing *form*-based presentation and widgets, e.g. `30/100 (slider)` is recognized and rendered as a slider widget. Graphs, diagrams, canvases, tables, trees, checkboxes, selection lists, radio buttons, and a multitude of other stateful widgets could be integrated this way. 

Visual Claw has potential to serve as a rich document model, an intermediate level between Claw text and full evaluation-based dictionary applications. Words defined in Visual Claw would support [direct manipulation](https://en.wikipedia.org/wiki/Direct_manipulation_interface) of the dictionary, and would also serve as *templates*: a dictionary application would copy a template word to provide a form, allow the user to edit and submit, and the result is saved into a new word then integrated (e.g. representing a command). 

By editing of canvases, graphs, tables we gain the ability to work with reasonably large structured documents (up to a few megabytes). We can mitigate growing sizes via hierarchical views and extra support from our dictionary app. An embedded editor for a word might easily be represented by code of the form `foo (embed)`. This might be represented by an HTML iframe. Embedding a slider, then, would allow us to control many definitions from a single widget. 

Between embedding and a model for graphs, it should be feasible to represent box-and-wire programming environments. Embedding also provides a simple basis for 'connected' views, e.g. where manipulating a slider in one form will manipulate it elsewhere. There are a lot of interesting possibilities here.

Visual Claw can hide content, e.g. via the `<input type="hidden" ...>` attribute in forms and similar techniques. While I dislike the idea of hiding runtime semantic content, we can benefit from hiding layout attributes. We can also use progressive disclosure techniques, e.g. the ability to drill down into tree-structured data or specify (in hidden attributes) which subtrees are initially visible. 

Developing a Visual Claw dialect targeting HTML seems immediately promising and highly feasible. I need to hammer out a bunch of details, though. Also, I'll need to improve performance of my Claw parsers, eventually, to work with the larger visual documents.

### Block Structured Plain Text?

Most programming languages are designed to use block-structured plain text, oriented vertically on a page or screen, as the primary development interface. This style of programming would enable Awelon project to take better advantage of existing tools, and would improve familiarity for potential users.

Claw could be extended to support block structured plain text programming. 

Most likely, I'd need to introduce the conventional set of mixfix keywords (such as `if then else` and `while do`). As a *view*, adding keywords to Claw isn't difficult. For example, if we suddenly add `while` as a keyword, existing uses of that word would simply render `\{%while}` to indicate our AO word. There is no risk of breaking code.

However, keywords interfere with factoring and abstraction. We cannot simply take the program fragment `if [cond]` and factor it into a separate word when that `if` is part of a larger `if_then_else_`. We cannot readily abstract the `[cond]` function when it is built into the `if_then_else_` syntax instead of provided externally. And it would be difficult for users to define new keywords in a first class manner, i.e. every such extension requires a change to the syntax. These properties conflict with my Awelon project goals.

I imagine that Visual Claw can fulfill the role of presenting a familiar coding style more effectively and extensibly. For example, `[cond] [body] while_do_` could be rendered appropriately in a block-structured style via a few generic heuristics regard words with underscores (two underscores → captures two Claw elements). No keywords are necessary. 

I am more interested in pursuing Visual Claw for now. But if working easily with plain text tools in a more familiar syntax proves critical - perhaps through Filesystem in Userspace or Callback File System - it shouldn't take long to develop a set of keywords and an appropriate Claw dialect. (The flexibility of view-based syntax is wonderful.)

### Named Variables and Lexical Closures

Tacit programming is effective in many problem domains, and is concise when it works. It works in a surprising range of problem domains: fewer than 1% of Factor functions are defined using named locals. But there is a small, essential subset of problem domains (such as polynomial expressions) where tacit programming is too painful. For example:

        f x y z = y^2 + x^2 − |y|

            might translate to

        f = drop dup square swap abs rot square sub add

Fortunately, named variables for otherwise tacit concatenative languages are not a new idea. Languages [Factor](http://factor-language.blogspot.com/2007/08/named-local-variables-and-lexical.html) and [Kitten](http://kittenlang.org/intro/) include a syntax for this. Examples:

        Factor: two options for defs and blocks.
            :: f | x y z | y square x square + y abs - .
            [|x y z| y square x square + y abs -]

        Kitten: two options for aesthetics.
            -> x y z { x square y square add y abs sub }
            { -> x y z; x square y square add y abs sub }

I find Factor's syntax difficult to read when layered, e.g. `[| a b | a [| c | c b +] map ]`.

Both Factor and Kitten support *lexical closures*. When a name is used within an internal block, it is bound to the value in scope at the time of binding. Lexical scoping and closure is very convenient for higher order programming. In Claw, lexical closures would be necessary for use with command sequences. 

Kitten's locals are a built-in feature. In Factor, the local variables are a syntactic sugar. However, Factor's desugared representation is not reversible. For Claw, any syntax for named variables must be reversible. Unfortunately, I have yet to conceive of a simple, reversible desugaring that supports lexical closures. 

Ignoring scope and closure, we could feasibly desugar to:

        "z" assign "y" assign "x" assign
        "y" deref square
        "x" deref square add
        "y" deref abs sub

With appropriate definitions for assign, deref, and scoped, the above program should behave as we expect. We could implement this with a stack of association lists. A compiler, perhaps aided by annotation, should be able to eliminate the runtime overhead of this explicit data plumbing (or at least inform you otherwise). 

But this half baked solution isn't acceptable. Until I have some better ideas, syntactic support for named variables will need to wait.

*Aside:* Despite potential utility, I have doubts about named local variables. They hinder refactoring because we cannot move a subprogram containing a variable name. They're also a bit more complicated in presence of substructural types, though I suppose we could specialize the *last use* of each variable to use move semantics rather than copy. I wonder if users would be better served by an EDSL for math.

### Qualified Namespaces

Constraining users to a single namespace per volume of code encourages a relatively flat namespace and prevents boiler-plate import lists. These are nice properties, so I'm reluctant to support qualified namespaces. However, the potential tweak to support them is trivial:

For multiple namespaces, we could write `#f/foo:` after which `f/word` expands to `foo:word`. Our namespace attribute would simply desugar to `[{&_foo:_f}]%`. When rendering words with multiple valid render options, we heuristically favor the shortest render (with `\{%word}` as a last resort). Qualified namespaces would more closely match conventional programming practices, where we tend to have a large list of imports at the top of a large page of code.
