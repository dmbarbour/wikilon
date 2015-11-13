
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

An attribute is an annotation *about* nearby code. Essentially, it's a comment, though not necessarily for a human. Examples of this sort of thing: todos, deprecations, licensing, authoring, quotas, categories, keywords, enabling or disabling full-text search, suggested view, extra typechecker options, or guides for robotic maintenance. 

Because they have no meaning within our code, we wrap attributes into a block then it away, e.g. `[{&deprecated}]%`. For expressing attributes in Claw code, I will to use parentheses:

        (license:BSD3 author:dmbarbour category:math)       desugars to
        [{&license:BSD3}{&author:dmbarbour}{&category:math}]%

The annotations in this context are presented as words.

*Aside:* We don't optimize presentation of annotations in normal code because they'll be abstracted behind words, e.g. to perform data-plumbing. So we just escape those normally. This is not the case for attributes, which are not abstracted.

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

In general, spaces and newlines are not exactly preserved. Escaped operators may collapse (e.g. from `\r \w` to `\rw`). If developers are using large programs or escaped forms frequently enough to care, they should probably add more words to the dictionary. 

## Claw Shell and Effects Model

AO and claw code are purely functional. However, claw is intended for command-line contexts. Each 'command' will be a `state → state` function that is applied to update the state of a shell. This state value may contain stacks and workspaces. Feedback to the user is achieved by rendering the state, or a partial view of it, for the user. A Forth-like REPL, for example, might render the top few values on the stack.

A command language, to be useful, must enable users to orchestrate external resources beyond the local state of the command shell. A user might update a database, query a web server, display an image to screen, control a robot. To achieve these effects, we will assume a background agent is able to observe and influence our shell state value between user commands. As a simplistic case, our background agent may integrate a mail service, and our state may include an inbox and outbox. By adding messages to our outbox, we could send messages to influence external systems. By receiving messages with our inbox, we could observe external resources to help make new command decisions.

To orchestrate multi-step behaviors on an inbox (e.g. query a database then, depending on the result, take one action or another), we would need to automate our handling of received messages. This suggests a function of a type similar to `(message*state)→state`. This function could process messages immediately, modify state, optionally update an outbox that is part of state. A function might record a subset of messages into an inbox or log for human perusal. By rendering state after updates by these asynchronous messages, rich curses-style and graphical command shells should be viable.

Effects modeled using functional updates on shared state have nice properties: 

* Updates are atomic; we can abort for any reason and restore original state.
* Priority may be modeled by aborting long-running, low priority computations.
* Non-termination is an error; termination analysis may be applied uniformly.
* Asynchronous tasks are represented in state; this can simplify debug views. 
* Effects are asynchronous; generalize easily to open or distributed systems.

On the other hand, performance may be an issue, with shell state becoming a bottleneck. Users may need to offload the more expensive computations to other machines via the effects model, such that the shell remains relatively more responsive.

### Abstract Virtual Machine (AVM) and Network Model

Messaging requires addressing. To receive a message, a shell needs an address. In general, we might want for this address to include some context information, i.e. such that we can route a received message to the correct subprogram. To send a message, a destination address is required. A fresh shell must be informed of which resources are available (or at least where to find them, e.g. via registry). 

These problems are addressed, more or less, by the [AVM](NetworkModel.md) and associated network model. The AVM behavior function has the form `(InMsg*State)→(OutMsgList*State)`. This is very similar to the `(message*state)→state` mentioned above. One motivation for using an output message list as opposed to an outbox within state is that we can easily recognize read-only queries on the state and extract additional parallelism or even replication of the machine. 

AVM input and output messages each have two parts. An input message has the form `(context*content)` where context is a value securely controlled by our AVM, and content is a value provided by the the sender. Output messages have the form `(capability*content)` where capability includes a network address and a cryptographically opaque context. To create capabilities, an AVM receives a *self* function, of type `context→capability`. This way, outgoing messages may contain capabilities to receive responses. In a type-safe environment, we might think of contexts and capabilities as indicating the type of associated message (dependent types).

Every AVM is formed of a triple: (state, behavior, signal). The signal value is a context used by the AVM host for generic life cycle purposes: init, halt, suspend, wakeup, etc.. A fresh AVM is initialized by signal with information about itself and its environment. Signals may serve other ad-hoc purposes, e.g. to indicate that an AVM should favor a power saving or low bandwidth modes.

### Command Capabilities

A claw command line will be a function of type `state→state`. This preserves nice properties for concatenation, composition, streaming. Though, the state type and effects models must also be chosen carefully to ensure that effects are also composable. To deliver a claw command, we reify it into a block, i.e. `[state→state]`, then deliver it to an AVM as a `(capability * [state→state])` message. This capability is also part of the editor's state, and routes the block to the appropriate machine. The AVM applies the function to the state and has immediate opportunity to observe and react to updated state.

Effects beyond state update are left to the discretion of the AVM and the specific model and use case. An interesting possibility is that our state value itself hosts an AVM triple for direct manipulation by the user, then the outer AVM queries the inner AVM for available messages. The nature of AVMs makes it trivial for one machine to host more machines. Together with namespaces, it is feasible to experiment with many command contexts with different state types.

### View Capabilities

Blind commands are probably bad commands. If we have the authority to update some state, we should have authority to view at least the same. But the converse isn't true: it may be useful to provide view capabilities for states we cannot directly manipulate. In the simplistic case, a view or query message might have a form similar to `(viewCap * (queryInfo * responseCap))`, where `responseCap` receives a message containing a copy of `state`. However, this is a bit too simple, and requires repeatedly polling the state even if it hasn't changed. Some properties that also would be valuable include:

* easily filter or compose views into new views
* subscribe to state and manage the subscription
* machines adapt based on active subscriptions

While I haven't hammered out the details, I think it would be wise to take a lot of inspiration from publish-subscribe models, and perhaps even integrate with some of those. In the mean time, I can probably implement editors with implicit views.

### Editor Capabilities and Asynchronous Effects and Views

In overview, I'm leaning towards a capability-secure model for commands and views, placing each REPL or Shell into an implicit AVM. The ability for commands to cause further effects is left to the discretion of the receiving AVM. View capabilities will be a bit trickier to get right, I think, though it's doable and a brute force simplistic approach should be sufficient to get started.

Usefully, these views and techniques should generalize nicely for RDP, which effectively pushes all of 'messaging' into machine adaptation to active subscriptions (with parameters).


## Multi-Media Claw 

Claw code can be extended with new literal types. While there are some rather severe limits on what extensions are viable for use in a typical command-line environment, these limitations might be relaxed in context of iPython-notebook style environments, live coding, or structure editors. A structure editor could feasibly support rich media such as graphs and diagrams as simple claw literals.

Independently if claw-structured bytecode (based on *parsing* bytecode), Awelon Object dictionaries offer structure aligned with the definition of words (based on *evaluation* of bytecode into an intermediate structure), i.e. as a basis of [extensible syntax](ExtensibleSyntax.md) on a per-word basis. A structure editor may present some words as graphs, diagrams, forms, or other resources to be edited inline.

My hope is that these two forms of structure will either augment each other, or at least fill different niches. 

### Extensible Claw Editors

We can build syntactic extensions into our editors or views. However, an interesting alternative is instead to support a generic claw editor that queries the dictionary to obtain information about how to parse and expand claw code content (perhaps on a per-namespace basis). This would allow a lot of very rich extensions to claw code.

### Late vs. Early Binding of Words

Our claw comands are bound to a dictionary. But do we expand the definitions of words at compile time? Should we wait until they're actually processed? The difference here impacts behavior when we update our dictionary. Assuming interaction with an outside world, I think early binding may be more appropriate. Late binding makes much less sense in a network context. 
