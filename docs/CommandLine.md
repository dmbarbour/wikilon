
# Command Language for Awelon (claw)

Awelon project and Wikilon would benefit from effective command line interfaces, e.g. as a REPL and shell. In context of Wikilon, providing console-like services through HTML forms or JavaScript should not be difficult. The underlying [Awelon Object (AO)](AboutAO.md) code is unsuitable for this application in its raw form. 

But we can certainly create a language better suited, which we can rapidly compile into bytecode, with easy access to dependencies on words in a dictionary. Useful features might include:

* write words easily: just use the word, `swap inc mul`
* inline text and number literals: `42 "foo"`
* easy and unambiguous access to bytecode, e.g. `\vrwlc` 
* first class blocks, higher order ops: `1 [2 *] 53 repeat`
* stable target environment for the code

One option is a straightforward expansion into bytecode, e.g. similar to the [original definition of Awelon Object](https://github.com/dmbarbour/awelon/blob/master/AboutAO.md). The original AO was weak for staged programming, visual DSLs, or structured programming, and wasn't strongly an '[object code](http://en.wikipedia.org/wiki/Object_code)', but it was well suited for command line interfaces. The result was a very Forth-like language. Though, in retrospect, I favor `\` as an escape for bytecode rather than `%`. We might also use `\{token}` for escaped tokens, and `\"text...~` for escaped multi-line texts.

I propose to call the language **claw**. Command Language (or Line) for AWelon.

## Claw Code

General points to help guide the design:

* need effective support for rational and decimal numbers
* enable flexible extension of programming environments
* transparent, simple, predictable, direct AO translation
* easy to preserve claw sesssions into an AO dictionary 
* round tripping, ability to recover claw code from AO
* implicitly bound to AO dictionary for definitions and data
* concatenative, streamable, composable, purely functional

### Rationals, Decimals, Extensible Literals

I want support for ratios and decimals (e.g. `2/3` and `3.141`) so I can at least use claw as a simple calculator. As of 2015 June, Awelon Bytecode no longer directly supports rational numbers, so I shall need to model these explicitly, e.g. using a pair of integers. A simple, promising option is to treat each as syntactic sugar:

        2/3     desugars to     2 3 rational    (or perhaps \#2#3 rational)
        3.141   desugars to     3141 -3 decimal (or perhaps \#3141#3- decimal)
        1.000   desugars to     1000 -3 decimal (preserving input format)

We might generalize this further for basic literal and integral types:

        "foo"   desugars to     \"foo\n~ literal
        42      desugars to     \#42 integral

Round tripping then works in reverse. Claw becomes a very simple lens to view and edit a raw AO stream. This lens is extensible. For example, one might introduce complex numbers such as `0.6+0.8i` expanding to `0.6 0.8 complex`. When viewed through another lens that doesn't know about complex numbers, we might simply halt re-sugaring at the `0.6 0.8 complex` form, which is still useful for programmers.

I think this could be a simple, effective solution to the problem of handling ratios, decimals, and possible future literal types and extensions that can be expressed in text (vectors, matrices, date-time values, physical units, URLs, etc..).

### Claw Environments and Edit-Layer Namespaces

Namespaces are valuable for textual manipulation of code with many different contexts. Awelon Object code doesn't have namespaces, i.e. every `{%foo}` token binds to the same word without regards to context. However, claw code has potential to model the namespace concept in a simple way as a configuration of the textual view and editor. For example, `swap` might desugar to `\{%xyzzy:swap}` depending on the configuration. This would also extend to desugaring of numbers and literal values, e.g. `2/3` becomes `2 3 ratio` becomes `2 3 \{%xyzzy:ratio}` (further expanding `2` and `3`).

Modeling namespaces as syntactic sugar at the editor layer doesn't tolerate ambiguity. We will only have one namespace for any given volume of code. Using just one namespace for a program or REPL should be sufficient for most use cases. However, there are ways to represent multiple namespaces, e.g. `#foo:` for a modal switch, or `(#foo: ...)` for an inline region. (An editor could also render in distinct styles code from different namespaces.)

Namespaces greatly augment exploration and extension of our environment models. A very simple environment might be `(stack*(hand*1))` such that we store literal values on the stack and the hand is an auxilliary stack to simplify data plumbing (with `take` and `put` words). A more sophisticated environment might model multiple stacks and workspaces, or even a complete filesystem. Without namespaces, it would become very difficult to experiment with REPLs and command lines for multiple environments.

### Claw Code as Syntactic Sugar for AO

From the prior two points on namespaces and literals, it is evident that I'm leaning towards a definition of claw code as a very simple syntactic sugar above AO code. The main weakness here is that I'll be depending on words like `integral` and `rational`, thus content not actually generated via claw interfaces is unlikely to translate into useful claw code. That said, the pseudo-literals of ABC at least aren't too difficult to read.

The syntactic sugar design meets most of my design goals. It's transparent, simple, predictable, direct. We can easily record claw REPL sessions and histories into a dictionary, e.g. as a list of blocks for future testing and refactoring. Round tripping of code is straightforward, desugaring into AO or parsing this generated AO code back into claw. Binding to a dictionary is inherited easily from AO. Claw structurally preserves the concatenative, composable, and streamable properties of ABC, i.e. because it expands directly into the AO subset of ABC. 

There is great potential to extend the set of literals with extended views of claw code. I'd be interested in introducing at least good support for sequences, towards a goal of collections oriented programming.

So, I'm feeling good about this design so far.

## Claw Shell and Effects Model

AO and Claw code are purely functional. However, claw is intended for command-line contexts. Each 'command' might be a `state → state` function that is applied to update the state of a shell. This state value may contain stacks and workspaces. Feedback to the user is achieved by rendering the state, or a partial view of it, for the user. A Forth-like REPL, for example, might render the top few values on the stack.

A command language, to be useful, must enable users to orchestrate external resources beyond the local state of the command shell. A user might update a database, query a web server, display an image to screen, control a robot. To achieve these effects, we will assume a background agent is able to observe and influence our shell state between user commands. As a simplistic case, our background agent may integrate a mail service, and our state may include an inbox and outbox. By adding messages to our outbox, we could send messages to influence external systems. By receiving messages with our inbox, we could observe external resources to help make new command decisions.

To orchestrate multi-step behaviors (e.g. query a database then, depending on the result, take one action or another), we must automate our handling of received messages. We need a behavior function of a type similar to `(message*state)→state`. This function could process messages immediately, modify state, optionally update an outbox. This function might record a subset of messages into an inbox or log for human perusal. By rendering state after updates by asynchronous messages, rich curses-style and graphical command shells are viable.

Usefully, the purely functional nature of updates ensures they are atomic. We can reject messages that cause runtime type errors or assertion failures and return to our original state. We may cancel processing of a message to prioritize a user command, then retry later. On the other hand, performance may become an issue, with shell state becoming a bottleneck. Users may need to offload the most expensive computations via the effects model, such that the shell remains relatively more responsive.

### Abstract Virtual Machine (AVM) and Network Model

Messaging requires addressing. To receive a message, a shell needs an address. In general, we might want for this address to include some context information, i.e. such that we can route a received message to the correct subprogram. To send a message, a destination address is required. A fresh shell must be informed of which resources are available (or at least where to find them, e.g. via registry). 

These problems are addressed, more or less, by the [AVM](NetworkModel.md) and associated network model. The AVM behavior function has the form `(InMsg*State)→(OutMsgList*State)`. This is very similar to the `(message*state)→state` mentioned above. One motivation for using an output message list as opposed to an outbox within state is that we can easily recognize read-only queries on the state and extract additional parallelism or even replication of the machine. 

AVM input and output messages each have two parts. An input message has the form `(context*content)` where context is a value securely controlled by our AVM, and content is a value provided by the the sender. Output messages have the form `(capability*content)` where capability includes a network address and a cryptographically opaque context. To create capabilities, an AVM receives a *self* function, of type `context→capability`. This way, outgoing messages may contain capabilities to receive responses. 

Every AVM is formed of a triple: (state, behavior, signal). The signal value is a context used by the AVM host for generic life cycle purposes: init, halt, suspend, wakeup, etc.. A fresh AVM is initialized by signal with information about itself and its environment. Signals may serve other ad-hoc purposes, e.g. to indicate that an AVM should favor a power saving or low bandwidth modes.

### Command and View Capabilities

A claw command has type `state→state`. This has nice properties for concatenation, composition, streaming. The claw namespace for the command will generally depend on the state type. We won't want too many state models, so we'll probably standardize just a few common structures for most purposes.

To deliver a claw command, we reify it into a block, i.e. `[state→state]`, then deliver it together with a `(capability * [state→state])` pair. This capability routes state update command to an appropriate AVM. Based on context, the AVM will gather the appropriate states together, apply the function, then scatter the states back to their original locations. Usually, the context will indicate both some shared state and a workspace for the user. Effects beyond state updates are left to discretion of the AVM. We could certainly model an outbox, or even a hierarchically embedded AVM.

In addition to command capabilities, we will need view capabilities. View capabilities should probably support both subscriptions and queries, and be compositional in useful ways. If you hold a command capability, you should probably have an associated view capability (because blind commands are usually bad commands)... but the converse is not necessarily true.

## Secondary Design Thoughts

### Multi-Media Claw 

Mixed text and media can be very interesting in some programming contexts - e.g. xiki, iPython notebooks, spreadsheets, tangible functional programming, live programming. Claw code is textual and probably shouldn't directly support structured multi-media content. However, an interesting possibility exists: a structure editor can simply *expand* in place the definitions of some words, allowing us to view and edit the word inline (perhaps subscribing to external updates on the same). Thus, this would effectively allow embedded media and shared resources within claw code without hindering access to the plain text fragments.

Conversely, a structure editor for AO might render otherwise opaque blocks as fragments of claw code.

Expansion of words for inline editing could be guided by simple naming conventions.

### Late vs. Early Binding of Words

Our claw comands are bound to a dictionary. But do we expand the definitions of words at compile time? Should we wait until they're actually processed? The difference here impacts behavior when we update our dictionary. Assuming interaction with an outside world, I think early binding may be more appropriate. Late binding makes much less sense in a network context. 
