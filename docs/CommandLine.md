
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

## Designing Claw Code

General points to help guide the design:

* need effective support for rational and decimal numbers
* enable flexible extension of programming environments
* transparent, simple, predictable, direct AO translation
* easy to preserve claw sesssions into an AO dictionary 
* round tripping, ability to recover claw code from AO
* implicitly bound to AO dictionary for definitions and data
* concatenative, streamable, composable, purely functional
* asynchronous effects bound to abstract [network model](NetworkModel.md)

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

### Claw as Syntactic Sugar

From the prior two points on namespaces and literals, it is evident that I'm leaning towards a definition of claw code as a very simple syntactic sugar above AO code. The main weakness here is that I'll be depending on words like `integral` and `rational`, thus content not actually generated via claw interfaces is unlikely to translate into useful claw code. That said, the pseudo-literals of ABC at least aren't too difficult to read.

The syntactic sugar design meets most of my design goals. It's transparent, simple, predictable, direct. We can easily record claw REPL sessions and histories into a dictionary, e.g. as a list of blocks for future testing and refactoring. Round tripping of code is straightforward, desugaring into AO or parsing this generated AO code back into claw. Binding to a dictionary is inherited easily from AO. Claw structurally preserves the concatenative, composable, and streamable properties of ABC, i.e. because it expands directly into the AO subset of ABC. 

There is great potential to extend the set of literals with extended views of claw code. I'd be interested in introducing at least good support for sequences, towards a goal of collections oriented programming.

So, I'm feeling good about this design so far.

### Claw Side Effects

I want a *useful* command language. This requires ability to interact with the outside world - e.g. command a robot, access the internet or update a database, impact what is rendered on a screen, etc.. However, claw is limited to the purely functional structure of AO code. Effects cannot be performed directly, but might be requested in the output of a function (e.g. in a monadic sense).

A promising approach to integrating side effects is to treat at least part of the command line environment as a shared data structure. Each user command may atomically manipulate the shared structure. Between user commands, another agent has opportunity to observe this structure, and further manipulate it to record feedback. Communications and effects are necessarily asynchronous, based on manipulation of the shared state. Also, because updates are purely functional, they're effectively atomic and it is easy to cancel an update or ensure user commands have priority. Parallel computation is feasible if we're willing to risk greater amounts of rework.

That said, we don't necessarily want to model the entire environment as shared. 

Users and agents benefit from managing their own private workspaces and precisely controlling when their actions might communicate information. So we may want to divide our environment in two spaces, e.g. `(privateState * sharedState)`. Alternatively, we could favor a shared state model that hides the agent's view behind a simple function or interface. An [abstract virtual machine](NetworkModel.md) (AVM) could contain private spaces directly but still have discretion to share this structure through messages. That said, I feel there is benefit in ability to structurally guarantee that at part of our environment is private without inspecting a behavior function. Thus, I favor a strong separation between private and shared state.

While shared state could be anything, a promising option is that shared state is simply an AVM (a state, behavior, signal triple). This would allow direct interaction between the REPL and other machines on an abstract network. Feedback would always be asynchronous.

## Secondary Design Thoughts

### Multi-Media Claw 

Mixed text and media can be very interesting in some programming contexts - e.g. xiki, iPython notebooks, spreadsheets, tangible functional programming, live programming. Claw code is textual and probably shouldn't directly support structured multi-media content. However, an interesting possibility exists: a structure editor can simply *expand* in place the definitions of some words, allowing us to view and edit the word inline (perhaps subscribing to external updates on the same). Thus, this would effectively allow embedded media and shared resources within claw code without hindering access to the plain text fragments.

Conversely, a structure editor for AO might render otherwise opaque blocks as fragments of claw code.

Expansion of words for inline editing could be guided by simple naming conventions.

### Late vs. Early Binding of Words

Our claw comands are bound to a dictionary. But do we expand the definitions of words at compile time? Or should we wait until they're actually processed? The difference here impacts behavior when we update our dictionary. Assuming interaction with an outside world, I think early binding may be more appropriate. Late binding makes much less sense in a network context. OTOH, there are benefits to late or continuous binding, such as it's easy to render or update the AVM behaviors.

## Initial Claw Definition

The basics of claw code are words, numbers (minimally integers, rationals, decimals), inline texts, blocks, and escaped content. Tokens and multi-line texts are escaped individually, and we may also escape sequences of ABC primitives. Many useful ASCII characters remain available for extensions to the claw code, e.g. `{}()<>`, and many more UTF-8 brackets are available if needed. I'd like to eventually use some of these for lists, JSON, XML or other basic structured values.

