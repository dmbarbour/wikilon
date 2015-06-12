
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

        2/3     desugars to     2 3 ratio       (or perhaps \#2#3 ratio)
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

From the prior two points on namespaces and literals, it is evident that I'm leaning towards a definition of claw code as a very simple syntactic sugar above AO code. 

This syntactic sugar design meets most of my design goals. It's transparent, simple, predictable, direct. We can easily record claw REPL sessions and histories into a dictionary, e.g. as a list of blocks. Round tripping of code is straightforward, desugaring into AO or parsing AO code back into claw. The binding to a dictionary is inherited from AO. Claw preserves the concatenative, composable, and streamable properties of ABC. Further, there is great potential to extend the set of literals.

So, I'm feeling good about this design so far.

### Multi-Media Claw 

Mixed text and media can be very interesting in some programming contexts - e.g. xiki, iPython notebooks, spreadsheets, tangible functional programming, live programming. Claw code is textual and doesn't directly support structured multi-media content. However, an interesting possibility exists: a structure editor can simply *expand* in place the definitions of some words, allowing us to edit it inline (perhaps subscribing to external updates on the same). Thus, this would effectively allow embedded media and shared resources within claw code without hindering access to the plain text fragments.

Conversely, a structure editor for AO might render otherwise opaque blocks as fragments of claw code.

Expansion of words for inline editing could be guided by simple naming conventions, or perhaps a memory of previous development sessions.

### Claw Side Effects

I certainly want a *useful* command language, where useful implies ability to interact with the outside world - e.g. command a robot, access the internet or update a database, impact what is rendered on a screen, etc.. 

So far, my best thought for integrating side effects is to treat our environment as a shared data structure, albeit with exclusive control. Each user command gains priority and exclusive control until it returns. Between commands, the structure may be processed by other agents in some simple, predictable way. Communications and effects, thus, are necessarily asynchronous and based on manipulation of state.

Any shared content must be relatively stable between the agents that share it. The user is one such agent. Conversely, users and agents both will desire relatively clean access to their own private state - e.g. for a user's stack, clipboard, workspaces. So we might want to operate on something like an `(agentState * sharedState)` pair. 

We would effectively pass our shared state from one agent to another. If we cancel an action that takes too long, we can return to our original state and be sure that no side-effects have become visible to other agents. Priority can be modeled by canceling a low priority agent in favor of a high priority agent. A lot of parallel computation is feasible if we're willing to risk greater amounts of rework.

The shared state could be anything - e.g. some control numbers, a shared filesystem model, or perhaps an [abstract virtual machine](NetworkModel.md) (a state, behavior, signal triple). And I expect the normal case is that there is only one agent other than the user.

Usefully, this approach to side effects is very simple to model or mock up in a purely functional context.






TODO
========







### Late Binding of Dictionaries?

Should we preserve linking `{%foo}` tokens within our REPL environments, such that we can observe updates from our dictionary?

## Initial Claw Definition

The basics of claw code are words, numbers (integers, rationals, decimals), inline texts, and blocks. Additionally, we'll support escaped 

I have access to many ASCII brackets: `{}[]()<>`. I also can grab some useful separators, such as `,` and `|`, though I think I'd prefer to avoid them (commas are noisy). I'll probably favor `[]` for blocks, as I did originally. But rather than optimize representation of tokens, I'll probably use `\{token}`, representing tokens as part of inline ABC. This will keep `{}` free for lists.

To keep the command language very thin, I probably don't want sophisticated value models built in. In particular, something like maps or tries are probably a bad idea for literal types. But what could I handle?

* lists  λa.μL.((a*L)+1)
* association lists; list of (key*value) pairs
* stacks (a * (b * (c * (... * 1))))
* pairs (a*b) in general
* unit, booleans, sums

One option is, instead of using different brackets, use different prefixes or suffixes to indicate how a collection is structured. E.g. `(1 2 3 4)L` might be a list while `(1 2 3 4)S` might be a stack and `(1 2 3 4)V` might be a vector.

I'm not really sure where I want to go with this at the moment. For now, I think I'll stick to texts and numbers as I did before, and simply reserve `{}(),;|` for later extensions. Potentially, I could support a very YAML-like experience for working with large structured values in text.


## Dictionaries

A potential issue is that our command lines will need easy access to words specific to the structure of the command line. If we want to experiment with many command line structures (i.e. with different AVM structures and extended user environments), we'll end up with many collections of similar words in a dictionary. This isn't necessarily a bad thing, but it might make selecting one version more difficult.

One option might be to use namespaces. I'd rather avoid namespaces at the language layer, but I think we could introduce them at the presentation and interaction layers, e.g. for tab completion and rendering words and so on. 

## Late Binding?

When using a claw shell (or AVM in general) I have many options for binding to dictionaries. An interesting question is whether we should bind to dictionaries at the time the code was written or later, at the time it is interpreted. The latter would provide an interesting sort of live coding, though may require special attention if we gain any capabilities to send messages outside contexts bound to the same dictionary.

