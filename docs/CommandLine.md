
# Command Language for Awelon 

Awelon project and Wikilon would benefit from effective command line interfaces, e.g. to issue rapid queries and commands. In context of Wikilon, providing console-like services through HTML forms or JavaScript should not be difficult. The underlying [Awelon Object (AO)](AboutAO.md) code is unsuitable for this application in its raw form.

But I think a relatively thin layer above AO could be suitable:

* write words easily: just use the word, `swap inc mul`
* inline text and number literals: `42 "foo"`
* easy and unambiguous access to bytecode, e.g. `\vrwlc` 
* stable environment against literals, e.g. `(stack*env)` pair

If you know about the evolution of Awelon project, the above should be familiar: it's a minor tweak of the [original definition of Awelon Object](https://github.com/dmbarbour/awelon/blob/master/AboutAO.md). The original AO was weak for staged programming, visual DSLs, and structured programming, but it was well suited for command line interfaces. The result was a very Forth-like language. I'm adapting this, but changing the bytecode escape to keep it unambiguous (no risk of confusing `\` with an ABC operator). 

## Naming: claw?

What should I call this proposed language? The name Awelon Object is taken, and AO today is a better fit for the connotations surrounding '[object code](http://en.wikipedia.org/wiki/Object_code)'. So far, my best idea is **claw**: Command Language for Awelon (or Command Line for Awelon, depending on context). The name also puns nicely. I'll run with this unless someone suggests a better idea. 

## Round Tripping?

A major design decision is whether to support round-tripping. If claw code is one way, i.e. converting into an ABC subprogram, that greatly simplifies some things such as introducing support for ad-hoc literal types. (Support for flexible literal types seems especially relevant


## Commanding What?

Claw code will somehow bind to a dictionary, for easy processing. One option is that we treat Claw code as a primitive 

## Literals

A reasonable question is whether I want to support the inverse, rendering ABC code into a command line format for display purposes. If so, I will need to be cautious about how literals are handled, e.g. using the implicit `\l` after each number or text literal to place it on a common stack (as I did with the original AO). Otherwise, I can try something like having `42` followed implicitly by `{%integer}` and I can potentially extend the number types and other literal types.

In 2015 June, Awelon Bytecode was updated to exclude rational numbers. At this time, ABC only supports integers. The intention is to model rational and floating point numbers in libraries, and then to eventually leverage this via compiler recognition or ABCD to get native performance (or nearly so).

But I expect rational and decimal numbers will have much value on the command line. So this creates some extra pressure towards the former approach, where literals are processed by words from the dictionary rather than in an implicit manner. This does have some advantages: the set of literals is potentially more extensible if processed by dictionary words.

With words, we could still force words onto a stack. But I'm not sure this would gain us very much.


## Value Types

The original AO supported only texts and numbers.

I'm not entirely sure what I want for Claw code. 



Originally, I did support fractional and decimal numbers, e.g. `3.141` and `2/3`, but  

  But good support for other value structures such as lists, maps, maybe vectors of some sort, would have been convenient. How much can I do, while keeping the command language thin and simple and suitable for one-liners?

I have access to many ASCII brackets: `{}[]()<>`. I also can grab some useful separators, such as `,` and `|`, though I think I'd prefer to avoid them (commas are noisy). I'll probably favor `[]` for blocks, as I did originally. But rather than optimize representation of tokens, I'll probably use `\{token}`, representing tokens as part of inline ABC. This will keep `{}` free for lists.

To keep the command language very thin, I probably don't want sophisticated value models built in. In particular, something like maps or tries are probably a bad idea for literal types. But what could I handle?

* lists  λa.μL.((a*L)+1)
* association lists; list of (key*value) pairs
* stacks (a * (b * (c * (... * 1))))
* pairs (a*b) in general
* unit, booleans, sums

One option is, instead of using different brackets, use different prefixes or suffixes to indicate how a collection is structured. E.g. `(1 2 3 4)L` might be a list while `(1 2 3 4)S` might be a stack and `(1 2 3 4)V` might be a vector.

I'm not really sure where I want to go with this at the moment. For now, I think I'll stick to texts and numbers as I did before, and simply reserve `{}(),;|` for later extensions. Potentially, I could support a very YAML-like experience for working with large structured values in text.




## Side Effects

A significant issue is **side effects**, and how to integrate them with our command lines. I want a *useful* command line system, which means access to real-world databases and HTTP and side-effects and so on, and the ability to manage tasks or orchestrate distributed behaviors. 

It seems wise to leverage the [AVM and network model](NetworkModel.md). Idea so far: 

        type Shell = (stack * (AVM * ext))
        type AVM = (State * (Behavior * Signal))
        type Behavior = (InMsg * State) → (OutMsgList * State)
        ... (see network model) ...

Every shell contains an abstract Awelon virtual machine (AVM) at a stable location. The AVM is responsible for side-effects and all interactions with the outside world. A Shell may be hosted within a larger AVM, which knows from context to route some messages to the shell's internal AVM and to apply command stream messages to the entire shell. Excepting the user's command stream, incoming messages are received by the internal AVM's behavior function.

Outgoing messages from user commands are achieved by arranging for the AVM to deliver messages in response to a future signal. This might be achieved by modeling an outbox within the AVM. After each user interaction, the contained AVM will be signaled.

While users are free to directly view and manipulate the contained machine, they also have a stack and an extended user environment (stack and ext, above). These provide a stable workspace and staging area for actions on the machine and REPL-like behavior. To make effective use of the stack, our command language will implicitly inject `\l` after every numeral, literal, and block value. I.e. `7` will be the same as `\#7l`, such that `7` is added to the stack while keeping the AVM and extended user environment at relatively stable locations. The extended user environment is available for use cases where a stack is insufficient, e.g. if developers wish to model workspaces, clipboards, or graphical UIs.

We'll also need a good 'default' AVM. But this could feasibly be left to the dictionary.

## Dictionaries

A potential issue is that our command lines will need easy access to words specific to the structure of the command line. If we want to experiment with many command line structures (i.e. with different AVM structures and extended user environments), we'll end up with many collections of similar words in a dictionary. This isn't necessarily a bad thing, but it might make selecting one version more difficult.

One option might be to use namespaces. I'd rather avoid namespaces at the language layer, but I think we could introduce them at the presentation and interaction layers, e.g. for tab completion and rendering words and so on. 

## Late Binding?

When using a claw shell (or AVM in general) I have many options for binding to dictionaries. An interesting question is whether we should bind to dictionaries at the time the code was written or later, at the time it is interpreted. The latter would provide an interesting sort of live coding, though may require special attention if we gain any capabilities to send messages outside contexts bound to the same dictionary.

