
# Command Language for Awelon 

Awelon project and Wikilon would benefit from effective command line interfaces, e.g. to issue rapid queries and commands. In context of Wikilon, providing console-like services through HTML forms or JavaScript should not be difficult. The underlying [Awelon Object (AO)](AboutAO.md) code is unsuitable for this application in its raw form.

But I think a relatively thin layer above AO could be suitable:

* write words easily: just use the word, swap inc mul
* inline the text and number literals: 1.234 2/3 "foo"
* easy and unambiguous access to bytecode, e.g. \vrwlc 
* stable environment against literals, e.g. `(stack*env)` pair

If you know about the evolution of Awelon project, the above should be familiar: it's a minor tweak of the [original definition of Awelon Object](https://github.com/dmbarbour/awelon/blob/master/AboutAO.md). The original AO was weak for staged programming, visual DSLs, and structured programming, but it was well suited for command line interfaces. The result is a very Forth-like language. I'm changing the bytecode escape to keep it unambiguous (no risk of confusing `\` with an ABC operator). 

## Naming: claw?

What should I call this proposed language? The name Awelon Object is taken. And a command line language isn't really [object code](http://en.wikipedia.org/wiki/Object_code) anyway (the current AO is much closer to that term). So far, my best idea is **claw**: Command Language for Awelon (or Command Line for Awelon, depending on context). The name also puns nicely. I'll run with it unless someone finds a better idea. 

## Side Effects

A significant issue is **side effects**, and how to integrate them with our command lines. I want a *useful* command line system, which means access to real-world databases and HTTP and side-effects and so on, and the ability to manage tasks or orchestrate distributed behaviors. 

It seems wise to leverage the [AVM and network model](NetworkModel.md). Idea so far: 

        type Shell = (stack * (AVM * ext))
        type AVM = (State * (Behavior * Signal))
        type Behavior = (InMsg * State) → (OutMsgList * State)
        ... (see network model) ...

Every shell contains an AVM at a stable location. The AVM is responsible for side-effects and all interactions with the outside world. Incoming messages are received normally by the behavior function. Outgoing messages from the user require some special attention: our user must somehow inject messages into the AVM's State such that they'll be added a future OutMsgList. We will signal the AVM after each user interaction. 

The stack serves a useful role as a staging area for user operations. Most user actions will occur on the stack. Literals and values will generally be added to the stack, i.e. by implicitly injecting a `\l` operation after each number, text, or block in the command language. Because a stack is very limited, the user environment may be extended (in the `ext` area) to contain workspaces, clipboards, and rich user models. The stack and extended environment are both invisible to the AVM, which gives us nice properties for stability, non-interference, and portability of the AVM.

With these different aspects, I think we have everything we need for a radically tailorable shell that can gradually become any machine users need. I think I've kept it very simple. It may be the case that most or all AVMs in Wikilon are grown in shells.

A remaining challenge is developing a good 'default' behavior and state for the AVM. The difficulty is increased a bit because we need easy access to the shell state, e.g. to find information about self or environment. A minimal model might use an inbox and outbox, and recognizes the signal to deliver pending messages from the outbox. An even more minimal default model might do nothing at all. For the latter case, it might be useful to provide users an easy ability to signal their shell (e.g. to reset or whatever). Default stack and ext are both unit.

## Binding to Dictionary

When using a claw shell, we have some options on how we bind words to meaning. One option is to preserve words within code and allow the meaning of words to change over time. This provides an additional opportunity for live programming, allowing meaning of words to change between processing commands or messages.

Conversely, it might be convenient to update and manage a dictionary via operations within a command shell. I'll need to give this a try.

## History

The  was more suitable as a command line interface language, and is very similar to what I've described above. However, it was unsuitable for many other roles I felt were important: effective support for visual DSLs, structured editing, staged programming. Also, while the original AO was a thin macro and linking layer above ABC, I feel modern AO is a closer fit for the connotations surrounding ''. Nonetheless, the old AO could feasibly be tweaked and fitted.

