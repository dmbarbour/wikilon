
# Command Language for Awelon (claw)

Awelon project and Wikilon would benefit from effective command line interfaces, e.g. to issue rapid queries and commands. In context of Wikilon, providing console-like services through HTML forms or JavaScript is not difficult. The underlying [Awelon Object (AO)](AboutAO.md) code is unsuitable for this application in its raw form.

But I think a relatively thin layer above AO could be suitable:

* writing words should be easier than writing bytecode
* inline the text and number literals: 1.234 2/3 "foo"
* easy and unambiguous access to bytecode, e.g. \vrwlc 
* stable environment against literals, e.g. `(stack*env)` pair

If you know about the evolution of Awelon project, the above should be familiar: it's a minor tweak of the [original definition of Awelon Object](https://github.com/dmbarbour/awelon/blob/master/AboutAO.md). The original AO was weak for staged programming, visual DSLs, and structured programming, but it was well suited for command line interfaces - a very Forth-like language. I'm changing the bytecode escape to keep it unambiguous (no risk of confusing `\` with an ABC operator). 

What should I call this proposed language? The name Awelon Object is taken. And a command line language isn't really [object code](http://en.wikipedia.org/wiki/Object_code) anyway (the current AO is much closer to that term). So far, my best idea is **claw**: Command Language for Awelon (or Command Line for Awelon, depending on context). The name also puns nicely. I'll run with it unless someone finds a better idea.

A significant remaining issue is **side effects**, and how to integrate them with our command lines. 

It seems wise to leverage the [AVM and network model](NetworkModel.md). 

Minimally, if we want our commands to do anything useful, we need both the ability to send and receive messages. This way, we can query and observe external systems then influence. Sending messages might be modeled via an outbox - i.e the user pushes messages into a stateful outbox, then the outbox is periodically dumped (perhaps by *signal*) onto the network. An AVM intrinsically consists of a triple: `(state, behavior, signal)`. Ideally, the user would control all three, ensuring that any console session is tailorable into arbitrary machines.

It's also important that our user commands don't *accidentally* disrupt asynchronous background behavior. 

The original AO's approach to numerals and literals was solid: each inline value such as `7` is implicitly followed by a `\l` operation to place it on the stack in a `(stack * env)` pair. This way, the environment preserves a stable location regardless of our actions on the stack. This behavior may easily be bypassed, e.g. by `7\r` or `\#7`. 


 Minimally, we need a stable workspace for the user.

So I propose the following as our basic claw environment:

        (stack * (workspace * avm))
        (stack * (workspace * (state * (behavior * signal)))






, which is implicitly processed between user commands (either we standardize the location of the outbox or we use 

 by knowing the location of the outbox

by sending a signal to dump the messages). Receiving messages could be modeled by an inbox, or more generically.

An AVM's intrinsic structure consists of a triple: . The beh


The user should, ideally, have full control over all three aspects at runtime to support live coding and so on.

However, how should we we expose this to users?

Users will operate in an environment that is frequently manipulated by asynchronous messages between user operations.

Obviously the stack cannot double as our 



While a stack is a convenient workspace, there are many cases where a user needs a bit more. I think having a stable user model as part of the environment will be very useful. So something like:

        (stack * avm * user)

Here, the user model may include clipboards and other content.

Our behavior function needs state with a relatively stable type to handle asynchronous messages. Since a stack's type will change every time a literal is added or removed, the AVM state needs to be part of the environment. 



The stack obviously cannot double as the AVM state. 


The 

The stack then doubles as a convenient workspace. The alternative is to implicitly follow each literal by a word, but that makes it a lot more difficult to optimize, reason about, and convert code. I'd rather

To keep claw very thin, simplify conversions and partial evaluations, etc.. I think it would be best if Claw doesn't introduce any dependencies on words that are not explicitly used. E.g. we won't compile `7 5 inc mul` to something like `#7{%lit}#5{%lit}{%inc}{%mul}`. But it's also important that we have a stable location for literals that doesn't break asynchronous messaging and so on. I believe the solution used in the original AO will also be good here: after each literal, we implicitly add `\l` to push it onto the stack. Developers may then cancel this behavior by writing `\r` after the literal. So `7\r` is not gaurded by the literal stack. Claw will perform a simplification pass so the `7\r` has the same type as `\#7`.


The main issues, I think, are literals and asynchronous messages.

If I write `7 5 inc mul`, then presumably I'm placing two literal values where I can reach them, incrementing the second one, then multiplying the numbers together. Every function such as `inc` and `mul` will need to know where to find where I placed these numbers. One option, since Claw is inherently bound to a dictionary, is to assume the dictionary also knows where literals should be placed, e.g. inject a `{%claw:numeral}` after each number. 

This would provide flexibility to our developers. But it would hinder stable conversions of structured values into claw, and it would introduce dependencies that aren't visibly obvious in the code. I think I'll favor instead 

w language (we cannot easily convert a number if it isn't followed by t use the word)

Our command language should be a very thin transform over AO code. Feasibly, I could introduce some hidden word dependencies in this transform, e.g. after each literal:

        7 1.23 "Hello"

            could become

        #7{%claw:integral}#123#100/*{%claw:decimal}"Hello
        ~{%claw:literal}




One question is how human actions should interact with asynchronous callbacks. One option is to prevent interference by providing users some additional state that is not accessible to the behavior function. But it 

 But the structure shouldn't be a simplistic `(state * (behavior * signal))` triple. Human commands - and, importantly, *partial* commands - must operate effectively on the state.

One option is to wrap the user environment around the AVM in a simple, stable way:

        (stack * (hand * (avm * ext)))

Then received messages are simply processed by the internal AVM. But in t


Receiving messages in the background should not break whatever the human is doing.

In general, we


User commands are associated with a dictionary. 

 the behavior function (and signal value) must be accessible and manipulable via the command language. Messages might generally operate on the same state as the user commands, but a behavior function could isolate incoming messages.



How, then, do we set our 'default'



* received messages
* user commands have access to the (state * behavior * signal) triple
* we may wrap access to this triple in a simple way to stabilize 
* 
* user commands may be followed by a signal message to dump an outbox
* we may need to tweak the 
* 
* after user commands, we may signal the 

* integrate useful, real network effects and queries
* side-effects observable at well-defined boundaries
* capability security to protect messages and methods
* live coding for processing of incoming messages
* transparently model purely functional consoles
* transparently host ad-hoc AVMs within a console (e.g. as 'jobs')

What is a reasonable minima for our Awelon Claw environment?

Well, let's start with an AVM.

* *behavior* - a function to receive incoming messages
* *signal* - a value to specify host signal messages
* *state* - a volume of state for message processing

Let's use this as the foundation. 




However, *user* commands won't pass through the behavior function. Instead, they operate directly on this AVM triple, and hence may modify behaviors or signal values. This ensures control remains with the user. But it does require some considerations: 

* we need a stable workspace (e.g. stack) for incomplete user operations
* we must acquire outgoing messages from the user (e.g. outbox)
* we need reasonable default behaviors and states

An AVM general learns about itself and its environment through signals from the host. So, we'll need to do the same here to learn abu

about a *self* function for callbacks and an *environment* function.






In order to receive messages from the environment, we need to know who we are:

* *self* - a function that generates callback capabilities

We also need a simple command language. So I believe:

* *stack* - a workspace to receive literal values








Those first three elements are essentially the AVM definition. We could still use the same AVM behavior type: `(InMsg * State) → (OutMsgList * State)`, where `State` is the entire Claw environment. Exposing the entire environment for message processing does permit badly written behavior functions to do silly things with the stacks and so on.

* *stack* - a workspace, a location for new literals





* *self* - a function to produce callback capabilities
* *env* - an association list of values and capabilities
* *hand* - an auxilliary stack or clipboard


The important feature is that the user's input happens on a higher level capability, one with full control of our environment. 



Using an outbox serves a useful role for a human user, but might not be relevant when processing messages. Our behavior function could use the same old `(InMsg * State) → (OutMsgList * State)` that AVMs use. Alter, we might treat sending messages instead as a signaled operation, e.g. a "send" message that gathers 

because the human user won't frequently be generating messages. We can treat them as a reasonably rare event.


When we start receiving callbacks, *stability* will suddenly become a major concern. However, I think it would be okay to leave the stability concern to our behavior function rather than create a lot of extra constraints at this layer. So:

* user action

 stable volume of state beyond the hand and stack. E.g. users might have multiple workspaces. However, le

I probably don't need anything other than these. Usefully, we can ensure that humans always have a useful degree of process control based on managing the behavior function and state.

to effectful is integration of asynchronous callback messages - e.g. to receive feedback from a command to grab a webpage - together with user actions in the same environment. 



I believe our human users will need a relatively stable environment, e.g. effective control over which 'parts' of the model are manipulated. We probably don't want incoming messages to start messing with the same stack and local model the human is using.



 So, my idea is that our console model essentially be a variant on our AVM model except with 


* the stack, hand, and local environment seen by the user are not seen by anyone else, to guarantee seen by the user are not seen provided to the user


So, let's say we have the following elements in our environment:

1. a stack to receive literals
1. a hand or secondary stack for convenience
1. a stateful workspace of tasks, inventories, etc.
1. an 'outbox' for user to place outgoing messages.
1. a 'self' function to construct callback capabilities
1. a behavior function to process non-user messages.
1. a generic signal context to receive host messages.

Essentially, we have a lot of similarities here with AVMs. The outbox is essentially the AVM's output message list, except we're going to keep it at a stable location to simplify user interaction with the outbox. A user 

There are similarities here with AVMs. The main difference is that we 

The main difference, I think, is the existence of an "outbox", which represents outgoing messages at a stable location in the environment. 

Conveniently, the latter 

It's worth noting that the latter five items are, more or less, the structure of an AVM. The main difference here is that our incoming messages are 

 The main difference is that the *outbox* now has a stable locati

        


To adapt this for human use, humans need a relatively stable environment: 

However, it doesn't make much sense to cast a human user in the context of a behavior function. W



However, from the user's view, I think this wouldn't work so nicely for our stable env


* side-effects are achieved through messaging
* effects are capability-secure and integrate nicely with AVMs
* locally, we can provide some control over how to process incoming messages
* 






* consoles should be directly usable with AVMs and networks
* side-effects should be achieved through messaging
* the basic side effect should be messaging
* consoles should 


 *there should be no side-effects* that are not modeled in term







 at the same time they issue commands. In this context, there isn't a lot of need for abstract conditionals. Process control is essential: loops execute immediately but should be easily halted or controlled. 

Textual commands are most useful for humans operating "in the now", i.e. manipulation of concrete and observable objects without a lot of conditions

There are many contexts where we might benefit from simple textual commands: consoles, shells, HTML text inputs. While [Awelon Object (AO)](AboutAO.md) code is not suitable for direct manipulation by keyboard, I believe a relatively thin 'skin' above AO may be sufficient for the job. 

The [original definition of AO](https://github.com/dmbarbour/awelon/blob/master/AboutAO.md) was more suitable for command 
, and was far more suitable for that purpose.



There are many contexts where textual input is convenient for quick queries and commands. 



## History

The  was more suitable as a command line interface language, and is very similar to what I've described above. However, it was unsuitable for many other roles I felt were important: effective support for visual DSLs, structured editing, staged programming. Also, while the original AO was a thin macro and linking layer above ABC, I feel modern AO is a closer fit for the connotations surrounding ''. Nonetheless, the old AO could feasibly be tweaked and fitted.

