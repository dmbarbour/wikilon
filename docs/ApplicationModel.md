
# Wikilon's Application Models

Wikilon is intended as both a software platform and development environment. 

As a software platform, Wikilon should be able to directly host wikis, forums, blogs, image canvases, document editors, REPL sessions, spreadsheets, chatrooms, interactive fictions or multi-user dungeons, and other applications or web services. As a development environment, Wikilon must support edit sessions and layout, debugging sessions, and compilation of applications for execution elsewhere. Development environments might be considered an important use-case of hosted applications.

Relevant concerns include *state* and *security*.

I have decided to focus Wikilon exclusively on *dictionary applications*. 

A **dictionary application** represents all of its state within the AO dictionary. This representation of state must be conveniently structured, well typed, and meaningfully executable. Dictionary applications come with many powerful benefits: 

* application state is persistent, portable, versioned
* live coding and continuous testing become implicit
* dictionary compression of state is uniformly available
* everything is visible and indexable for debugging

The disadvantage is that our applications cannot directly *push* information. This can be mitigated in many cases by using some simple strategy to pull information (polling, long-polling, Comet, subscription, etc.) and by relying on external agents to push updates occasionally. But it does force us to favor a more RESTful architectural style. 

The *command pattern* or append-only log is widely useful for dictionary applications. Expressed within a dictionary, this might look something like:

        @foo.v0 (init foo)
        @foo.v1 {%foo.v0}(command to update foo)
        @foo.v2 {%foo.v1}(another update command)
        @foo.v3 {%foo.v2}(yet another command)
        @foo {%foo.v3}{%fooType}

With this pattern, we gain several features: unlimited undo, uniform cloning, back-in-time debugging, visible command histories for abstraction and refactoring, cache-friendly computations for common update patterns (i.e. assuming we mostly add commands and undo or tweak recent commands). Taken together with an annotation to stow large values into VRefs, it is feasible to construct truly massive objects (e.g. filesystems, databases, game worlds) within a single dictionary object without massive RAM overheads.

Keeping a complete history is reasonable for many applications, especially those primarily driven by coarse-grained explicit human interactions (like a forum or mailing list). 

But if histories grow too large for comfort, there are some strategies we can apply to shrink them. We could rebase objects, e.g. losing the first K updates and combining them into a new initial state (modeling a limited history window). We could perhaps combine every other update, simplify, and try to refactor common patterns (an exponential decay model). 

compute a current version.

If a history threatens to grow to ten thousand words, that might be a bit too much for comfort. We could perhaps mitigate this by occasionally rebasing the object (e.g. drop the first K versions, model a limited history window) or by combining every other update and simplifying or refactoring (e.g. exponential decay, losing intermediate states), or by trying something different (e.g. a log-structured merge tree instead of an append-only log). But 


For busy objects, where histories grow to tens of thousands of steps, this may become far more than is reasonable and comfortable. This could be mitigated. For example, we could could rebase objects, take a snapshot of its state, lose the history that led to it. Or we could try exponential decay, e.g. periodically cut large histories in half, combine every other command, apply simplification and refactoring techniques (dictionary compression). It is also feasible to try some alternative designs that more naturally allow eliminating some history, e.g. perhaps take inspiration from a structured log merge trees instead of the append-only log.



 might have some absurd storage overheads in the long run, even if we only preserve the more frequently used versions in the cache. This  by 

This might be mitigated by 

Interestingly, potential exists to model *very large* applications. In particular, 

Further, for Wikilon I plan to support a `{&stow}` annotation, which allows us to push large values to disk (i.e. leveraging VCache VRefs). 




We can view historical states, or perhaps even animate them. Working with *very large* values is feasible if we support something like a `{&stow}` annotation to push content out of main memory.




If taken together with a `{&stow}` annotation that allows us to push content out of main memory

 and we gain several more features. The common update pattern, adding one word and redefining the head, is very cache friendly. This allows us to compute and retain only the active 'heads' of our applications. We can easily fork our applications from within the dictionary. The command history remains available for mining patterns and refactoring them into provably useful new commands. 

We can recompute and animate an object's historical state for back-in-time debugging. 

, though it isn't always a best fit. Further, this allows us to model updates as commands

It's an especially good fit for REPL sessions, where viewing the command history is convenient.

The mutable objects are easily forked. The update 

, where it provides trivial access to command his. 

This works for almost any object, really, though AO can only express purely functional objects.




T

semantic compression of state is easy to automate. 


## Dictionary Applications


Dictionary applications offer several immediate benefits:

* application state is easily persisted, ported, and versioned
* implicit live coding, updating definitions to recompute state
* implicit dictionary compression model for application state



However, 

hosted applications serv


We design representations that are both conveniently structured and meaningfully executable. We use Wikilon to provide several immediately useful *editable views* of the dictionary, up until we can bootstrap construction of new views.

For example, a simple edit session might be expressed by a word whose definition is a sequence of `"key" [word]` pairs. This would allow us to name and load a session (loading the appropriate words for editing), while 'executing' the session would provide us a simple record-of-functions. A more sophisticated edit session might involve expressing box-and-arrow diagrams with layout and connectivity that can be presented upon a canvas or compiled into a larger function.

A persistent console or REPL session might be modeled within a dictionary using a simple sequential command structure:

        @repl:sessionId.0 {%repl.new}
        @repl:sessionId.1 {%repl:sessionId.0}(first command)
        @repl:sessionId.2 {%repl:sessionId.1}(second command)
        @repl:sessionId.3 {%repl:sessionId.2}(third command)
        @repl:sessionId {%repl:sessionId.3}

This command pattern generalizes to almost any mutable object. And it has a many convenient properties. We can easily cache computed values and extend the repl session. We can easily view the command history and discover common patterns to refactor into new words. We can easily 

We can easily view the command history, r

 that might be updated by an append-only log of commands. And it has many convenient properties.


 e.g. it's easy to view the command history, or fork the REPL.

It might not  be the best representation for a specific application

 where we have a mutable 'head' and sequential commands. However, 



Forums or REPL sessions might be modeled by monotonically adding words for each thread or reply, implicitly computing values. Mutable objects might generally be modeled using a dictionary command pattern:

        @foo.v0 (init foo)
        @foo.v1 {%foo.v0}(command to update foo)
        @foo.v2 {%foo.v1}(another command to update foo)
        @foo {%foo.v2}{%fooType}

This command pattern would allow easy use of cached computations, e.g. such that recomputation is only needed if we change how the commands or initializers are defined. Wikilon supports a simple annotation (currently `{&stow}`) to push large values onto the disk, so it is feasible to efficiently construct large objects including filesystems or databases.



include layout information, and perhaps even connectivity defining an FBP-like diagram or similar. 


, while a more sophisticated session model might express FBP-inspired diagrams with layout and connectivity (the session can be compiled into a larger function).




 might just be a list of `[word] "word"` pairs, but more sophisticated options might involve expressing 

 expressed as 
We can leverage structured definitions and editable views. An edit session might involve an list of `[word] layout` pairs. A forum might be modeled by adding a word for each thread and each reply. A mutable object might be represented using a dictionary-embedded command pattern so we keep previous versions until we're ready to destroy them:

Dictionary applications have a lot of advantages. They are very RESTful, which is a good fit for Wikilon. The application state becomes very portable, easy to import, export, fork, or share. We gain live coding and continuous testing implicitly: updates to a word propagate through the dictionary, and errors become immediately visible within our applications. The commands themselves

Applications implicitly serve as *continuous tests*.


As a development environment, Wikilon must provide convenient ways to edit and compile programs for external use. As a software platform, Wikilon be able to host a variety of applications including wikis, spreadsheets, blogs, chat.


The distinction between development environments and software platforms is what kind of *state* we're handling. Hosted applications and web services tend to be very stateful, e.g. maintaining databases and filesystems. 

Development environments use state to track edit sessions and layout. Software platforms have state for every stateful hosted application.

With hosted applications, 

With hosted applications, 






 that hosted applications will frequently be *stateful*, so we need some way to represent the state.



My work on [command language for awelon (claw)](CommandLine.md) offers a viable basis for extensible syntax, based on bi-directional translation between high level code (for humans) and lower level bytecode (for machines). I currently focus on numbers (integers, decimals, ratios) and inline literals appropriate for a command line, but this same idea could be extended to structured programming (e.g. recognizing `[cond] [body] while_do_` as a while loop, and the like). 

This approach has an advantage of simplicity. Simplicity shouldn't be underestimated. OTOH, this does limit our ability to represent and manipulate ad-hoc objects within the dictionary, and it pushes all staged evaluations to the optimizer. Though we may use annotations to guide partial evaluations.


We could `HTTP GET` our compiled representations, and we could support a variety of cross-compilers (e.g. compiling to JavaScript or C).

As a software *platform*, Wikilon 


 compute a block of bytecode then cross-compile it (perhaps to JavaScript or C) we could access 


Wikilon can support several kinds of applications, leveraging the dictionaries and potentially a little extra state. At this time, I've identified a few very broad classes of application that Wikilon could or should support, allowing developers to mostly make up the differences here.



## Dictionary Applications

It seems feasible to leverage a dictionary in a manner similar to iPython Notebook or a spreadsheets. In this case, the user experience becomes an edit-eval-render cycle, using information primarily from the dictionary. 

I could have a spreadsheet where every 'cell' is simultaneously a structure-editor and a 'lens' on the original structure, thus enabling multiple ad-hoc views of any module. If the dictionary evolves to include a lot of world data and content, the dictionary itself might provide interesting inputs to view through such lenses. In context of CSCW, I could even support multiple concurrent views and edits of this structure, assuming I'm careful about it. 

The simplicity and purely functional nature of ABC should make this entire approach a lot easier than iPython notebooks.

To render functions, operating on `void` might simulate abstract interpretation, and we might also leverage approaches similar to tangible functional programming. We might also use annotations to indicate intermediate rendering points, perhaps under different named 'views' that can be toggled by the viewer.

But there are many hurdles to clear for this approach, such as making it perform well.

## Cross-Compiled Applications

One basic mode for Wikilon as a development platform is to *compile* applications for use elsewhere. In this case, we specify the compiler and application - each of them functions - in a URL for HTTP GET. Typically, these links will be easily constructed. We may also need to specify a dictionary and version number. The 'build' process is implicit, highly cacheable, and performed by the Wikilon server.

For these apps, nothing is directly run on Wikilon other than the compiler function, which is pure or close enough (maybe leveraging memoization or ABC's linker model). There are no stateful requirements other than maintenance of the dictionary. There is no entanglement between the dictionary and the application after compilation. The application is downloaded for use elsewhere.

The encoding of functions in a URL can be pretty straightforward, perhaps using base64 if we need more than a single word. Any user-defined syntax is feasible in a base64 encoding. By encoding ad-hoc functions in URLs, it becomes easier to share or bookmark content up to about the size of an AO module or command line, without always being asked to generate a new wiki page.

"Application" here doesn't need to be executable. It might be documentation, music, video, etc.. It's just something that requires an additional compilation pass after we hit the ABC bedrock.

## Hosted Web Service Applications

As an IDE and software platform, Wikilon should be able to directly execute at least a useful subset of applications. I'd like to focus on those requiring persistent state and network access - e.g. web services, publish-subscribe systems, orchestration models, internet of things. I have additional desiderata here. 

* **live programming**: edit code to modify behavior at runtime
* **extensibility**: add features by adding code, shallow modification
* **portability**: control *entanglement*; may migrate app or compile to server 
* **maintainability**: easy debugging, resilience, persistence, administration
* **securability**: can control who administrates any given application

### Abstract Virtual Machines for Extensibility, Portability

Extensibility can be achieved by sharing stateful resources, e.g. a common database or queue or tuple-space or demand monitors. Unfortunately, this sharing entangles components. To control entanglement, we can partition resources, such that each partition may be internally entangled but is cleanly separated from other partitions. Communications between partitions are then subject to delays and disruptions. Modeling potential disruption allows a partition to be migrated to another physical machine. Each independent application uses an independent partition. 

Each partition is essentially an abstract virtual machine. Eventually, we might even compile these abstract virtual machines (or subsets thereof) into unikernels. But for now it's sufficient that our abstract machines operate within one Wikilon instance or can migrate between Wikilon servers.

More thoughts:

* [state model for AVM](StateModels.md)
* [network model for AVM](NetworkModel.md)

I've been refining my thoughts and designs over a few weeks, so don't expect full consistency.

### Machine Layer Resources with Stable Identity for Live Programming

*Live programming is of primary importance.* A principle of Awelon project is that all long-running behaviors have human-meaningful, human-modifiable representation, such that modifying the representation causes corresponding change in behavior. This gives humans comprehension and control over processes and systems, helps unify PX and UX, and supports intuitive physical metaphors. Securability is then addressed by controlling access to the representation.

Live programming requires stable identity for stateful resources, such that code and programmed behavior may change independently of state. In context, this suggests our stateful resources are modeled as belonging to our abstract virtual machines - i.e. no `new StatefulObject()` created by code because that clearly lacks stable identity. Each abstract virtual machine must also accept changes to its defined behaviors, perhaps via RDP or RESTful update. 

The dictionary might be available as an external resource for some machines.

### Atomic Updates, Batched Messages

Atomic updates greatly simplify maintenance. The developers needn't think about partial failures, and may focus on consistency within any given step. Explicit support for partial failure might legitimately be modeled as a side-effect, e.g. a `{try}` capability or monadic effect. For updates to remain atomic, of course, we must batch all the outputs during a step and process them only after successful commit.

Since we're batching messages anyway, it seems reasonable that the basic communication primitive between machines should be 'batched messages'. The ability to receive batches of messages associated with atomic operations in other partitions is convenient for reasoning about consistency, i.e. you only see consistent snapshots of other machines. Further, it can vastly improve performance relative to individual messaging.

### Copy on Write or Purely Functional Machines

To further simplify maintenance and debugging, the ability to take cheap snapshots of a full machine - for rollback and recovery, for historical debugging, for exploration and 'what if' development - is also very convenient. So I'd like to focus on state models suitable for copy-on-write updates and structure sharing between snapshots. It turns out, these same state models are probably also suitable for purely functional implementation, which would be very convenient for modeling abstract virtual machines within Wikilon. 

### Flexible Networking

Communication between machines, batched messages, is essentially an abstract primitive. It could be implemented many ways, simultaneously - e.g. an ad-hoc mix of HTTP and web-sockets and AMQP. I think this could be very useful because it allows us to migrate apps and adapt to whichever communication protocol they need based on where they're hosted. Applied carefully, I can ensure that messaging is idempotent and monotonic over pretty much any medium. E.g. HTTP PUT to an incremental address is favorable to HTTP POST. 

I may need ad-hoc sockets or similar for networking with external resources. But I don't want to focus application development on low-level communications. It may be useful to model layers of abstract virtual machines, with lower layers adapting communication protocols and other low-level details.

Disruption must also be easily modeled. I have a few ideas there, e.g. based on logical connections. It might be useful to estimate logical latencies and clock drift, both for imperative code and RDP.

### Unikernels

Long term, I'd like the ability to migrate an application to a unikernel... and back again. I think this is viable if we keep the model simple enough.

