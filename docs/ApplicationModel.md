
# Wikilon's Application Models

Wikilon is intended as both a software platform and development environment. 

As a software platform, Wikilon should be able to directly host wikis, forums, blogs, image canvases, document editors, REPL sessions, spreadsheets, chatrooms, interactive fictions or multi-user dungeons, and other applications or web services. As a development environment, Wikilon must support edit sessions and layout, debugging sessions, and compilation of applications for execution elsewhere. Development environments might be considered an important use-case of hosted applications.

Primary concerns as a software platform include *state* and *security*. 

I have decided to focus Wikilon exclusively on *dictionary applications*. 

A **dictionary application** represents all of its state within the AO dictionary. This representation of state must be conveniently structured, well typed, and meaningfully executable. Structure includes both bytecode and relationships between words. Dictionary applications offer many powerful benefits: 

* application state is persistent, portable, versioned
* live coding and continuous testing become implicit
* dictionary compression of state is uniformly available
* everything is visible and indexable for debugging

The disadvantage is that our applications cannot directly *push* information. However, we can utilize external software agents to pull requests/todos and push updates. This can even be low latency with Comet-like long polling or subscriptions. Dictionary applications are very RESTful in nature.

## Modeling State with Dictionary Applications

Dictionary applications are free to present editable views of bytecode. Such views are potentially convenient for representing form or document based user inputs.

However, dictionary applications are additionally free to *evaluate* code. This enables operation on structured values. An application might render a view of a value, or represent application state as a value. This allows us to push update logic from our views into our dictionaries, representing update commands as pure functions of type `AppState → AppState`.

The *command pattern* is especially useful in context. It provides several useful features: cache-friendly updates, universal undo, back-in-time debugging, visible command histories, trivial cloning. Expressed within a dictionary, this pattern might look something like:

        @foo.v0 (init foo)
        @foo.v1 {%foo.v0}(command to update foo)
        @foo.v2 {%foo.v1}(another update command)
        @foo.v3 {%foo.v2}(yet another command)
        @foo    {%foo.v3}

Here, object `foo` has a pointer to its head version that we update, and a word for each version. For many applications, like mailing lists and web forums, it is entirely acceptable to preserve the complete history or leave maintenance to humans. But in other cases we might prefer to gradually stabilize and compact our histories; for these, see *Managed Dictionaries*, below. 

With a long history or big-value commands, applications can potentially grow larger than memory. This is acceptable, so long as developers leverage [stowage](Performance.md) to indicate which parts of the value should be kept out of immediate memory. With stowage, we can model massive tree structures, modeling filesystems and databases and large queues, only loading the pieces we need for each command or view.

## Real-World Effects and Reflection via Software Agents

Effectful dictionary applications must be modeled as multi-agent systems: external software agents (bots) observe our dictionary for some easily observed condition where they can contribute updates to the dictionary. For example, a command-pattern dictionary object may include an 'outbox' for messages in its state model. An external agent would query for this, capture and clear the outbox, and deliver the content. It could also provide a reply service and inject content into an inbox.

This technique generalizes to many kinds of 'effects'. 

We can model requests for web content, video feeds, news feeds, etc.. We can model control systems for smart houses, multi-media systems, and robots. We can integrate SMTP, Twitter, SMS, databases. We can model reflection and automated error recovery. Managed dictionary techniques allow injected information to eventually be deleted. Low-latency interactions are feasible via Comet-style long polling or a subscriptions model. Real-time effects are feasible if we use timestamps appropriately.

The primary filter should be a set of opt-in attributes. We shouldn't have software agents jumping in and interacting with our applications uninvited, and it is convenient to easily disable effects while debugging or forking an application object. Attributes additionally enable our software agents to efficiently discover where their attentions are desired.


## Managed Dictionaries and Attributes

Words in an AO dictionary provide a basis for mutable meaningful structure, modularity, and structure sharing. The mutable meaningful structure allows AO dictionaries to serve as a platform for live coding and dictionary applications. 

Unfortunately, the presence of mutable meaningful structure interferes with direct optimization at the dictionary layer. This isn't a major problem: a compiler could maintain a cache of optimized definitions separate from the dictionary. But there are some opportunity costs with respect to separating the optimizer, redistributing optimized code, and generalizing automatic  management of dictionary applications. 

To recover the lost opportunities, we can enable developers and dictionary applications to *declaratively relax* the constraints on an assumed dictionary optimizer for useful subsets of the dictionary. I propose the following:

* A word may be declared **hidden** to relax the requirement for stable external reference. An optimizer is free to delete a hidden word if it has no clients. This enables garbage collection of dictionaries.
* A word may be declared **opaque** to relax the requirement for stable structure of its definition. An optimizer is free to rearrange, refactor, or reorganize the definition of an opaque word in ways that preserve its behavior. 
* A word may be declared **frozen** to relax the requirement for mutable behavior. An optimizer is free to inline definition of a *frozen* word into the definition of an *opaque* word. A frozen word is *deep-frozen* if all transitive dependencies are also frozen.

To declare a list of attributes, I propose prefixing any definition as follows:

        [{&hidden}{&opaque}{&frozen}]%

This structure is preserved on import/export but trivially eliminated by simplifiers. It is extensible with ad-hoc new attributes (todos, deprecation, authorship, categories, relations, deprecation, licensing, decay models, etc.) and has no need for runtime semantics. A reverse lookup can find all uses of a token. It's voluminous, but space is cheap. The main disadvantage is that most other dictionary applications will need to recognize and handle this structure, too.

Note: These attributes only affect a dictionary optimizer. The *frozen* attribute is not a security attribute. If a developer wants to modify the definition for a frozen word, he can do so. Though, a development environment is free to warn the user about the potential issues.

## Parallelism in Dictionary Applications

I use 'parallelism' to describe multiple computations ongoing at the same *physical* time, keeping multiple CPUs busy. This is a performance concern separate from the application model (except. See the section on parallelism in the [performance doc](Performance.md)

## Concurrency for Dictionary Applications

> concurrency is a property of systems in which several computations 
> are executing simultaneously, and potentially interacting
> - Wikipedia

Concurrent systems are useful abstractions to model. 

AO can model multi-agent systems, concurrent constraint systems, [message passing machines](NetworkModel.md), actors systems, multi-threaded shared memory, and more. In most cases a formal concurrency model will need to be coupled to an 'environment' model to form the complete system. Non-deterministic choice and scheduling decisions, for example, would usually be separated from a formal concurrency model as belonging to the environment. When modeled properly, we should be able to capture a concurrent system as a value at any well-defined point in time and 'run' the model forward to a future time under the assumption of no external interference. The notion of 'time' here is logical and model dependent - e.g. between processing messages. 

We then develop dictionary applications in terms of our concurrency abstractions, and compose the pieces together into our larger system, and use the command pattern to step forward (e.g. until a stable condition is reached, or just a few messages, etc.). We benefit greatly from doing so:

* freely extend applications by injecting additional computations
* capture, model, render, and extract application in medias res
* render active application states: [animate, scrub, or graph](http://worrydream.com/LadderOfAbstraction/)
* implicit conventional debug-mode, breakpoints and state included
* concurrency introduces systemic opportunities for parallelism

*Aside:* The only feature here unique to concurrent systems is the *extensibility*. That is, concurrency is all about "several computations interacting", so we won't break our abstraction by injecting one more computation that interacts with those already present. All other features could be achieved by modeling an interpreter for a sequential language that exposes opportunities for parallelism, such as ABC.

Debugging concurrent applications hosted in a dictionary is vastly less unpleasant than debugging concurrent applications hosted on physical hardware. We're free to step back in time due to the command pattern. We're free to tweak the environment and see how the application would progress. There are no [heisenbugs](https://en.wikipedia.org/wiki/Heisenbug) because non-deterministic choice is no longer wired to races whose winners change under the scope of a debugger.

On the other hand, we lose those convenient timeouts that rely on hardware race non-determinism. We cannot rely on timeouts as a basis for hand-wavy soft real-time behavior. This constrains which applications we can effectively express with the concurrency abstractions. Applications that are very time-dependent will need to be pushed to real-time systems models, i.e. systems whose 'logical timeline' is designed to align tightly with real-world time (or musical beat, etc.).

## Security for Dictionary Applications

Imagine our goal is to model a game where each player is limited to viewing and manipulating a shared game world through a personal avatar. The security requirement here involves limiting the players, as opposed to asking them to police themselves.

To implement this security policy requires we cripple player access to the dictionary. Even read-only access is sufficient for players to view the game world through they eyes of any avatar. We could provide a private space for each player to install scripts or macros (e.g. new character animations, musical scores for characters to play on instruments, etc.), but fragmented access to the dictionary would be stifling. The AO programming experience is designed for the holistic dictionary. 

If we're content that this remains a 'dictionary application' only for our non-player participants (game masters, developers, bots), we can proceed easily enough. Constrain player access to a set of whitelisted dictionary applications that we know to be compatible with our security policy. Player authentication could optionally be separated from dictionary state. But application state would remain within the dictionary.

I imagine that most security policies will follow a similar pattern. The features that make AO dictionary applications debuggable also make them hackable. We can still enforce security, but the cost is the ability for some participants to view the application as part of an AO dictionary. Yet, embedding application state in the dictionary remains useful to our developers, and potentially to players after the play is done.

*Aside:* If the number of players is small, asking them to police themselves is reasonable. Shared storytelling, quests, roleplaying, and their like are frequently represented on conventional forums. Doing so on a shared programming medium like AO has potential to create a far more real-time interactive multi-media experience than natural language. It will also automate the administrative burdens. And if a storyteller or player wants to keep a surprise, they can just keep it to themselves, perhaps using a private fork of the dictionary if the surprise needs development and debugging.

## Extraction of Applications from the Dictionary

Extraction of applications allows Wikilon to double as a more conventional application development platform, e.g. compiling a separate binary for a desktop or android phone, or a JavaScript+DOM program for a web page. 

The extractor itself is a RESTful dictionary application, and downloading a compiled version of some function may be expressed as a trivial HTTP GET on a link. Extractors will frequently be specific to a particular application model. And "application model" can be interpreted very loosely here. We could have extractors specifically for image data, for example, computing an image and converting to PNG or SVG. 

For Wikilon, the short term goal is to support immediately useful extractors like compiling to JavaScript or presenting an SVG view, even if each such extractor must be specifically within Haskell code. The long term goal is to allow definition of new extractors from within the dictionary, e.g. functions that will receive on the stack a powerblock of reflective capabilities and a command target.
