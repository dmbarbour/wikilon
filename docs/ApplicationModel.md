
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

Software agents (colloquially, 'bots') can observe our dictionary for some easily tested conditions, interact with the real world, and contribute updates to the dictionary. For example, a dictionary object may include *an inbox and outbox* for messages, or perhaps a *publish-subscribe arena*. Our 'bot' could integrate the inbox/outbox with external systems (e-mail, twitter, AMQP). Similarly, publish-subscribe could be integrated with external publish-subscribe models (DDS, atom, time-series data). 

Bots can also reflect on the dictionary. For example, it is possible for a software agent to track down all words attributed with (category:math) and automatically maintain a word that provides an association list of these words to their definitions.

Interactions can occur in real-time via comet-style long-polling or subscriptions. Of course, this is probably *soft* real-time unless we control a lot of other factors, e.g. caching and number of users and real-time GC.

The ability to easily control bots will be valuable for developers. Bots should accept declarative guidance from attributes, e.g. to opt-in to bot interactions. Ideally, bots are themselves dictionary applications, keeping state and behavior in the dictionary. 

Wikilon may eventually directly host a useful subset of bots.

## Managed Dictionaries and Attributes

Words in an AO dictionary provide a basis for mutable meaningful structure, modularity, and structure sharing. The mutable meaningful structure allows AO dictionaries to serve as a platform for live coding and dictionary applications. 

Unfortunately, the presence of mutable meaningful structure interferes with direct optimization at the dictionary layer. This isn't a major problem: a compiler could maintain a cache of optimized definitions separate from the dictionary. But there are some opportunity costs with respect to separating the optimizer, redistributing optimized code, and generalizing automatic  management of dictionary applications. 

To recover the lost opportunities, we can enable developers and dictionary applications to *declaratively relax* the constraints on an assumed dictionary optimizer for useful subsets of the dictionary. I propose the following:

* A word may be declared **hidden** to relax the requirement for stable external reference. An optimizer is free to delete a hidden word if it has no clients. This enables garbage collection of dictionaries.
* A word may be declared **opaque** to relax the requirement for stable structure of its definition. An optimizer is free to rearrange, refactor, or reorganize the definition of an opaque word in ways that preserve its behavior. 
* A word may be declared **frozen** to relax the requirement for mutable behavior. An optimizer is free to inline definition of a *frozen* word into the definition of an *opaque* word. A frozen word is *deep-frozen* if all transitive dependencies are also frozen.

To declare a list of attributes, I propose prefixing any definition as follows:

        [{&hidden}{&opaque}{&frozen}]%

It is extensible with ad-hoc new attributes: todos, deprecation, authorship, categories, relations, deprecation, licensing, decay models, automatic testing, cache and quota control, security, etc. I expect attributes will be common enough that I've introduced a specialized syntax to my claw view: `(hidden opaque frozen)` desugars to the above. Attributes have no runtime semantics. Dropping the block of annotations via `%` helps clarify this, e.g. a runtime will generally eliminate the entire block during a simplification pass and never wonder what `{&category:math}` is supposed to do.

Note: The *frozen* attribute is not a security attribute. If developers want to modify the definition of a frozen word, they may do so. At worse, they'll face a "do you *really* want to do this?" dialog. Security attributes may have potential utility, though. They're discretionary, but we can enforce them if a dictionary is gated behind a service API. See security, below.

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

To provide security, we must provide 'gated' access to the dictionary, e.g. via a service API. This is similar to how we might control access to a filesystem or database. We could separate authentication from the dictionary to avoid polluting the dictionary with the equivalent of password files.

Imagine our goal is to model a game where each player is limited to viewing and manipulating a shared game world through a personal avatar. Read-only access is sufficient for players to view the game world through they eyes of any avatar. Write access would allow a player to control other characters. Even full access to a private namespace in the dictionary might be problematic because players could retroactively modify their actions. Though, we could support a private copy-on-write namespace for macros and the like.

Ultimately, the holistic experience of AO dictionary applications would be available only to participants in non-player roles (e.g. game masters, developers, the gateway service). Only these participants have the full ability to debug, snapshot, fork, optimize, etc. the game service. Players may still benefit from dictionary apps indirectly, e.g. we could open up snapshots of the player-enriched game world a month later.

*Aside:* If the number of players is small and the game isn't too competitive, asking players to police themselves is reasonable. Shared storytelling, quests, tabletop roleplaying, interactive fictions, and their like do not benefit from security. Nor would a chess game. 

## Extraction of Applications from the Dictionary

Extraction of applications allows Wikilon to double as a more conventional application development platform, e.g. compiling a separate binary for a desktop or android phone, or a JavaScript+DOM program for a web page. 

The extractor itself is a RESTful dictionary application, and downloading a compiled version of some function may be expressed as a trivial HTTP GET on a link. Extractors will frequently be specific to a particular application model. And "application model" can be interpreted very loosely here. We could have extractors specifically for image data, for example, computing an image and converting to PNG or SVG. 

For Wikilon, the short term goal is to support immediately useful extractors like compiling to JavaScript or presenting an SVG view, even if each such extractor must be specifically within Haskell code. The long term goal is to allow definition of new extractors from within the dictionary, e.g. functions that will receive on the stack a powerblock of reflective capabilities and a command target.
