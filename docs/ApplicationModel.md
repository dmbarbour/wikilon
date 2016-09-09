
# Wikilon's Application Models

Wikilon is intended as both a software platform and development environment. 

As a software platform, Wikilon should be able to directly host wikis, forums, blogs, image canvases, document editors, REPL sessions, spreadsheets, chatrooms, interactive fictions or multi-user dungeons, and other applications or web services. As a development environment, Wikilon must support edit sessions and layout, debugging sessions, and compilation of applications for execution elsewhere. Development environments might be considered an important use-case of hosted applications.

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

## Incremental Computing with Dictionary Apps

Dictionaries apps have a very 'spreadsheet-like' aspect to them, so incremental computing will be highly valuable for performance. Incremental computing means minimizing computation after each update. Consider a scenario involving a database object and a useful view of that object:

        ...
        @myDB.v99 {%myDB.v98}(update99)
        @myDB     {%myDB.v99}

        @myDB:usefulView   {%myDB}{%usefulView}

If we assume a cached version of `myDB.v98` then it seems obvious that we can incrementally compute and cache `myDB.v99`, so at least that respect for our incremental computing is straightforward. The challenge, then, regards incremental computing of the useful view of our database.

One feasible solution involves caching. Assumptions:

* the view is compositional
* the database object has multiple 'component' partitions
* the updates are isolated to just a few components
* annotation `{&cache}` creates caching functions

By *view is compositional*, I mean that the view of the composite is a function of the views of the components. Formally, `∃F.∀x,*,y. V(x*y) = F(V(x), quote(*), V(y))` for our view `V`. I believe that a lot of useful views fit this criteria, or at least can be made to fit with a little wrapping and tuning. Time-series data processing, for example, is almost always compositional in this sense.

Annotation `[V]{&cache}` will create the caching version of function `V`. This must load the cache if it already exists, so our runtime will use a serialization of `[V]` (perhaps indirectly, via secure hash or stowage address) to load the appropriate cache. In general, we'll also need to serialize *arguments* to `[V]` to access cached results or save new results. With a cached view function, we can directly apply it within composition view function to access deep cache structure.

Efficient use of cache can be achieved by heuristic decisions about what to cache. At the runtime layer, we may observe effort vs. space tradeoffs, and avoid caching if recomputing is relatively cheap. At the development layer, we might make explicit, conditional decisions about which views to try caching, so we aren't serializing minor or unstable components.

It seems that explicit caching could serve effectively as a basis for incremental computation in dictionary apps, even in the presence of state, for a widely useful subset of views and related processes. 

## Real-World Effects and Reflection via Software Agents

Software agents (colloquially, 'bots') can observe our dictionary for some easily tested conditions, interact with the real world, and contribute updates to the dictionary. For example, a dictionary object may include *an inbox and outbox* for messages, or perhaps a *publish-subscribe arena*. Our 'bot' could integrate the inbox/outbox with external systems (e-mail, twitter, AMQP). Similarly, publish-subscribe could be integrated with external publish-subscribe models (DDS, atom, time-series data). 

Bots can also reflect on the dictionary. For example, we could use the dictionary as a blackboard metaphor: observe a subset of words (perhaps `*.x`) for a specific condition (`x → Maybe y`) then act upon the resulting `(word,y) List` whenever it changes. Almost any multi-agent system can be directly embedded at the dictionary level, with a little ingenuity.

Interactions can occur in real-time via comet-style long-polling or subscriptions. Of course, this is probably *soft* real-time unless we control a lot of other factors, e.g. caching and number of users and real-time GC.

The ability to easily control bots will be valuable for developers. Bots should accept declarative guidance from attributes, e.g. to opt-in to bot interactions. Ideally, bots are themselves dictionary applications, keeping state and behavior in the dictionary. 

Wikilon may eventually directly host a useful subset of bots.

## Concurrency of Dictionary Applications

> concurrency is a property of systems in which several computations 
> are executing simultaneously, and potentially interacting
> - Wikipedia

A multi-agent system is naturally concurrent. All we really need to do is make it easy to control and scale concurrency. Small updates, incremental computations, maybe some lightweight transactions at the dictionary level, i.e. by tracking which definitions were read when forming an update.

An interesting point is that we can have a *clock* be one of our software agents. This works as follows:

* an application may explicitly request a clock 'tick' event
* when conditions for the event are met, the clock provides
* the application enters a new state, updates propagate

Clock events could be based on absolute time (January 7th at 11am) or periodic (once a second) or tied to some other dictionary state. Regardless, it provides a simple basis for explicit races against a clock, and an opportunity for iterative computation and 'progress' for long-running computations that might need runtime interference.

This gives us a simple basis for non-deterministic race conditions, where our application concurrently waits for a 'tick' event or some other update from another agent, and might meanwhile continue subcomputations driven by the clock. By coupling this with incremental computing, we can support a wide variety of 'views' of real concurrent systems.

Updates are ultimately recorded in our dictionary in a specific order. Thus, we can readily replay and debug computations that involve multiple external agents. We cannot necessarily *reproduce* them, but that's the normal case when integrating real-world systems.

*Aside:* Parallelism doesn't require any special attention. We'll have a great deal of parallelism at the level of individual computations, and also from propagating updates to multiple clients of a word.

## Managed Dictionaries

Monotonic dictionary applications work very well when the 'useful state' to 'update' ratio is close to 1, whether due to debugging or just the nature of the application (such as a forum where we keep every message). But for other applications, we might want to reduce the dictionary. In those cases, a software agent could recover space by dictionary rewriting. For example, given a command pattern of the form:

        @myDB.v0  (initialDB)
        @myDB.v1  {%myDB.v0}(update1)
        ...
        @myDB.v98 {%myDB.v97}(update98)
        @myDB.v99 {%myDB.v98}(update99)
        @myDB     {%myDB.v99}

We could inline and delete `myDB.v0` and `myDB.v98` and our resulting dictionary will have the form:

        @myDB.v1  (initialDB)(update1)
        ...
        @myDB.v99 {%myDB.v97}(update98)(update99)
        @myDB     {%myDB.v99}

By itself, this only saves a little bit of space. But further rewriting could collapse the `(initialDB)(update1)` into a checkpoint and more efficiently compress the `(update98)(update99)` sequence than we would achieve with the individual updates. Thus, it is feasible to perform gradual, systematic compaction and garbage collection of our dictionary.

The main difficulty with this idea is telling our software agent *what* it is permitted to inline and delete. I believe this could easily be achieved by adding metadata attributes to the dictionary. Consider three declarative attributes:

* **opaque** - definition's structure is irrelevant and may be rewritten
* **frozen** - behavior of this word should never change in the future
* **hidden** - assume no external references directly access this word

This would permit a corresponding set of rewrites:

* *opaque* definitions may be simplified, evaluated, compressed
* *frozen* definitions may be *inlined* into *opaque* clients
* *hidden* definitions may be *deleted* if they have no clients

Each declaration gives our software agent a more permit to safely rewrite, manage, and optimize the dictionary in specific ways. Compression can be achieved by introducing new hidden, frozen words for common substructures - i.e. dictionary compression.

Regarding *mechanics* of declaration, see *Attributes and Objects*, below.

## Attributes and Objects

It is not uncommon that we'll want associative metadata for words: documentation, discussion, version, authorship, copyright, licensing, special rendering instructions, memory management, sophisticated types or proofs, etc.. To address a lot of these concerns, a simple mechanism is to use 'dot' attributes:

* `foo.doc` to document `foo`
* `foo.talk` to discuss `foo`
* `foo.gc` for managed `foo` 
* `foo.type` for type hints 
* `foo.proof` for theorem provers
* `foo.hash` for verifying `foo`
* `foo.version` for version info

This model of attributes is useful because:

* attribute updates don't invalidate cache unnecessarily
* attributes get full access to templating, abstraction
* attributes can model cyclic relationships (parent)
* attributes are easily preserved across imports, exports
* convenient, flexible notion for dictionary-level objects

Attributes are rather ad-hoc, subject to de-facto standardization by the agents (human and software). For example, `foo.gc` might describe how to handle all of `foo.v*` rather than require specific definitions like `foo.v3.gc`. 

Embracing this idiom only requires we default to exporting `foo.*` whenever we export `foo`. At the definition level, our dictionaries remain acyclic. But it is feasible for `foo` to refer to `bar` and `bar.x` to refer back to `foo`. 

For modeling 'objects', consider a spreadsheet. The attribute `foo.cA1` might indicate definition for cell `A1` when `foo` is rendered as a spreadsheet, and may be bundled with a few hundred more cells. While not OOP by itself, there is some general sense that a [bundle of attributes](https://en.wikipedia.org/wiki/Bundle_theory), e.g. for export and rendering, is what we humans perceive to be an 'object'.

I'll expand on the 'objects' with the *Security* concern, below.

## Object Capability Security for Dictionary Applications

Securing a dictionary is not dissimilar from securing multi-user filesystems or databases. However, I am not particularly impressed how most filesystems and databases handle security. Let's see if we can do a better job here.

What do I want to secure?

* words are the unit of state, update, and observation
* definitions may encapsulate sensitive information
* structure of updates and type of results is critical

Tentatively, I contemplate coarse word-level permissions:

* *read* - we can see the underlying definition for a word
* *query* - we can query a definition and observe result
* *write* - we can overwrite an existing definition directly
* *update* - we can apply iterative updates to a definition

This is similar to a filesystem, but with 'query' and 'update' permissions enabling me to control a bit more structure. Consider the update action specifically. A general approach is something like:

        @foo.update (action)
        @foo.v0 (init foo)
        @foo.v1 [{%foo.v0}][(update1)]{%foo.update}
        @foo.v2 [{%foo.v2}][(update2)]{%foo.update}
        @foo {%foo.v2}

Unfortunately, the above update model isn't very precise or extensible, i.e. because the `[(update)]` value provides access to *all* the possible update methods. I think it would be useful to restrict agents to specific updates. Thusly, we might break updates into multiple methods. For example:

        @foo.update.methodA (methodA)
        @foo.update.methodB (methodB)
        @foo.v0 (init foo)
        @foo.v1 [{%foo.v0}][(updateArgs1)]{%foo.update.methodA}
        @foo.v2 [{%foo.v1}][(updateArgs2)]{%foo.update.methodB}
        @foo {%foo.v2}

With this, I can now give an agent permission for a specific subset of *update* methods. I believe the same idea can be adapted to *query* methods, i.e. such that we may define queries on an object then automatically gain permission to observe and use the results of said query. Further, *read* and *write* can be modeled as specific query and update methods. 

Documenting, explaining, and managing permissions at the level of fine-grained methods is likely to prove painful. However, we can easily document coarse-grained 'methods' and instead call them 'interfaces'. An interesting property is that these interfaces themselves might also be objects, subject to update.

An important 




updates and queries may both be restricted such that they fail with error if they don't complete - i.e. produce a single value - within reasonable quota limits. 


this seems rather *too* fine grained. Documenting and explaining permissions at the level of individual methods will be painful. I think a happy medium might involve interfaces, with the following features:

* interfaces can provide multiple update and query methods


i.e. such that we can say that Alice has access to object `foo` through a specific set of interfaces `bar, baz, qux`. I'll get back to the question of 'what is an interface' in just a bit.

At this 



At this point, we're getting something arbitrarily close to object-oriented. 




Everything I'm describing of what we're discussing with 'updates' can easily be generalized to 'queries'. 

Also, *read* and *write* permissions are effectively specializations on update and queries.



But I think we can do better than this.



At this point, we have the ability to give agents method-level permissions for updating an object.

        





 could actually scale to support *multiple* update actions, such that agents could be given very specific update permissions. 

        @foo.update.action1 (action1)
        @foo.update.action2 (action2)
        







For *update*, I'm assuming we'll use a simple `[Update][Program]i` model, producing a new program - i.e.


The *write* permission would be strictly stronger than *update*, and *read* is strictly st


source code (because sensitive info) but we can query


To guard sensitive information, it must be possible to observe/evaluate a function without seeing its source code. (But ability to read would imply evaluation.) 

We'll need *read* access to be separate from *observe* access, i.e. where the latter requires a simple model like producing an output. 


, simple techniques like 'access control lists' can be made to work. Capab

To provide security, we must constrain access to the dictionary. imilar to security problems surrounding multi-user filesystems or databases. 




 In particular, the following features must be controlled:

* ability to write or update
* ability t

, e.g. via a service API. This is similar to how we might control access to a filesystem or database. We could separate authentication from the dictionary to avoid polluting the dictionary with the equivalent of password files.

Imagine our goal is to model a game where each player is limited to viewing and manipulating a shared game world through a personal avatar. Read-only access is sufficient for players to view the game world through they eyes of any avatar. Write access would allow a player to control other characters. Even full access to a private namespace in the dictionary might be problematic because players could retroactively modify their actions. Though, we could support a private copy-on-write namespace for macros and the like.

Ultimately, the holistic experience of AO dictionary applications would be available only to participants in non-player roles (e.g. game masters, developers, the gateway service). Only these participants have the full ability to debug, snapshot, fork, optimize, etc. the game service. Players may still benefit from dictionary apps indirectly, e.g. we could open up snapshots of the player-enriched game world a month later.

*Aside:* If the number of players is small and the game isn't too competitive, asking players to police themselves is reasonable. Shared storytelling, quests, tabletop roleplaying, interactive fictions, and their like do not benefit from security. Nor would a chess game. 

## Extraction of Applications from the Dictionary

Extraction of applications allows Wikilon to double as a more conventional application development platform, e.g. compiling a separate binary for a desktop or android phone, or a JavaScript+DOM program for a web page. 

The extractor itself is a RESTful dictionary application, and downloading a compiled version of some function may be expressed as a trivial HTTP GET on a link. Extractors will frequently be specific to a particular application model. And "application model" can be interpreted very loosely here. We could have extractors specifically for image data, for example, computing an image and converting to PNG or SVG. 

For Wikilon, the short term goal is to support immediately useful extractors like compiling to JavaScript or presenting an SVG view, even if each such extractor must be specifically within Haskell code. The long term goal is to allow definition of new extractors from within the dictionary, e.g. functions that will receive on the stack a powerblock of reflective capabilities and a command target.

