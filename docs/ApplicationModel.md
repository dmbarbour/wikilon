
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

It is not uncommon that we'll want associative metadata for words: documentation, discussion, version, authorship, copyright, licensing, rendering hints, memory management, rich types or proofs, etc.. To address a lot of these concerns, a simple mechanism is to use 'dot' attributes:

* `foo.doc` to document `foo`
* `foo.talk` to discuss `foo`
* `foo.gc` for managed `foo` 
* `foo.type` for type hints 
* `foo.proof` for theorem provers
* `foo.class` to approach OOP

This model of attributes is useful because:

* attribute updates don't invalidate cache unnecessarily
* attributes can be used together with values in cache
* attributes get full access to templating, abstraction
* attributes can model cyclic relationships (parent)
* attributes are easily preserved across imports, exports
* convenient, flexible notion for dictionary-level objects

This idiom only requires default bundling:

* export `foo.*` whenever we export `foo`
* rename `foo.*` whenever we rename `foo`
* delete `foo.*` whenever we delete `foo`

Similarly, if we 'fork' an object, we might clone all the attributes and rewrite internal structure.

*Important:* Deep hierarchical structure is unnecessary and discouraged.

Attributes with rich structure can be defined by reference to another toplevel object in the dictionary. Richly attributed references, similarly, may also be reified as objects. Thus we never need more than one level of attribute. Favoring a 'flat' namespace also gives us shorter words, a more object-oriented and relational structure, more precise security decisions, etc.. 

To discourage deep hierarchy, AO words remain size-limited to 60 bytes UTF-8. Shallow hierarchy seems acceptable, so long as it remains close to flat.

Attributes have no semantics beyond the de-facto conventions durrounding them. An attribute like `foo.gc` may cover policy for GC of all attributes within `foo`, not just `foo` itself. It is feasible to use attributes like `foo.class` to model something similar to class-based or prototype-based OOP, or at least support inheritance of attributes.

## Explicit Spreadsheets

An AO dictionary with a cache for incremental computing gives us some spreadsheet-like characteristics. An update to a definition may propagate through our dictionary. However, we can take this further by explicitly modeling spreadsheets - a grid of reactive cells that can be edited in table form, and are bundled for rendering and export. 

Trivially, we could use a convention like `mySheet.A1` to define cell `A1`, or a slightly more structured `mySheet.1.A`. However, I believe we can do much better.

Define each row by *reference* to another object, e.g. `@mySheet.1 {%foo}`. Then use attributes of the referenced object as columns, e.g. `foo.A` and `foo.B` fill columns `A` and `B` for row `1`. The definition of `foo` may be treated as a special attribute `foo.@`. In general, we may also render rows by object name, e.g. rendering `foo` instead of `1` in the row. And we can automatically sort rows based on a choice of row number, object name, attribute data, etc..

Modeling spreadsheets as collections of object references allows spreadsheets to serve as direct manipulation interfaces and lightweight development environments instead of passive 'views' of the dictionary. We can model higher-dimensional spreadsheets, e.g. where each row is a spreadsheet. Multiple spreadsheets can easily share structure, but we may just as easily define fresh objects like `mySheet:1` when introducing a new row. When we introduce security models, we may enforce them at row-level. Visual or interactive renderings for objects readily apply.

## Immutable Objects

Immutable objects will be modeled via the AO link layer. Token `{%foo@source}` will link an object `foo` from another dictionary, `source`. When `source` is identified by secure hash, we can easily validate that we have a specific, immutable `foo` object. 

can validate the secure hash for the immutable dictionary which contains `foo` among other objects. In general, we could reduce each source to the scope of either a single object or a strongly connected component (where multiple objects refer to each other via attributes).

Immutable objects have potential to greatly improve the efficiency of large AO systems. They are easily shared, validated, cached, separately compiled, and linked by secure hash. But they preserve application-level structure and attributes, which simplifies tooling - search, rendering, type checking, and anything else we might support with attributes.

## Security for Dictionary Applications

When multiple agents muck about in a stateful dictionary, it becomes valuable to precisely control who interacts with what. Fortunately, the rich computational structure of our dictionary admits some expressive security models. For example, automatic curation is feasible - rejecting updates that break types or tests. We can also adapt object capability security for a dictionary-level object model.

Consider two authorities:

* query *object* via *interface* 
* update *object* via *interface*

In context of a dictionary, words are the smallest unit that we observe and control. Our interface and object name words. Authorities can generally be represented and shared via cryptographic bearer-tokens of the form `update:object/interface/HMAC`. User logins might be modeled as a bag of bearer tokens together with relevant view states (active edits, color schemes, alerts, etc..).

Query and update permissions specify an interface:

        query interface:    Params → Object → Result
        update interface:   Params → Object → Object

These interfaces constrain our agents. We're only permitted to observe the result of our query, which allows for flexible information hiding. Similarly, update interfaces constrain which updates may be expressed for our object. There is a most powerful interface: `apply :: (o → a) → o → a`, which can be used to construct any other interface. 

At the dictionary level, updates could be implemented by command pattern:

        @alice.foo.update (alice's foo update interface)
        @bob.foo.update (bob's foo update interface)

        @foo.v0 (init foo)
        @foo.v1 [{%foo.v0}][Parameters1]{%alice.foo.update}
        @foo.v2 [{%foo.v1}][Parameters2]{%alice.foo.update}
        @foo.v3 [{%foo.v2}][Parameters3]{%bob.foo.update}
        @foo    {%foo.v3}

Queries might be reified in the dictionary:

        @alice.foo.query (alice's foo interface)
        @alice.foo.q0 [{%foo}][Params]{%alice.foo.query}

For the update case, we may be limited to constant parameters. It may require a separate authority for retroactive update.

*Aside:* Agent specific interfaces give us a lot of precision. If modeling a multi-user dungeon, for example, players might each have interfaces specific their avatar. This also enables precise management and revocation of authority.

The above model is insufficient, and could probably be improved. In addition to query and update, we need authorities regarding source-level reads, writes, delete, rename, fork, history, search and discovery, and other such features. In general, I imagine it would be *frustrating* to use a dictionary through a limited interface, and it would certainly undermine some advantages of dictionary applications. 

Nonetheless, it seems dictionaries could be used as an effective foundation for mutually distrustful multi-agent systems with a relatively generic security layer.

## Extraction of Applications

Extracting and compiling an 'application' for external use is certainly feasible, especially for certain kinds of applications (e.g. those with monadic effects models). Ideally, this extraction process should be very simple and cacheable, expressed as a simple HTTP GET.
