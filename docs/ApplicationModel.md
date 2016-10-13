
# Application Model for Awelon Project

Awelon project will focus on RESTful applications, with the primary resources being [Awelon Object](AboutAO.md) dictionaries and objects modeled within them. Applications are modeled in terms of creating, reading, updating, and deleting definitions of words. Effects are carried out by a multi-agent system. Real-time behavior can be supported via publish-subscribe patterns. 

This document will mostly discuss patterns for modeling applications, and why they're useful.

## Command Pattern

The command pattern might be represented as:

        @foo.v0 (initial state)
        @foo.u1 (command1)
        @foo.v1 {%foo.v0}{%foo.u1}
        @foo.u2 (command2)
        @foo.v2 {%foo.v1}{%foo.u2}
        ...
        @foo.u99 (command99)
        @foo.v99 {%foo.v98}{%foo.u99}
        @foo {%foo.v99}

Use of command pattern enables many small updates to construct and modify an object, in this case `{%foo}`.  This particular design records every command as a distinct set of words, simplifying view of historical values and actions on the object, easy forking and undo, etc..

Command pattern can be used for such things as:

* addending a time-series database
* updating a key-value database or other large object
* editing documents modeled as finger-tree ropes
* modeling user actions - a button press, HTTP POST

Effectively, command pattern models a mutable object within a dictionary, albeit only mutable by external agents. 

## Compositional Views

Evaluating and caching words can support spreadsheet-like propagation of updates, but that alone is insufficient to efficiently work with words that contain large objects. In context of command pattern, for example, we'll have small changes to large objects. 

To fully leverage cache, favor compositional views on persistent objects:

        ∃f. ∀x,*,y. view(x * y) = f(view(x),'*',view(y))

That is, the view of a composition is a composition of views. With this, we may explicitly cache `view(x)` and `view(y)`. Our assumption of persistence (i.e. a lot of structure sharing) means it is unlikely both `x` and `y` have changed, so we can reuse at least part of the cache.

Fortunately, a lot of views and data fit these constraints or at least can be adapted by keeping metadata with the view. Thus, we can support spreadsheet-like propagation even in contexts like time-series data and massive key-value databases.

*Aside:* ABC supports explicit caching via `[computation]{&cache}`. Caching does have overhead for serialization and storage. To mitigate this overhead, we might cache only at coarse-grained, stable boundaries. For example, with time-series data, we could batch updates into frames (based on sample count) and only cache full frames.

## Publish Subscribe

AO resources can be observed and updated in real-time by both human and software agents. A human may subscribe to dashboard views and use commands (or direct manipulation) to update policy. An 'internet of things' device might use a command pattern to publish time-series data while subscribing to a schedule of goal states. 

Publish subscribe provides a foundation for continuous effects, live programming of real world systems. It is greatly enhanced by both the ability to subscribe to computed views and to support eventful updates to shared database objects.

## Effectful Orders

A RESTful pattern for effectful systems is to model each request as a first class resource - a [work order](https://en.wikipedia.org/wiki/Work_order) (of sorts) to be fulfilled by agents in a multi-agent system. 

Agents party to this system would search for orders matching some ad-hoc conditions (e.g. unfulfilled, authorized, within the agent's domain). Upon discovering a suitable order, the agent may claim it briefly, perform some work, then update the resource to indicate progress or results. A single order may be fulfilled by multiple agents over time, and in the general case might result in construction of orders for subtasks.

This is similar in nature to the [tuple space](https://en.wikipedia.org/wiki/Tuple_space) concept.

Orders must be coarse grained to amortize the search, dispatch, and update overheads. In practice, orders will frequently include decisions and loops so we can get more work done. A sophisticated order might be represented as a [KPN](KPN_Effects.md) or monadic action. 

Orders can be used for one-off effects. But when orders involve ongoing work, we might record incremental progress, integrate with publish-subscribe and time-varying policies. 

In context of AO, orders may be modeled as objects in a dictionary.

## Dictionary Objects

At the AO layer, the only formal meaning of a word is its definition. However, humans and software agents may treat words as having more meaning and structure. We may associate `foo` with `foo.doc`, `foo.talk`, `foo.type`, `foo.author`, and so on. This association can be realized in our development environment. Tools would delete or rename the `foo` object as a whole. Conventional attributes may guide rendering and editing of objects. 

Redirects can model references between objects, eliminating need for hierarchical structure. And while AO does not permit cyclic definitions, we can easily express cyclic relationships between objects (e.g. from `foo → bar` and `bar.parent → foo`).

## Hypermedia Applications

AO evaluation preserves some link structure. The output of evaluation may be understood as a document containing references to other objects in the dictionary. Those references may be displayed as hyperlinks, frames, or other techniques, perhaps depending on object attributes. Attributes may also indicate tools or forms to update an object, supporting type or application specific operation.

## Tables and Spreadsheets

With AO and ABC, any subprogram can be evaluated. Hence, if we have a table full of subprograms, we can evaluate each of them. When some subprograms depend on others, the evaluation may propagate in the style of a spreadsheet. Awelon can potentially model very expressive spreadsheets by use of stowage and caching, enabling time-series databases and similar to be presented as individual 'cells' in the spreadsheet. 

Of course, we still need the tabular layout. 

One option is to render a set of dictionary objects. We could take `foo.doc` as meaning row `foo` column `doc`. In this sense, a dictionary itself becomes a potential spreadsheet, though we might filter only for objects meeting certain criteria. Alternatively, we could construct an object where each attribute redirects to a row object:

        @myTable.r1 {%foo}
        @myTable.r2 {%bar}
        @myTable.r3 {%baz}
        ...

This would be convenient as a basis to construct a stable spreadsheet by hand.

Modeling spreadsheets in an AO dictionary, together with hypermedia, might be a convenient basis for rapid application development - dashboards, etc..

Regardless of how we specify our set of objects, AO makes it easy to render both the definitions of each word and their context-free evaluations or linker objects. Thus, we can render evaluated cells in a table while enabling editing of each cell's definitions, and propagating updates to the rendered views. We effectively get spreadsheets without any special effort.

## Managed Dictionaries

We may need to perform garbage collection at the dictionary level, eliminating words and collapsing command histories that are no longer relevant. This could be performed by a software agent, e.g. assume three attributes:

* *opaque* - definition's structure is irrelevant and may be rewritten
* *frozen* - behavior of this word should never change in the future
* *hidden* - assume no external references directly access this word

This would permit a corresponding set of rewrites:

* *opaque* definitions may be simplified, evaluated, compressed
* *frozen* definitions may be *inlined* into *opaque* clients
* *hidden* definitions may be *deleted* if they have no clients

Each declaration gives our software agent a more opportunity to safely rewrite, manage, and optimize the dictionary in specific ways. Compression can be achieved by introducing new hidden, frozen words for common substructures - i.e. dictionary compression.

## Application Security

AO supports multiple named dictionaries, which may be referenced by `{%word@dict}`. Security properties will generally focus on a named dictionary.

I would like to have a generic security model for AO that is useful for developing applications in the face of mutual distrust. The *granularity* for security should be a full 'dictionary'. However, we may have some static type constraints. 

because dictionaries are the unit for communication in AO systems.


Preferably while ensuring dictionaries are used a holistically in the common case (i.

*TODO:* Redesign in context of multi-dictionary evaluations.
* distributed programming: HMAC or PKI
* controlling connectivity between dicts
* eval/read/write access control?
* 




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
