
# Application Model for Awelon Project

Awelon project will focus on RESTful applications, with the primary resources being [Awelon Object](AboutAO.md) dictionaries and objects modeled within them. Applications are modeled in terms of creating, reading, updating, and deleting definitions of words. Effects are carried out by a multi-agent system. Real-time behavior can be supported via publish-subscribe patterns. 

This document will discuss useful patterns for modeling applications.

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

## Dictionary Objects

At the AO layer, the only formal meaning of a word is its definition. However, humans and software agents may treat words as having meaning and structure. We may associate `foo` with `foo.doc`, `foo.talk`, `foo.type`, `foo.author`, and so on. This association can be realized in our development environment. For example, tools that would copy, drop, or rename `foo` might include every `foo.field` word by default.

Dictionary objects can always be flat, non-hierarchical, via use of redirects (i.e. instead of `foo.bar.baz` we have `foo.bar → object` and `object.baz`). However, use of hierarchy can be convenient to help control default behavior - e.g. copy would not follow redirects, instead sharing the object referenced.

While AO does not permit cyclic dependencies between definitions, it is trivial to express cyclic relationships at the level of 'dictionary objects'. For example, `foo.author` references `users.dave` and `users.dave.appA` references `foo`. Cyclic relationships can be useful for modeling and navigating hypermedia applications.

## Hypermedia Applications

AO has a rich link structure. Definitions, dictionary objects, and entire dictionaries can be viewed as hypermedia. With dictionary objects, we get a lot of associative metadata for each word that can be linked together or provide hints for rendering. Thus, we can have cyclic graphs, or render some objects with sound or video. Potential presentation of AO as hypermedia is discussed under [editing AO](EditingAO.md). 

Intriguingly, *AO evaluates to AO*, preserving both behavior and a great deal of link structure. Hence, we can view this as *hypermedia evaluates to hypermedia*. We can potentially model texts that evaluate to canvases and tables. We can model canvases and tables that evaluate to text. With *program animation* (described in [About ABC](AboutABC.md)), we might graphically render intermediate hypermedia views.

In context of RESTful applications, treating AO as hypermedia could be useful both for viewing AO code and results, and for updating it. Depending on context, we could model updating forms/data in place or command-pattern updates where each 'command' is represented by a copy of a templated form. Forms could generally be evaluated independent of context, providing a basis lightweight applications and local input validation.

## Compositional Views

Evaluating and caching words can support spreadsheet-like propagation of updates, but that alone is insufficient to efficiently work with words that contain large objects. In context of command pattern, for example, we'll have small changes to large objects. 

To fully leverage cache, favor compositional views on persistent objects:

        ∃f. ∀x,*,y. view(x * y) = f(view(x),'*',view(y))

That is, the view of a composition is a composition of views. With this, we may explicitly memoize `view(x)` and `view(y)`. Our assumption of persistence (i.e. a lot of structure sharing) means it is unlikely both `x` and `y` have changed, so we can reuse at least part of the cache. In ABC, we would express this memoization as something like `[[X]view]{&memo}`. 

Fortunately, a lot of views and data fit these constraints or at least can be adapted by keeping metadata with the view. Thus, we can support spreadsheet-like propagation even in contexts like time-series data and massive key-value databases.

*Aside:* Memoization does have overhead for serialization and storage. To mitigate this overhead, we should memoize only at coarse, stable boundaries. This isn't difficult. For an LSM tree, with natural batching, we might simply cache everything but the root node. For time-series data, we might batch sample counts into frames, and cache only on the first frame.

## Publish Subscribe

Publish subscribe is a model for continuous, live programming of real world systems.

Command pattern provides a basis for small updates to large objects. Compositional views provide a basis for efficient ad-hoc subscriptions including large objects. Between these, large AO resources can be observed and updated in real-time by both human and software agents. A human may subscribe to dashboard views and use commands (or direct manipulation) to update policy. An 'internet of things' device might use a command pattern to publish time-series data while subscribing to a schedule of goal states.

The ability to share flexible views of large objects without requiring each endpoint to manage fine-grained update events makes AO a lot more expressive and easier to use than many state-of-the-art publish subscribe systems.

## Effectful Orders

A RESTful pattern for effectful systems is to model each request as a first class resource - a [work order](https://en.wikipedia.org/wiki/Work_order) (of sorts) to be fulfilled by agents in a multi-agent system. Mostly software agents, but humans might also participate in fulfilling orders.

Agents party to this system would search for orders matching some ad-hoc conditions (e.g. unclaimed, unfulfilled, authorized, and within the agent's domain). Upon discovering suitable orders, the agent may claim it temporarily, perform some work, then update the order to indicate progress. A single order may be fulfilled by multiple agents over time. In the general case, subordinate orders may be constructed for subtasks.

In practice, orders will express conditional behavior and loops so a single agent can make more than one small step of progress. This helps amortize the search, claim, and update overheads. Consequently, we might model orders using monads or [process networks](KPN_Effects.md).

When orders are used for long-running labor, we gain opportunity to view progress for the work over time, and to interrupt, pause, prioritize, or guide the work in various ways.

*Aside:* Modeling orders is similar in nature to the [tuple space](https://en.wikipedia.org/wiki/Tuple_space) concept. The main difference is the proposed method for mutual exclusion: stake a 'claim' on an object rather than remove it. I believe this will be more robust, accessible, and extensible - e.g. expire claims on agent failure, support subscribed views of progress, adding schedules to work orders or partial claims for collaborative work.

## Tables and Spreadsheets

With AO and ABC, any subprogram can be evaluated. Hence, if we have a table full of subprograms, we can evaluate each of them. When some subprograms depend on others, the evaluation may propagate in the style of a spreadsheet. Awelon can potentially model very expressive spreadsheets by use of stowage and caching, enabling time-series databases and similar to be presented as individual 'cells' in the spreadsheet. 

Of course, we still need the tabular layout. 

One option is to render a set of dictionary objects. We could take `foo.doc` as meaning row `foo` column `doc`. In this sense, a dictionary itself becomes a potential spreadsheet, though we might filter only for objects meeting certain criteria. Alternatively, we could construct an object where each attribute redirects to a row object:

        @myTable.r1 {%foo}
        @myTable.r2 {%bar}
        @myTable.r3 {%baz}
        ...

This would be convenient as a basis to construct a stable spreadsheet by hand.

Together with [editable views](EditingAO.md) and rendering output as hypermedia, spreadsheets could serve as a convenient basis for rapid application development. 

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

### Dictionary Based Communications

Dictionaries are a flexible and efficient representation for message-passing.

A dictionary naturally represents a collection of named dictionary objects. So we'll want two components for our message: the dictionary, and a word naming the message object. The dictionary will be represented as an AO patch. The bulk of the message - standard vocabulary, recommended [views](EditingAO.md), default fields, and so on - would be indicated via secure hash in the AO patch header. In practice, the word would frequently name a templated message object in the secure hash dictionary, or even a standard *view* on another object that translates between how we input data and how it is viewed.

This is flexible both because our messages can contain behavioral information, procedurally generated data, translations and views, etc.. and efficient because of structure sharing with the secure hash reused between messages and potential for provider-independent content distribution networks.

Conveniently, a document is essentially a message that we maintain over time. And many applications can be understood as documents that interact with humans and external software systems (via command pattern, work orders, publish subscribe, etc..)

## Multi-Dictionary Applications

AO does not support foreign functions, not even linking between AO dictionaries. Interaction with external AO resources must be modeled effectfully like any other external resource - via work orders, publish subscribe patterns, etc.. At most, we have an advantage of simpler translations.

*Aside:* I have considered special support for binding dictionaries with `{%word@dict}`. However, it does not generalize nicely in context of application layer hypermedia, command patterns, etc.. and meanwhile it complicates the evaluator, entangles the environment, and has awkward security semantics. Externalizing connectivity as an effect is the wiser option.
