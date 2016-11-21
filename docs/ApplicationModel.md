
# Application Model for Awelon Project

Awelon project will focus on RESTful applications, with the primary resources being [Awelon](AwelonLang.md) dictionaries and objects modeled within them. Applications are modeled in terms of creating, reading, updating, and deleting definitions of words. Effects are carried out by a multi-agent system. Real-time behavior can be supported via publish-subscribe patterns. 

This document will discuss useful patterns for modeling applications.

## Command Pattern

The command pattern might be represented as:

        @foo.v0 initial state constructor
        @foo.v1 foo.v0 command1
        @foo.v2 foo.v1 command2
        ...
        @foo.v99 foo.v98 command99
        @foo foo.v99

Use of command pattern enables many small updates to construct and modify a large object. This particular design records every command as a distinct set of words, simplifying view of historical values and actions on the object, easy forking and undo, etc..

Command pattern can be used for such things as:

* addending a time-series database
* updating a key-value database or other large object
* editing documents modeled as finger-tree ropes
* modeling user actions - a button press, HTTP POST

Effectively, command pattern models a mutable object within a dictionary, albeit only mutable by external agents. 

## Futures and Promises

Awelon can easily evaluate in context of undefined words. A convenient idea is to treat undefined words as having a 'future' definition, to be fulfilled by human or software agents. Evaluation then usefully proceeds in context of multiple futures, enabling intermediate observations that do not depend on the definition of the future.

Unlike command pattern, futures and promises can be fully monotonic. This is a very valuable feature. It means we do not need to recompute our views after an update. Instead, we can checkpoint and continue our computations as needed, without backtracking.

## Dictionary Objects

At the Awelon layer, the only formal meaning of a word is its definition. However, humans and software agents may treat words as having meaning and structure. We may associate `foo` with `foo.doc`, `foo.talk`, `foo.type`, `foo.author`, and so on. Informally, `foo.doc` is a field in object `foo`. Numbered fields can represent informal sequences or collections - commands in the command pattern, lines in a REPL or notebook application, rows in a spreadsheet, etc.. Fields can serve as metadata 'tags' on an object, to support efficient search or extra rendering hints.

Tools to copy, drop, or rename `foo` should by default copy, drop, and rename the contained fields. When working with code, we might edit and view objects rather than individual definitions.

Depending on use case, developers may choose to represent ad-hoc hierarchical structures like `foo.bar.baz.qux`. But it is feasible to keep structures relatively flat by use of redirects - a word whose definition is simply another word - as references. It really depends on what the application should copy, drop, rename, view, and edit collectively.

## Hypermedia Applications

Awelon has a deep link structure and supports rich editable views. Further, Awelon has implicit structure in form of dictionary objects, a reverse link index, and static type analysis. Awelon forbids cyclic dependencies between definitions, but no such constraint exists on the implicit relationships. We can view Awelon dictionary objects as hypermedia resources.

Intriguingly, *Awelon evaluates to Awelon*, preserving its link structure and meaning. Hence, we can view this as *hypermedia evaluates to hypermedia*. We could model texts that evaluate to canvases and tables, and vice versa. With program animation, we could render interesting intermediate results during evaluation. The equivalent of HTTP POST forms might be represented in terms of cloning a template object - the initial 'form' - for each action, then editing it. 

We can also recognize URL values as we might present them in a work order, and render those values appropriately. Thus, integration of hypermedia with the rest of the world can be performed in the view layer.

## Compositional Views

Evaluating and caching words can support spreadsheet-like propagation of updates, but that alone is insufficient to efficiently work with words that contain large objects. In context of command pattern, for example, we'll have small changes to large objects. 

To fully leverage cache, favor compositional views on persistent objects:

        ∃f. ∀x,*,y. view(x * y) = f(view(x),'*',view(y))

That is, the view of a composition is a composition of views. With this, we may explicitly memoize `view(x)` and `view(y)`. Our assumption of persistence (i.e. a lot of structure sharing) means it is unlikely both `x` and `y` have changed, so we can reuse at least part of the cache. In ABC, we would express this memoization as something like `[[X]view](memo)`. 

Fortunately, a lot of views and data fit these constraints or at least can be adapted by keeping metadata with the view. Thus, we can support spreadsheet-like propagation even in contexts like time-series data and massive key-value databases.

*Aside:* Memoization does have overhead for serialization and storage. To mitigate this overhead, we should memoize only at coarse, stable boundaries. This isn't difficult. For an LSM tree, with natural batching, we might simply cache everything but the root node. For time-series data, we might batch sample counts into frames, and cache only on the first frame.

## Publish Subscribe

Publish subscribe is a model for continuous, live programming of real world systems.

Command pattern provides a basis for small updates to large objects. Compositional views provide a basis for efficient ad-hoc subscriptions including large objects. Between these, large Awelon resources can be observed and updated in real-time by both human and software agents. A human may subscribe to dashboard views and use commands (or direct manipulation) to update policy. An 'internet of things' device might use a command pattern to publish time-series data while subscribing to a schedule of goal states.

The ability to share flexible views of large objects without requiring each endpoint to manage fine-grained update events makes Awelon a lot more expressive and easier to use than many state-of-the-art publish subscribe systems.

## Effectful Work Orders

A RESTful pattern for effectful systems is to model each request as a first class resource - a [work order](https://en.wikipedia.org/wiki/Work_order) (of sorts) to be fulfilled by agents in a multi-agent system. 

Agents party to this system would search for orders matching some ad-hoc conditions (e.g. unclaimed, unfulfilled, authorized, and within the agent's domain). Upon discovering suitable orders, the agent may staje a claim, perform some work, then update the order to indicate progress or completion. A single order may be fulfilled by multiple agents over time. In the general case, subordinate orders may be constructed to handle subtasks.

Both human and software agents may participate.

Modeling orders in a codebase or database is similar in nature to the [tuple space](https://en.wikipedia.org/wiki/Tuple_space) concept. The main difference is the proposed method for mutual exclusion: instead of *removing* a tuple, we might stake a 'claim' on an order. Use of claims is more expressive for long-running tasks with publish-subscribe views, scheduling or expiration of claims, and concurrent interactions (interrupts, collaborative claims, etc.). 

Large orders amortize the search, claim, and update overheads over multiple operations. In practice, orders will include lists, conditional decisions, loops. Sophisticated orders might be modeled as monadic tasks or a reactive process networks. Conveniently, Awelon's rewrite-based evaluation enables arbitrary incomplete tasks to be stored to the codebase, which allows checkpointing, scheduling, or collaborative work with other agents.

## Tables and Databases

Modeling tables or databases within the dictionary is straightforward. For example, a command pattern might represent an append-only log for a table, or collection thereof. The challenge is everything else - indexing, queries and query optimization, incremental computing. Fortunately, indexes can generally be modeled as compositional views. This allows indexing to be incremental using the same techniques described earlier.

The main challenge, I suspect, is that we'll frequently update fields that are not relevant for computing a given view, or some fields will update more frequently than others. I imagine this will require careful attention to support fine-grained memoizations, separating columns of the database, arranging for stable intermediate views, etc..

*Aside:* I suspect 'spreadsheets' in Awelon would best be modeled as first-class tables, such that we can usefully summarize them into reports and other structures. In context of a spreadsheet, a 'column' could be original data, or computed using relational algebra and so on.

## Managed Dictionaries

We may need to perform garbage collection at the dictionary level, eliminating words and collapsing command histories that are no longer relevant. This could be performed by a software agent, e.g. assume three attributes:

* *opaque* - definition structure is irrelevant and may be rewritten
* *frozen* - behavior of this word should never change in the future
* *hidden* - assume no external references directly access this word

This would admit a corresponding set of rewrites:

* *opaque* definitions may be simplified, evaluated, compressed
* *frozen* definitions may be *inlined* into *opaque* clients
* *hidden* definitions may be *deleted* if they have no clients

Hence, we can evaluate and optimize opaque words, link frozen words, and GC the hidden words. Each attribute gives our software agent a more opportunity to safely rewrite, manage, and optimize the dictionary in specific ways. We can assume secure-hash resources are frozen and hidden, but not opaque. Future values might be assumed opaque and frozen, but not always hidden.

Representation of these attributes is ad-hoc, subject to de-facto standardization. We could add tag fields on a dictionary object. Or we might model general policies under `META.GC`... which might be sensitive to tag fields. Whatever.

### Dictionary Based Communications

Awelon dictionaries offer an interesting foundation for communication due to the secure hash resources and implicit dictionary compression. 

A message becomes a (root word, dictionary) pair. The root word allows implicit reference to dictionary object fields for hypermedia and documentation purposes. The dictionary is likely a patch deriving from more common dictionaries via secure hash. By reusing secure hashes, we can efficiently cache the vocabulary across many messages. By developing a suitable vocabulary, each message can effectively be templated and highly compressed.

## Multi-Dictionary Applications

Awelon does not support foreign functions, not even for linking between dictionaries. Interaction with external resources must be modeled effectfully, for example via work orders or publish subscribe. There is no special exception for dictionaries. However, sharing between dictionaries will likely benefit from simplified translations.

