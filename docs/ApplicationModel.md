
# Application Models for Awelon

## Goals

My goal for Awelon project is to integrate the programmer and user experiences. 

Today, using conventional application models and programming languages, the programmer exists "above" the program and "before" its execution. In contrast, the user exists "within" living seas of data, shared with other users. Programmers have flexible and fine-grained control over how data is integrated or analyzed. In contrast, users have coarse-grained "user interfaces" that are inflexible and it's awkward to share results.

If successfully integrated, the programmer-user lives within the program, which is also a living sea of data shared with other programmer-users. The programmer-user can integrate and analyze data in a flexible, fine-grained manner. Source data is composed transparently with computed information. Yet, for aesthetics and convenience, it must be possible to overlay attractive interfaces with coarse-grained views and manipulations of a system.

## Quick Start via Conventional Models

Although I have some lofty goals, I believe it's best to get started swiftly with conventional application models. In purely functional languages, conventional applications can be modeled using simple functions with continuations. For example:

        -- imperative process with synchronous effects
        type Imperative eff r = ((eff a) * (a → Imperative eff r)) + r

        -- blackboard system, concurrent shared state rewriting
        type BB st = st * List of (st → (1 + BB st))

Besides these, we can model state machines, component entity systems, mobile processes navigating a map, multi-threaded imperative systems with channels or rendezvous, stream processing pipelines or Kahn process networks, and so on. In any case, 'effects' must be modeled in terms of incremental exchange of *values* between application and environment.

With conventional models, we can develop useful software artifacts, whether it's a web-server or interactive fiction. We could compile and run the application outside the dictionary, or embed it within the dictionary.

It's feasible to compile programs for conventional operating systems. Further, because these models are first-class values, it isn't difficult to simulate or checkpoint a process within the dictionary by either rewriting state in-place or recording the incremental inputs and computing the current state. 

The main disadvantage is that most conventional models don't remain attached to the user very well. Fortunately, multi-agent systems (like the blackboard, mobile processes, or a publish-subscribe environment) can help, since we can treat the users as some of the agents.

*Aside:* Object identity is semantically awkward in purely functional languages. It isn't difficult to represent, e.g. using natural numbers. But it requires fine-grained effects, manual memory management, and a relatively 'flat' namespace. I would not favor application models or effect APIs that rely heavily on object identity. But an exception can be made for component entity systems and similar data-driven designs, which are a pretty good fit for purely functional computations.

## Foundation

The dictionary can serve as a spreadsheet-like filesystem or database, with structured data and transparent procedural generation of data. Its representation is a persistent LSM-trie, which allows for efficient writing, sharing, scaling, and versioning. It's feasible to shove both code and data into the dictionary, and to freely compose computed data with source data.

With *Stowage* and *Memoization*, it's feasible to support large-scale computations and spreadsheet-like incremental updates without requiring an external database and explicit caching. These features stretch the limits of what purely functional languages can do without explicit side-effects.

The lightweight syntax, local rewriting semantics, and lazy linking of words will simplify projectional editing, rendering of results, debugging, and immediate reuse of results for further computation. These properties allow us to treat code as a user interface, for example.

The concatenative structure of both Awelon and the dictionary allows us to conveniently model programs or user-inputs as streams, append-only logs. It's feasible to treat the user actions as continuously extending a program or editing a dictionary.

Awelon's features offer a foundation to build upon, but we need more.

## Binding Live Data

To serve as a "living sea of data", we first need live data in our dictionary.

Dictionaries can support "package" based software distributions. And software includes arbitrary data. For example, we could have a `weather-*` almanac that records weather histories on a regional basis. We can easily update this package with a one-liner `/weather- secureHash` or patch. If we *automate* this update, binding the `weather-` prefix to a remote service, we can effectively have live data. 

Automated package bindings shouldn't be too difficult. It simply requires configuration. This configuration could easily be represented within the dictionary, e.g. using a `local-bind-*` that defines a record with URL, authorization, and authentication. When we don't fully trust a package provider, we could require a trusted third party to test and sign the package as part of our authentication, e.g. to ensure it respects local words, value sealers, type safety. 

We can have a community dictionary that is maintained and curated by multiple agents. It's not difficult to automatically extract packages from a community dictionary, or to bind entire dictionaries but reserve a volume `local-*` for local definitions. Intriguingly, it is not difficult to support streaming updates to a dictionary, although we cannot do nearly as much validation as we could with package level updates.

## Sharing Live Data

For an agent or user to bind live data packages, other agents must publish.

This isn't too complicated. One option is to configure the intention to publish, synchronizing some prefix of our local dictionary into a public volume with appropriate authorizations. Perhaps we use `local-share-*` for configuration. This could be performed at a package level or using an embedded dictionary. Alternatively, we could have remote services into which we publish data, which is then converted to a package that we bind.

Original "source" data will almost always be simple - numbers, texts, binaries, lists, matrices, records, trees - in order to control dependencies. But we can also share derived observations.

A relevant concern is that gathering data can be expensive. Similar to the lazy download of packages, we might want demand-driven maintenance of data. This can be supported indirectly: an agent that shares data will usually do so through an intermediate service, and that service can provide metadata about actual observed demand. This may also require lazy bindings on the client side, however. Larger packages could feasibly be partitioned into smaller ones for fine-grained demand.

## Stateful Applications

Stateful apps in Awelon will normally be represented as computations over time-series data. That is, we partition inputs into multiple time-series databases or streams (which may include user inputs), we incrementally merge this data into a common timeline of events, and we observe the computed state. With careful choice of data structures and *Memoization*, we can cache most of application state and only compute the differences.

The advantage of this design is decentralized input, the ability to develop new applications and views from the databases, the ability to efficiently correct our input histories near the head, and the ability to treat state as a pure spreadsheet-like computation. 

But there are some weaknesses.

First, space overheads. If we're asked to remember our entire input history, that's not going to work nicely in the long run. This might not be a problem for some input types, e.g. posting to a forum. But, for example, we wouldn't want to preserve every character-level edit of every post. This could be mitigated by intelligently scrubbing event histories near each input source. Multiple small edits can be incrementally composed into a larger patch. We can leverage partial history models, e.g. based on exponential decay.

Second, effects models. Any small change to our dictionaries, or adding a new application, could potentially result in alternative requests for action or information. But although we can recompute state, we cannot change past effects. Most problematic are synchronous effects models. 

*Note:* I've considered more conventional approaches, such as checkpointing state into a dictionary or modeling a set of persistent variables by writing to a dictionary. But this interacts very awkwardly with partial evaluation and live maintenance of input sources. I do not feel it is the right path to pursue.

## Modeling Effects

Effects are interactions between a computation and its environment. 

In Awelon systems where we model applications within a dictionary, our "environment" is ultimately a set of external agents. Our "inputs" are provided through binding of live data packages. And "outputs" can only be observed by a small subset of agents that perform a computation. The relevant question is how applications should model outputs to request action or information. 

We can recompute state after a change to code or input, but we cannot easily change our input in response to a change in outputs. For example, Bob asks, "What do you want?", and Alice answers, "Nachos", but then Bob's program changes and the question becomes "Who are you?", the old answer "Alice" is no longer appropriate. Mostly, this constrains against synchronous effects models where interpretation of the next input depends on prior outputs.

An asynchronous variant might instead involve an intermediate database. Bob tells Alice to write certain information to the database. Then Bob looks up each "Who are you?" and "What do you want?" question in an appropriate database. Unanswered questions are written into the output together with final application state. If we use a "local" answers database, we can model multiple instances of an application with different answer sets and hence different states.

*Aside:* In many cases, such as modeling animations or music, it's better to use temporal data types (i.e. so we can query them at a given time frame) rather than model them via second-class effects.




## Spreadsheet-like Applications

Spreadsheets are among the most natural applications to express in Awelon. For example, we can define a word per cell, perhaps using a simple naming convention like using `foo-3-c` to represent the definition of row 3 column c in spreadsheet foo. A spreadsheet-based editable view could then render hundreds of cells in a compact manner.

A read-eval-print-loop (REPL) can also be modeled as a spreadsheet-like application, such that each line starts with the prior line:

        :repl-foo-0 repl-init
        :repl-foo-1 repl-foo-0 command-1
        :repl-foo-2 repl-foo-1 command-2
        ...
        :repl-foo-42 repl-foo-41 command-42
        :repl-foo-hd repl-foo-42

This could then be rendered as a REPL session, with outputs rendered after each line. We can recompute outputs whenever the dictionary or underlying data changes. Adding a new line only requires two changes to our dictionary: add a definition for the new line, and modify the head entry. If outputs are rendered graphically (based on our *Editable Views*), this might be better compared to a matlab session or [jupyter notebook](http://jupyter.org/).

In practice, we may prefer to accompany our spreadsheet-like application with a first-class value representation, e.g. a matrix of `[cell-name]` values. Then we can render the first-class matrix value, or compute a spreadsheet value to render after evaluation.

Spreadsheet-like applications can cover a lot of valuable use-cases - documents, slide-show presentations, system dashboards. But we still need to consider state and effects.

## Computable Forms

The idea:

* we can project some definitions as interactive forms
* copy a template form as needed, integrate edited form

This relies on projectional editors to provide an interactive view for updating a form, e.g. using a checkbox for a boolean or a color-picker in place of an HSV value. We'll simply copy the form as often as required, and integrate the complete template into some input stream. This doesn't generalize well to the larger problems, but forms are useful for many user-interactive applications.

## Effectful Work Orders

Most effect models today are based around imperative commands. But a more RESTful concept is the [work order](https://en.wikipedia.org/wiki/Work_order), a document that describes some work to be done and allows agents to select which jobs they perform. My intuition is that work orders would be a better basis for exporting work requirements to a multi-agent system compared to imperative commands. Of course, any individual work order might consist of a sequence of commands.

This is similar to the [tuple space](https://en.wikipedia.org/wiki/Tuple_space) concept, except that work orders aren't necessarily removed while they're worked upon. They might be updated to indicate who is working on them, though.

# Managed Dictionaries

We may need to perform garbage collection at the dictionary level, eliminating words and collapsing command histories that are no longer relevant. This could be performed by a software agent, e.g. assume three attributes:

* *opaque* - definition structure is irrelevant and may be rewritten
* *frozen* - behavior of this word should never change in the future
* *hidden* - assume no external references directly access this word

This would admit a corresponding set of rewrites:

* *opaque* definitions may be simplified, evaluated, compressed
* *frozen* definitions may be *inlined* into *opaque* clients
* *hidden* definitions may be *deleted* if they have no clients

Hence, we can evaluate and optimize opaque words, link frozen words, and GC the hidden words. Each attribute gives our software agent a more opportunity to safely rewrite, manage, and optimize the dictionary in specific ways. We can assume secure-hash resources are frozen and hidden, but not opaque. However, when a secure hash resource is referenced from an opaque definition, we could rewrite the secure hash to a simplified or evaluated form.

Representation of these attributes is ad-hoc, subject to de-facto standardization. For example, we could define `foo-meta-gc` for elements under `foo`, or we could represent our policies under a global word like `meta-gc`. I only recommend that the policy be separated from the definitions, i.e. using separate words instead of comments.

# Natural Language Inspired Meta-Programming

A stream of natural language manipulates a 'context' in the listener. This context involves disjoint ideas and concepts, vagaries and ambiguities, rich knowledge of the world, desiderata and requirements. We can tell a story if we speak of cause and consequence, describe events over time. It's left to our listener to find a reasonable interpretation, fill in the details.

A stream of Awelon code manipulates a 'context' in the computer. This context is a stack of data and function values. This is much more precise than natural language. But we could take some inspiration from natural language by modeling a 'value' that represents a partial model with hard and soft constraints, such that we can automatically 'search' for valid implementations of the model.

I believe this would offer a powerful basis for generic programming, adaptive programming, program refinement, and application development. We can also leverage intermediate ideas like [Dyna](https://github.com/nwf/dyna) or [μKanren](http://webyrd.net/scheme-2013/papers/HemannMuKanren2013.pdf).

This is a long-term goal for Awelon. But it's not a complete application model.

# Hierarchical Dictionary Structure (Potential Awelon Extension)

We could efficiently represent hierarchical embedding of dictionaries by using a `dictname/` prefix with implicit scope. For example, `:d/foo bar baz` will implicitly depend on `d/bar` and `d/baz`. With this, we could update individual words deep in the hierarchy or update the full embedded dictionary as a one-liner `/d/ secureHash`. We can extend Awelon syntax so the host can reference words of form `d/foo`. Embedded dictionaries would simplify issues of packages related to version hell, global names, information security, separate compilation, and caching. 

A significant disadvantage is that we require the aesthetically awkward `d/42` or `d/"hello world"` to indicate dependency on `d/succ`, `d/cons`, and so on. For convenience and consistency, we might accept `d/[x y z]` as equivalent to `[d/x d/y d/z]`, such that `d/42 => d/[41 succ] => [d/41 d/succ]`. Aesthetics could feasibly be mitigated by *localization* - allowing a rewrite from `d/foo` to `foo` when the meaning is obviously equivalent, which should be the case for common data types.

Another disadvantage is that there is reduced pressure to develop a common community with shared data and language. One of Awelon's goals with the dictionary model is related to easy sharing within a community, the ability for humans in a community to learn and share a common set of 'words' to communicate and share computation artifacts with each other.

At this time, I'm uncertain whether hierarchical dictionary structure is is worth these costs. Fortunately, it is easy to delay introduction of hierarchical dictionary structure without risk of incompatibility. I will leave this feature out of Awelon unless there is sufficiently strong and clear use case.

