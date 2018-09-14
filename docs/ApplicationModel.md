
# Application Models for Awelon

## Goals

A goal for Awelon project is to integrate the programmer and user experiences. 

Today, using conventional application models and programming languages, the programmer exists "above" the program and "before" its execution. In contrast, the user exists "within" living seas of data, shared with other users. Programmers have flexible and fine-grained control over how data is integrated or analyzed. In contrast, users have coarse-grained "user interfaces" that are inflexible, and it's awkward to share results between these interfaces. 

If successfully integrated, the programmer-user lives within the program, which is also a living sea of data shared with other programmer-users. The programmer-user can integrate and analyze data in a flexible, fine-grained manner. Source data is composed transparently with computed information. Yet, for aesthetics and convenience, it must be possible to overlay attractive interfaces with coarse-grained views and manipulations of a system. Data and computation should both be more accessible.

## Regarding Conventional Models

Although I have some lofty goals, I believe it's best to get started swiftly with conventional application models. In purely functional languages, conventional applications can be modeled using simple functions with continuations. For example:

        -- imperative process with synchronous effects
        type Imperative eff r = ((eff a) * (a → Imperative eff r)) + r

        -- blackboard systems, concurrent shared state rewriting
        type BB st = st * List of (st → (1 + BB st))

Besides these, we can model state machines, component entity systems, mobile processes navigating a map, multi-threaded imperative systems with channels or rendezvous, stream processing pipelines or Kahn process networks, and so on. In any case, 'effects' must be modeled in terms of incremental exchange of *values* between application and environment.

With conventional models, we can develop useful software artifacts, whether it's a web-server or interactive fiction. We could compile and run the application outside the dictionary, or embed it within the dictionary.

It's feasible to compile programs for conventional operating systems. Further, because these models are first-class values, it isn't difficult to simulate or checkpoint a process within the dictionary by checkpointing the current state (and stowage dependencies) back into the dictionary. 

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

I assume there exist multiple agents maintaining a shared community dictionary. The different agents may be authorized to maintain different components or volumes, usually distinguished by prefix (like `weather-*` for a weather almanac). Definitions within the community may be automatically curated, e.g. via type checking, integration testing, enforcing locality constraints (e.g. that `foo-local-*` and `(seal-foo)` are only used from words of form `foo-*`), and so on. 

This models an entire "sea of data" as a dictionary.

But many agents won't use all the data, nor even be authorized for all data. If it's simply not using data, that can be implicitly handled by lazy download of secure hash resources. For example, the `/weather- secureHash` resource can be lazily downloaded when required. But securing data would require extra attention.

## Access Control

I can see two basic options for securing data. In the first case, we assume a multi-agent shared dictionary, but agents may have limited heterogeneous authority regarding which words they observe or edit. In the second case, we assume the dictionary is the smallest granularity for access control, so an agent has full control of its dictionary, but we restrict construction of said dictionary based on agent authority.

To restrict agents within a dictionary seems very ad-hoc. We can apply simple package-style implementation hiding techniques with value sealers, restricting `foo-local-*` to `foo-*`. Rich access control restrictions could feasibly be represented under `foo-meta-access` together with some authorizations model. Alternatively, we could attempt to augment dictionary representation at a more fundamental level for security, similar to [Tahoe-LAFS](https://www.tahoe-lafs.org/trac/tahoe-lafs).

To restrict construction of a dictionary, we can leverage package-based software distribution. For example, we might give our `weather-*` package only to paying clients. The agent might record a URL, authorization, and authentication requirements (such as third-party signatures) in `local-install-weather` and automatically update the package.

I favor the second option. It avoids any ad-hoc models at the Awelon layer, and it offers more power to the individual agent to choose alternative package sources. Thus for now I assume we'll use package-based distributions for sensitive or expensive data. A community package repository could serve a similar role as an app-store and guard against name conflicts.

## Soft Real-Time Updates

Conveniently, Awelon's dictionary representation makes live dictionaries or even live packages easy to update live via binary streams. We can easily distribute live data after it has been already formatted for the Awelon dictionary. Further, it's robust to disruption because the 'update events' contribute in such a simple way to the Awelon dictionary's current state.

We would need external services to validate updates in real-time, e.g. to ensure type safety, guard against undesirable package dependencies, or even test correctness of data. In this sense, updates must go through proper 'channels' to be distributed.

## Stateful Applications

To model stateful applications, we have two basic options: 

* maintain a representation of application state in the dictionary
* recompute state from the time-series history of relevant events

However, we can eliminate the first option. Maintaining state is semantically awkward because the current state will depend on *when* the application was evaluated. Further, it requires centralizing the "inputs" to the application in a manner that does not work nicely with data packages.

Thus Awelon systems will normally use time-series data to compute the current application state. The "time-series" feature is important because it provides a common dimension to merge multiple independent input sources into a shared stream where the bulk is relatively stable and thus subject to *Memoization*. A weakness of this design is the inability to easily forget old data, but we could design for lossy history models (for example, probabilistic or monoidal), allowing us to summarize past events to recover space.

*Aside:* At very large scales, we also require spatial partitioning of our stateful resources. This can be modeled by adding latency to logical timestamps based on the origin of data, simulating transmission or processing delays.

## Modeling Effects and Interactions

Effects are interactions between a computation and its environment.

Two relevant environments are the "real world" and the "dictionary". Applications must interact with the real-world to have useful 'effects'. But modeling interactions within the dictionary are convenient for system extensibility.

Fortunately, modeling interactions within a dictionary is not too difficult. First we model a shared environment, such as a network, a publish-subscribe bus, or a multi-user dungeon. Second, we "install" applications (services, time-series inputs, etc.) within said environment. Finally, we compute the current state of the entire environment. In general, we should model the shared environment rather than individual apps. 

A major challenge for real-world interaction is that *definitions are unstable*, yet we cannot undo past real-world effects. Use of time-series data can help resist instability, but won't help for adding features or removing bugs. Consequently, we should favor interaction models that are robust to historical instability: IO must be asynchronous, inputs should avoid application-specific identifiers (such as allocated actor IDs), and  outputs must not assume exclusive control. Consequently, outputs might be taken as "advice" to guide some external device.

There are some interaction models that fit both criteria, such as [publish-subscribe](https://en.wikipedia.org/wiki/Publish%E2%80%93subscribe_pattern), [entity-component-system (ECS)](https://en.wikipedia.org/wiki/Entity%E2%80%93component%E2%80%93system), or even [functional reactive programming (FRP)](https://en.wikipedia.org/wiki/Functional_reactive_programming). Although FRP is normally associated with a single top-level behavior, we could feasibly adapt it for use in a shared environment.

        type Registry  = Name → SharedEnv →  Behavior
        type SharedEnv = Name → Behavior
        registryToEnv :: Registry → SharedEnv
        registryToEnv = // fixpoint and memoization magic

There are other concerns to contemplate. For example, ECS and FRP are deterministic up to input. ECS is easier to extend (with new system rules, new components/topics, new entities) by default, while FRP is easier to compose. And, of course, performance is very relevant.

In any case, we must wire computed advice back to real-world devices. Wiring outputs to 'continuous' operators seems simple enough. But there is a lesser challenge regarding integration with imperative APIs. It seems feasible to resolve this by ad-hoc means, e.g. first integrate outputs with an external publish-subscribe system then have external agents translate published data to imperative operations.

## Conclusion

It seems that FRP or ECS would make a reasonable application model for Awelon systems, despite being awkward for integrating with imperative systems. It's at least worth trying.

# Second Class Models

Besides trying to solve the general problem, it seems we could represent useful application models at the dictionary layer via projectional editors.

## Spreadsheet-like Applications

Spreadsheets are among the most natural applications to express in Awelon. The dictionary and definitions already have spreadsheet-like behavior, but lacks the spreadsheet *layout*. We can resolve this via projectional editor. For example, we could treat a prefix `sheetname-*` as a spreadsheet, using names like `sheetname-3-c` (or `-c-3`, for column-row order). We can then render the entire volume as a spreadsheet.

Alternatively, we could simply define a set of related variables under a `sessionid-*` prefix, with ad-hoc names and connectivity, and display all the variables together as a worksheet with local names whose definitions can be redefined or tweaked. This gives spreadsheet-like characteristics without the table structure. We could also represent this with nodes and wires.

Intriguingly, our spreadsheets can support *Stateful Applications* if the inputs reference time-series data that is managed elsewhere in the dictionary. We can also directly include application states in our views.

## REPLs and Notebook Applications

A read-eval-print-loop (REPL) can be modeled as a spreadsheet-like application in a single dimension, where each line starts with a reference to the prior line, and we have a clear final line:

        :repl-foo-0 repl-init
        :repl-foo-1 repl-foo-0 command-1
        :repl-foo-2 repl-foo-1 command-2
        ...
        :repl-foo-42 repl-foo-41 command-42
        :repl-foo-hd repl-foo-42

This could easily be rendered as a REPL session, with an output rendered at each line. We can recompute outputs whenever the dictionary or underlying data changes. Adding a new line requires only two changes to our dictionary: add a definition to the new line, and modify the head entry. If outputs are rendered graphically (depending on our *Editable Views*), this might be better compared to a matlab session or [jupyter notebook](http://jupyter.org/).

## Forum-like Applications

We can drop the head entry from a REPL or Notebook, and still compute the relationships using a reverse-lookup index. For example, we can reverse-lookup all references to `repl-foo-1`, and filter for those where the first word is `repl-foo-1`, and we'd allow for a many-to-one relationship between an entry and replies. Taken transitively up to, say, five levels and we'd have a typical forum-like display. Optionally, we could relax the first word filter or filter for replies within the thread `repl-foo-*` to further tweak the threading model.

In any case, this produces an append-mostly tree structure (or acyclic graph) with spreadsheet and REPL-like characteristics. This could make an interesting basis to model actual multi-user forums, allowing for data-rich "discussions" that draw data from packages and compute a shared environment, especially if taken together with *Natural Language Inspired Meta-Programming* to build partial models with soft constraints.

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

# Brainstorming

## Hierarchical Dictionary Structure (Rejected For Now)

We could efficiently represent hierarchical embedding of dictionaries by using a `dictname/` prefix with implicit scope. For example, `:d/foo bar baz` will implicitly depend on `d/bar` and `d/baz`. With this, we could update individual words deep in the hierarchy or update the full embedded dictionary as a one-liner `/d/ secureHash`. We can extend Awelon syntax so the host can reference words of form `d/foo`. 

Embedded dictionaries have potential to simplify issues related to name conflicts and information security, which would otherwise handled in an ad-hoc manner via community dictionaries and static analysis. But the advantage gained seems to be limited. 

A significant disadvantage is that we require the aesthetically awkward `d/42` or `d/"hello world"` to indicate dependency on `d/succ`, `d/cons`, and so on. For convenience and consistency, we might accept `d/[x y z]` as equivalent to `[d/x d/y d/z]`, such that `d/42 => d/[41 succ] => [d/41 d/succ]`. Aesthetics could feasibly be mitigated by *localization* - allowing a rewrite from `d/foo` to `foo` when the meaning is obviously equivalent, which should be the case for common data types.

Another disadvantage is that there is reduced pressure to develop a common community with shared data and language. One of Awelon's goals with the dictionary model is related to easy sharing within a community, the ability for humans in a community to learn and share a common set of 'words' to communicate and share computation artifacts with each other.

At this time, I'm uncertain whether hierarchical dictionary structure is is worth these costs. Fortunately, it is easy to delay introduction of hierarchical dictionary structure without risk of incompatibility. I will leave this feature out of Awelon unless there is sufficiently strong and clear use case.

## Local Identifiers (Rejected)

Assume we're defining `very-long-prefix-x` and we want to reference `very-long-prefix-y`. This seems a common scenario for Awelon systems and application models. It could be convenient to support a shorthand, for example just use `-y` (perhaps with more hyphens to step further back). That said, if we're only interested in *convenience*, it seems sufficient to address this via projectional editor.

What are the benefits of this feature at the Awelon layer? A moderate space savings (but not as much as compressed stowage, and only up to a hundred bytes per reference). A potential benefit for copying or renaming prefixes (but we must be careful about renaming prefixes containing hyphens, and we must still search for the full version of a word). 

It seems to me the potential benefits aren't worth the overheads and complexities this would introduce. If the storage is tempting, it would seem much wiser to simply use compressed storage, like LevelDB, for secure hash resources. But even uncompressed, we aren't working with overly large entries.

