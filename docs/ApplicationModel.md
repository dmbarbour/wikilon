
# Application Models for Awelon

There are two basic kinds of applications for an Awelon environment:

* software agents that interact with systems and maintain the dictionary
* internal structures with live data and direct manipulation

 two *kinds* of applications in an Awelon system:


bots as
* transactional scripts on repeat
* system of constraint variables
* RDP behaviors with shared sets
* actors with mailboxes

editors
* scripts for macro edits
* 


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

## Stateful Applications

To model stateful applications, we have a few basic options: 

* directly maintain representation of app state in the dictionary
* recompute state from temporal databases, input event histories
* enable apps to manage explicit record of state, limited structure

I'd prefer to eliminate the first option. Maintaining state with lazy evaluation of words is semantically awkward in context of a mutable dictionary. It also hinders debugging and repairing in case of an incorrect definition, which makes the system more fragile. Further, it's mechanically awkward because app states will tend to grow larger than their initial definitions (though *stowage* can help).

Assuming we favor the second option, we'll be depending heavily on databases and time-series inputs. Fortunately, it's quite easy to model time-series data using an LSM-trie indexed on timestamps or a similar mechanism, and such representations should work very nicely together with memoization and caching. The main concern with keeping input histories is that they might become very large for long-running applications. For those cases, we must model *lossy* histories, e.g. using a windowed or exponential decay model or a probabilistic frame model, then carefully design our applications to be *robust* in context of these lossy records.

While that's doable and fits many use-cases, I think programmers would complain it's too restrictive. 




So we'll 

Thus Awelon systems will normally use time-series data to compute the current application state. The "time-series" feature is important because it provides a common dimension to merge multiple independent input sources into a shared stream where the bulk is relatively stable and thus subject to *Memoization*. A weakness of this design is the inability to easily forget old data, but we could design for lossy history models (for example, probabilistic or monoidal), allowing us to summarize past events to recover space.

*Aside:* At very large scales, we also require spatial partitioning of our stateful resources. This can be modeled by adding latency to logical timestamps based on the origin of data, simulating transmission or processing delays.


## Binding Live Data

How should we get live data into our dictionary?

One option is to have some live "packages" like a `weather-*` package for weather data. This package could be published through a shared server. Updates to the package would result in a later update to some dictionaries, potentially in near real-time. This seems useful for one-to-many distributions, but perhaps not for many-to-many communications.

An ad-hoc option is software agents that interact with a network and a dictionary. For these agents we could try to record their state, or simply allow an agent reset at any time. Either way, the agents could be defined within the dictionary, and would allow flexible interactions with the real world.

An intermediate option, perhaps, is to model publish-subscribe at the dictionary layer. The dictionary describes some subscriptions and published elements, and the system inputs and outputs some data accordingly. We might have an entire temporal database of published elements, and also receive a temporal database for each subscription.

It's also feasible to represent mailboxes within a dictionary, allowing for incoming messages to be recorded and outgoing messages to be acknowledged. Although fire-and-forget message passing is a terribly awkward fit for Awelon, a monotonic temporal database with a full history of messages should work well enough, and limiting messages to binaries (or another constrained vocabulary) would mitigate issues related to incompatible definitions between dictionaries.


 Although message-passing is an awkward fit, it is much less a problem if we can assume a monotonic system where all inputs and outputs are recorded isn't a bad fit. 

As a specialization of publish-subscribe, we can support active message-passing dictionaries. We simply have a unique "input channel" for our incoming messages (perhaps based on a secure hash of a public key). And similarly, we publish to many output channels. 

models with inboxes and outboxes. Basically, we just need publish-subscribe where 

 of binary data, we could support networked dictionaries. Basically, all we need

 

The network interaction would involve waits (to read a socket) while a transaction might involve explicit retries (wait for change in any definition read within transaction). Because it's a stateless agent, we don't need to record agent state - we simply restart as needed.





 execute the same behavior: interact with a network and construct a transaction on a dictionary, potentially waiting on the network or waiting on an independent change within the dictionary. 


 In this case, our "agent" might be a monadic script that interacts with a dictionary and a network, perhaps building one transactional dictionary update then repeating (with an option to wait for a read definition to change). These active agents could be defined within the dictionary and executed by an external system, and would be flexible in their external behavior.






 that will, via network sockets and reflection, incrementally maintain the dictionary. 


Agent state could be recorded back to the dictionary, or we could leave that ad-hoc and simply assume the agent can be reset at any time.



 describe monadic "agents" that have access to sockets and reflective access to the dictionary. 

These agents will obtain real-world data, publish it into a dictionary. But this approach has a lot of issues: where is agent state recorded? if we're synchronizing another dictionar

An interesting option is to adapt publish-subscribe models. Some words within a dictionary could describe



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

## Modeling Effects and Interactions

Effects are interactions between a computation and its environment. It's useful to model a "logical environment" that we can easily extend with new behaviors or input sources. Real-world inputs can always be recorded into a dictionary, e.g. as time-series data.

Unfortunately, we cannot assume stability of *outputs* to the real-world. Not when the definitions of applications might change at any time. Use of time-series data can improve stability enough to leverage memoization for stateful computations, but doesn't make strong guarantees.

Thus, our real-world interaction model should be robust to unstable behavior.

This implies asynchronous interaction, stable input identifiers, and soft requests. Asynchronous interactions are robust to small changes in order or timing of requests. Stable input identifiers (in contrast to spawned actor IDs, pixel offsets, etc.) can be recorded in the real-world input history without changing meaning. Soft requests don't assume exclusive control over the environment, i.e. we cannot assume there is a strong relationship between past outputs and future inputs. Outputs can be taken as advice to influence external devices.

There are some interaction models that fit these criteria, such as [publish-subscribe](https://en.wikipedia.org/wiki/Publish%E2%80%93subscribe_pattern), [entity-component-system (ECS)](https://en.wikipedia.org/wiki/Entity%E2%80%93component%E2%80%93system), or even [functional reactive programming (FRP)](https://en.wikipedia.org/wiki/Functional_reactive_programming). Of these, ECS and FRP have the advantage of being inherently deterministic, whereas pubsub requires modeling an arbitrary scheduler (which complicates reasoning about real-time behavior, etc.). But FRP is a little difficult to extend or model a shared registry of services.

## RESTful Applications

As an alternative to modeling stateful interactive apps, we could separate the "stateful" from the "interactive". For example, we can model stateless functions that reference and update a shared database in a manner reminiscent of web-services. Indeed, we could directly implement web services in this way, assuming we develop a suitable model for reflective updates to databases within a dictionary.

## Conclusion

It seems that FRP or ECS would make a reasonable application model for Awelon systems, despite being awkward for integrating with imperative systems. It's at least worth trying. Meanwhile, RESTful or conventional apps make acceptable fallbacks.

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

We can drop or ignore the head entry (`repl-foo-hd`) from a REPL or Notebook, and still compute the relationships using a reverse-lookup index. For example, we can reverse-lookup all references to `repl-foo-1`, and filter for those where the first word is `repl-foo-1`, and we would find `repl-foo-2`. But in general, this would allow for a many-to-one relationship, multiple "direct continuations" of `repl-foo-1`. Alternatively, we can relax the first-word constraint to find more reactions in a directed acyclic graph, or limit our view to thread-local reactions via `repl-foo-*`.

If we compute reactions to some depth (such as five levels), we might render it as a forum thread with occasional "discussion continues here" links. Besides source, we can render the computed values at each step. This could represent a discussion forum with embedded data and spreadsheet-like characteristics. This could be especially interesting if we're working with higher level functions that construct partial models with soft constraints (cf. *Natural Language Inspired Meta-Programming*), so our "discussion" is highly robust and reactive to incremental changes.

Compared to the linear REPL or Notebook, the forum tree structure is relatively awkward to render. But it's much more amenable to multi-party discussion.

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

I believe this would offer a powerful basis for generic programming, adaptive programming, program refinement, and application development. We can also leverage intermediate models like [Dyna](https://github.com/nwf/dyna) or [μKanren](http://webyrd.net/scheme-2013/papers/HemannMuKanren2013.pdf). But we should certainly use a "scored" logic of some form: search is much more efficient if we can focus on a few high quality options.

This is a long-term goal for Awelon. It's not a complete application model, but could be used to construct other application models that are more robust and adaptive to changes in data or definition.

# Brainstorming

## Hierarchical Dictionary Structure (Rejected)

We could efficiently represent hierarchical embedding of dictionaries by using a `dictname/` prefix with implicit scope. For example, `:d/foo bar baz` will implicitly depend on `d/bar` and `d/baz`. With this, we could update individual words deep in the hierarchy or update the full embedded dictionary as a one-liner `/d/ secureHash`. We can then extend Awelon syntax so the host can reference words of form `d/foo`.

Embedded dictionaries have potential to simplify control over name conflicts and information security, which would be otherwise handled in an ad-hoc manner via community dictionaries and static analysis. 

But there are some significant disadvantages. First, the resulting dictionary is aesthetically awkward, occasionally requiring `d/42` or `d/"hello world"`. We could support `d/[x y z]` but it complicates parsers and interpreters. More importantly, there is a broad impact on the social level if we reduce pressure to develop references (words) with globally shared meanings. 

With a flat namespace, developers can more easily share data. We may need ad-hoc analysis to control dependency relationships between "packages", and we'll need a community center (like a package registry) to resist name conflicts. But we can develop an environment where words have stable meanings - a shared language.

## Local Identifiers (Rejected)

Assume we're defining `very-long-prefix-x` and we want to reference `very-long-prefix-y`. This seems a common scenario for Awelon systems and application models. It could be convenient to support a shorthand, for example just use `-y` (perhaps with more hyphens to step further back). That said, if we're only interested in *convenience*, it seems sufficient to address this via projectional editor.

What are the benefits of this feature at the Awelon layer? A moderate space savings (but not as much as compressed stowage, and only up to a hundred bytes per reference). A potential benefit for copying or renaming prefixes (but we must be careful about renaming prefixes containing hyphens, and we must still search for the full version of a word). 

It seems to me the potential benefits aren't worth the overheads and complexities this would introduce. If the storage is tempting, it would seem much wiser to simply use compressed storage, like LevelDB. But even uncompressed, we aren't working with overly large entries - the longest words in practice should be around a hundred bytes.

