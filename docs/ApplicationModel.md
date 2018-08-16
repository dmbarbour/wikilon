
# Application Models for Awelon

## Goals

My goal for Awelon project is to integrate the programmer and user experiences. 

Today, using conventional application models and programming languages, the programmer exists "above" the program and "before" its execution. In contrast, the user exists "within" living seas of data, shared with other users. Programmers have flexible and fine-grained control over how data is integrated or analyzed. In contrast, users have coarse-grained "user interfaces" that are inflexible and it's awkward to share results.

If successfully integrated, the programmer-user lives within the program, which is also a living sea of data shared with other programmer-users. The programmer-user can integrate and analyze data in a flexible, fine-grained manner. Yet, for aesthetics and convenience, it must be possible to use attractive interfaces with coarse-grained views and manipulations of a system.

## Quick Start via Conventional Models

Although I have some lofty goals, I believe it's best to get started swiftly with conventional application models. In purely functional languages, conventional applications can be modeled using simple functions with continuations. For example:

        -- imperative process with synchronous effects
        type Imperative eff r = ((eff a) * (a → Imperative eff r)) + r

        -- blackboard system, concurrent shared state rewriting
        type BB st = st * List of (st → (1 + BB st))

Besides these, we can model anonymous mobile processes that navigate a shared map-like environment; multi-threaded imperative systems that communicate by taking, modifying, and returning entire volumes of shared memory; stream processing pipelines or Kahn process networks; component entity systems or simulations that model periodic update behaviors over large tables of data; publish-subscribe environments; and so on. In all cases, "effects" are modeled in terms of incremental output and input of values by the environment.

By starting with conventional models, we can develop useful software artifacts right away. It's feasible to compile programs for conventional operating systems. Further, because these models are first-class values, it isn't difficult to simulate or checkpoint a process within the dictionary by either rewriting state in-place or recording the incremental inputs and computing the current state. 

The main disadvantage is that most conventional models don't remain attached to the user very well. Fortunately, multi-agent systems (like the blackboard, mobile processes, or a publish-subscribe environment) can help, since we can treat the users as some of the agents.

*Aside:* Object identity is semantically awkward in purely functional languages. It isn't difficult to represent, e.g. using natural numbers. But it requires fine-grained effects, manual memory management, and a relatively 'flat' namespace. I would not favor application models or effect APIs that rely heavily on object identity. But an exception can be made for component entity systems and similar data-driven designs, which are a pretty good fit for purely functional computations.

## Foundation

The dictionary can serve as a spreadsheet-like filesystem or database, with structured data and transparent procedural generation of data. Its representation is a persistent LSM-trie, which allows for efficient writing, sharing, scaling, and versioning. It's feasible to shove both code and data into the dictionary, and to freely compose computed data with source data.

With *Stowage* and *Memoization*, it's feasible to support large-scale computations and spreadsheet-like incremental updates without requiring an external database and explicit caching. These features stretch the limits of what purely functional languages can do without explicit side-effects.

The lightweight syntax, local rewriting semantics, and lazy linking of words will simplify projectional editing, rendering of results, debugging, and immediate reuse of results for further computation. These properties allow us to treat code as a user interface, for example.

The concatenative structure of both Awelon and the dictionary allows us to conveniently model programs or user-inputs as streams, append-only logs. It's feasible to treat the user actions as continuously extending a program or editing a dictionary.

Awelon's features offer a foundation to build upon, but not a complete model.

## Multi-Agent Systems and Partitioning

Too many cooks spoil the broth! 

At large scales, we have too many users, projects, bots, etc.. This leads to various naming conflicts and information security concerns. Awelon provides a limited tool to mitigate this: hierarchical embedding of dictionaries via qualified `dictname/` prefixes. How we should wield this tool is a relevant question. 

There are two basic ways we can embed things: 

1. embed world into agent: system data is 'mounted' into agent dictionary
1. embed agent into world: agent controls a prefix under world dictionary

In the first case, I allude to Linux file-system mounting. For example, we may have a dictionary under `sys/` or `weather/` that contains data automatically synchronized from external sources. Mounts are normally read-only, but it's feasible to intercept write operations and translate them to HTTP POST or PUT on a remote source. Authorities associated with the mount are easily recorded within the agent dictionary, e.g. under `mount-sys` and `mount-weather`. Conversely, we may publish locally maintained dictionaries so other agents may mount them. For example, we could have a `db/` and a `share-db` word to specify how it's shared.

In the second case, agents would operate under some sort of `agent-home/` subdirectory, but only have limited authority to view and manipulate the host dictionary. Applications that integrate data from multiple sources can only be represented in the host dictionary. It's feasible to record a value into a shared registry or environment, or model applications in the host, but it's unclear which agents should be responsible for maintaining applications, controlling type safety problems, or managing the registration, or how much authority any given agent should have over the host.

Between these options, the first - embedding world into agent - is by far the better fit for Awelon's goals. It gives each agent the greatest control over its codebase, and integration with the world. It's easy to create useful views of living systems on a per-agent basis. Agents run their own 'applications' to maintain, compute, and share data, and may freely tweak the application models. Responsibility and trust are relatively clear.

*Aside:* There may be some issues with cyclic mounts forming continuous update cycles. But this could be corrected if we normally exclude mounted data when sharing dictionaries. That is, drop `sys/` when we have `mount-sys` and instead require the recipient to process the mount. Mounting could also be lazy, to avoid unnecessary mount efforts.

*Note:* Besides mounting under `env/` it's also feasible to mount under `env-`. In the latter case, we'd be giving the mount full access to the rest of our dictionary, so it would essentially model a dataflow component.

## Stateful Applications

There are basically two ways to model stateful applications:

* recompute state: from initial state and input history
* save state: write or checkpoint value into dictionary

Recomputing state has some nicer properties for working with decentralized input sources (such as time-series databases), system extension (adding new applications to the existing system), and debugging (rewind, replay). In contrast, saving state gives us better control over space requirements, avoids huge recompute efforts, and integrates conveniently with synchronous effects models. Also, if we change definitions a little after saving state, we could simply continue with the new definitions.

In context of Awelon systems where data is provided through mounted dictionaries, much data is decentralized. Further, with agents going online and offline, system extensibility is a relevant feature. It seems to me that recomputing state must be the normal modus operandi for modeling stateful applications in Awelon. 

Unfortunately, recomputing state interacts awkwardly with incremental effects and the potential to change inputs or program definitions. We cannot change which effects we've performed in the past. This issue can be mitigated if we avoid synchronous effects models, such that our inputs don't need to correspond directly to past outputs. Application models with highly asynchronous effects APIs, such as publish-subscribe, are the best option if we're going to regularly recompute state.

Careful use of *Memoization* can mitigate recompute efforts. And gradually forgetting or summarizing past inputs can help constrain space costs.

## Effects and Environments

If our world is modeled as embedded dictionaries, then the basic effect is to observe changes in the world or to cause changes to it. The former is simply a natural consequence of updates to a mounted partition. The latter requires writing to a partition we share with other agents, which they may mount. We basically have a publish-subscribe system where reading a mount corresponds to a subscription, and writing to a shared dictionary corresponds to publishing a topic.

If we pursue this application model, then we'll need to model software agents to maintain our shared data and so on. 

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

## Managed Dictionaries

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

Awelon project started with a vision: a stream of natural language manipulates a 'context' in the listener. Rather than a concrete model, this context is full of disjoint ideas and concepts, vagaries and ambiguities, resources and data, soft desiderata and hard requirements, with some meta-knowledge of how to search for a model instance. The partial model itself corresponds to an arbitrary type, which might be composed with other partial models, which further constrains and refines the selection of partial models based on composition compatibility. 

A longer stream will incrementally clarify, connect, refines, extend, and modify this context. Ideally, we can incrementally *render* this context to a human or agent, perhaps in terms of example search results. By iteratively extending the stream and rendering the context, we form an ad-hoc dialogue between the agent and the Awelon system. It is feasible for multiple human and software agents could cooperatively participate, sharing influence on a model like a blackboard system metaphor, indirectly forming a dialogue between all involved agents. This also provides a basis for generic programming, since program fragments would represent partial programs that adapt to their context.

Awelon programming language was influenced by this vision insofar as it is a designed for streaming programs (via concatenative structure), partial evaluation (via purity, local rewriting), and massive scale with embedded data (via dictionary representation, hierarchical component structure, binary large objects, and stowage). However, Awelon's primitives are very low level. We must build an enormous foundation before we have Awelon 'words' suitable for manipulating and refining an ambiguous context! Before we achieve this, we can also leverage simpler intermediate ideas like [Dyna](https://github.com/nwf/dyna) or [μKanren](http://webyrd.net/scheme-2013/papers/HemannMuKanren2013.pdf).

This is a long-term goal for Awelon. And it's applicable to almost any output type, including documents or presentations or interactive software. We'll still need a suitable target model in any case. So this is ultimately more of a meta-programming concept.

