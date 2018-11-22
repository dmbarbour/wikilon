
# Application Models for Awelon

Awelon's vision proposes projectional editing for most user-interfaces. Essentially, spreadsheets at scale, with live data and graphical projections. Users can manipulate definitions or view computed data through the same projections. This design has many nice properties, and the results are easily shared or reused. Importantly, projectional editing ensures a more computation environment more accessible and controllable by the user, contributing to an empowered user experience. Projectional editing should be favored where feasible.

However, projectional editing isn't a good fit for every problem.

Awelon proposes bots for background services. A bot is modeled as a transaction, repeating indefinitely, implicitly waiting when unproductive. This also has nice properties - liveness, resilience, extensibility. Effects are modeled via asynchronous sharing of transaction variables, such as task queues. An effectful reflection API enables a bots to maintain a dictionary, and effectful network APIs can connect to remote services or listen on a network.

Where projectional editing is an awkward fit, we can fall back on web applications or hybrid native apps (perhaps via [Jasonette](https://jasonette.com/)). It's also feasible to use the reflection API to cross-compile native applications.

This document sketches how various applications might be modeled in Awelon.

## Spreadsheet Applications

I mentioned "spreadsheets at scale". Let's start with plain old spreadsheets! 

We can project `foo-a2`, `foo-a3`, `foo-b2`, `foo-b3`, and so on as a spreadsheet `foo` with definitions in cells `a2`, `a3`, `b2`, and `b3`. Like most spreadsheets, under normal conditions we would render evaluated normally, and source only for cells with user focus. When we project source, we can treat `foo-` as an implicit namespace, using `$a2` or `A2` to name a local cell.

Awkwardly, a spreadsheet projection can rewrite formulas on copy-paste (in violation of [DRY](https://en.wikipedia.org/wiki/Don%27t_repeat_yourself)), and support a shorthand for ranges so `$a2:b6` expands to a second-class table select (which hinders extension). This semantic awkwardness is intrinsic to conventional spreadsheets, not a fault of the projection.

Awelon supports structured data and graphical projections at individual cells, both for source and results. We could use a [zooming UI](https://en.wikipedia.org/wiki/Zooming_user_interface) to effectively interact with such data through a spreadsheet.

## Database Applications

Modeling a database doesn't require any special effort in Awelon, assuming suitable collection types for modeling tables (tries, maps, arrays, records). Data tables could then be constructed at `mydb-goods` or `mydb-sales`, and we can also name computed views (which filter, join, or summarize tables). Conveniently, we can easily integrate external data resources like `yourdb-sales` insofar as they are represented in our dictionary. 

A projectional editor, then, would enable users to browse tables and views, and support editing data or development of views. This is both simpler than spreadsheets and more convenient for collections processing. But the user experience would be closer to browsing and managing a database.

The main challenge with database applications is sheer scale. To help address this, we can leverage annotations for *Stowage* and *Memoization*, and favor monoidal computations over persistent data structures.

## REPL Applications

A pure REPL can be modeled as a context and a sequence of commands with a head:

        :myrepl-0 defines initial context
        :myrepl-1 myrepl-0 command1
        :myrepl-2 myrepl-1 command2
        ...
        :myrepl-42 myrepl-41 command42
        :myrepl-head myrepl-42

A projectional editor can render context and command at each step. This may involve graphical projections similar to a [Jupyter notebook](https://jupyter.org/). Insofar as we reference live data or unstable definitions, the REPL context can be recomputed automatically. We can edit prior commands, undo commands (move `myrepl-head` back to a prior command), or add new commands (add `myrepl-43` then redirect `myrepl-head` to `myrepl-43`). An observer of the REPL can either operate on the current state `myrepl-42` or on the mutable `myrepl-head`.

Many single-user applications can easily be modeled as pure REPLs via projectional editor: calculators, querying an expert system, interactive fiction. It's feasible for multiple users to take turns operating on a REPL. But there are some inherent limitations: computation is pure, and users aren't structurally restricted in observation or action.

## Forum Applications

Assuming a reverse-lookup index, we can extend pure REPLs with branching. For each node like `myrepl-17`, we perform a reverse-lookup to discover replies or reactions. These replies could be rendered as a massive tree, with progressive disclosure where needed. Instead of a singular 'head' we can maintain a set of 'tags' at branches of interest in the tree.

I find this intriguing. To raise the level of discussion between humans, it is useful to directly include the evidence and statistics, graphs and graphics, and chains of reasoning that lead to a conclusion, and to verify these arguments remain reasonable under the light of new or corrected evidence. It seems that forums modeled as purely functional code could help with this. Awelon's dictionary representation is scalable, so embedding enormous hierarchical forums is feasible.

We're only missing a suitable DSL for *Natural Language Programming*, to make encoding such discussions comfortable for humans.

## Command Shells and Orchestration

In Awelon, effectful behavior implies bot participation.

Bots may reflect on REPL contexts and contribute opportunistically. Users essentially do the same via projectional editors. Thus, users and bots would participate in a conversation. Of course, unlike natural language contexts, our REPL contexts can be structured in a manner friendly to bot participation.

Depending on the context model, we can flexibly support one-off requests or queries and long-lived behavior policies.

*Aside:* We should exclude references from our REPL or forum contexts. But we might use a few associated references to coordinate behavior.

## Temporal Data



## Large Volumes of Live Data


## Worksheet Applications

We could discard a lot of structure from a spreadsheet and instead model a session where we define a few local variables.

        :scratch-x      
        :scratch-y


scratch-x

 - just define some local variables

and still have a spreadsheet-like application.



A worksheet is like a spreadsheet with a lot less structure.

A spreadsheet sometimes has too much structure. 



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

# Effectful APIs for Bots

The [Awelon language document](AwelonLang.md) specifies how we'll represent transaction variables, and a basic API for bots. Effectful APIs rely on asynchronous communication through variables known by the host system. For example, a variable may contain a system task queue for asynchronous request-response. A full API should be specified in terms of concrete types for queues and requests and how responses are provided, and the system's behavior contract. Here, I'll only provide a general abstract sketch.

Our main communication effect is network access. Bots could initiate connections or listen for connections. Access might be provided at a few "layers" such as IP sockets vs. requesting or serving HTTP. A significant motive for higher layers is to support multi-hosting - we might share one HTTP socket across several Awelon environments based on URL.

Our host may support a few simple effects - current time, clock driven signals, variables for describing or configuring the host environment. I do not intend to support host FFI or filesystem access - doing so makes me nervous about security and portability. Accelerated computation, such as via GPGPU, may be implicit, via accelerator annotations.

Reflection is our final class of effects. A reflection API must support sharing and synchronization of dictionary volumes, versioning and caching, alerts on change, and so on. This will become a significant development for Awelon in the future.

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








# Brainstorming Extensions

## Hierarchical Dictionary Structure (Rejected)

We could efficiently represent hierarchical embedding of dictionaries by using a `dictname/` prefix with implicit scope. For example, `:d/foo bar baz` will implicitly depend on `d/bar` and `d/baz`. With this, we could update individual words deep in the hierarchy or update the full embedded dictionary as a one-liner `/d/ secureHash`. We can then extend Awelon syntax so the host can reference words of form `d/foo`.

Embedded dictionaries have potential to simplify control over name conflicts and information security, which would be otherwise handled in an ad-hoc manner via community dictionaries and static analysis. 

But there are some significant disadvantages. First, the resulting dictionary is aesthetically awkward, occasionally requiring `d/42` or `d/"hello world"`. We could support `d/[x y z]` but it complicates parsers and interpreters. More importantly, there is a broad impact on the social level if we reduce pressure to develop references (words) with globally shared meanings. 

With a flat namespace, developers can more easily share data. We may need ad-hoc analysis to control dependency relationships between "packages", and we'll need a community center (like a package registry) to resist name conflicts. But we can develop an environment where words have stable meanings - a shared language.

## Local Identifiers (Rejected)

Assume we're defining `very-long-prefix-x` and we want to reference `very-long-prefix-y`. This seems a common scenario for Awelon systems and application models. It could be convenient to support a shorthand, for example just use `-y` (perhaps with more hyphens to step further back). That said, if we're only interested in *convenience*, it seems sufficient to address this via projectional editor.

What are the benefits of this feature at the Awelon layer? A moderate space savings (but not as much as compressed stowage, and only up to a hundred bytes per reference). A potential benefit for copying or renaming prefixes (but we must be careful about renaming prefixes containing hyphens, and we must still search for the full version of a word). 

It seems to me the potential benefits aren't worth the overheads and complexities this would introduce. If the storage is tempting, it would seem much wiser to simply use compressed storage, like LevelDB. But even uncompressed, we aren't working with overly large entries - the longest words in practice should be around a hundred bytes.


