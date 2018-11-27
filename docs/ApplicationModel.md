
# Application Models for Awelon

Awelon proposes projectional editing for most user-interfaces. Essentially, spreadsheets at scale, with live data and graphical projections. Users can manipulate definitions or view computed data through the same projections. This design has many nice properties, and the results are easily shared or reused. Importantly, projectional editing ensures a more computation environment more accessible and controllable by the user, contributing to an empowered user experience. Projectional editing should be favored where feasible.

Where projectional editing is an awkward fit - such as automating dictionary maintenance, or bootstrapping the projectional editor - Awelon proposes use of bots. Awelon's bot are modeled as transactions, repeating indefinitely, implicitly waiting when repetition would obviously be unproductive. These transactions may interact with a subset of 'system' variables to access a network or reflect upon a greater dictionary. This design also has many nice properties - liveness, idempotence, extensibility, securability. Through networking and reflection, we can support ad-hoc web applications or hybrid native apps (cf. [Jasonette](https://jasonette.com/)). Projectional editors and games can feasibly be modeled in this manner.

This document sketches how various applications might be modeled in Awelon.

## Spreadsheet Applications

I mentioned "spreadsheets at scale". Let's start with plain old spreadsheets! 

We can project `foo-a2`, `foo-a3`, `foo-b2`, `foo-b3`, and so on as a spreadsheet `foo` with definitions in cells `a2`, `a3`, `b2`, and `b3`. Like most spreadsheets, under normal conditions we would render evaluated normally, and source only for cells with user focus. When we project source, we can treat `foo-` as an implicit namespace, using `$a2` or `A2` to name a local cell.

Awkwardly, a spreadsheet projection can rewrite formulas on copy-paste (in violation of [DRY](https://en.wikipedia.org/wiki/Don%27t_repeat_yourself)), and support a shorthand for ranges so `$a2:b6` expands to a second-class table select (which hinders extension). This semantic awkwardness is intrinsic to conventional spreadsheets, not a fault of the projection.

Awelon supports structured data and graphical projections at individual cells, both for source and results. We could use a [zooming UI](https://en.wikipedia.org/wiki/Zooming_user_interface) to effectively interact with such data through a spreadsheet.

*Note:* We might simplify spreadsheets into worksheets with a small set of user-named variables. In this case, we might render a connected graph of computations based on dependencies within the worksheet. This is suitable for ad-hoc, unstructured scratchpads.

## Database Applications

Modeling a database doesn't require any special effort in Awelon, assuming suitable collection types for modeling tables (tries, maps, arrays, records). Data tables could then be constructed at `mydb-goods` or `mydb-sales`, and we can also name computed views (which filter, join, or summarize tables). Conveniently, we can easily integrate external data resources like `yourdb-sales` insofar as they are represented in our dictionary. 

A projectional editor, then, would enable users to browse tables and views, and support editing data or development of views. This is both simpler than spreadsheets and more convenient for collections processing. But the user experience would be closer to browsing and managing a database.

The main challenge with database applications is sheer scale. To help address this, we can leverage annotations for *Stowage* and *Memoization*, and leverage these in monoidal computations over persistent data structures for incremental computation.

## Editor and View Widget Applications

A projectional editor will recognize values of common types, such as a date-time field or a color value. For editing these fields, a projectional editor could provide a full set of ad-hoc widgets, such as a date picker with a calendar, a color picker with color wheels and sliders and a pallette.

Similarly, views for large or sophisticated values may require interactive widgets for progressive disclosure or navigation. A large tree structure value might be opened incrementally or accessed using tabs. A table value might involve widgets to filter which rows are in view.

Editor and view widgets will often require auxiliary state: a color picker might remember recently used colors, a date picker could default to the current date-time, a text editor might access a clipboard, and we'll want to remember navigation status like location in an open tree when we update the tree. For large values, we might further have state for collaborative multi-user editing. This auxiliary state can generally be considered part of a users model.

Our projectional editor should be extensible with user-defined, easily shared editor and view widgets. For example, our projectional editor might accept a list of 'installed' widgets, each of which describes the conditions under which it applies. Many [CRUD apps](https://en.wikipedia.org/wiki/Create,_read,_update_and_delete) can be modeled as ad-hoc widget extensions to a projectional editing environment.

## REPL Applications

A pure REPL is a sequence of commands, manipulating a context, with a head:

        :myrepl-0 defines initial context
        :myrepl-1 myrepl-0 command1
        :myrepl-2 myrepl-1 command2
        ...
        :myrepl-42 myrepl-41 command42
        :myrepl-head myrepl-42

A projectional editor can render context and command at each step. This may involve graphical projections similar to a [Jupyter notebook](https://jupyter.org/). Insofar as we reference live data or unstable definitions, the REPL context can be recomputed automatically. We can edit prior commands, undo commands (move `myrepl-head` back to a prior command), or add new commands (add `myrepl-43` then redirect `myrepl-head` to `myrepl-43`). An observer of the REPL can either operate on the current state `myrepl-42` or on the mutable `myrepl-head`.

*Aside:* REPLs are easily subsumed by spreadsheets or worksheets with explicit symbols. However, the extra structure can be useful.

## Command Shells

A simplistic model for a command shell is a user-bot chat session. The user can write requests or ask questions. The bot can automate background tasks, answer questions, perhaps ask a few questions of its own. But this model couples many concerns: bots and users must interpret each other's messages, the conversational context is implicit, and the bots are ad-hoc. This hinders message abstraction, graphical projections, and user control.

An improved design: model a shared conversational context as a first-class value. This value includes open questions, pending requests, and background tasks. But it can model them in distinct tables or partitions. Users can view and manipulate the context through a projectional editor, optionally using functional 'commands' like a REPL application. Our bot's behavior should be a simple function of the context's value, enabling users to comprehend and control associated bot behavior by controlling the shared context. For example, job control might involve adding or removing some background tasks to the context.

This essentially gives us a simple [blackboard system](https://en.wikipedia.org/wiki/Blackboard_system). We can easily extend this design to multiple bots and users. 

*Aside:* We should avoid embedding `[ref-*]` words (which represent local capabilities) within our context, because any embedded references will severely hinder sharing or forking the context, or modeling and sharing applications as an initial context.

## Conventional GUI Applications

We can combine a command shell context for our back-end state and a dedicated editor/view widget for our front-end. This combination can directly model conventional GUI applications while preserving Awelon's goals for a high degree of user control and extensibility. Interestingly, we get transactional GUI operations for free, since any sequence of operations may reduce to a pure update to a dictionary.

All effects in this design are asynchronous. A button press will edit the context, which controls the bot, which performs the effect and eventually injects a response. But it's feasible to simulate synchronous behavior, e.g. by disabling a submit button (leaving it in the 'pressed' position) until a bot acknowledges the request.

*Aside:* We can use a generic context model with a problem-specific view widget. This would allow a standard back-end process, and only specialize the front-ends. It would also simplify debugging, since one debugger widget could work for most applications.

## Forum Applications

Assuming a reverse-lookup index, we can extend REPLs with branching. For each node like `myrepl-17`, we perform a reverse-lookup to discover a set of replies. These replies could be rendered as a massive tree, with progressive disclosure where needed. Instead of a singular 'head' we can maintain a set of 'tags' at nodes of interest in the tree. If bots participate, the forum can become a branching command shell.

I find this intriguing. To raise the level of discussion between humans, it seems useful to embed the evidence and statistics, graphs and graphics, and chains of reasoning that lead to a conclusion. By doing so, humans can review the subject and verify which arguments remain reasonable under the light of new or corrected evidence. Forums represented in Awelon's dictionary could serve this role very nicely.

*Aside:* Since we assume branching, it seems useful to model a 'context' type that supports easy and meaningful merges and reintegration. A CRDT or lattice-based context would work. However, I'm uncertain how easy it is to build applications above such a context. 

## Temporal Data

Temporal or time-series data is convenient for modeling stateful applications that integrate from multiple sources, without relying on interaction from bots or users. Essentially, if we have two temporal data sources (and suitable data structures for memoization) we can incrementally compute a temporal application state. This can be further used in computation of other applications.

The benefit of doing so is the ability to model stateful applications without effectful interactions. We can keep a lot more of our computation 'pure'. And we can integrate sources that we (users or local bots) do not interact with or control. Further, with temporal data we can easily browse or animate past states, and project likely future states.

This is compatible with command shell interactions. We'd simply have bots and users include a timestamp with their inputs. 

I'm uncertain how much Awelon systems will rely on temporal data. However, it's almost never a bad thing for a datum to include contextual metadata about when or where it applies. So I will encourage a preference for temporal data where feasible.

## Binding Live Data

Projectional editors *usually* won't be involved in this. Most live data should be managed by bots. However, there might be some exceptions where we can reasonably consider 'live data' to be a form of continuous user editing, like pushing a live stream from a user's camera. In any case, the projectional editor shouldn't directly modify anything outside of the bound Awelon environment.

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

A context for a REPL or command shell is usually a solid, unambiguous data type. A context for a natural language discussion, however, is full of gaps, heuristic assumptions that rely on rich world knowledge, and iterative refinement where past ambiguities become clearer with future sentences.

I find intriguing the opportunity to model a REPL or command shell context closer in nature to a natural language context. This can feasibly be achieved with soft, weighted logics. If we pursue this, it's feasible to support rapid application developments and iterative refinement in application models. Further, by developing widgets above such a context, we can support applications that are more robust to user imprecision, and more adaptive to a changing environment.

