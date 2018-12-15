
# Application Models for Awelon

Awelon proposes projectional editing for most user-interfaces. Essentially, spreadsheets at scale, with live data and graphical projections. Users can manipulate definitions or view computed data through the same projections. This design has many nice properties, and the results are easily shared or reused. Importantly, projectional editing ensures a more computation environment more accessible and controllable by the user, contributing to an empowered user experience. Projectional editing should be favored where feasible.

Where projectional editing is an awkward fit - such as automating dictionary maintenance, modeling a game server where user actions are hidden from other users, or bootstrapping the projectional editor - Awelon proposes use of bots. Awelon's bots are based on repeating transactions, installed at `bot-*` for easy control by the user. Bots are given a host-environment object, through which they can access the network, dictionary, and auxiliary state. This design also has many nice properties - liveness, idempotence, resilience, security, extensibility. Through network access, we can construct web applications. Leveraging tools like [Jasonette](https://jasonette.com/), these might still look and feel like native apps.

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

### GUI Command Shells

We can combine a command shell context for our back-end state and a specialized editor/view widget for a front-end. This combination can directly model most conventional GUI applications while preserving Awelon's goals for a high degree of user control and extensibility. 

Widgets in GUI command shells should only update two things: the pure value of our context, and potential auxiliary state representing the user-model (clipboard, navigation status, presence tracking for coordination with other users, etc.). Importantly, there should be no communication effects aside from these updates. Background bots may observe the command context or user-model, and react appropriately. If we desire, we can simulate "synchronous" GUIs by disabling parts of a GUI while awaiting response from a bot.

This design has many nice properties: lightweight support for atomic updates, checkpoints, undo, and support for multiple local reactive views (edits to one view of the context are observed in all others). With a relatively generic context model, we can develop some standard views for debugging, and standard language for macro manipulations.

This does require a sub-language for quickly constructing widget models for our projectional editors. Perhaps something based on lenses?

## Forum Applications

Assuming a reverse-lookup index, we can extend REPLs with branching. Given a REPL node like `myrepl-17`, we can perform a reverse-lookup to discover a set of associated 'replies'. These replies or their evaluated results could be rendered as a tree, like a forum. The tree could contain many associated tags instead of a single head. Like normal forums, we could easily support multiple users and bots, with requests and responses and background effort.

I find this intriguing. To raise the level of discussion between humans, it seems useful to embed the evidence and statistics, graphs and graphics, and chains of reasoning that lead to a conclusion. By doing so, humans can review the subject and verify which arguments remain reasonable under the light of new or corrected evidence. Forums represented in Awelon's dictionary could serve this role very nicely.

*Aside:* Since we assume branching, it seems useful to model a 'context' type that supports easy and meaningful merges and reintegration. A CRDT or lattice-based context would work. However, I'm uncertain how easy it is to build applications above such a context. 

## Temporal Data

Temporal or time-series data is convenient for modeling stateful applications that integrate from multiple sources, without relying on interaction from bots or users. Essentially, if we have two temporal data sources (and suitable data structures for memoization) we can incrementally compute a temporal application state. This can be further used in computation of other applications.

The benefit of doing so is the ability to model stateful applications without effectful interactions. We can keep a lot more of our computation 'pure'. And we can integrate sources that we (users or local bots) do not interact with or control. Further, with temporal data we can easily browse or animate past states, and project likely future states.

This is compatible with command shell interactions. We'd simply have bots and users include a timestamp with their inputs. 

I'm uncertain how much Awelon systems will rely on temporal data. However, it's almost never a bad thing for a datum to include contextual metadata about when or where it applies. So I will encourage a preference for temporal data where feasible.

## Binding Live Data

Projectional editors *usually* won't be involved in this. Most live data should be managed by bots. However, there might be some exceptions where we can reasonably consider 'live data' to be a form of continuous user editing, like pushing a live stream from a user's camera. In any case, the projectional editor shouldn't directly modify anything outside of the bound Awelon environment.

# Code Completion in Awelon

Code completion is a valuable HCI feature for a programming language, doubly so if projectional editors are a primary user-interface model. So I'd like to support code completion in Awelon. Code completion requires a *context* that usefully limits a set of *operations*.

We can consider Awelon a stack-based language for this purpose. The top elements on the stack, or the inferred types for those elements, provide an input context for code completion. We might also have an output context, based on observable usage of the program we define. We could use these contexts to present some suggestions. Missing parameters seem relatively awkward, however. 



However, stack-based programming can be a little awkward. Mostly, the problem


 for code completion. We might also have a 'right context' based on where our code is used, so we know the final type of our context. However, there is some unfortunate awkwardness to deal with missing parameters: after we select an operation, we must move our focus backwards to attend those missing details. 

For contrast, OOP offers a convenient `object .method( parameters ) .method2( more params )` pattern. The object type can give us a context restricting the choice of methods. Each method gives us a context of typed holes for parameters. It's all left-to-right. Very nice, when it fits. 



 The parameters further provide a typed context. If we can arrange code carefully, our cursor can mostly move from left to right. Of course, this also has its awkward cases. It doesn't nicely handle methods producing a pair of objects, or a computation producing a pair of parameters that we'd prefer to divide across methods, or a conditional choice of methods.

I'm certain we can do better. But it may be specialized to certain projections. 

In general, code completion is essentially the anticipation of the user. We could reasonably argue that widget-based projectional editors are already providing this, albeit in a more ad-hoc and explicit manner. The developer of each widget is anticipating a use-case. It seems feasible to approach code completion the same way, developing a registry for automated suggestions (each with a mini-widget) based on knowing what we have and what we need.

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

An intriguing opportunity is to model a REPL or command shell context that simulates a natural language context. A context for natural language discussion is full of vagueness and ambiguity, heuristic assumptions and associated world knowledge. We can iteratively refine, extend, retract, and correct prior statements with future sentences. This is technically feasible by using soft constraint logics, and some means to ignore or delay interpretations with trivial or irrelevant differences so we can focus attention on useful distinctions.

It is feasible to develop edit and view widgets for a probabilistic context. We could render multiple interpretations side-by-side, and help users select between them by refining the context. We could make it easy for users or bots to highlight the variables that need attention before a decision can be made, modeling an implicit query. 

I believe such a context would support rapid application development, allowing refinement and extension of the application concurrent with its use. Vagueness and ambiguity can be utilized as features, enabling a applications to adapt to different or changing 'environments' insofar as information about the greater environment constrains those unknown variables. With staged computing (meta-programming), we could still optimize specific instances of the context as a concrete application.

# Bot Scripts? Rejected.

I had earlier imagined use of bot-scripts: one-off transactions with the same *type* as a bot transaction (type `forall v . Env v -> TX v e a`). The transaction would retry in case of concurrency conflict (unless canceled by a user), but otherwise would succeed or fail. This is easy to implement. However, this is insufficient for most use-cases because a single transaction cannot wait for effects or model interaction with the user, and it's even unclear how to bind such effects with our application models. 

Command shells over a blackboard system are a simple and superior mechanism to model scripting. We can 'script' macro-updates to our blackboard context. And, via projectional editors, we can render GUI widgets or application front-ends if doing so is appropriate.

