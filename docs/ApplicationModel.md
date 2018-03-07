
# Application Models for Awelon Project

The conventional, mainstream "application model" is a detached process that will effectfully access and manipulate external data and services, perhaps registering itself with a network service to listen for external requests. We observe this process only indirectly through its effects on shared stateful resources. Awelon could easily follow convention, e.g. by modeling monadic IO or a message-passing process. But my ultimate goal for Awelon project is to unify programming and user experiences. To that end, the "detached" process seems a poor fit. Application state should be visible and accessible. 

An intriguing feature of [Awelon language](AwelonLang.md) is that, due to rewrite semantics, we can always "save" process state as an Awelon program to simplify debugging or distribution. We could save process state into a dictionary, for example, to provide sufficient context to interpret it.

        /myDict/ secureHash
        :job_1123 myDict/[process state is recorded here]

An external agent could then "run" the jobs in our dictionary, continuously executing requested effects, providing inputs, and saving the updated state. This does provide a fair level of visibility and access. For example, we can easily checkpoint or share the process, or kill it by deleting the `job_1123` word, or directly inject messages or modify states, or change some of the code it's running against. We can feasibly support useful real-time views like, "how would the process reply to this message?" without risk of influencing that process's state:

        :view_example job_1123 [example_message] send

However, I believe we could do better than this by more broadly embracing the use of dictionaries for representing application state. Instead of centralized process state, we could feasibly model structured application state in our dictionaries. For example, REPLs and iPython-inspired "notebook" applications could feasibly be directly encoded in a dictionary.

        :repl_1473_0 initial_state
        :repl_1473_1 repl_1473_0 command2
        :repl_1473_2 repl_1473_1 command2
        :repl_1473_3 repl_1473_2 command2
        :repl_1473   repl_1473_3

Actually, this corresponds to the more general *command pattern* from OOP (see below). This document is mostly a brainstorm and exploration of patterns and structures that may be suitable for Awelon application models. It shouldn't be a problem for a dictionary to simultaneously support several "kinds" of applications. But we will need to explore ideas to see what actually works in practice.

## Functional Relational Programming

Functional-relational programming involves storing all user inputs in a table and computing the current application state. Operations presented to users (e.g. a button on a form) must come with associated instructions for adding new inputs. This sort of approach fits very naturally with RESTful programs.

## Monotonic Dictionaries

It is feasible to operate on a dictionary monotonically - such that we never redefine a word. In some cases, this requires leaving undefined words in the dictionary as placeholders for the undefined "future". Awelon can easily evaluate in context of undefined words, although one must also arrange computation such that we can extract useful results from partial evaluations.

The main advantage of monotonic dictionaries is that they may be continuously evaluated in-place. There is never any need to recompute, only to continue computations. By leveraging stowage and secure hash resources, state may scale freely. Hence, a word may correspond to an application whose state is updated based on filling gaps in a definition. See also *Managed Dictionaries*, below - monotonic dictionaries are essentially *frozen* by default, and we might need to gradually GC the dictionary to limit size.

## Command Pattern

A command pattern might be represented as:

        :foo_v0 initial state constructor
        :foo_v1 foo0 command1
        :foo_v2 foo1 command2
        ...
        :foo_v99 foo_v98 command99
        :foo foo_v99

In this case, we mutate the "head" word for definition of an object `foo`, but the command stream could be monotonic. Use of command pattern enables a stream of small updates to construct and modify a large object. This particular design records every command as a distinct set of words, simplifying views of historical values, forking, undo, etc.. We could try to automatically compact and simplify the command stream, within the limitation of stable dependencies.

Command pattern can be used for such things as:

* addending a time-series database
* updating a key-value database or other large object
* editing documents modeled as finger-tree ropes
* modeling user actions - a button press, HTTP POST

Effectively, command pattern models a mutable object within a dictionary, albeit only mutable by external agents. A disadvantage of the command pattern is its non-monotonic structure, the destructive update of `foo`

## Expressive Spreadsheets

It would not be difficult to view and project parts of a dictionary as spreadsheets. For example:

        :foo_a_2 "world"
        :foo_a_3 "hello"
        :foo_b_2 foo_a_3 foo_a_2 concat

We could choose to view `foo` as a spreadsheet, with automatic layout of columns `a b c ..` and rows `1 2 3 ..`. We could generalize to named columns and rows, so long as the resulting display is dense enough. Due to Awelon's evaluation by rewriting, it's possible for any cell to contain a subprogram that we can edit and evaluate in place, render (using other editable views), and reference as a function. Edited cells would simply be written into the dictionary.

## Publish Subscribe

Publish subscribe is a model for continuous, live programming of real world systems.

With ad-hoc conventions a dictionary might describe subscriptions to external data resources. An agent can fulfill these subscriptions, pushing data into the dictionary. Conversely, external agents might subscribe to words or expressions on a dictionary and observe changes in their evaluation due to changes in the underlying data. 

By leveraging hierarchical dictionaries, it's also feasible to publish-subscribe entire dictionaries rather than individual words. This would be more convenient from a security and modularity perspective: we can synchronize an independent, externally maintained dictionary into our dictionary then access the relevant data.

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

Hence, we can evaluate and optimize opaque words, link frozen words, and GC the hidden words. Each attribute gives our software agent a more opportunity to safely rewrite, manage, and optimize the dictionary in specific ways. We can assume secure-hash resources are frozen and hidden, but not opaque. However, when a secure hash resource is referenced from an opaque definition, we could rewrite the secure hash to a simplified or evaluated form.

Representation of these attributes is ad-hoc, subject to de-facto standardization. For example, we could define `foo_meta_gc` for elements under `foo`, or we could represent our policies under a global word like `meta_gc`. I only recommend that the policy be separated from the definitions, i.e. using separate words instead of comments.

# Programs as Processes

While Awelon is designed for non-conventional apps, we can certainly model a more conventional application structure where a program directly interacts with the real world and humans as part of a computation. Applications can operate on streams of messages or subscription updates.

## Interactive Evaluations

An agent can evaluate a program, render or make observations about it, then inject some input or modify code and continue evaluation. This design most clearly fits REPLs and variants (like interactive fictions). But it can also fit some GUIs, e.g. if we represent button pressing as a form of modifying code (by inlining a block or selecting a case from a record of options). Programs can be modeled as publish-subscribe systems, where a set of relatively stable subscriptions is presented to the agent and the agent inputs messages. More generally, monadic or [KPN based](KPN_Effects.md) IO also fits interactive evaluation: evaluation halts with some set of outputs and requests for input, and we continue evaluation after providing more inputs. 

Interactive evaluation at the larger dictionary level is feasible with futures/promises and a monotonic codebase, or sufficient use of memoization with the command pattern. So the main difference with interactive evaluation of programs is doing so at the anonymous program layer.

## Command Stream Processing

Awelon is amenable to command stream processing. The general form is:

        [process] commandA  => commandB [process']

This simplistic stream processing model conveniently supports composition:

        [procF] [procG] commandA => [procF] commandB [procG']
                                 ...
                                 => commandC [procF'] [procG']

We can monotonically input commands to the right hand side of our program, and incrementally output commands from the left hand side. The process object contains any state and may have some background parallelism. This model isn't a great fit for 'interactive' evaluation, but it may work nicely with Unix-like pipelines and has an advantage of not needing an external agent to explain.

