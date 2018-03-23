
# Application Models for Awelon Project

The conventional, mainstream "application model" is a detached process that interacts with shared services and data. Awelon could easily follow convention, e.g. modeling monadic IO or a message-passing process. But my goal for Awelon project is to unify programming and user experiences, to make sharing and composition easy. To that end, the detached process seems a poor fit, being inaccessible and invisible.

An intriguing feature of [Awelon language](AwelonLang.md) is that, due to rewrite semantics, process state corresponds to an Awelon program. We can render a program for debugging, serialize it for distribution, or save it to our dictionary for durability. We can feasibly model active jobs as embedded within a dictionary.

        :job-1123 [process state is recorded here]

Presumably, an external agent would find available jobs, perform requested operations (such as sending e-mail or fetching HTML), then compute the next process state. It might run many steps between saving process states. Besides maintaining the process over time and viewing its state in a debugger, we could capture checkpoints or probe uncommitted sample messages.

        :my-view job-1123 [sample message] send

Thus, even for a relatively conventional process model, we could improve visibility and accessibility. However, I believe we can do even better by representing 'applications' as structured objects within a dictionary. This results in very RESTful applications with deeply accessible state. For example, REPLs and iPython-inspired "notebook" applications could be encoded in a dictionary using a word per logical line: 

        :repl-1473-0 initial-state
        :repl-1473-1 repl-1473-0 command2
        :repl-1473-2 repl-1473-1 command2
        :repl-1473-3 repl-1473-l2 command2
        :repl-1473 repl-1473-l3

This REPL corresponds closely to a *command pattern* from OOP. It enables access to and editing of prior lines, rendering an output per line, infinite undo, branching the REPL, or embedding its current value (via the head) into other programs. In general, this document is just brainstorming patterns that might be suitable for modeling application state within a dictionary. 

## Expressive Spreadsheets

An Awelon dictionary is essentially a filesystem with spreadsheet-like characteristics. But it's feasible to actually record spreadsheets within the dictionary, defining a word per cell. Every Awelon definition can then be evaluated independently. Essentially, this is just an editable view on part of a dictionary, using a row-column convention:

        :foo-2-a "world"
        :foo-3-a "hello"
        :foo-4-a foo-3-a foo-2-a concat

To be useful as a spreadsheet, the main requirement is a sufficiently dense encoding of rows and columns. If everything is sparsely distributed, we'd be better off favoring an ad-hoc dependency lattice layout.

## Functional Relational Programming

Although we can tabilize data into a dictionary like a spreadsheet, it isn't readily accessible in that form for relational algebra operations. It might be better to model tables as first-class values that we maintain within the dictionary. Doing so would allow our dictionaries to double as databases. Application state could be computed as views of tables, while operations could correspond to actions on the database value.

## Monotonic Dictionaries

Awelon can evaluate in context of undefined words. It is feasible to model applications such that we never modify a word's definition, only define words that are undefined and occasionally garbage-collect unnecessary words. An advantage of doing this: it's trivial to continuously 'evaluate' the dictionary in-place without losing information. There is a lot of natural garbage collection. (See also *Managed Dictionaries*. A monotonic dictionary is essentially *frozen* in every word.)

## Command Pattern

A command pattern might be represented as:

        :foo-0 initial state constructor
        :foo-1 foo-0 command1
        :foo-2 foo-1 command2
        ...
        :foo-99 foo-98 command99
        :foo foo-99

Essentially, we represent a stream of commands manipulating a state. The explicit representation simplifies historical views, forking, undo, and similar. In general, keeping the entire history of commands may cost too much in some cases, so we may need to occasionally checkpoint the state. But for many use cases, the number of commands won't be too large and checkpoints may be managed explicitly.

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

Representation of these attributes is ad-hoc, subject to de-facto standardization. For example, we could define `foo-meta-gc` for elements under `foo`, or we could represent our policies under a global word like `meta-gc`. I only recommend that the policy be separated from the definitions, i.e. using separate words instead of comments.

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

