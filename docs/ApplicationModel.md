
# Application Model for Awelon Project

In general, I hope for Awelon project to focus on RESTful systems. Applications are modeled in terms of creating, sharing, reading, updating, and deleting definitions of words or entire hierarchical dictionaries. Effects are carried out in context of a multi-agent system. Real-time behavior can be supported via publish-subscribe patterns. 

This document is a brainstorm and exploration of potential patterns for modeling applications. [Awelon](AwelonLang.md) represents an alternative to the mainstream `void main()` programming model. We will need to explore ideas in practice to see what works.

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

It would not be difficult to view part of a dictionary as a spreadsheet. For example:

        :foo_a_2 "world"
        :foo_a_3 "hello"
        :foo_b_2 foo_a_3 foo_a_2 concat

This essentially becomes a view and display problem. That is, when we choose to view `foo` as a spreadsheet, we must automatically evaluate and render the appropriate definitions in the correct cells. Conversely, when we edit a cell, the modified definition should be written into our dictionary. What cells we can render is limited only by editable views, so could include hypermedia.

## Hypermedia Applications

Awelon words provide a flexible link structure, and projectional editors can provide a flexible rendering and interaction model. Intriguingly, *Awelon code evaluates to Awelon code*, preserving link structure and meaning. Hence, we can view this as *hypermedia evaluates to hypermedia*. We could model texts that evaluate to canvases and tables, and vice versa. 

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

