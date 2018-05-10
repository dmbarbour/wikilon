
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

## Application Model Desiderata

I have several desiderata for application models in Awelon systems:

* composable: we can systematically embed or integrate applications
* first-class: we can represent the application model within Awelon
* accessible: we can render, animate, directly manipulate app state
* immutable: state is represented by an immutable, non-unique value
* sharable: can copy or move apps to a new environment and continue
* concurrent: model many simultaneous interactions with environment
* distributed: partition state and computation across many machines
* deterministic: behavior is fully repeatable up to external inputs

First-class value representation of processes is convenient for composition, immutability, and sharing. However, first-class "object identity" - where we explicitly communicate references to process or state components - is problematic for sharing, composition, determinism, and even memory management. I'll directly exclude models that rely on object identity.

Concurrency requires we both represent multiple external requests and receive multiple inputs, and progress incrementally (i.e. such that supplying any requested input may result in new requests, no need to supply all inputs up-front). Distribution, meanwhile, requires long-lived components (relative to latencies), which tends to require incremental communication. Determinism, in context of concurrency, requires that any internal communications between components be confluent.

Transparent representations are accessible. But opaque representations (abstract data types, closures, etc.) are convenient for protecting assumptions and state invariants. I suspect we'll want something in-between in practice.

## Monadic Processes

Monadic computations match several desiderata. Especially the [Free and Freer (operational) monads](http://okmij.org/ftp/Computation/free-monad.html), i.e. modeling all monads with an extensible, interpreted "effects" type.

Concurrency and distribution without first-class channels requires careful consideration. Using a free monad representation, it is feasible to introduce concurrency extension like `fork-join : m a → m b → m (a * b)` that expose both component requests. For remote computations, we could feasibly introduce support for ad-hoc remote requests, e.g. `remote : Location → R a → m a`. But it's unclear how to effectively use *internal* distribution.

Nonetheless, monadic processes are a *proven* model for effects with functional programming. It should be worth investing in as a means to quickly get started, implementing user-programmable tasks on Awelon web servers. Distribution isn't essential for many problems, and use of distributed resources is still feasible via accelerated models internally (e.g. accelerated subset of OpenCL). 

## Kahn Process Networks

I am interested in leveraging [Kahn Process Networks (KPNs)](https://en.wikipedia.org/wiki/Kahn_process_networks) in Awelon. KPNs support multiple input and output channels. The order with which messages for separate input channels are provided does not matter, which provides a simple basis for concurrency on input. Messages may be pending on multiple output channels, which provides a simple basis for concurrency on output. Gluing outputs to inputs provides a simple basis for composition of KPNs. Importantly, it is feasible to *accelerate* the evaluation of KPNs such that we avoid explicit routing of messages. This would be useful for large scale applications, distributing computations across multiple machines. 

We can model reactive KPNs by adding explicit time-step message to every channel. This enables us to express either "no input until later" or "no input this time" to a process waiting on that channel. (The subtle difference is whether we permit streaming of many inputs within a time-step.) Each process would send similar messages on the appropriate output channels. With this, we can model real-time systems with asynchronous, interleaved inputs. We can also use acknowledgement channels to model network back pressure, i.e. so an upstream process doesn't push a message until prior messages are received. KPNs are effectively a deterministic variant of flow-based programming.

The main weakness of KPNs is that it is difficult to represent a dynamic network structure. We can work around this a little using first-class KPN values, but even then it would be awkward to extract and preserve process state.

## Machines

The [machines](https://hackage.haskell.org/package/machines) model developed by Edward Kmett and Rúnar Bjarnason attempts to solve several issues with conventional monadic IO. The underlying types:

        Step k o r = Stop | Yield o r | Await (k t) ((1+t) → r)
        Machine k o = Step k o (Machine k o)

In each step, a machine can either halt, yield an output, or await an input. Awaiting an input allows an explicit request `(k t)` and may fail so we accept a `(1+t)` result. Uniform input failure is convenient for accessibility: it allows us to compute and render a "current" incremental output assuming there are no more inputs.

Effectively, this is a model of streams with incremental inputs. It's more rigidly structured than KPNs, yet close in nature to monadic computations. 

## State Sharing Processes

Awelon project's goals include visibility, accessibility, and extensibility. A shared state model is convenient for these goals: the state value is something we can render, animate, directly manipulate, debug, extend with concurrent processes. Purely functional programming forbids ad-hoc aliasing of shared state, but we can easily model time-sharing with one writer at a time. Naively, a simple state sharing process model is:

        type P s = s → (s * P s)

Each process step returns the next state and next step function. We assume the state value of type `s` will be observed and manipulated by other processes between steps. Human users or external software agents also act as processes in this respect: they view and manipulate state, and update their own memory while doing so. Processes are anonymous. Communication between processes is indirect, via state. The state model `s` can provide message queues, tables, registries, tuple spaces, or other concepts as needed for communications. 

Process model `P` is naive because it doesn't support waiting for input or process life cycles. We can extend our model to fix these issues. Consider:

        type PR s = s → 1 + (s * PR s)                  P+Retry
        type PRC s = List of (s → 1 + (s * PRC s))      PR+Concurrency

By introducing retry, a process can explicitly await state changes without a busy-wait loop. By modeling concurrent operations via collections of processes, we support non-determinism (scheduling within list is unspecified), and we can model process life cycles (spawning processes, termination via empty list). 

Some other weaknesses can be addressed by focusing on the state model `s`. If our state model is logically monotonic in nature, we can reason more readily about behavior in the face of non-determinism. Use of accelerated state models, such as KPNs, could support distributed and parallel computing. State invariants could be protected by modeling `s` as an abstract data type with a constrained API.

The main weakness is that there is no built-in coordination between processes. Each process obtains exclusive control of the state, and any coordination must be represented within the model itself. Also, there's no effective means to make this system observably deterministic.

## Behavioral Programming

In [behavioral programming (BP)](http://www.wisdom.weizmann.ac.il/~bprogram/), processes don't directly manipulate state. Instead, concurrent processes - called behaviors - will automatically coordinate to compute a sequence of events. Each behavior can suppress events, propose events, observe events, and potentially terminate or spawn concurrent behaviors. In a purely functional system, we might model this as:

        type B e = e → 1 + (S e)
        type S e = (Set of e) * (Set of (B e))

Our system has a set of proposed events and a set of behaviors. Each behavior has an opportunity to either suppress or accept an event. Upon acceptance, we will compute a next set of proposed events and behaviors. An event is accepted only if all behaviors in the current system accept it, and our next system is the union of next events and behaviors. External agents could interfere with this system by injecting new behaviors or events, or by selecting a non-deterministic "next" event after we filter the rejected events.

Although behavioral programming uses the word "event", the value could feasibly represent a system state. Doing so would offer similar advantages as state-sharing processes. We can also support "soft" coordination models, e.g. by including a score upon acceptance, or representing sets as priority-ordered lists.

## Multi-Variable Behavioral Programming 

For large systems, we need fine-grained state and behaviors. 

Assume our state or event is represented by a collection of single-assignment shared variables. Concurrent behaviors will observe and assign subsets of variables. A potential API:

        get : Label a → m a
        set : Label a → a → m 1
        alt : m a → m a → m a
        fail : ∀ a . m a

Use of `alt` allows for non-deterministic decisions. 

To model stateful behaviors, we might loop or enable access to history:

        type B = List of (m B)      STATEFUL LOOP
        hist : Label a → m a        STATEFUL HISTORY

Access to historical information is more convenient in context of live software updates, job control, state accessibility, etc.. Loops, especially with concurrency, tend to be opaque and escape the thumb of human users. In case of access to history, we might assume a low-priority optional behavior of form `hist label >>= set label` to preserve unassigned variables across time-steps by default, plus a `reset` function to drop variables.

To support priority explicitly, we could prioritize `alt` by including a number that heuristically indicates how much weight we should associate with the first option above the second. To support scratch-spaces for computations and composition of applications, we could also support a hierarchical state model with subdirectories, e.g. `in : Directory → m a → m a`. For concurrent operations, we can safely add a `fork` operation due to single-assignment restrictions.

The main weakness of this model is that it results easily in combinatorial explosions of possible solutions. This seems to be a fundamental issue, unavoidable for fine-grained state. It can be mitigated by prioritized choice.

# Second Class Models

Although these aren't suitable as a foundation for *general* applications, they could still be useful and interesting.

## Expressive Spreadsheets

An Awelon dictionary is essentially a filesystem with spreadsheet-like characteristics. But it's feasible to actually record spreadsheets within the dictionary, defining a word per cell. Every Awelon definition can then be evaluated independently. Essentially, this is just an editable view on part of a dictionary, using a row-column convention:

        :foo-2-a "world"
        :foo-3-a "hello"
        :foo-4-a foo-3-a foo-2-a concat

To be useful as a spreadsheet, the main requirement is a sufficiently dense encoding of rows and columns. If everything is sparsely distributed, we'd be better off favoring an ad-hoc dependency lattice layout.

Awelon does not provide any automatic mechanisms to access collections or tables encoded this way, but it is feasible for agents or projectional editors to additionally maintain records and lists based on which words are defined in the dictionary. This would allow full-table access, while preserving the acyclic structure between individual "cells".

## Command Pattern

A command pattern might be represented as:

        :foo-0 initial state constructor
        :foo-1 foo-0 command1
        :foo-2 foo-1 command2
        ...
        :foo-99 foo-98 command99
        :foo-hd foo-99

Essentially, we represent a stream of commands manipulating a state. We could render the state after each command, like a REPL or notebook application. The explicit representation simplifies historical views, forking, undo, and similar. In general, keeping the entire history of commands may cost too much in some cases, so we may need to occasionally checkpoint the state. But for many use cases, the number of commands won't be too large and checkpoints may be managed explicitly.

*Aside:* This could also be viewed as a specialized 1D spreadsheet.

## Publish Subscribe

Publish subscribe is a model for continuous, live programming of real world systems.

With ad-hoc conventions a dictionary might describe subscriptions to external data resources. An agent can fulfill these subscriptions, pushing data into the dictionary. Conversely, external agents might subscribe to words or expressions on a dictionary and observe changes in their evaluation due to changes in the underlying data.

We can leverage hierarchical dictionaries to publish-subscribe entire databases. Various topics such as weather or geography could be maintained independently in separate dictionaries, then synchronized when the network is available. This benefits from dictionary-layer structure sharing, lazy downloads, streaming and checkpoints, etc..

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

