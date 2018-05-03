
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
* accessible: we can render, animate, directly manipulate app state
* immutable: state is represented by an immutable, non-unique value
* sharable: can copy or move apps to a new environment and continue
* concurrent: model many simultaneous interactions with environment
* distributed: partition state and computation across many machines
* deterministic: behavior is very predictable up to external inputs

First-class representation is good for composition and sharing. Object identity and allocation should be avoided because they interfere with immutable representation, sharing, determinism (of ID creation), and garbage collection. Hierarchical structure could be useful for composition and distribution. Having a stable structure and state model is very convenient for accessibility and extensibility.

*Note:* For fast early development, it might be best to initially focus on a simple proven model, like Erlang-style message passing or a simple monadic process model. 

# First Class Models

## Kahn Process Networks

I am interested in leveraging [Kahn Process Networks (KPNs)](https://en.wikipedia.org/wiki/Kahn_process_networks) in Awelon. KPNs support multiple input and output channels. The order with which messages for separate input channels are provided does not matter, which provides a simple basis for concurrency on input. Messages may be pending on multiple output channels, which provides a simple basis for concurrency on output. Gluing outputs to inputs provides a simple basis for composition of KPNs. Importantly, it is feasible to *accelerate* the evaluation of KPNs such that we avoid explicit routing of messages. This would be useful for large scale applications, distributing computations across multiple machines. 

We can model reactive KPNs by adding explicit time-step message to every channel. This enables us to express either "no input until later" or "no input this time" to a process waiting on that channel. (The subtle difference is whether we permit streaming of many inputs within a time-step.) Each process would send similar messages on the appropriate output channels. With this, we can model real-time systems with asynchronous, interleaved inputs. We can also use acknowledgement channels to model network back pressure, i.e. so an upstream process doesn't push a message until prior messages are received. KPNs are effectively a deterministic variant of flow-based programming.

The main weakness of KPNs is that it is difficult to represent dynamic network structure. We can potentially use first-class KPN values to model temporary subnets.

## Machines

The [machines](https://hackage.haskell.org/package/machines) model developed by Edward Kmett and Rúnar Bjarnason attempts to solve several issues with conventional monadic IO. The underlying types:

        Step k o r = Stop | Yield o r | Await (k t) ((1+t) → r)
        Machine k o = Step k o (Machine k o)
        MachineT m k o = m (Step k o (MachineT m k o))

In each step, a machine can either halt, yield an output, or await an input. In the model provided above, awaiting an input may fail so we have a continue option. A `(k t)` value represents a request returning a type `t` value, relying on Haskell's support for GADTs to constrain the input type. The `(1+t)` at Await allows for a machine to handle request failure, e.g. if upstream input stops. The `MachineT` variant permits ad-hoc interactions with the monadic environment.

Awelon doesn't have great support for GADTs. But we could this simplify to:
        
        Step' i o r = Stop | Yield o r | Await ((1+i) → r)

The simplified form cannot describe what it's waiting for. Regardless, we model a machine as a stream that requests incremental input, and potentially performs some interactions on the side. This structure is relatively more rigid than KPNs. But there is also a clear halting condition, which can serve as a useful basis for dynamic structure.

## State Sharing Processes

Awelon project's goals include visibility, accessibility, and extensibility. A shared state model is convenient for these goals: the state value is something we can render, animate, directly manipulate, debug, extend with concurrent processes. Purely functional programming forbids ad-hoc aliasing of shared state, but we can easily model time-sharing with one writer at a time. Naively, a simple state sharing process model is:

        type P s = s → (s * P s)

Each process step returns the next state and next step function. We assume the state value of type `s` will be observed and manipulated by other processes between steps. Human users or external software agents also act as processes in this respect: they view and manipulate state, and update their own memory while doing so. Processes are anonymous. Communication between processes is indirect, via state. The state model `s` can provide message queues, tables, registries, tuple spaces, or other concepts as needed for communications. 

Process model `P` is naive because it doesn't support waiting for input or process life cycles. We can extend our model to fix these issues. Consider:

        type PR s = s → 1 + (s * PR s)                  P+Retry
        type PRC s = List of (s → 1 + (s * PRC s))      PR+Concurrency

By introducing retry, a process can explicitly await state changes without a busy-wait loop. By modeling concurrent operations via collections of processes, we support non-determinism (scheduling within list is unspecified), and we can model process life cycles (spawning processes, termination via empty list).

Some other weaknesses can be addressed by focusing on the state model `s`. If our state model is logically monotonic in nature, we can reason more readily about behavior in the face of non-determinism. Hierarchical structure could simplify routing and distributed partitioning. Invariants could be protected by modeling an abstract data type with a constrained API.

Although the general idea seems promising, it's too high level to use directly.

## Mobile Process Objects

We can model a process as having a location on a stateful tape, grid, or network. In each step, the process could observe and manipulate "local" states, update its own state, and optionally navigate. We can also extend the model to processes with multiple locations - two fingers on the same tape, or on two separate tapes, logically pinching spaces together like a wormhole.

The main advantage of this over state sharing processes is that we could distribute our grid or network across a physical machines to improve parallelism (assuming the process objects are also well distributed). The weaknesses of state sharing processes still apply.

## Behavioral Programming

In [behavioral programming](http://www.wisdom.weizmann.ac.il/~bprogram/), our processes don't directly manipulate state. Instead, multiple processes - called behaviors - will automatically coordinate to compute a sequence of events. A behavior will suppress events, propose events, observe events, and potentially spawn new behaviors. Upon occurrence of an awaited event, a behavior's state may change. In simplest form, we might model this as:

        type B e = (List of e * (e → 1 + List of (B e)))

That is, each behavior is proposing a list of events and also provides a function to either suppress a proposed event or spawn a set of behaviors if that event is accepted. To be accepted, an event must be accepted by all coordinating behaviors. This idea could reasonably be extended with partial events, weighted events, or multiple event channels. 

I think this is interesting, but it seems difficult to integrate with real IO.

## Message-Passing Models

Message-passing systems are easy to model and implement. In each step a machine will receive one message, compute a finite set of output messages, and update its private state. Messages are addressed to specific machines can contain arbitrary values. Unlike actors model, we do not assume the ability to spawn new machines - but we could model that as an effect. 

        type Machine    = Message → (Machine * Queue of Message)
        type Message    = Address * Content
        type Network    = Map of Address to Machine * Queue of Message

An application would be represented as a set of machines within a network. An implementation can readily partition the network of virtual machines across multiple physical machines. Distributed performance would rely on empirical determination of 'cliques' (groups of machines that mostly communicate internally). Effects and external outputs are represented by sending messages to addresses outside the network. But there are some issues:

* Message arrival order is generally non-deterministic. This generally requires a lot of overhead for coordination protocols. This could be ameliorated by assuming a "friendly" network that preserves batching and order of messages between any two machines.

* Messages will usually include a reply-to address. Unfortunately, this entangles machines and messages within a specific network. We cannot casually identify or rewrite addresses embedded within a message body. We cannot compose two networks that might have overlapping machine addresses. Relative addresses would not preserve meaning when tasks are refactored or delegated to subordinate machines. 

For the latter reason, I'm hesitant to favor any model relying on first-class object identity. Yet message-passing would be an easy path to useful real world systems.

# Second Class Models

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

Essentially, we represent a stream of commands manipulating a state. The explicit representation simplifies historical views, forking, undo, and similar. In general, keeping the entire history of commands may cost too much in some cases, so we may need to occasionally checkpoint the state. But for many use cases, the number of commands won't be too large and checkpoints may be managed explicitly.

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

