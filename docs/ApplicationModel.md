
# Application Models for Awelon Project

The conventional, mainstream "application model" is a detached process that interacts with shared services and data. Awelon could easily follow convention, e.g. modeling monadic IO or a message-passing process. But my goal for Awelon project is to unify programming and user experiences, to make sharing and composition easy. To that end, the detached process seems a poor fit, being inaccessible and invisible.

An intriguing feature of [Awelon language](AwelonLang.md) is that, due to rewrite semantics, process state can always be represented by and recorded as an Awelon program. This provides a foundation for durability, distribution, and debugging. Of course, one needs to also preserve a reference to the associated dictionary. We can feasibly embed application task state within a dictionary:

        :job-1123 [process state is recorded here]

Presumably, an external agent would find available jobs, execute pending IO operations (such as sending e-mail or fetching HTML), then compute the next process state. It might run many steps between saving process states. Besides maintaining the process over time and viewing its state in a debugger, we could capture checkpoints or probe uncommitted sample messages.

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
* sharable: can copy or move apps to a new environment and continue
* live: we can tweak and modify application behavior during runtime
* immutable: state is represented by an immutable, non-unique value
* concurrent: model many simultaneous interactions with environment
* distributed: partition state and computation across many machines
* deterministic: behavior is fully repeatable up to external inputs

First-class value representation of application state and behavior is convenient for composition, immutability, sharing, and process control. However, first-class "object identity" - whereby we communicate names of processes or state components - is more problematic for sharing, composition, determinism, and even memory management. The basic issue is duplication or forgery of names when we try to replicate things, preserving behavior for these names in a new context.

I'll directly exclude models that rely on object identity such as runtime allocation of 'new' variables or channels. This constrains our choice of concurrency and communication models. Fortunately, we can still model useful concepts, like second-class channels or variables, hierarchical filesystems or state models, and publish-subscribe topics.

Concurrency requires we both represent multiple external requests and receive multiple inputs, and progress incrementally (i.e. such that supplying any requested input may result in new requests, no need to supply all inputs up-front). Distribution, meanwhile, requires that state can be partitioned across multiple processors, and long-lived state relative to communication latencies. Determinism, in context of concurrency and intra-process communication, requires some form of confluence for many-to-one communications (otherwise, we'll have a bottleneck).

If necessary, I'm reluctantly willing to relax determinism. We could assume an external scheduler, and support arrival-order non-determinism or external interrupts. But our model must enable a purely functional simulated scheduler for testing, hierarchical embeddings, and interactive queries of application state. Preferably without a huge performance loss where the scheduler becomes a bottleneck.

## Monadic Processes

Monadic computations match several desiderata. Especially the [Free and Freer (operational) monads](http://okmij.org/ftp/Computation/free-monad.html), i.e. modeling all monads with an extensible, interpreted "effects" type. Of course, this still leaves a big question regarding *which* effects to model. For example, we could support sockets for network access, or limit ourselves to servicing and sending HTTP requests, or focus on distributed queues. Many models, such as RDP, can be given a monadic API.

Concurrency is a little awkward. A monad can generally only present one request at a time, so for concurrent requests we end up spawning lots of small monad processes that require a lot of ad-hoc inter-process communications. This could be ameliorated by a carefully designed API. Blackboard metaphor might help, registering active requests into the environment.

Since monadic IO is well proven in Haskell, it's not a bad place to get started. We could at least model some simple web services and agents this way.

## Kahn Process Networks

I am interested in leveraging [Kahn Process Networks (KPNs)](https://en.wikipedia.org/wiki/Kahn_process_networks) in Awelon. KPNs support multiple input and output channels. The order with which messages for separate input channels are provided does not matter, which provides a simple basis for concurrency on input. Messages may be pending on multiple output channels, which provides a simple basis for concurrency on output. Gluing outputs to inputs provides a simple basis for composition of KPNs. Importantly, it is feasible to *accelerate* the evaluation of KPNs such that we avoid explicit routing of messages. This would be useful for large scale applications, distributing computations across multiple machines. 

We can model reactive KPNs by adding explicit time-step message to every channel. This enables us to express either "no input until later" or "no input this time" to a process waiting on that channel. (The subtle difference is whether we permit streaming of many inputs within a time-step.) Each process would send similar messages on the appropriate output channels. With this, we can model real-time systems with asynchronous, interleaved inputs. We can also model acknowledgement channels to create network "back pressure", i.e. so an upstream process doesn't push more than K messages before prior messages are acknowledged as received. 

KPNs are essentially a deterministic variant of flow-based programming.

The main weakness of KPNs is that it is difficult to represent a dynamic network structure. We can work around this a little using first-class KPN values, but even then it would be awkward to extract and preserve process state.

## Reactive Demand Programming

Reactive Demand Programming (RDP) allows concurrent behaviors to contribute to shared values, which may change reactively over time. Contributions accumulate in a shared set, such that behaviors (and effects) are commutative and idempotent. This preserves useful equational reasoning properties from functional programming, while enabling useful IO through shared variables. A monadic API might have a few operations:

        get : Label a → m (Set of a)        CURRENT STATE
        put : Label a → Set of a → m ()     FOR NEXT STATE
        at  : Label () → m a → m a          HIERARCHICAL STRUCTURE

Unlike conventional monadic request-response loops, with RDP we have a stable monadic behavior that is applied 'continuously', repeating in every time step. This is convenient for live programming: a human could adjust the monadic behavior function over time, applying updates upon the next step.

Additionally, we could support a concept of negation or deletion:

        del  : Label a → Set of a → m ()
        del' : Label a → (a → bool) → m ()

With either of these, we can additionally support durable data - labels that preserve data with an implicit `get >>= put` behavior. This would have many similarities to functional-relational programming (cf. [Out of the Tarpit](https://github.com/papers-we-love/papers-we-love/blob/master/design/out-of-the-tar-pit.pdf) by Ben Moseley).

The main weakness of RDP is that the data type `a` in the `Set of a` is restricted to comparable value types. (Although, we could use specialized set models in some cases, such as a trie of strings/binaries.) This is necessary for both determinism and recognizing when a set of labels have entered a stable state. (We could feasibly relax this constraint by permitting specialized use of single-assignment labels.) We might also introduce specialized functions, such as `each : Label a → (a → m ()) → m ()` and `contains : Label a → a → m bool` to further optimize system stabilization.

RDP integrates most easily with external topic-based publish-subscribe systems. Effectively, labels are fine-grained topics.

## Machines

The [machines](https://hackage.haskell.org/package/machines) model developed by Edward Kmett and Rúnar Bjarnason attempts to solve several issues with conventional monadic IO. The underlying types:

        Step k o r = Stop | Yield o r | Await (k t) ((1+t) → r)
        Machine k o = Step k o (Machine k o)
        MachineT m k o = m (Step k o (MachineT m k o))

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

Note: For live programming, we might additionally wish to stabilize the `P` value, i.e. such that `P s = s → s` and `PR s = s → 1 + s`. A stable process value simplifies external modification of the step function, between steps. In this case, we cannot fork concurrently.

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

Use of `alt` allows for non-deterministic decisions, with `fail` eliminating some decisions. Setting a variable twice will also result in failure. Labels can feasibly be constructed based on types, or have a finite set of types.

To model stateful behaviors, we might loop or enable access to history:

        type B = List of (m B)      STATEFUL LOOP
        hist : Label a → m a        STATEFUL HISTORY

Access to historical information is more convenient in context of live software updates, job control, state accessibility, etc.. Loops, especially with concurrency, tend to be opaque and escape the thumb of human users. In case of access to history, we might assume a low-priority optional behavior of form `hist label >>= set label` to preserve unassigned variables across time-steps by default, plus a `reset` function to drop variables.

To support priority explicitly, we could prioritize `alt` by including a number that heuristically indicates how much weight we should associate with the first option above the second. To support scratch-spaces for computations and composition of applications, we could also support a hierarchical state model with subdirectories, e.g. `in : Directory → m a → m a`. For concurrent operations, we can safely add a `fork` operation due to single-assignment restrictions.

Weaknesses of this model are combinatorial explosions of search paths (too many options) and expression of concurrent constraints or contributions to a single value (e.g. no way for individual behaviors to say "render this" then combine all these options).

# Second Class Models

Although these aren't suitable as a foundation for *general* applications, they could still be useful and interesting.

## Computable Forms

Using editable views in a graphical environment, it is not difficult to represent a boolean as a checkbox or provide a color-picker together with an HSV type, a slider for a ranged number, or a calendar widget for a date type. We can model a drop-down "selection list" in terms of a list together with an indexed selector. Hence, we can model interactive forms as normal values or definitions in Awelon.

Although the above is a usable approach, we can benefit from properly separating a record of input data from the form, i.e. such that our form is described by a *function* that receives a single *record argument*, and the record itself just contains the input data. With this approach, we can infer the required fields and types for the record, and perhaps present to our clients a value with holes in it, and perhaps render a partially evaluated result.

This doesn't generalize easily for interactive applications, but forms seem widely applicable as a specialized feature.

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

## Content Addressed Network Models

Conventional networks associate a network address (socket, IP, URL, etc.) with a servicing process. This association is external. But an alternative is to associate a network model with *content*. Topic-based publish-subscribe is one example. Another might involve creating an asymmetric encryption key per process, and communicating based on public key (in practice a unique address/channel per process, but logically content addressed). In the latter case, we could leverage distributed hashtables for routing (instead of centralized DNS).


