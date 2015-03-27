
# Communication between AVMs

Wikilon will host a set of abstract virtual machines, which I'm calling AVMs.

Each AVM has a user-defined [state](StateModels.md) and a step function typically bound to a [dictionary](BranchingDictionary.md) for live programming. AVMs and their state are *purely functional*. While AVMs lack internal concurrency, they should support reasonable levels for internal parallelism. Hierarchical AVMs should be entirely feasible, i.e. modeling a larger AVM in terms of simulating smaller ones with a purely functional network. However, the main idea with AVMs is that they should support scalable, concurrent, distributed implementations.

For a while, AVMs will primarily be hosted by Wikilon instances. But, long term, I would like to develop a feature to compile a set of AVMs into a unikernel, system process, Docker app, etc.. 

Modeling effects ultimately falls to the network layer. Some machines will provide adapters to outside resources (Redis, S3, AMQP, web services, printers, clocks, etc.), and AVMs can access those through the communications layer. In this sense, AVMs are perhaps similar to actors model. But AVMs are more constrained than actors. Significantly, AVMs generally cannot directly create new AVMs (there is no ether), and communications between AVMs will have enough structure to simplify reasoning about whole-system behavior.

Wikilon will provide an AVM-facing API in addition to a web-facing API.

## Design Concerns and Desiderata

Summary List:

1. simple, pure, non-monadic AVM behavior definition
1. schedulers that avoid worst-case non-determinism
1. scheduling with attention to latency and priority
1. effective means to deal with disruption and failure
1. usable security via capabilities and cryptographic sealers
1. natural visibility, revocability, process control, interrupts
1. decentralize naming, distributed hash tables, flexible routing
1. purely functional networks for mockups, mobility, etc.
1. simple network to implement, predict, and use effectively
1. effective foundation for integrating imperative resources
1. effective foundation for reactive demand programming

## Design Overview

The design elements and motivations are described under *Design Elements*. Here's the rough overview:

        type AVM = (State * (Step * Signal)){:AVM}
        type Step = (InMessage * State) → (OutMessages * State)
        type Signal = Context (with Content = SignalContent)
        ∃ type Context (depends on AVM)
        ∃ type State (depends on AVM)

        type SignalContent = (Text * Args) 
            where type of Args depends on Text

        type InMessage = (Context * Content) 
            where type of Content depends on Context & AVM

        type List of α = µL.((α*L)+1)
        type OutMessages = List of OutMessage (reverse order)
            (head of list is last message sent)
        type OutMessage = ((Capability * Content) * MsgSignal)
            where Content depends on source of Capability

        type MsgSignal = Context (with Content = MsgSignalContent)

        type MsgSignalContent = (Error + OK) where
            OK = unit (type 1)
            Error = (ReturnToSender + FateUnknown)
            ReturnToSender = (Cause * (Capability * Content))
            FateUnknown = ??? some investigative caps
            Cause = StatusCode (small integer)

        type Capability = (GlobalAddress * (LocalCap + CommonsCap)){$@}
        type GlobalAddress = Text (secure hash of Identity, in ABC Base16)
        type Identity = ABC resource containing public key and ad-hoc params
        type CommonsCap = (Text * Args) where type of Args depends on Text
        type LocalCap contains a Context. Exact type depends on host.

        type Self = Context → Capability
        type SelfSignal = ("self" * Self)
        type InitSignal = ("init" * 1)
        ... lots of ad-hoc signals

Not every AVM exists at the 'toplevel' with a global address. A whole network of AVMs could ultimately be modeled as a pure function, and we can also have concurrent containers and distributed models.

## Design Elements

### Message Based Communications

While messaging has [inherent flaws](https://awelonblue.wordpress.com/2012/07/01/why-not-events/), it has the advantages of being easy to implement and integrate with existing resources and physical networks. Messaging doesn't inherently require handshakes. And RDP may readily be constructed above a message-passing network model. 

In the basic form, we might try something like:

        AVM step: (Message * State) → (Messages * State)

Other design elements must address the following problems:

* how do we address our messages for sending?
* how do we secure our communications?
* how do we route messages within an AVM?
* what happens when a message causes an error?
* what happens when message cannot be delivered?

### Transactional Batching of Messages

When the message is the unit for atomic queries and updates, developers are pressured to awkwardly grow a large vocabulary of messages to represent atomic composite behaviors. Transactions mitigate this problem by allowing clients to compose operations. Unfortunately, transactions in distributed systems have their own problems, such as high latency coordination and complicated implementation.

Transactional batching offers a simple sweet spot: 

* a batch of messages is the atomic unit of communication
* messages in a batch are processed sequentially, in order
* output messages are partitioned into one batch per machine
* batches are implicitly ordered between two machines
* messages from any two batches are not interleaved

These batches may be *transparent* to our purely functional AVMs. An AVM processes each message separately. Batching becomes relevant only in context of non-deterministic concurrency. *Transactional batching is a constraint on scheduling to ensure a friendly default behavior.* 

Besides friendly batching, I may seek other nice default properties from the scheduler: fairness, heuristic timestamps for global ordering, etc.. 

Developers may still need to model queues, buffers, and so on to more precisely control communications. But, relative to systems with an adversarial scheduler, the need for explicit control should be less frequent, and oriented more towards reducing synchronization rather than increasing it.

## Handling Failure

A physical network is unreliable. If we send a message then, even with acknowledgements, we cannot be absolutely certain it has arrived and that the other party knows that we know it has arrived. After a message arrives, it might be rejected for other reasons, such as causing divide-by-zero error.

What should happen next?

* expand the failure
* provide feedback

Partial failure is difficult to handle. We can avoid this by similar means as transactional batching. Expand the failure by modeling a complete disconnection. Both current and pending batch is fully aborted.

Providing a little feedback allows the sender to be responsive to changes in network conditions. This requires a field per outgoing message, such that we may call back using that field to provide context. Also, to guard any substructural properties, we will return the message to sender when we know it was not delivered.

In some cases, we might never learn the true fate of a message. But, even in these cases, we can at least report a fate-unknown status. This way, we know what we don't know. We may also offer some investigative capabilities, i.e. so we may obtain feedback in case the fate is later discovered.

### Substructural Types

I want unrestricted use of substructural types, both in messages and AVM state. This is useful to resist accidental violation of protocols or contracts. Duplication or destruction of linear values would still be achievable through an effects model. Some annotations may support assertions that incoming messages are copyable or droppable.

### Capabilities and Addressing

I will leverage [capability-based security](http://en.wikipedia.org/wiki/Capability-based_security) between AVMs. Capabilities have advantages of being very explicit and visible for security reasoning, though other nice security features (such as revocability) generally require special attention.

The internal structure of a capability will be opaque to the user.

Each capability will have two parts:

* a *global* part, uniquely naming an AVM in the global network
* a *local* part, for secure routing and context within the AVM

The local part becomes the only unforgeable element of our capabilities. The global part doesn't need to be secure against observation or tampering, and indeed *must* be accessible to any network implementation. However, our global name should provide another form of security, guarding against man-in-the-middle attacks.

#### Global Addressing

The global part of a capability shall simply be the secure hash of an ABC value resource that provides:

* a public cryptographic key in a widely acceptable format
* an association list having text keys and ad-hoc metadata

I expect RSA and some specific ECC curves would be widely acceptable. I favor ECC and perhaps choosing just one initial curve, such as Ed448 or M-383 (cf. [safe curves](http://safecurves.cr.yp.to/)).

Anyhow, the public key enables the sender to autenticate the receiver, guarding against man-in-the-middle attacks. A little extra protocol can help guard against replay attacks (and provide idempotence).

The ABC resource is then made available through a distributed hashtable, such as Chord, Pastry, or Tapestry. In addition, the DHT may contain some dynamic metadata: current routing information and replicas, last seen, etc.. Thus, there is no dependency upon DNS or common web URLs.

AVMs that want a "common name" might additionally leverage a service such as NameCoin. However, by default, an AVM is effectively anonymous, and there is no dependency on DNS.

*Aside:* Should I forbid the `$` and `?` operators for the toplevel of any 'values' stream? Limiting to first-order computations isn't a problem.

#### Local Routing and Context

A message received by an AVM will have two parts: context and content.

        type InMessage = (Context * Content)

The context is secure, private, and serves useful roles such as method names, local routing, callback methods or identifiers. The content, on the other hand, is determined entirely by the message sender.

To secure the context, it is cryptographically sealed into the capability. The capability then inherits all substructural attributes of the context. If we have a linear capability, it is generally because we have a linear context.

Naturally, we need a function to perform this sealing:

        type Self = Context → Capability

An AVM must acquire access to a `Self` function at some point, otherwise it cannot model query-response patterns and callbacks and cannot secure its own state. A `Self` function is provided as part of the *AVM Life Cycle*.

*Aside:* If we model a hierarchical network, a local context may additionally include heuristic network-layer attributes: priority, idempotence, safety in the HTTP sense, etc.. No new features are needed.

#### Commons Capabilities 

Normal capabilities are opaque and specialized to a machine AVM. This default is good when we have objects with unique identifiers. However, consider: 

* access random numbers
* access estimated UTC Time
* /dev/null to trash data

If I'm just asking for random numbers, I might not care whom I'm asking. It might not even matter whether the answer is truly random. Similar with asking for time, or sending messages to a bit bucket. In these cases, and many others, sending a message across the network seems wasteful. A local implementation should work just as well.

That said, this is a performance feature. Commons caps will be discretionary.

Commons caps may be especially useful for high performance computing. E.g. we could embed OpenCL code in a commons cap, secured by HMAC or similar, for use in a specific domain. When developers need performance *right now*, hacking in a commons cap will certainly be easier than waiting on improvements in Awelon Bytecode compilers or ABCD extensions.

These commons capabilities can also provide a foundation for message pipelining, alternatives and multi-homing, mobile code, the [unum pattern](http://wiki.erights.org/wiki/Unum) and CRDTs, etc.. They are discretionary, flexible, securable. 

Commons capabilities can make Awelon systems network extensible and efficient without hindering unsophisticated mplementations.

#### Capability Representation

Basically, this:

        (GlobalAddress * (LocalContext + CommonsCap)){$@}

        type GlobalAddress = Text (secure hash of public key)
        type LocalContext = ADT, often cryptographically sealed
        type CommonsCap = (Text * Args); Args depends on Text

        ["encryptedLinearContext"]kf{$aes}V"globalAddress"l{$@}
        "/dev/null"#lVVRWLC"globalAddress"l{$@}

For effective security in a distributed system, we must cryptographically secure the local context. But any convention is acceptable. For commons capabilities, the text provides a quick discriminator for the local machine or network.

In this case, the `{$@}` value sealing format is simply to prevent normal dictionary code from trying to branch on address or forge commons capabilities. 

As per usual, secure hashes, cryptographic text, and other binary content will be represented using ABC's Base16 encoding (alphabet `bdfghjkmnpqstxyz`).

### Parallelization, Replication, and Scalability

When an AVM is mostly used for read-only operations, we can parallelize by forking the state for each message (or batch thereof). If the final state is the same as the initial state, our operation was read-only and we can continue. 

If the batch modifies state, we *might* be forced to do a little rework. But there is a race condition: if our operation is the first to modify state, we may commit our modified state. When read-write operations are infrequent or externally serialized (e.g. if write updates only come from one place), we'll almost never need to do any rework for local parallelism.

Conveniently, parallelization of AVMs can be used effectively with only a few statistics and heuristics, and may adapt to changing loads. Additionally, we can have a lot of paralleization *within* AVMs.

We may also replicate AVMs with read-mostly workloads. A replica will need to push occasional read-write batches back to master, and hold off further messages from the same origin until synchronization. However, it can be more or less transparent.

Using these techniques, AVMs are highly scalable at least for read-mostly work. To scale write-heavy work, sharding may be necessary (e.g. modeling DHTs).

### Interrupts, Deadlines, and Priorities

If an AVM is taking too long to process a message, and other messages are pending, we are free to try running the others first. If the others cause a change in state, we might need to restart the first batch with the new initial state.

This is a heuristic basis for interrupts. 

We could be more precise if we track information about priorities or deadlines. This sort of information could be added to the local context for a capability, e.g. for a hierarchical network layer with its own local context.

Naturally, if a batch takes much too long to complete even after a few tries, we might reject that as a timeout failure. Ultimately, developers might need to address timeout failures by altering algorithms to use iterative methods over multiple time steps. Timeouts might also be modeled precisely in an extended context for a hierarchical network.

### AVM Signals and Life Cycle

An AVM needs:

* a state
* a step function
* a world to manipulate
* a self to perceive
* a signal to get started

In context of Wikilon, the step function will usually be bound to a dictionary. The state must be provided when first creating the AVM, though it might be viewed and manipulated explicitly through a structure editor. The question is where should the other three elements come from?

My current plan is to model an AVM's basic structure as a triple:

        (State * (Step * Signal)){:AVM}

Here, Signal is simply a Context value, same value AVMs use for all incoming messages. Signal message contents will have form `(Text * Args)` where Args depends on Text and Text is an ad-hoc extensible set of signal names. 

As a convention: unrecognized signals, or signals the AVM cannot obey, should generally assert in error. However, a simple prefix like `~` might be used to indicate 'weak' signals that should be ignored if unrecognized.

Signals are then used to provide self, to provide a world or environment, and to get started. Later, signals might also be used to support graceful sleep, shutdown, mobility, indicate low-power mode, and so on. All signals are related to our hosting environment.

Interestingly, it is possible to stage some signals to an AVM, up until it really starts interacting in with the environment in an open loop. Also, because signals are ultimately simple messages, developers are able to transparently support remote signals. 

There is no primitive to 'spawn' a new AVM. Some environments may provide a spawn capability through the environment. However, even without spawn, it is easy to model one AVM within another.

## Communication with Self

An AVM can send messages to itself. But it might be wise to limit this, e.g. by treating all self messages as part of the same batch and thus refusing to progress while self messages are processed.

## AVM Hosts or Containers

In addition to the purely functional AVMs, I think it would be wise to explicitly model AVM hosts and containers:

* **AVM host** anonymous, supports multiple AVM containers. Potentially supports migrations of containers, and histories of them. Provides relationships to the distributed hashtables, and perhaps state for distributed hashtables. Might provide commons capabilities, adapter effects for basic network layer, and generic multi-protocol network layer integrations. AVM hosts might also model latency between containers, and full-container process control. 

* **AVM container** supports a single address containing zero or more AVMs, where contained AVMs are implemented concurrently. The container provides the environment model, support for priorities and deadlines, etc.. Provides extra state, such as the secure hash keys. 

So, minimal implementation is a host with a container with an AVM. However, we can have a host with many containers each with many AVMs, providing a high degree of concurrency and efficient hierarchy.

For communication within a host, even between containers, we should be able to avoid encryption almost entirely. This gives us efficient IPC.

Security thoughts: separate keys for context vs. container ID should provide some very strong protection even if a technology breaks public keys. Access to the man-in-the-middle attack can be resisted further at the DHT layer, by rejecting all instances when two locations for the same secure hash are valid.

## Second Class Sealers

So far I provide access to sealer resources just for the self function. Can other value sealing models be implemented in terms of this, without state?

To seal a value, send it to an AVM 'sealer' context, and the response is a capability whose context contains the sealed value. To unseal it, then, you must then send the capability to the unsealer context.

        AVM receives (sealer, ("xyzzy", reply))
        let SV = self (sealed "xyzzy")
        AVM sends (reply, SV)
        AVM receives (unsealer, (SV, reply))
        AVM sends (SV, ("hidden password", reply))
        AVM receives (sealed "xyzzy", ("hidden password", reply))
        AVM sends (reply, "xyzzy")

It seems I'll need a hidden value with high entropy in the unsealer, otherwise anyone could send a value directly to the sealed value in order to have the network layer open the context. I would also need to compare this hidden password to the origin in constant time. 

I could maybe use a high-entropy discretionary sealer, such as `{:hidden password}`. But then I'd need to ensure constant time comparisons even for discretionary unsealers. ... Maybe this would be a good idea anyway.)

This isn't very efficient, but it is doable.

## Request Response Patterns

Request-response isn't built into the AVM messaging model. If a reply is desired, a reply capability should be provided with the message. This allows the reply to be asynchronous or forwarded. However, even without a reply, the caller does receive a simple feedback signal after a message is processed.

## Batch Message Model

Messages are batched for communication between containers. They're further serialized for communication between hosts. What information should a batch possess? Maybe:

* a list of messages
* a time stamp (plus logical latency)
* connections layer acks, naks, origin, etc..

## Behavioral Programming

Work on [Live Sequence Charts](http://www.wisdom.weizmann.ac.il/mathusers/amarron/bp-sigerl2010.pdf) might offer an interesting basis for coordinating apps. This is easily built upon AVMs and the current network structure.

