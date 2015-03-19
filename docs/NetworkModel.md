
# Communication between AVMs

Wikilon will host a set of abstract virtual machines, which I'm calling AVMs.

Each AVM has a user-defined [state](StateModels.md) and a step function typically bound to a [dictionary](BranchingDictionary.md) for live programming. AVMs and their state are *purely functional*. While AVMs lack internal concurrency, they should support reasonable levels for internal parallelism. Hierarchical AVMs should be entirely feasible, i.e. modeling a larger AVM in terms of simulating smaller ones with a purely functional network. However, the main idea with AVMs is that they should support scalable, concurrent, distributed implementations, and maybe even compile down to Docker apps or Unikernels!

Modeling effects ultimately falls to this network layer. Some machines will provide adapters to outside resources (Redis, S3, AMQP, web services, printers, clocks, etc.), and AVMs can access those through the communications layer. In this sense, AVMs are perhaps similar to actors model. But AVMs are more constrained than actors. Significantly, AVMs generally cannot directly create new AVMs (there is no ether), and communications between AVMs will have enough structure to simplify reasoning about whole-system behavior.

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
1. simple to implement, comprehend, predict, and utilize
1. effective foundation for integrating imperative resources
1. effective foundation for reactive demand programming

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

These batches are *transparent* to our purely functional AVMs. An AVM processes each message separately. Batching becomes relevant only in context of non-deterministic concurrency. *Transactional batching is a constraint on scheduling to ensure a friendly default behavior.* 

Besides friendly batching, I may seek other nice default properties from the scheduler: fairness, heuristic timestamps for global ordering, etc.. 

Developers may still need to model queues, buffers, and so on to more precisely control communications. But, relative to systems with an adversarial scheduler, the need for explicit control should be less frequent, and oriented more towards reducing synchronization rather than increasing it.

## Handling Failure

Messaging is not reliable in a physical system. Best effort delivery can improve reliability, but is still not perfect. Maybe we want to control latencies anyway. Additional failures may occur due to inconsistencies, e.g. divide-by-zero error. 

What happens next?

The simple, easy to implement and understand option is to silently drop undelivered messages. 

One option is to silently drop our messages into the ether. This option is simple, easy to implement, and is perhaps the most realistic model of the network we can implement: we call our network unreliable, and our developers work around this limitation. RDP should handle an unreliable network well enough, e.g. due to its idempotent messages.

it might be invalid - e.g. causing a type error or divide-by-zero or similar. In this case, it isn't always clear whom to blame. Messaging doesn't maintain a good model for responsibility.



Thought: We might even want to leverage this, e.g. introduce some latency requirements when we're sending a message. 



### Substructural Types

I want unrestricted use of substructural types, both in messages and AVM state. This is useful to resist accidental violation of protocols or contracts. Duplication or destruction of linear values would only be achieveable through the effects model. Some annotations may support assertions that incoming messages are copyable or droppable.


### Capabilities and Addressing

I will leverage [capability-based security](http://en.wikipedia.org/wiki/Capability-based_security) between AVMs. Capabilities have advantages of being very explicit and visible for security reasoning, though other nice security features (such as revocability) generally require special attention.

The internal structure of a capability will be opaque to the user.

Each capability will have two parts:

* a *global* part, uniquely naming an AVM in the global network
* a *local* part, for secure routing and context within the AVM

The *context* is the unforgeable element of our capabilities. 

An outgoing message will then have at least a capability to indicate its destination, and content to carry information. In addition, we may need some information to properly handle failure conditions and message pipelining.

#### Global Addressing

The global part of a capability shall simply be the secure hash of an ABC value resource that provides:

* a public cryptographic key in a widely acceptable format
* an association list having text keys and ad-hoc metadata

I expect RSA and some specific ECC curves would be widely acceptable. I favor ECC and perhaps choosing just one initial curve, such as Ed448 or M-383 (cf. [safe curves](http://safecurves.cr.yp.to/)).

Anyhow, the public key enables the sender to autenticate the receiver, guarding against man-in-the-middle attacks. A little extra protocol can help guard against replay attacks (and provide idempotence).

The ABC resource is then made available through a distributed hashtable, such as Chord, Pastry, or Tapestry. In addition, the DHT may contain some dynamic metadata: current routing information and replicas, last seen, etc.. Thus, there is no dependency upon DNS or common web URLs.

AVMs that want a "common name" might additionally leverage a service such as NameCoin. However, by default, an AVM is effectively anonymous, and there is no dependency on DNS.

*NOTE:* I may forbid the `$` and `?` operators from toplevel of value resources, to guarantee a first-order computation. 

#### Local Routing and Context

A message received by an AVM will have two parts: context and content.

        type InMessage = (Context * Content)

The context is secure, private, and serves useful roles such as method names, local routing, callback methods or identifiers. The content, on the other hand, is determined entirely by the message sender.

To secure the context, it is cryptographically sealed into the capability. The capability then inherits all substructural attributes of the context. If we have a linear capability, it is generally because we have a linear context.

Naturally, we need a function to perform this sealing:

        type Self = Context → Capability

Access to Self supports modeling of callback capabilities, which are essential for common communication patterns such as query-response. 

A relevant question is: how does the AVM gain access to Self? I'll address this question under *AVM Life Cycle*.

#### Concrete Capability Representation

A capability is modeled as a sealed value. A purely functional model of capabilities with discretionary sealers might look like:

        (GlobalPart * LocalPart){:net}

Of course, for effective security in a distributed system, we must use cryptographic sealers. If we desired, we could explicitly represent both stages. Example linear capability:

        ["encryptedLinearContext"]kf{$fmtLocal}"globalPart"l{$@}

That could work pretty well, and would permit sealing of the inner context to be somewhat more discretionary. But we might be able to improve performance and security with a specialized token:

        {@globalPart/encryptedLinearContext'kf}

An ABC implementation would then understand this token as creating a sealed value with the relevant structural and substructural properties. Of course, to construct a value with this representation, we'll need support from the Self function. Our cryptographic sealer/unsealer might look like:

        {:@globalPart/aes$key}      sealer   (via Self function)
        {.@globalPart/aes$key}      unsealer (not exposed)

The convention is that cryptographic sealers use `{:format$key}`, so I'm essentially treating `@globalPart` as a specialized class of formats. In this case, symmetric key cryptography should be okay.

*ASIDE:* Contexts are sealed cryptographically. Networks will not provide any sort of stateful sealing, not even for linear contexts. Developers can always add a layer of indirection if they want to model very large or stateful contexts while keeping capabilities small.

### Common Capabilities (Tentative)

By default, capabilities are highly specialized to one machine AVM. This works pretty well when our objects have unique identities. However, consider: 

* secure random numbers
* access estimated UTC Time
* /dev/null effect to drop linear values
* reflection on a value's structure

If you're just asking for the time or some random numbers, you shouldn't need to send a message all the way across the network. The idea here is that some capabilities are 'common' and it wouldn't be a problem to substitute the local implementation. Of course, we should still support calling back to origin in case a local implementation isn't available. 

We might represent and distinguish commons capabilities by using a `~` suffix instead of the substructural suffix:

        {@globalPart/null~}

The network then has discretion regarding which implementation to use, local or origin.

Common capabilities might also be leveraged for high performance computation. For example, we might use it for high performance matrix multiplication or a secure hash function. However, this role does overlap with ABCD extensions and development of ABC compilers. It's just an idea for short-term performance.

Of course, we will need some global agreement regarding which common capabilities should be supported. As a general rule, common capabilities should not require any state (besides an optional cache)... and should be easy to implement securely, safely, and consistently. 

#### Regarding the Unum Pattern

An interesting intermediate between normal AVMs and common capabilities is the [Unum pattern](http://wiki.erights.org/wiki/Unum), where a stateful machine has a distributed implementation.

The unum pattern will NOT be primitive in the AVM network model. Unums are far beyond my quota for implementation complexity. However, it might be useful to model una above normal AVMs. Especially for CRDTs (commutative replicated data types). 

### Interrupts and Priority (Tentative)

If an AVM is running one batch and several others are pending, a scheduler may heuristically decide to abort the running batch in order to apply the others. After the others have run, the scheduler may restart the original batch. If the batch has restarted a few times, we might instead fail the batch permanently.

This implicit, heuristic mechanism can address a lot of concerns, such as ensuring that new user input can be processed, and that slow batches cannot completely choke service.

Explicit priorities and interrupts might be more reliable. But it isn't very clear how to address these within the model. 

The best I can think of right now: add a simple priority argument to `Self`. Developers may bind a zero argument if they want priority-free messaging. If we constrain priorities into some fixed range (e.g. -1 to 1) or give them a physical correspondence (e.g. in terms of latency), it might be easier to control relative priorities for hierarchical models. 

If every capability has a simple priority together with context, we can compute the priority for the whole batch very easily. Interrupts could then be high priority messages with some special effects, such as: we model a suspend operation with a message that seals the AVM state under `{:suspend}` and thus causes all incoming messages to fail.

For now, let's see how far the implicit, heuristic mechanisms and a little developer discipline can take us. I think most responsiveness concerns can be addressed by simply favoring iterative methods or dividing large efforts into separate machines (similar to conventional divisions of threads).

### Network Disruption


### Query Response Messaging


### AVM Life Cycle



### Replication and Scalability



### Expirations?






=============


Relevant questions and concerns:

* How do we reason about concurrency?
 * Transactional batches of messages?
 * Priority and interrupts?
 * Ordering and serialization properties?
* How do we reason about disruption?
 * Reify connections or relationships? 
* How do we reason about latency?
 * Asynchronous communication by default?
 * No immediate 'response' to a message?
 * Explicitly model latency of connections?
* How do we achieve high scalability?
 * Replication and parallelism for queries?
 * Conventions for long-lived subscriptions?
 * Shift computation into connection state?
 * What about congestion and latency?
* How shall we identify machines on the network?
 * Can we transparently model hierarchy?
 * Can these identities be capability-secure?
* How do we support functional implementation?
 * Monadic effects? Or return list of events?
* How do we address security concerns?
 * Fine-grained grant of authority?
 * Debuggable visibility of authority?
 * Attenuation of authority?
 * Revocability of authority?
 * Malformed or malign messages?
 * Process control?
 * Denial of service attacks?

I'm [not fond] of message passing models. However, they are easy to implement and integrate with existing systems. Long term, I'd like to move towards RDP-based communication and computation. I'd like to arrange for network communications and conventions to be 80% of the way there.

The types currently look like:

        State of AVM: any ABC value, user defined per AVM
        Step Function: (InputMessage * State) → (List of Message * State)  (unlikely)
        InputMessage: either just an ABC value, or (Capability * Content) ??
        Message: (Capability * Content) (or something with connections)
        Content: any ABC value, user defined, specific to context
        Capability: ??? (opaque, unforgeable, breakable, substructural)
        Network: ??? (implicit, substructural, stateful)

These types are in flux and aren't the full picture. Points:

* avoids `{token}` driven effects as unfit for purely functional simulation
* avoids monadic effects as awkward to express in straight line AO or ABC
 * but, sadly, might not adjust well to working with connections
 * might instead operate in a Network monad (AVM's version of IO)
 * in this case, message list is also replaced by output effect
* easily grok that no new input arrives during processing of existing message
* capability secure communication; easy to reason about and control
* step function can be modified by live programming independently of state
* state may be browsed and directly manipulated via debugger

More questions:

* Should messages have response option built into the structure? 
* How do we recognize disruption or failed communications?
* Should capabilities be 'flat' or 'hierarchical'?
* How do we manipulate hierarchical capabilities in messages?
* How do we revoke or break a capability?
* How do we usefully associate a query with a callback?

## Design Concepts

### Transactional Batches of Messages

### Connections (rejected)

A notion of 'connections' at the network layer could be very convenient:

* ordering between batches
* precise control over batching
* model network disruption
* model revocation of authority
* model stateful relationships
* network-layer computation

Connections would provide ordering for a stream of multiple batches. Communications on different connections might be result in independent batches. We might supply support variant of 'commit' or 'flush' operation per connection to further break up batches. Connections could be stateful, i.e. such that a connection is irrevocably 'broken' when we're done with it (either due to authority or a bad network).

Connections with rich logic (such as transforming values or containing some internal state) could be an effective basis for parallelizing computations 'in between' the AVMs. On average, the number of links will be larger than the number of nodes, so this is an opportunity for scalability.

But, while connections sound neat... how can I implement them in a purely functional manner? One option, perhaps, is to switch to *monadic* operations for effects on our network. I'm not especially keen on this because it's a bad fit for straightline AO. But, sadly, if this is the only thing I can imagine, then it's certainly what I'll need to do.

A related concern: if I have monadic operations that are *stateful* at the network layer (e.g. creating connections), but *read-only* at the AVM layer, I'll probably want to ensure that I can compose these operations in parallel. The trick, I guess, will be forking a source of identities for each parallel computation.


### Read-Only Queries and Mirroring

Batches of read-only messages can be processed in parallel with one read-write batch. 

A batch is read-only when the AVM finishes in the same state as it started. It would not be difficult to use a few heuristics or statistics to estimate probability that a batch is read-only (e.g. based on history, origin, message count), and later decide whether it is worth attempting to process a batch in parallel. If it turns out the batch was not read-only, we can fall back to sequential mechanisms.

This technique can also be used in a replication scenario. A replica of an AVM may directly process read-only batches. A writer batch, however, must be delivered to the 'master' to provide global serialization. The master may then synchronize replicas by sending some update messages.

This after-the-fact decision for read only messaging is convenient from a UI standpoint, though it probably hurts performance. Fortunately, developers could mitigate the performance hit by arranging such that some AVMs are read-only (or nearly so) while writes are indirectly offloaded to other AVMs, e.g. by having one AVM for the blog content, and another to count the reads per page.

### Explicit Models for Locality, Mobility, Ambient Authority

I like the idea of 'ambient oriented programming' such that a machine might easily find (for example) a local printer. However, I believe that this should be modeled explicitly, using capabilities to provide the AVM a directory or registry of local services. The explicit approach is more flexible, e.g. because we can have different registries based on different attributes (trust, cost, region, etc.) and we can potentially model compositions of registries. The result is that machines only talk to machines. 

So, I'm NOT going to make a special exception from capability security for service discovery.
























Reconsidering the Model
=======================

Despite [inherent flaws](https://awelonblue.wordpress.com/2012/07/01/why-not-events/), message passing between AVMs is a promising option in most respects.

An AVM step function would, more or less, have the form: `(Message * State) → (Messages * State)`. We might divide messages into `(Context * Content)` where context is locally meaningful and capability-secure, i.e. as a basis for internal routing and callbacks. The awful unbounded non-determinism of actors model can be bounded, e.g. with *transactional batching*, ordered batches, and timestamps. Messages are readily applied to cause imperative effects.

Where I get stuck is *failure and disruption* concerns - i.e. what happens when a message is lost, or when it causes a divide-by-zero error at the recipient? I'm also a bit concerned about preserving visibility - i.e. the network seems semantically opaque, and access to it seems a hack.

Of course, if we choose something other than messaging, we will need adapters to external message passing systems and effects. 

## Transfer-Based Communications Model?

Focusing on visibility and accessibility properties:

* data is visible, accessible, meaningful at all times
* data is never destroyed or duplicated by accident

The first point is rejecting the idea of an opaque, invisible network layer that holds onto data. Instead, data must be hosted within AVM state. Further, it should be accessible and meaningful, using local conventions for type. There is no opaque 'outbox' or 'inbox' unless developers choose explicitly to model one.

Since data is neither destroyed nor duplicated, the default concept is *movement* of data. We transfer data from one AVM's state to another AVM's state. Of course, we might copy the data before moving it. But movement works nicely with substructural types, too. Unfortunately:

* atomic transfer is unreliable in a distributed system
* atomic transfer does not acknowledge physical latency

If I attempt to transfer data in a non-atomic manner, I'll have all the same problems as messaging, with data being lost in transit. Plus this is more complicated to implement and explain. 




