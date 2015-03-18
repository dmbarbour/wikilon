
# Communication between AVMs

Wikilon will host a set of abstract virtual machines, which I'm calling AVMs.

Each AVM has a user-defined [state](StateModels.md) and a step function typically bound to a [dictionary](BranchingDictionary.md) for live programming. AVMs and their state are *purely functional*. While AVMs lack internal concurrency, they should support reasonable levels for internal parallelism. Hierarchical AVMs should be entirely feasible, i.e. modeling a larger AVM in terms of simulating smaller ones with a purely functional network. However, the main idea with AVMs is that they should support scalable, concurrent, distributed implementations, and maybe even compile down to Docker apps or Unikernels!

Modeling effects ultimately falls to this network layer. Some machines will provide adapters to outside resources (Redis, S3, AMQP, web services, printers, clocks, etc.), and AVMs can access those through the communications layer. In this sense, AVMs are perhaps similar to actors model. But AVMs are more constrained than actors. Significantly, AVMs generally cannot directly create new AVMs (there is no ether), and communications between AVMs will have enough structure to simplify reasoning about whole-system behavior.

Wikilon will provide an AVM-facing API in addition to a web-facing API.

## Design Desiderata

Summary List:

1. First order, purely functional step functions. No monad or power block.
2. Keep it simple. Easy to understand, predict, and implement network.
3. Support distributed, local concurrent, and purely functional networks.
4. Avoid non-essential latency. Handshakes must subject to pipelining.
5. Suitable for resource adapters. No global coordination or rollbacks.
6. Simplify reasoning about concurrency, consistency, latency, disruption.
7. Simplify reasoning about authority, visibility, attenuation, revocation.
8. Simplify reasoning about distributed code. Opt-in. Clear responsibility.
9. Easy scalability through replication, decomposition, sharding, CRDTs.
10. Potential to compile AVM-sets to unikernels, docker objects, OS apps.
11. Decentralize AVM namespaces via Chord, Tapestry, DHT, NameCoin, etc.
12. Easy foundation for Reactive Demand Programming signals and vats.

Derived properties:

* AVM cannot obtain synchronous feedback from network (1).
* Network cannot directly host services, discovery, or resources (2, 7).

Potentially relevant considerations:

* Network disruption, process killed, cap revoked: all very similar.
* Feedback on network disruption is generally unreliable.
* May benefit from mobility and locality model, a hosting concept.
* Encourage conventions: idempotent, causally commutative messaging.

## Design Elements

### Message Based Communications

While messaging has a [great many weaknesses](https://awelonblue.wordpress.com/2012/07/01/why-not-events/), it has the advantages of being easy to implement, understand, and integrate with existing resources. Messaging doesn't inherently require handshakes. And RDP can later be constructed above a message-passing network model. 

In the basic form, we might try something like:

        AVM step: (InMessage * state) → (OutMessages * state)

Other design elements must address the following problems:

* how do we address our messages for sending?
* how do we secure our communications?
* how do we route messages within an AVM?
* what happens when message cannot be delivered?

### Substructural Types

I am not going to restrict use of substructural types. State, messages, etc. may be affine, relevant, linear. This is useful to resist accidental violation of protocols or contracts. 

Of course, developers will be free to explicitly violate substructural types. For example, send a message to an AVM equivalent of /dev/null to drop any value, and another resource to duplicate valeus. (See *Common Capabilities* below.)

### Capabilities and Addressing

My plan is to leverage [capability-based security](http://en.wikipedia.org/wiki/Capability-based_security) between AVMs. Capabilities have advantages of being very explicit and visible for security reasoning, though other nice security features (such as revocability) generally require special attention.

The internal structure of a capability will be opaque to the user.

Each capability will have two parts:

* a *global* part, uniquely naming an AVM in the global network
* a *local* part, for secure routing and context within the AVM

An outgoing message will have at least two parts:

* a *capability* to specify where the value is going
* the *content* of the message: a plain old ABC value

#### Secure Global Addressing

The global part of a capability shall simply be the secure hash of an ABC value resource that provides:

* a public cryptographic key in a widely acceptable format
* an association list having text keys and ad-hoc metadata

I expect RSA and some specific ECC curves would be widely acceptable. I favor ECC and perhaps just choosing one curve, such as Ed448 or M-383 (cf. [safe curves](http://safecurves.cr.yp.to/)).

Anyhow, the public key enables the sender to autenticate the receiver, guarding against man-in-the-middle attacks. A little extra protocol can help guard against replay attacks (and provide idempotence).

The ABC resource is then made available through a distributed hashtable, such as Chord, Pastry, or Tapestry. In addition, the DHT may contain some dynamic metadata: current routing information and replicas, last seen, etc.. Thus, there is no dependency upon DNS or common web URLs.

AVMs that want a "common name" might additionally leverage a service such as NameCoin. However, by default, an AVM is effectively anonymous, and there is no dependency on DNS.

*NOTE:* I may forbid the `$` and `?` operators from the toplevel of value resources, to guarantee a first-order computation. 

#### Secure Local Routing and Context

A message received by an AVM will have two parts: context and content.

        type InMessage = (Context * Content)

The context is secure, private, and serves useful roles such as method names, local routing, callback methods or identifiers. The content, on the other hand, is determined entirely by the message sender.

To secure the context, it is cryptographically sealed into the capability. The capability then inherits all substructural attributes of the context. If we have a linear capability, it is generally because we have a linear context.

Naturally, we need a function to perform this sealing:

        type Self = Context → Capability

Access to Self supports modeling of callback capabilities, which are essential for common communication patterns such as query-response. 

A relevant question is: how does the AVM gain access to Self? 

I'll come back to this question later.

#### Concrete Capability Representation

A capability is modeled as a sealed value. A purely functional model of capabilities with discretionary sealers might look like:

        (GlobalPart * LocalPart){:net}

Of course, for effective security in a distributed system, we must use cryptographic sealers. If we desired, we could explicitly represent both stages. Example linear capability:

        ["encryptedLinearContext"]kf{$fmtLocal}"globalPart"l{$@}

This could work pretty well, and would permit sealing of the inner context to be somewhat more discretionary. But we might be able to improve performance and security with a specialized token:

        {@globalPart/encryptedLinearContext'kf}

An ABC implementation would then understand this token as creating a sealed value with the relevant structural and substructural properties. Of course, to construct a value with this representation, we'll need support from the Self function. Our cryptographic sealer/unsealer might look like:

        {:@globalPart/aes$key}      sealer
        {.@globalPart/aes$key}      unsealer

The convention is that cryptographic sealers use `{:format$key}`, so I'm essentially treating `@globalPart` as a special class of formats. In this case, symmetric key cryptography should be okay.

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

The network then has discretion regarding which implementation to use.

Common capabilities might also be leveraged for high performance computation. For example, we might use it for high performance matrix multiplication or a secure hash function. However, this role does overlap with ABCD extensions and development of ABC compilers. It's just an idea for short-term performance.

Of course, we will need some global agreement regarding which common capabilities should be supported. As a general rule, common capabilities should not require any state (besides an optional cache)... and should be easy to implement securely, safely, and consistently. 

#### Regarding the Unum Pattern

An interesting intermediate between normal AVMs and common capabilities is the [Unum pattern](http://wiki.erights.org/wiki/Unum), where a stateful machine has a distributed implementation.

The unum pattern will NOT be primitive in the AVM network model. Unums are far beyond my quota for implementation complexity. However, it might be useful to model una above normal AVMs. Especially for CRDTs (commutative replicated data types). 

### Transactional Batching of Messages

I want to easily reason about behavior in the presence of concurrency. However, I also want to avoid distributed transactions, time warp protocols, or other mechanisms that require high-latency coordination between machines. A nice middle ground is *local* transactions: 

* the unit of communication becomes a *batch* of messages
* messages in the batch are processed together, atomically
* order of messages within each batch is preserved
* if the batch does not complete, it is rolled back
* output messages are delayed until the batch finishes
* output messages are grouped into new batches per recipient



(deprecated): The batch of incoming messages is not exposed to the AVM. The AVM receives one message at a time. But the network layer transparently schedules messages such that they are batched, then delays outgoing messages until the batch completes. 

Transactional batching supports *snapshot* consistency. Multiple queries on an AVM can be guaranteed to be consistent up to a snapshot. Further, the ordering within the batch greatly simplifies reasoning about composition of behaviors.

#### Interrupts and Priorities

Batches are transactional, atomic. Thus, we can safely model interrupts by aborting a batch, e.g. to prioritize some other batch. We may later retry the interrupted batch. If the interruption happened to be read-only, we might also continue where we left off, which would ensure user-input can be prioritized over background computations.

I imagine that priorities would be modeled as part of each capability, and thus be capability-secure. I'm not sure priorities should be a global feature for all networks, though. I'll need to return to this issue after I better understand how to represent capabilities. Even without priorities, a scheduler in a non-deterministic model may heuristically decide to abort a long-running batch (especially one that isn't making steady progress through the batch) to handle pending input, resist denial-of-service attacks, etc..



### Query Response Messaging

The query-response pattern is very common, both to gather information and to obtain results. 

### Network Disruption


### AVM Life Cycle


### Expirations?


================


This context becomes the local part of a communications capability. 



A good question, then, is how an AVM should go about creating capabilities for itself. 




Developers, of course, will need the ability to create new capabilities for the AVM.

Because capabilities are opaque, we'll generally need a constructor for them.













The internal structure of an AVM's state is user-defined. Consequently, it is important that 

 is user-defined,m 

Importantly, an AVM must be able to define new 'internal' routes and contexts





The local part shall also preserve *substructural type* information associated with the context/routing information. This prevents accidental discard, though developers are free to destroy even 'relevant' objects by sending them off to a "/dev/null" resource.









 will also be cryptographic, using a symmetric encryption that is entirely local to the AVM. 






My current plan is that the global part shall simply be the secure hash of an ABC value resource, where said resource contains some public key information 

 secure hash of an ABC value resource, where the value contains a public cryptographic key and maybe an association list with metadata (such as: date created, ).



 containing a public cryptographic key and some ad-hoc metadata. This text will be available through a distributed hashtable. (Likely, the text itself will be modeled as an ABC resource.)

Basically, the secure hash of a simple text that includes a public key and other data.

  used for communicating with the AVM. Or maybe just the key, directly, if ev


) and a *local* part (for secure routing and context). 

) and a *local* part (secure routing and context). 

(naming a toplevel AVM) and a *local* part (secure routing and context, 






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

### Interrupts and Priorities

Batches are transactional, atomic. Thus, we can safely model interrupts by aborting a batch, e.g. to prioritize some other batch. We may later retry the interrupted batch. If the interruption happened to be read-only, we might also continue where we left off, which would ensure user-input can be prioritized over background computations.

I imagine that priorities would be modeled as part of each capability, and thus be capability-secure. I'm not sure priorities should be a global feature for all networks, though. I'll need to return to this issue after I better understand how to represent capabilities. Even without priorities, a scheduler in a non-deterministic model may heuristically decide to abort a long-running batch (especially one that isn't making steady progress through the batch) to handle pending input, resist denial-of-service attacks, etc..

### Read-Only Queries and Mirroring

Batches of read-only messages can be processed in parallel with one read-write batch. 

A batch is read-only when the AVM finishes in the same state as it started. It would not be difficult to use a few heuristics or statistics to estimate probability that a batch is read-only (e.g. based on history, origin, message count), and later decide whether it is worth attempting to process a batch in parallel. If it turns out the batch was not read-only, we can fall back to sequential mechanisms.

This technique can also be used in a replication scenario. A replica of an AVM may directly process read-only batches. A writer batch, however, must be delivered to the 'master' to provide global serialization. The master may then synchronize replicas by sending some update messages.

This after-the-fact decision for read only messaging is convenient from a UI standpoint, though it probably hurts performance. Fortunately, developers could mitigate the performance hit by arranging such that some AVMs are read-only (or nearly so) while writes are indirectly offloaded to other AVMs, e.g. by having one AVM for the blog content, and another to count the reads per page.

### Explicit Models for Locality, Mobility, Ambient Authority

I like the idea of 'ambient oriented programming' such that a machine might easily find (for example) a local printer. However, I believe that this should be modeled explicitly, using capabilities to provide the AVM a directory or registry of local services. The explicit approach is more flexible, e.g. because we can have different registries based on different attributes (trust, cost, region, etc.) and we can potentially model compositions of registries. The result is that machines only talk to machines. 

So, I'm NOT going to make a special exception from capability security for service discovery.
