
Wikilon will host a set of abstract virtual machines, which I'm calling AVMs.

Each AVM has a user-defined [state](StateModels.md) and a step function typically bound to a [dictionary](BranchingDictionary.md) for live programming. AVMs and their state are *purely functional*. While AVMs lack internal concurrency, they should support reasonable levels for internal parallelism. Hierarchical AVMs should be entirely feasible, i.e. modeling a larger AVM in terms of simulating smaller ones with a purely functional network. However, the main idea with AVMs is that they should support scalable, concurrent, distributed implementations, and maybe even compile down to Docker apps or Unikernels!

Modeling effects also falls to this network layer. That is, AVMs can talk to other AVMs, and some of those will provide adapters to outside resources (Redis, S3, AMQP, web services, printers, clocks, etc.). In this sense, AVMs are perhaps similar to actors model. But AVMs will be a lot more constrained than actors. Significantly, AVMs generally cannot directly create new AVMs, AVMs don't know their own network identity (no default 'self', allows open recursion), and communications between AVMs will have enough structure to simplify reasoning about whole-system behavior.

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

I'm [not fond](https://awelonblue.wordpress.com/2012/07/01/why-not-events/) of message passing models. However, they are easy to implement and integrate with existing systems. Long term, I'd like to move towards RDP-based communication and computation. I'd like to arrange for network communications and conventions to be 80% of the way there.

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

I want to easily reason about behavior in the presence of concurrency. However, I also want to avoid distributed transactions, time warp protocols, and other mechanisms that require coordination of multiple machines. A nice middle ground is *local* transactions: 

* the basic unit of communication is a *batch* of messages
* messages in one batch are processed together, atomically
* no output messages emitted unless entire batch succeeds
* output messages become new batches per logical recipient
* batch preserves order of messages of messages emitted

The batch of incoming messages is not exposed to the AVM. The AVM receives one message at a time. But the network layer transparently schedules messages such that they are batched, then delays outgoing messages until the batch completes. 

Transactional batching supports *snapshot* consistency. Multiple queries on an AVM can be guaranteed to be consistent up to a snapshot. Further, the ordering within the batch greatly simplifies reasoning about composition of behaviors.

### Connections

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
