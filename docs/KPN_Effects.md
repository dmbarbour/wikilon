# KPNs as an Effects Model

In this article, I motivate a variant of <a href="https://en.wikipedia.org/wiki/Kahn_process_networks">Kahn Process Networks</a> (KPNs) with bounded channels as a better - more efficient, scalable, composable, securable, serializable, comprehensible, and debuggable - approach to modeling effectful application behavior. Especially in context of purely functional languages and conventions today.

## Background

In context of purely functional programming, Haskell pioneered a simple and effective approach to effects by simulation of imperative programming: model a data structure that returns either a final value, or yields with a (request, continuation) pair. By making this data structure opaque, inaccessible to user code, a compiler aware of the structure can optimize and inline requests heavily to achieve acceptable performance. In Haskell, this opaque data structure is called `IO`. (This trivial pattern is also called by obscure and scary names from category theory, but that hasn't done Haskell any favors.)

One fundamental weakness of this design, however, is the singular nature of that `(request, continuation)` pair. Implicitly, this becomes a performance bottleneck: we're performing only one 'request' at a time. Fortunately, we can work around this limitation in a few ways:
<ul>
	<li>Asynchronous requests. Rather than awaiting a result immediately, we may continue with a 'future' reference for the result that can be used in a future request. The request itself may then be processed in parallel with ongoing computation.</li>
	<li>Multi-threading. We can support a request to fork a thread that itself can make more requests. Further, we can support requests for communication between threads, e.g. via channels or shared mutable variable.</li>
</ul>
And this is essentially what Haskell does. We have our `forkIO` request, and a panoply of communication methods (IORefs, MVars, TVars, channels modeled with them, etc..). Asynchronous futures are frequently implicit to Haskell's lazy evaluation model.

Unfortunately, these workarounds come at significant cost. Those futures and variables deeply entangle the application with its environment, which hinders serialization, persistence, and debugging. Multi-threading introduces non-deterministic behavior, which can hinder robust testing or reproduction of error conditions.

A second major weakness of this design regards the <i>opacity</i> of that `(request, continuation)` pair.

Assume two applications with opaque effects - `foo.main()` and `bar.main()`. We could compose these sequentially, or in parallel, or invoke one from within the other. However, we cannot control the interleave of requests. We cannot intercept requests in a mockup environment for testing, security, precise embedding. It is not possible to construct wrappers around an application to tune requests and responses for a different environment. It is not possible to cleanly halt an application. Ultimately, this hurts composition, testing, security, portability, and process control.

Again, there are workarounds - e.g. sandboxes, ad-hoc dependency injection, object capability security. But again, there is a cost - this time to those performance benefits we originally received from compiling with opaque effects.

## Kahn Process Networks

KPNs are a simple model consisting of <i>processes</i> and <i>channels</i>. A process is limited to two actions: read from a channel, or write to a channel. A channel carries a FIFO queue of messages, and may have at most one reader and at most one writer. Reading from a channel will block if there is no message available. Generally, writing to a KPN channel does not block. But I believe a variant of KPNs where each channel is annotated with a maximum capacity is superior for every practical use case.

Construction and composition of KPNs is readily achieved in the style of <a href="https://en.wikipedia.org/wiki/Flow-based_programming">Flow-Based Programming</a> (FBP). Instead of directly naming a channel, a process will read or write a <i>port</i>. Separately, a configuration wires output ports from one process to input ports on another.

<i>Aside:</i> KPNs and FBP are similar. Some important differences: FBP has bounded channels by default, ad-hoc side-effects, and non-deterministic 'merge' for multiple writers into a shared wire.

KPNs can be used as an effects model.

Open (aka dangling) output channels can be filled with requests to be read by the KPN's interpreter. Responses can be written to open input channels. A lightweight, declarative configuration can tell our interpreter from which channels it should read requests, and in each case to which channel it should write responses. This might be informally understood in terms of declaring a subset of channels as 'effectful' when wiring up our KPN processes.

These 'effectful' channels have some nice features:
<ul>
	<li>Multiple effectful channels represent concurrent requests.</li>
	<li>Multiple responses can easily be injected concurrently.</li>
	<li>An individual effects channel naturally sequences its effects.</li>
	<li>Requests and responses are buffered and batched up to channel capacity.</li>
	<li>Internal process model supports parallelism and distribution.</li>
</ul>
This offers a significant improvement over the singular `(request, continuation)` pair implicit to imperative programming.

Instead of a peephole view of our computation, our effects interpreter may handle many concurrent requests and provide many concurrent responses, and our KPN's continuation is inherent to the unblocking of processes when our environment drains full request channels or fills empty input channels. Because buffering, parallelism, and concurrency are pervasive, we don't need workarounds like futures, threads, or shared variables for communication between threads.

By avoiding those workarounds that entangle evaluation with the environment, the resulting system becomes much simpler, easier to serialize and debug.

<i>Note:</i> Alternatively to effects <i>channels</i>, we could augment our <i>process</i> type to support requests on the environment. However, this can trivially be represented by adding two ports to each process (one to write requests, one to receive responses), then wiring an effectful channel between them, then immediately reading a response after each request. Effectful processes would be less expressive for asynchronous behavior, offer less opportunity to intercept and translate requests for a different environment, and cannot buffer more than one pending request per process. <i>Effectful channels are by far the superior design.</i>

## KPNs in context of Purely Functional Programming

In context of purely functional programming, KPNs have more nice properties.

A KPN can easily be described by an immutable value - e.g. a collection of named processes and wires, with a list of messages pending on each wire. Granted, this may prove a fair bit more difficult to represent in <i>statically typed</i> PLs, due to the challenges surrounding heterogeneous collections. I expect clever people can get it done.

Evaluation of a KPN is pure and deterministic. This might be represented as a pure function, effectively `evalKPN : KPN → KPN`. Of course, squeezing the most parallelism and performance from the KPN may require special attention from our runtime or compiler.

Intriguingly, evaluation of a KPN can easily proceed in parallel with further actions upon it. This could be expressed by a variant on the evaluator. Something like: `stepKPN : StepKPN k a → KPN → k (a, KPN)`.

During stepKPN, our KPN is running in the background and evaluating, e.g. using conventional threads and queues. The final returned KPN is fully evaluated. But the `StepKPN` type would represent a program that may read and write channels, potentially modify the KPN (add or delete processes and wires, adjust wire capacity, etc..). Importantly, every action on the KPN is deterministic. For example, a read will return immediately if possible (if the channel has data), but if the channel is empty we must wait until input becomes available or until all upstream processes block. Besides actions on the KPN, StepKPN might support operations in some surrounding context `k`.

Effects can be supported either way.

With `evalKPN` we might use a simple tactic to repeatedly:
<ul>
	<li>extract pending effect requests</li>
	<li>inject all available responses</li>
	<li>initialize evalKPN in parallel</li>
	<li>process the extracted requests</li>
</ul>
This would give us frame-oriented effects, with big batches of effects being processed efficiently between frames. This is easy to reason about, and offers adequate parallelism on a single system. Unfortunately, it doesn't scale very well to distributed systems. However, we could tweak this to model a partitioned KPN system, each partition running its own frames.

With `stepKPN` we can very precisely extract requests, perform effects, and inject responses in a fine-grained manner. This could work very well because we can tune our strategy for reading and writing of open channels based both on the problem and on meta-knowledge of the KPN's structure.

A third option is viable: associate effects handlers, e.g. of type `request → IO response`, with each effectful channel. Then run the KPN in context of IO. This could maximize parallelism and scalability, albeit at a cost to deterministic ordering of concurrent effects and reproducible system behavior.

That last option would most generically enable KPNs to replace simple imperative IO as the effects model, since it would effectively have the full power of multi-threading but with advantages of inherent batching and reduced complexity.