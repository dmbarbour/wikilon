Hierarchical transaction machines (HTXMs) are an interesting idea I've been developing recently. HTXMs accomplish my goals of live, resilient, declarative, extensible software systems. Further, HTXMs accomplish these goals in a way that's <em>easy to integrate</em> with existing service and devices, thus avoiding the major weakness of <a href="https://awelonblue.wordpress.com/2018/06/19/rdp-simplified/">RDP</a>.

First, let's ignore that 'hierarchical' adjective. I'll get back to it!

A transaction machine (TXM) is essentially: <strong>a transaction, repeated indefinitely</strong>. We assume our system has a set of TXMs.

TXMs intrinsically support <strong>extension and liveness</strong>: We can model our set of TXMs as a transaction variable. This allows us to modify our system behavior atomically, adding new machines and removing others. If necessary, we can simultaneously transition our state resources.

TXMs intrinsically have <strong>nice resilience properties</strong>: transactions are fail-safe, they can degrade gracefully by fallback behavior or halting individual machines, and the system will recover swiftly after a problem is corrected. Further, they can easily support a user interface with consistent views and transactional intervention, allowing for administration or debugging.

TXMs intrinsically are <strong>idempotent</strong>: having more than one copy of a TXM won't change the system's behavior. This allows us to reason simply about whether or not we have a behavior. A list of TXMs is also <strong>commutative</strong>, but that's not special for concurrency models.

TXMs intrinsically support <strong>reactive, concurrent coordination</strong> behavior: If a deterministic transaction does not modify state, then it will not if repeated under the same conditions. So we can wait for conditions to change. We can leverage this, e.g. using voluntary failure to make TXMs wait for arbitrary conditions.

For example, a TXM implementing a stream processor in each step will drain an input queue, maintain some state, and push to an output queue. If input is empty or output is at capacity, it can abort (voluntarily fail) to force a wait. Haskellers might already be familiar with this technique, via the software transactional memory package and use of TQueue or TChan. 

Beyond stream processing, it's easy for TXMs to model reactive constraint systems (maintaining cells according to some goal), blackboard systems (read blackboard, opportunistically contribute), publish-subscribe. Object-oriented code is not difficult to model within the transaction. To more precisely support larger-scale reactive systems, we could include logical timestamps with streaming values. Multiple models can be easily mixed. 

External systems, such network or display, can effectively be integrated as shared memory, or perhaps writing to a system task queue. The main limitation is that we cannot directly model synchronous wait. To indirectly model synchronous wait, we can add a request to a system queue including a reply-to variable, modify some state so we know to wait on that channel, commit, then wait for a reply in a subsequent transaction.

These are all wonderful properties, and achieve several of my goals. However, an essential feature is missing: support for divide-and-conquer tactics.

Imagine we have a dynamic set of input queues to handle, with over a hundred elements. In this case, I would like the option to spawn one TXM per input queue. Detached spawning would be trivial to implement. Unfortunately, it also escapes user control, hurting liveness and resilience.

The idea of attached spawn leads me to Hierarchical TXMs (HTXMs).

In each step, our machines can fork subordinate behaviors, but these behaviors only remain active until just before the next step. A failed transaction cannot fork. A successful transaction, unless it results in a stable state, will immediately begin the next step. Thus, only successful, stable-state transactions will result in active subordinates. 

To recognize successful, stable-state transactions is not difficult. For a singular transaction, only a read-write behavior on a variable can result in unstable behaviors. For example, if we read a queue, take one element, write modified queue back, then when we repeat our transaction we'll have different observed conditions. If we only read a variable, or blindly write a variable, we're stable. Further, for data types that support efficient equivalence checks, we can stabilize writes that are equivalent to the current value.

Programmers of HTXM systems will want to control stability. Fortunately, there are many patterns they can use to filter noise, cache stable conditions, partition data into relatively stable volumes, etc.. The desire for stability certainly isn't unique to HTXMs.

I haven't implemented HTXMs yet. But I believe this model has much to offer, and I'll continue exploring it for now.
