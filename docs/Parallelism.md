# Parallel Runtime

Parallelism will use separate memory per CPU, with no direct sharing of memory. This simplifies GC. 

Originally, I was thinking to use Haskell-style par-seq parallelism. For example, we could construct a lazy value then parallelize its evaluation with `{&par}` or `{&need}`. This form of parallelism is convenient, in that parallel values can be treated as plain old values in many contexts. However, the technique isn't all that scalable.

An interesting alternative that I've developed is oriented around the idea of *process functions*, whereby a function may be forked and treated as a separate process.

        PF i o = [i → (o * PF i o)]
        {&fork} :: ((PF i o) * e) → ((PF i (future o)) * e)
        {&join} :: ((future a) * e) → (a * e)

Process functions are more sophisticated than par-seq because of the `(o * PF i o)` division. However, they are simultaneously more expressive and scalable. Par/seq can be implemented by a one-off fork/join action. But we can easily construct large processes just once then amortize their construction/state costs over a series of calls. Unlike with par/seq, we can afford to use remote processes for parallelism.

Conveniently, we can understand our forked PF as having an implicit *queue* for messages that have yet to be handled. We can also freely use techniques like pushback to help control memory, prevent fast producers from running too far ahead of slow consumers. (The purely functional structure ensures that pushback introduces no risk of deadlock.)

So, that's the *idea* anyway. How will we implement this?

## Desiderata

* Minimize synchronization latency.
* Minimize copying of messages.
* Minimize interference with GC.
* Support buffering/batching.
* Load balancing of work. 
* Configurable parallelism.
* N:M lightweight process model.
* Copy and Drop for parallel futures.
* Upgrade lazy futures to parallel.
* Integrate easily with stowage.

## Load Balancing vs. Work Stealing

Load balancing is an interesting problem. Ideally, we want to simultaneously keep CPUs busy *and* minimize communication between them. Simple techniques like work stealing can 'keep CPUs busy'. But minimizing communication may require a larger analysis of dataflows relationships.

## Communications between Contexts

I would like zero-copy communication between contexts to be the default, but allowing for value copying when appropriate. Something like:

* messages are formed in our sender context
 * sender *does not* touch messages after send.
* sender alerts receiver of available messages
 * e.g. increment a number in a comms matrix
* receiver waits for good time to receive or copy
 * just after GC, before computing skip space
  * good for batch processing, minimize copies
* receiver processes available message sources
 * directly read messages in sender context.
 * need a reader-writer lock against GC!
 * messages marked delete/forward when done.

This could work well, I think. I lock primarily against the GC process. Thus, most of the time the messages are available for reading without synchronizations. Forwarding of messages could require special attention, but for now I could model that explicitly by copying and forwarding the messages, and it shouldn't be critical to optimize. 

Anyhow, this design avoids any stop-the-world locks. And it doesn't even lock individual threads for very long. OTOH, it leaves plenty of challenges surrounding expression of messages to send.

## Message Set

Contexts should be able to:

* request work.
* send work.
* track forwarding of processes.
* track forwarding of futures(?). 
* 

