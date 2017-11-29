# KPNs as an Effects Model

I believe [Kahn Process Networks (KPNs)](https://en.wikipedia.org/wiki/Kahn_process_networks) or variants would offer a better foundation for purely functional effects models and applications compared to monads. KPNs offer a convenient basis for pipeline style concurrency, working with multiple concurrent input sources, and distributed computation.

Desiderata:

* Confluent. KPNs have a deterministic result no matter the order of scheduling. 
* Concurrent. Order of inputs or outputs on separate channels is not relevant.
* Monotonic. No rollbacks or rework. Every output produced is final.
* Composable. We can compose entire networks externally.
* First Class. KPNs are immutable values that can be passed around.
* Reactive. We can merge and interleave asynchronous input sources. 
* Dynamic. Network structure may change incrementally based on input.

The first several features are implicit for KPNs. 

KPNs as values is implicit to how I plan to represent them, using accelerated evaluation of a DSL-like network description. Explcit copy in Awelon helps: I can use linear update between copies. By passing KPNs around, they can serve as a sort of functional 'object' with asynchronous update and result.

But reactive and dynamic behavior requires some attention. 

## Reactive

Reactivity is not a built-in feature of KPNs, but can be supported indirectly by adding a simple time model to our KPNs. The simplest option, perhaps, is to rewrite every message type from `a` to `Either Wait a` where a `Wait` indicates passage of time. A little extra logic upon read could handle the waits and forward to output channels. 

Hence, the fundamental KPN structure doesn't need to change.

## Dynamic

Dynamic structure is a big challenge for KPNs, and is a potential weakness compared to monadic programming. How should we handle this?

My best idea so far: 

* a process network has a set of labeled open input and output ports
* a stateful process may 'become' a process network with the same ports
* special support for composable processes (like an `fmap` process).

With this much, a single process could rewrite itself into multiple smaller processes. Some smaller processes, such as a pair a sequence of two `fmap` processes, could collapse into a single process. Others may be garbage collected when awaiting 'dead' channels (assuming we'll wait forever for an input). 

Failed ideas: first class channels are problematic due to the issue of naming channels, unique names when composing networks, need for linear types, etc.. Explicit delegation is too inefficient for pipelining. 

## Effects Models

Modeling effects means providing outputs that result in some feedback. We could do this by convention with a small set of channels labeled based on the types of effects or expected independent sources of input. We can model message buses or routing within a KPN as needed to manage arbitrary effects.

IO involving independent channels can feasibly be decentralized by sharding a process network, assuming an appropriate context.


