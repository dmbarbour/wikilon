

A simple approach to keeping a LONG history without keeping a LOT of history is to gradually collapse information about intermediate states. This can offer benefits similar to a ring buffer, except keeping more information about older states by probabilistically favoring loss of intermediate near-term states.

There are multiple approaches to this, both deterministic and probabilistic. But the deterministic decay is event driven, while the probabilistic delay is much easier to align with time.

Rather than decaying with events, a very nice option is to decay with *time*, for example:

* keep transactions for perhaps 84 hours; half life of twice that!
* for every decay event, compute K options and select least lossy
* decay events that target before branch root are considered lossless
* events that target a branch-node are ignored (loss computed normally)
* track decay windows and number nodes collapsed since last checkpoint
* new decay event for every commit on a branch

This approach should work very well, gradually and smoothly losing information about older transactions, while keeping enough history to be useful for mining.

Session information may be decayed on a more input-driven basis.
