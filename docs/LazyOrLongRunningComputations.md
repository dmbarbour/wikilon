
Lazy and Long-Running Iterative Computations
============================================

Laziness has potential advantages for performance. 

In ABC, lazy evaluation could potentially be modeled as:

        value[action]{&lazy}$

That is, we mark a block 'lazy', then apply it to a value. We could treat this structured result as a value for most purposes, delaying evaluation as much as necessary. However, this particular implementation of laziness doesn't work well with duplication, and we probably shouldn't deliver lazy values on a network.

Iterative computation is another interesting possibility. We might model this as follows:

        value[action]{&lazy}$#10000{&step}%

This might tell our lazy computation to try to perform e.g. 10k operations. We then return our updated lazy value that is not (necessarily) fully evaluated yet. We can do this with an annotation. ABC doesn't have semantics for observing lazy values, but an annotation doesn't need to observe the actual value.

That said, this is a bit of a hack. We haven't explicitly modeled progress, so we can't actually know whether we've finished evaluation. Parallel computations might work a little better, using a similar technique of evaluating them so many steps at a time. I might feel a bit more comfortable about implicit laziness and iterative evalautions if I had already performed a termination analysis (or better, a performance analysis to judge how long it should take).

Lazy and long running iterative computations via annotations both seem like hacks to me. I would generally favor models that are explicitly lazy (like streams) or iterative (like machines). These will be a lot more reliable
