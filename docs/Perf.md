# Performance Monitoring

To avoid blind optimization, I'll try to develop a useful set of benchmarks with local metrics.

Benchmarks:
* **bench.repeat10M** a trivial `0 [4 +] 10000000 repeat` benchmark.

* benchmark ideas
 * implement μKanren relational language.
 * text processing - regex, parsers, backtracking, etc..
 * Pov-Ray style scene computing programs
 * vector and matrix processing, machine learning
 * big data processing, stowage tests
 * look into computer benchmarks game

Interpreters:
* **old aoi** - From the `awelon` project.
 * Runs a simplifier by default. 
 * Times include linking.
* **alpha runABC Haskell** 
 * No simplifier or accelerators.
 * Doesn't validate copyable/droppable. 
 * Time excludes linking.
* **alpha runABC Wikilon**
 * No simplifier or accelerators. 
 * No shared objects. 
 * Time excludes linking.

## Repeat 10M

* old `aoi` interpreter: 213 seconds
* alpha `runABC` Haskell: 413 seconds
* alpha `runABC` Wikilon: 19.2 seconds

This is a promising start. A 10x improvement over my old `aoi` interpreter even before optimization work or JIT. 

However, objectively this is not *good* performance. 20 seconds for 10M loops is ~2 microseconds per loop. I hope to achieve *at least* another 10x improvement here in the medium term, via accelerators. Long term, a ~100x improvement is desirable via JIT.

 Cheap fixpoints and loops will almost certainly be a basis for many object/stream models.

A theoretical limit is closer to a 1000x improvement.



