
# Wikilon Performance

I don't expect Wikilon to be blazing fast, not anytime soon. However, I expect to achieve a reasonable degree of performance. Developing simple text games, turtle graphics, or visual novels should be within easy reach. To achieve this, Wikilon will compile ABC to a more efficient internal representation that can trivially be converted back to ABC.

* Compact: use bytestrings in memory rather than expanded lists.
* Slicing: no parsing to end of block or text, no escaping text.
* Embedding: compute constant values only once within a loop.
* Stowage: structure sharing and lazy loading of large values.
* Accelerators: common sequences of ABC recognized and optimized.
* Simplifiers: clean up unnecessary data plumbing, partial evaluations.
* Parallelism: `{&par}` and `{&seq}` annotations, strategies.

Additionally, I have several ideas to improve performance further:

* lazy or parallel computations that survive within the cache
* accelerated data structures, especially vectors 
* annotation to copy affine vector into 'mutable' vector
* compile loopy subprograms to machine code for fast evaluation
* compile subprograms written in constrained manner to GPGPU

However, some of these require a lot more work to implement or use effectively.

## Stowage for Large Values

        {&stow} :: (val * env) → (val(stowed) * env)

Stowage is useful for modeling larger-than-memory values in cases where only a small portion of the value needs our attention at a given moment. Databases, filesystems, streaming media, and more fit this criteria. The `{&stow}` annotation suggests to an ABC runtime that it serialize the value to cold storage. The value would be transparently loaded if later needed. Transitive substructural properties (affine, relevant) of stowed values will be tracked so the primitive copy and drop operators (`^` and `%`) need not invoke the loader. 

Structure sharing is a common feature for stowage. Computations producing the same stowed values will share the same space. In addition to potential space savings, structure sharing simplifies other potential features such as managing cached computations. 

Stowage of values is designed to closely model ABC value resources, i.e. where we use `{#resourceId'kf}` to name a bytecode resource. Here, `resourceId` is a secure hash of the bytecode, and the `'kf` suffix identifies our resource as a linear quoted value to permit lazy loading and forbid copy/drop. Stowed values could be directly implemented as ABC resources. However, `{&stow}` leaves the representation opaque and local to the runtime, which simplifies GC and provides more room for optimizations.

A suggestion for stowage may be silently rejected, e.g. because a value is not significantly larger than its stowage overhead. This helps developers a bit because it leads to larger 'chunks' of data corresponding to a couple layers of tree nodes that can be loaded together or compressed together.

When serialized for export, stowed values will re-inject the `{&stow}` annotations.

## Parallelism 

Awelon Bytecode has a lot of latent parallelism. So the problem becomes teasing it out at an appropriate granularity to keep the overheads low and the processors busy. We can utilize a simple variant of [parallel Haskell's](https://hackage.haskell.org/package/parallel-3.2.0.6/docs/Control-Parallel.html) `par` and `seq` techniques.

        {&par} :: (block * env) → (block with 'par' attribute * env)
        USAGE: (prep args)[(expensive)]{&par}$(move pending result)

        {&seq} :: (lazy or pending value * env) → (value * env)

Our runtime would support opaque 'pending' results. Trying to operate on the pending result may have us waiting. But we're at least free to move it around, e.g. tuck it into a data structure and spark off a few more parallel computations. With just this much ABC and AO gain access to all of Haskell's well developed [strategies](https://hackage.haskell.org/package/parallel-3.2.0.6/docs/Control-Parallel-Strategies.html) for purely functional parallelism. It's also trivial to implement!

It is also possible to support massively parallel GPU computations. Assume a subprogram constructed from a constrained set of recognizable functions and data structures that we know how to easily implement on the GPU. Annotate this for evaluation on the GPU. The runtime applies its internal compilers to CUDA or OpenCL or a shader language. When we later apply the function it will run on the GPU. Other forms of heterogeneous computing, e.g. FPGAs, will tend to follow a similar pattern. Haskell has used a similar approach with [accelerate](https://hackage.haskell.org/package/accelerate).

In context of a dictionary application, every form of parallelism could kick in for every evaluation. This includes partial evaluations and cache maintenance. I imagine a mature, active, and popular AO dictionary full of apps could productively leverage as many CPUs, GPUs, and other computing devices as you're willing to throw at them.

## Laziness with Persistence

ABC can easily support laziness and parallelism. We could consider `{&lazy}` a variant of `{&par}` that doesn't immediately spark off a thread. 

Laziness doesn't seem to buy much in ABC unless we can statically predict our 'relevant' attributes. You're still forced to perform the computation to drop the result. OTOH, if you're going to hold onto it, we *can* serialize our lazy values without computing them. Just use `arg[function]{&lazy}$`, same as when constructed. Same goes for continuations after a partial evaluation, which we might try to see if the lazy value is easily reduced.

In context of dictionary applications, lazy values could be used in multiple futures (words are not linear, and definitions may freely be forked). It would be nice if a lazy value constructed during partial evaluation is computed once in the larger dictionary-application context, as opposed to once per evaluation on the dictionary. This is feasible with VCache if lazy 'holes' are reified into PVars to share computations. It may be interesting if I can also provide some structure sharing for these shared computations, i.e. such that we may recover the same PVar. This probably would help with preserving cached structure.

Usefully, this may also support long-running "background" computations at the Wikilon level. Large computations that are important have some shared efforts gradually computing them, adding a little quota here and there to partially evaluate, returning `202 ACCEPTED` responses. Indeed, this may be the default for parallel computations, tracking how much effort has been expended.

The difficulty with lazy and parallel 'holes' is naming them. Perhaps I should ensure these have shared structure, too, just like 


