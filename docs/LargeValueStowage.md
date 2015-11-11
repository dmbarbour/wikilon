
# Stowage for Large Values

        {&stow} :: (val * env) → (val(stowed) * env)

Stowage is useful for modeling larger-than-memory values in cases where only a small portion of the value needs our attention at a given moment. Databases, filesystems, streaming media, and more fit this criteria. 

The `{&stow}` annotation suggests to an ABC runtime that it serialize the value to cold storage. The value may be transparently loaded if later needed. Transitive substructural properties (affine, relevant) of stowed values will be tracked so the primitive copy and drop operators (`^` and `%`) need not invoke the loader. 

Structure sharing is a common feature for stowage. Computations producing the same stowed values will share the same space. In addition to potential space savings, structure sharing simplifies other potential features such as managing cached computations. 

Stowage of values is designed to closely model ABC value resources, i.e. where we use `{#resourceId'kf}` to name a bytecode resource. Here, `resourceId` is a secure hash of the bytecode, and the `'kf` suffix identifies our resource as a linear quoted value to permit lazy loading and forbid copy/drop. Stowed values could be directly implemented as ABC resources. However, `{&stow}` leaves the representation opaque and local to the runtime, which simplifies GC and provides more room for optimizations.

A suggestion for stowage may be silently rejected, e.g. because a value is not significantly larger than its stowage overhead. 

