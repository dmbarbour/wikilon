namespace Stowage
open Data.ByteString

/// Abstract Storage for Binaries referenced by RscHash.
///
/// Stowage is often one aspect of a larger key-value database, where
/// values provide a durable root set for binary references. But for 
/// immutable data structures, this is the only relevant interface.
///
/// Stowage serves as a purely functional variant of virtual memory,
/// since we can assume the large binaries are offloaded to a high
/// latency storage layer. However, stowage may be relatively lazy
/// about this, buffering some data in memory.
type Stowage =

    /// The Stow operation should add a value to the Stowage database
    /// and return its RscHash, such that a subsequent Load can access
    /// the data. Additionally, it must atomically Incref the RscHash
    /// to prevent concurrent GC.
    abstract member Stow   : ByteString -> RscHash

    /// The Load operation should access data from Stowage. If this
    /// data cannot be located, a MissingRsc exception must be raised.
    abstract member Load   : RscHash -> ByteString

    /// RscHash references to binaries can be understood as a form of
    /// 'unmanaged' resources from perspective of the .Net runtime. We
    /// use explicit reference counting to ensure a bytestring remains
    /// available for future Load operations.
    ///
    /// Of course, this only accounts for direct in-memory references.
    /// If a ByteString contains a RscHash (see RscHash.foldHashDeps),
    /// then a reference to that bytestring should implicitly hold onto
    /// its transitive referenced dependencies.
    ///
    /// Conveniently, secure hashes make cyclic dependencies infeasible
    /// to represent, so reference counting works well at every layer. 
    ///
    /// Decref is frequently called by .Net finalizers, and so should
    /// be safe even if the Stowage implementation has been finalized.
    abstract member Decref : RscHash -> unit
    abstract member Incref : RscHash -> unit

/// Exception on Load failure.
exception MissingRsc of Stowage * RscHash 


// TODO: Develop a useful set of Stowage combinators. (Low Priority.)
//  layered, cached, mirrored, multi-homed, distributed hashtables...
//  I'll need to think about how keys might be multi-homed, of course.


