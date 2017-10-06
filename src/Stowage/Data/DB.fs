namespace Stowage

/// Abstract transactional key-value Stowage Database.
type DB =

    /// Access to Stowage resources. 
    abstract member Stowage : Stowage

    /// API for singular reads and writes. Each operation is a trivial
    /// one-off transaction. Singular reads and writes cannot conflict
    /// with any transaction, and so must always succeed. Thread-safe.
    abstract member Singular : TX

    /// Transactional updates. All reads and writes on the provided
    /// TX will be committed atomically upon returning. Commit may
    /// fail, so we return an extra pass/fail boolean. Early abort
    /// must be modeled by raising an exception. Assume the TX is
    /// not thread-safe and that Transact is not safely reentrant.
    abstract member Transact : (TX -> 'X) -> struct('X * bool)

    /// Flush buffered writes to durable storage.
    ///
    /// Writes to a DB may be buffered in memory until explicit flush.
    /// To model durable transactions, simply flush after a successful
    /// transaction but before reporting success to client. Periodic
    /// flush, such as every several seconds, may also prove useful. 
    /// 
    /// Delaying serialization can improve performance, especially when
    /// we have many writes to a TVar within a short period of time. So
    /// try to avoid flushing after every minor write. 
    abstract member Flush : unit -> unit

    /// Graceful Shutdown (if feasible).
    inherit System.IDisposable

