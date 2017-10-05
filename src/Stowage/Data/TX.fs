namespace Stowage

/// Abstract transactional variables.
///
/// A TVar is obtained by allocation or registration in a TX. Reads
/// and writes on the TVar always operate through the TX. A new TVar
/// may only be safely used where writes to it would be visible.
type TVar<'V> = interface end

/// Abstract transaction.
type TX =
    /// Access to associated Stowage Database.
    ///
    /// The values at registered TVars essentially serve as a durable
    /// root set for binary resources in Stowage. But most data should
    /// still be held within the Stowage layer. Ideally, values at a
    /// TVar should be small or bounded size (such as a CVRef), and a
    /// transaction shouldn't need to read or write very many keys.  
    abstract member Stowage : Stowage

    /// Register a durable variable with codec and key.
    ///
    /// Keys are represented as arbitrary strings, and values use the
    /// option type with `None` representing deletion or absence of a
    /// key. The string itself may be mangled and escaped in various
    /// ways for the backend, so long as uniqueness is preserved.
    /// 
    /// If a Key is registered more than once, equivalent Codecs must
    /// be used every time. In general, the DB may cache the TVar for
    /// a given key. However, for frequently used TVars, it's wise to 
    /// register up front then hold the TVar for many transactions. 
    abstract member Register : Codec<'V> -> string -> TVar<'V option>
    
    /// Create a new ephemeral variable.
    ///
    /// Ephemeral variables do not survive process crash or shutdown.
    /// But they may be transparently used the same way as durable 
    /// key-value TVars. This is useful for intermediate computations
    /// or to unify various interfaces.
    abstract member Allocate : 'V -> TVar<'V>

    /// Write a TVar.
    abstract member Write : TVar<'V> -> 'V -> unit

    /// Read a TVar.
    ///
    /// If the TVar was read or written before within this TX, this
    /// should return the appropriate, stable value. Otherwise, we'll
    /// form a read dependency on the TVar and return a recent value.
    /// Ideally, a transaction should exhibit snapshot isolation for
    /// multiple reads, but that isn't strictly required.
    abstract member Read : TVar<'V> -> 'V

/// Abstract transactional key-value Stowage Database.
type DB =
    /// Access to Stowage resources. 
    abstract member Stowage : Stowage

    /// API for singular reads and writes. Each operation is a trivial
    /// one-off transaction. Singular reads and writes do not conflict
    /// with any transaction, and so must always succeed.
    abstract member Singular : TX

    /// Transactional updates. All reads and writes on the provided
    /// TX will be committed atomically upon returning. Commit may
    /// fail, so we return an extra pass/fail boolean. Early abort
    /// can be modeled by raising an exception. Transactions are not
    /// implicitly durable (see Flush). 
    abstract member Transact : (TX -> 'X) -> struct('X * bool)

    /// Flush buffered writes to durable storage.
    ///
    /// Writes to a DB may be buffered in memory until explicit flush.
    /// For durable transactions, flush after a successful transaction
    /// and before reporting success to a client. Consider creating a
    /// periodic event to flush the DB every several seconds.
    /// 
    /// Delaying serialization can improve performance, especially when
    /// we have many writes to a TVar within a short period of time. So
    /// try to avoid Flush where it isn't required by the problem.
    abstract member Flush : unit -> unit


