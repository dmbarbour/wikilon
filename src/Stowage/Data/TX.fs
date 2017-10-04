namespace Stowage
open System.Threading.Tasks

/// Abstract transactional variables.
///
/// A TVar is obtained by allocation or registration in a TX. Reads
/// and writes on the TVar always operate through the TX. A new TVar
/// may only be safely used where writes would be visible.
type TVar<'V> = interface end

/// Abstract transaction.
///
/// TX is an API for manipulating an abstract transaction on a database.
/// In this case, our database is relatively simple: we may construct 
/// a few TVars then read and write those TVars as needed. 
type TX =
    /// Access to associated Stowage Database.
    abstract member Stowage : Stowage

    /// Register a durable variable with codec and key.
    ///
    /// Keys are represented as arbitrary strings, and the values are
    /// always optional with `None` representing deletion of the key. 
    /// Problematic keys may be rewritten to use a secure hash.
    /// 
    /// If a Key is registered more than once, an equivalent Codec must
    /// be used in each case. In return, it is possible to cache this
    /// operation such that we receive the same TVar each time and avoid
    /// redundant parse operations.
    abstract member Register : Codec<'V> -> string -> TVar<'V option>
    
    /// Create a new ephemeral variable.
    ///
    /// Ephemeral variables will not survive process shutdown, but may
    /// be convient for intermediate computations or to unify interfaces.
    abstract member Allocate : 'V -> TVar<'V>

    /// Write a TVar. 
    ///
    /// Until commit, the value written may only be read within this
    /// transaction or a child transaction created afterwards. Writes
    /// are essentially "committed" into the local TX. 
    abstract member Write : TVar<'V> -> 'V -> unit

    /// Read a TVar.
    ///
    /// If the TVar was read or written before within this TX, this
    /// should return an appropriate, stable value. Otherwise, we'll
    /// form a read dependency on the TVar and return a recent value.
    /// Ideally, a TX should exhibit snapshot isolation across reads.
    abstract member Read : TVar<'V> -> 'V

/// An abstract transactional key-value Database!
type DB =
    /// Access to Stowage database features.
    abstract member Stowage : Stowage

    /// API for singular reads and writes. Each operation can be
    /// understood as a trivial one-off transaction. But we can
    /// frequently optimize this a great deal!
    abstract member Singular : TX

    /// Transactional update. The provided transaction will try to
    /// commit upon returning. This does not implicitly retry, so
    /// we'll just return success or failure. To Abort, you will 
    /// need to raise an exception.
    abstract member Transact : (TX -> 'X) -> struct('X * bool)

    /// Synchronize Writes to Durable Backing Store.
    ///
    /// DB writes are not implicitly flushed to disk. Instead, writes
    /// may be buffered for performance. To guarantee durability, you
    /// may explicitly sync. Should return immediately if no writes
    /// are buffered (or if there is no durable backing store).
    abstract member Sync : unit -> unit



