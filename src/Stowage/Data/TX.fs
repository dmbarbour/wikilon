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
    /// Registering a key more than once is possible, but it must be
    /// registered with an equivalent codec every time such that the
    /// result may be memoized and cached. For frequently used keys,
    /// it's better to register once then use many times.
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
    /// should return the given value. Otherwise, we'll form a read
    /// dependency on the TVar and return a recent value. TX should
    /// be snapshot consistent, such that read-only transactions 
    /// always succeed.
    abstract member Read : TVar<'V> -> 'V



