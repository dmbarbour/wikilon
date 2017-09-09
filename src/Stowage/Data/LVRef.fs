namespace Stowage

module LVRef =

    /// An LVRef has three primary states:
    ///
    /// * Stowing: the LVRef holds a Value but no VRef.
    /// * Stowed:  the LVRef holds a VRef but no Value.
    /// * Cached:  the LVRef holds both Value and VRef.
    ///
    /// Additionally, creation and destruction of the LVRef is important
    /// to the life-cycle. The important state transitions are:
    ///
    /// * Create as Stowed via `wrap` operation
    /// * Create as Stowing via `stow` operation
    /// * Stowed to Cached via `load` operation
    /// * Cached to Stowed after some latency
    /// * Stowing to Stowed after some latency
    /// * Stowing to Stowed forced by request
    /// * Stowing to Destroyed via .Net GC
    ///
    /// Stowage may be forced early due to potential requests for the 
    /// secure hash ID (e.g. for serialization) or the captured VRef.
    ///
    /// If we GC the LVRef while Stowing, nothing will be written to the
    /// Stowage DB. This would be observed by the .Net WeakReference type.
    type State<'V> =
        | Stowing of Codec<'V> * DB * 'V
        | Stowed of VRef<'V>
        | Cached of VRef<'V> * 'V

    let inline private stCodec (s:State<'V>) : Codec<'V> =
        match s with
        | Stowing (c,_,_) -> c
        | Stowed r -> r.Codec
        | Cached (r,_) -> r.Codec

    let inline private stDB (s:State<_>) : DB =
        match s with
        | Stowing (_,db,_) -> db
        | Stowed r -> r.DB
        | Cached (r,_) -> r.DB

    /// Latent Value References
    /// 
    /// A little latency improves VRefs. If we delay serialization for
    /// stowage, we can avoid writing short-lived intermediate structure
    /// to disk. If we cache loads, we simplify logic for lookup-modify
    /// operations. It doesn't take much. A few tens of milliseconds is
    /// sufficient for most use cases.
    type Ref<'V> =
        val mutable internal S : State<'V>
        // potential cache management fields
        internal new (s:State<'V>) = { S = s }
        new(ref:VRef<'V>) = { S = Stowed ref }
        member r.Codec with get() : Codec<'V> = stCodec (r.S)
        member r.DB with get() : DB = stDB (r.S)

    // TODO: 
    // I will want some simple cache-management options for LVRefs,
    // such that we can intelligently configure latencies or even set
    // a condition for manual-only cache management.

type LVRef<'V> = LVRef.Ref<'V>

