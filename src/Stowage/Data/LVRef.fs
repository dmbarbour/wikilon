namespace Stowage
open Data.ByteString

/// Latent Value References
/// 
/// Stowage represents access to high-latency data that may be on
/// disk or remote. On load, we must parse the data. On stow, we 
/// must serialize, manage a secure hash resource, and eventually 
/// pay a little extra for stowage-layer GC.
///
/// The LVRef integrates a simple in-memory cache and write buffer
/// to help mitigate these costs. On load, we keep the value in
/// memory for a while so future loads don't need to do much work.
/// On stow, we delay immediate writes to improve opportunity for
/// GC at the .Net layer and avoid unnecessary writes.
/// 
/// In context of LVRefs, the `IDisposable` interface only clears
/// the cached data. I contemplated creating a generic interface.
///
/// Note: to improve structure sharing between LVRefs, it may be
/// useful to create a MemCache with LVRefs that are shared upon
/// read. 
type LVRef<'V> =
    val internal lvref : Lazy<VRef<'V>>
    val mutable internal cache : 'V option
    val mutable internal tc : int
    member r.VRef with get() = r.lvref.Force()
    member inline r.ID with get() = r.VRef.ID
    override r.ToString() = r.VRef.ToString()
    override r.GetHashCode() = r.VRef.GetHashCode()
    override x.Equals yobj =
        if System.Object.ReferenceEquals(x,yobj) then true else
        match yobj with
        | :? LVRef<'V> as y -> (x.ID = y.ID)
        | _ -> false
    interface System.IComparable with
        member x.CompareTo yobj =
            if System.Object.ReferenceEquals(x,yobj) then 0 else
            match yobj with
            | :? LVRef<'V> as y -> compare (x.ID) (y.ID)
            | _ -> invalidArg "yobj" "cannot compare values of different types"
    interface Cached with
        member r.Usage with get() = r.tc
        member r.Clear() = 
            r.lvref.Force() |> ignore<VRef<'V>>
            r.cache <- None
    internal new (lvref,cache) = { lvref = lvref; cache = cache; tc = 0 }

module LVRef =

    /// Wrap an existing VRef.
    let wrap (vref:VRef<'V>) : LVRef<'V> = 
        let lvref = lazy vref
        lvref.Force() |> ignore<VRef<'V>>
        new LVRef<'V>(lvref, None)
        

    /// Non-buffered, immediate stowage.
    let inline stow' (cV:Codec<'V>) (db:Stowage) (v:'V) : LVRef<'V> = 
        wrap (VRef.stow cV db v)

    /// Non-caching Load. 
    ///
    /// Will use cache opportunistically, but does not cause data
    /// to be buffered if it isn't already available in memory. This
    /// method can be useful if you know you're about to produce an 
    /// updated value and won't be holding onto the Ref.
    let load' (ref:LVRef<'V>) : 'V =
        match ref.cache with
        | None -> VRef.load (ref.VRef)
        | Some v -> v

    /// Stow a value, eventually. 
    ///
    /// Meanwhile, the data is a cached resource. And if the LVRef is
    /// garbage collected before stowage, we can avoid serializing the
    /// data structure entirely. This is a feature: it reduces risk of
    /// writing intermediate states for persistent data structures.
    ///
    /// Any request for the VRef or ID (for serialization, comparison,
    /// or hash) will prematurely force stowage. So try to avoid that.
    let stow (c:Codec<'V>) (db:Stowage) (v:'V) (sz:SizeEst) : LVRef<'V> =
        let ref = new LVRef<'V>(lazy (VRef.stow c db v), Some v)
        Cache.receive (ref :> Cached) sz
        ref

    let private loadAndCache (ref:LVRef<'V>) : 'V =
        let vref = ref.VRef
        lock ref (fun () ->
            match ref.cache with
            | None ->
                let bytes = vref.DB.Load (vref.ID)
                let v = Codec.readBytes (vref.Codec) (vref.DB) bytes
                ref.cache <- Some v
                let szEst = 80UL + uint64 (BS.length bytes)
                Cache.receive (ref :> Cached) (80UL + uint64 (BS.length bytes)) 
                v
            | Some v -> v
        )

    let inline private touch (ref:LVRef<_>) : unit =
        // race conditions are possible for updating touch counter, but
        // are irrelevant due to the heuristic nature of caching.
        ref.tc <- (1 + ref.tc)

    /// Load a value, caching it briefly for future lookups.
    ///
    /// If the value was already cached, or has never been fully stowed,
    /// this will reuse the cached value and extend the lifespan of the
    /// cache. Otherwise it will load, parse, and cache the value.
    let load (ref:LVRef<'V>) : 'V =
        touch ref
        match ref.cache with
        | Some v -> v
        | None -> loadAndCache ref


module EncLVRef =
    let size = EncVRef.size
    let inline write (ref:LVRef<_>) (dst:ByteDst) : unit = 
        EncVRef.write (ref.VRef) dst
    let inline read (cV:Codec<'V>) (db:Stowage) (src:ByteSrc) : LVRef<'V> =
        LVRef.wrap (EncVRef.read cV db src)
    let codec (cV:Codec<'V>) =
        { new Codec<LVRef<'V>> with
            member __.Write ref dst = write ref dst
            member __.Read db src = read cV db src
            member __.Compact _ ref = struct(ref, size)
        }


