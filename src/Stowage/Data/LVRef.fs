namespace Stowage
open Data.ByteString

/// Value References with cache-driven 'latency' upon stow and load.
/// 
/// When first stowing data, we treat the cache as a buffer, delaying
/// serialization until space is required. When loading data, we will
/// cache data in the reference and hold it in memory until space is
/// required again. 
///
/// Note: LVRef does not attempt sharing of cached resources. That is,
/// there is no equivalent to an ephemeron table.
type LVRef<'V> = VRef<'V>

module LVRef =

    // I assume some overheads when parsing a value, but it isn't
    // clear the size of a value in .Net memory. For now, just using
    // an assumption of 50% compression and some base overheads.
    let inline private memSize (sz:SizeEst) : SizeEst =
        200UL + (sz <<< 1)

    type private R<'V> =
        inherit VRef<'V>
        val mutable lvref : Lazy<VRef<'V>>
        val mutable cache : (struct('V * SizeEst)) option
        val mutable tc : int
        member inline r.Touch() = r.tc <- (r.tc + 1)
        member private r.Load() = lock r (fun () ->
            match r.cache with
            | Some vsz -> r.Touch(); vsz // benign race to load data
            | None ->
                let vsz = r.lvref.Value.Deref()
                r.cache <- Some vsz
                let struct(_,sz) = vsz
                Cache.receive (r :> Cached) (memSize sz)
                vsz
            ) // end lock
        override r.Addr with get() = r.lvref.Value.Addr
        override r.Deref() = 
            match r.cache with
            | Some v -> r.Touch(); v
            | None -> r.Load()
        interface Cached with
            member r.Usage with get() = r.tc
            member r.Clear() = 
                r.lvref.Force() |> ignore<VRef<'V>> // move data to Stowage
                lock r (fun () -> r.cache <- None) // clear data from cache
        new(lvref,cache) = 
            { inherit VRef<'V>()
              lvref = lvref
              cache = cache
              tc = 0
            }

    /// Wrap an existing VRef, adding a cache layer.
    let wrap (vref:VRef<'V>) : LVRef<'V> = 
        let lvref = lazy vref
        lvref.Force() |> ignore<VRef<'V>>
        (new R<'V>(lvref,None)) :> LVRef<'V>

    /// Non-buffered, immediate stowage, with caching on load.
    let inline stow' (cV:Codec<'V>) (db:Stowage) (v:'V) : LVRef<'V> = 
        wrap (VRef.stow cV db v)

    /// Stow a value lazily, when there is memory pressure in cache or
    /// when the RscHash address is first required. Until stowage, the
    /// value will dereference using the given value and size estimate.
    let stow (c:Codec<'V>) (db:Stowage) (v:'V) (sz:SizeEst) : LVRef<'V> =
        let ref = new R<'V>(lazy (VRef.stow c db v), Some (struct(v,sz)))
        Cache.receive (ref :> Cached) (memSize sz)
        ref :> LVRef<'V>

    /// Non-caching Load. 
    ///
    /// When applied to a wrapped LVRef, this will use the cached
    /// data if available, or otherwise loads without caching. If
    /// applied to other VRef types, simply loads normally.
    let load' (ref:LVRef<'V>) : 'V =
        match ref with
        | :? R<'V> as r ->
            match r.cache with
            | Some (struct(v,_)) -> v // use cached value
            | None -> VRef.load (r.lvref.Value) // load without caching
        | _ -> VRef.load ref // not an LVRef, just load directly

    /// Load a value, caching it briefly for future lookups. (Default Deref()).
    ///
    /// If the value was already cached, or has never been fully stowed,
    /// this will reuse the cached value and extend the lifespan of the
    /// cache. Otherwise it will load, parse, and cache the value.
    let inline load (ref:LVRef<'V>) : 'V = VRef.load ref

/// Compared to EncVRef, EncLVRef will perform LVRef.wrap upon read.
module EncLVRef =
    let size = EncVRef.size
    let inline write (ref:LVRef<_>) (dst:ByteDst) : unit = 
        EncVRef.write ref dst
    let inline read (cV:Codec<'V>) (db:Stowage) (src:ByteSrc) : LVRef<'V> =
        LVRef.wrap (EncVRef.read cV db src)
    let codec (cV:Codec<'V>) =
        { new Codec<LVRef<'V>> with
            member __.Write ref dst = write ref dst
            member __.Read db src = read cV db src
            member __.Compact _ ref = struct(ref, size)
        }


