namespace Stowage
open System.Threading

/// Abstract cached resource. 
///
/// A cached resource provides both an interface to clear the cache
/// and a 'usage' heuristic, usually a simple touch count, to delay
/// release of a recently used resource.
type Cached =
    abstract member Usage : int
    abstract member Clear : unit -> unit

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
/// the cached data. I contemplated creating a generic interface
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
        new LVRef<'V>(lazy vref, None)

    /// Non-buffered, immediate stowage.
    let inline stow' (cV:Codec<'V>) (db:Stowage) (v:'V) : LVRef<'V> = 
        wrap (VRef.stow cV db v)

    /// Non-caching Load. 
    ///
    /// Will use cache opportunistically, but does not cause data
    /// to be buffered if it isn't already available in memory.
    let load' (ref:LVRef<'V>) : 'V =
        match ref.cache with
        | None -> VRef.load (ref.VRef)
        | Some v -> v

    module private C =

        [<Struct>]
        type Obj =
            val tc  : int 
            val sz  : uint32
            val ref : System.WeakReference // WeakRef of `Cached`
            new(tc,sz,ref) = { tc = tc; sz = sz; ref = ref }

        // essentially, a resizable array with some utilities.
        // The main difference here is how we model removal.  
        type CacheFrame =
            val mutable elems : Obj[]   // resizable
            val mutable count : int
            new() = { elems = Array.empty; count = 0 }

        let cfGrow (f:CacheFrame) : unit =
            let new_size = max 16 (2 * (f.elems.Length))
            let new_array = Array.zeroCreate new_size
            Array.blit (f.elems) 0 (new_array) 0 (f.count)
            f.elems <- new_array

        let cfTrim (f:CacheFrame) : unit =
            let tgt_len = min 16 (f.elems.Length >>> 2)
            if (f.count >= tgt_len) then () else
            let new_array = Array.zeroCreate tgt_len
            Array.blit (f.elems) 0 (new_array) 0 (f.count)
            f.elems <- new_array

        let cfAdd (f:CacheFrame) (o:Obj) : unit = 
            if(f.tail = f.elems.Length) 
                then cfGrow f
            f.elems.[f.count] <- o
            f.count <- (1 + f.count)

        // removal assumes order within frame is irrelevant
        let cfRemoveAt (f:CacheFrame) (ix:int) : unit =
            assert(ix < f.count)
            f.count <- (f.count - 1)
            f.elems.[ix] = f.elems.[f.count]

        // scrubFrame will erase a percentage `p` of elements that
        // haven't been touched (according to Usage) since the prior
        // scrub. This provides a simple basis for exponential decay.
        // Also, anything GC'd according to WeakRef is always scrubbed. 
        // The return value is the number of bytes scrubbed.
        let scrubFrame (rnd:System.Random) (p:int) (cf:CacheFrame) : uint64 =
            let rec loop amt ix = 
                if (ix >= cf.count) then amt else
                let o = cf.elems.[ix]
                match o.ref.Target with
                | null ->
                    cfRemoveAt cf ix 
                    loop (amt + uint64 o.sz) ix
                | :? Cached as c ->
                    let tc = c.Usage
                    let scrub = (tc = o.tc) && (rnd.Next(100) < p)
                    if scrub then
                        cfRemoveAt cf ix
                        c.Clear()
                        loop (amt + uint64 o.sz) ix
                    else // keep, but track new touch count
                        cf.elems.[ix] = Obj(tc,o.sz,o.ref)
                        loop amt (ix + 1)
                | _ -> failwith "scrub non-Cached object"
            let result = loop 0UL 0
            cfTrim cf
            result

        let defaultCacheMax : uint64 = 
            // eighty megabytes is a reasonable default
            uint64 (80 * 1000 * 1000)

        // If we have 12 frames and scrub 60% per frame, then we'll
        // erase about 5% of cached per frame. At least, after the
        // cache is fully 'warmed up'. 
        //
        // During warmup, we can effectively treat the Cache as having
        // one frame, then two, then three, etc. up to the maximum.
        let frameCount = 12
        let percentScrub = 60

        // This cache uses a flexible, heuristic combination of two
        // policies: least recently used and random replacement. We
        // scrub random, untouched elements from the oldest frames.
        type Cache = 
            val mutable szMax   : uint64  // configured maximum size
            val mutable szCur   : uint64  // computed size of data
            val mutable ixHd    : int          // index of 'head' frame
            val frames          : CacheFrame[] // rotating array of frames
            val rnd             : System.Random
            new() =
                { szMax = defaultCacheMax
                  szTotal = 0un
                  frames = Array.init frameCount (fun i -> new ResizeArray<Obj>()) 
                  rnd = new System.Random(0)
                }

        let cacheOverflow (c:Cache) : bool = (c.szCur > c.szMax)

        let cacheAdd (c:Cache) (o:Obj) : unit =
            lock c (fun () ->
                cfAdd (c.frames.[ixHd]) o
                c.szCur <- (c.szCur + uint64 o.sz)
                if (cacheOverflow c) 
                    then Monitor.PulseAll(c))

        let inline cacheNextHd (c:Cache) : int =
            (1 + c.ixHd) % (c.frames.Length)

        let rec cacheManagerLoop (c:Cache) : unit = 
            lock c (fun () ->
                if not (cacheOverflow c) 
                    then Monitor.Wait(c) |> ignore<bool>
                c.ixHd <- cacheNextHd c)
            // concurrently scrub the next frame we'll write.
            let cfScrub = c.frames.[cacheNextHd c]
            let amtScrubbed = scrubFrame (c.rnd) percentScrub cfScrub
            lock c (fun () ->
                assert(c.szCur >= amtScrubbed)
                c.szCur <- (c.szCur - amtScrubbed))
            cacheManagerLoop c

        let setCacheSize (c:Cache) (sz:uint64) : unit =
            lock c (fun () ->
                c.szMax <- sz
                if(cacheOverlow c)
                    then Monitor.PulseAll(c))

        let sharedCache : Cache =
            let c = new Cache(12) 
            (new Thread(fun () -> cacheManagerLoop c)).Start()
            c

    /// Configure the LVRef shared cache size. 
    let setCacheSize (sz:uint64) : unit =
        C.setCacheSize (C.sharedCache) sz

    /// Add an object to the shared cache, with option for to live for
    /// one extra cycle. Some overhead is added to the size estimate.
    /// We use weak refs, so the object may be GC'd by the .Net runtime. 
    let cacheAdd<'V when 'V :> Cached> (v:'V) (szEst:SizeEst) (extraLife:bool) =
        assert(szEst >= 0)
        let tc = (v :> Cached).Usage + (if extraLife then (-1) else 0)
        let sz = 200u + uint32 szEst // assuming non-trivial overhead 
        let wref = System.WeakReference(o :> System.Object)
        C.cacheAdd (C.sharedCache) (C.Obj(tc,sz,wref)) 

    /// Stow a value eventually. Meanwhile, data is cached in memory,
    /// and concurrent GC of the LVRef at the .Net layer could prevent
    /// the data from ever being stowed.
    let stow (c:Codec<'V>) (db:Stowage) (v:'V) (sz:SizeEst) : LVRef<'V> =
        let ref = new LVRef<'V>(lazy (VRef.stow c db v), Some v)
        cacheAdd ref sz true
        ref

    let private loadAndCache (ref:LVRef<'V>) : 'V =
        let vref = ref.VRef
        lock ref (fun () ->
            match ref.cache with
            | None ->
                let bytes = vref.DB.Load (vref.ID)
                let value = Codec.readBytes (vref.Codec) (vref.DB) bytes
                ref.cache <- Some value
                cacheAdd ref (bytes.Length) false
                value
            | Some v -> v
        )

    let inline private touch (ref:LVRef<_>) : unit =
        // race conditions are possible for updating touch counter, but
        // are irrelevant due to the already heuristic nature of caching.
        ref.tc <- (1 + ref.tc)

    /// Load a value, caching it briefly for future lookups.
    ///
    /// If the value is already cached, this will extend the lifespan
    /// of the cache. Otherwise, it will load the value into memory.
    let load (ref:LVRef<'V>) : 'V =
        touch ref
        match ref.cache with
        | Some v -> v
        | None -> loadAndCache ref



