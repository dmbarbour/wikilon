namespace Stowage
open System.Threading
open Data.ByteString

/// Abstract cached resource. 
///
/// A cached resource provides both an interface to clear the cache
/// and a 'usage' heuristic - usually a touch count or time stamp -
/// to delay release of a recently used resource.
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
            new() = 
                { elems = Array.empty 
                  count = 0 
                }

        let cfResize (f:CacheFrame) (sz:int) : unit =
            assert(sz >= f.count)
            if (sz = f.elems.Length) then () else
            let new_array = Array.zeroCreate sz
            Array.blit (f.elems) 0 (new_array) 0 (f.count)
            f.elems <- new_array

        let cfGrow (f:CacheFrame) : unit =
            let len' = max 16 (f.elems.Length <<< 1)
            cfResize f len'

        let cfTrimExcess (f:CacheFrame) : unit =
            // reduce size only if we have a lot of excess
            let threshold = ((f.elems.Length >>> 3) > (f.count))
            if not threshold then () else
            let len' = max 16 (f.elems.Length >>> 1)
            cfResize f len'

        let cfAdd (f:CacheFrame) (o:Obj) : unit = 
            if(f.count = f.elems.Length) 
                then cfGrow f
            f.elems.[f.count] <- o
            f.count <- (1 + f.count)

        // removal assumes order within frame is irrelevant
        let cfRemoveAt (f:CacheFrame) (ix:int) : unit =
            assert(ix < f.count)
            f.count <- (f.count - 1)
            f.elems.[ix] <- f.elems.[f.count]

        // scrubFrame will erase a percentage `p` of elements that
        // haven't been touched (according to Usage) since a prior
        // scrubbing. 
        let scrubFrame (rnd:System.Random) (p:int) (cf:CacheFrame) : uint64 =
            assert(p > 0)
            let rec loop amt ix = 
                if (ix = cf.count) then amt else
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
                        cf.elems.[ix] <- Obj(tc,o.sz,o.ref)
                        loop amt (ix + 1)
                | _ -> failwith "invalid state: non-Cached object in cache"
            let result = loop 0UL 0
            cfTrimExcess cf
            result

        // If we have 12 frames and scrub 60% per frame, then we'll
        // erase about 5% of total cache per frame after warmup.
        let frameCount = 12
        let percentScrub = 60

        // Our maximum cache size determines how much (approximately)
        // we'll keep in memory at any given moment. This is runtime
        // configurable, but we'll default to about eighty megabytes.
        let cacheSizeDefault = uint64 (80 * 1000 * 1000)

        // This cache uses a flexible, heuristic combination of two
        // policies: least recently used and random replacement. We
        // scrub random, untouched elements from the oldest frames.
        //
        // Random replacement can mitigate aliasing concerns when the
        // cache is a little too small for some use case. It means the
        // old cache has a chance for survival at the cost of newer
        // values.
        type Cache = 
            val mutable szMax   : uint64  // configured maximum size
            val mutable szCur   : uint64  // computed size of data
            val mutable ixHd    : int          // index of 'head' frame
            val frames          : CacheFrame[] // rotating array of frames
            val rndSrc          : System.Random
            new() =
                assert(frameCount > 1)
                { szMax  = cacheSizeDefault
                  szCur  = 0UL
                  ixHd   = 0
                  frames = Array.init frameCount (fun i -> new CacheFrame()) 
                  rndSrc = new System.Random(0)
                }

        let cacheOverflow (c:Cache) : bool = (c.szCur > c.szMax)

        let cacheAdd (c:Cache) (o:Obj) : unit =
            lock c (fun () ->
                cfAdd (c.frames.[c.ixHd]) o
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
            // scrub next write-frame in background
            let cfScrub = c.frames.[cacheNextHd c]
            let amtScrubbed = scrubFrame (c.rndSrc) percentScrub cfScrub
            lock c (fun () ->
                assert(c.szCur >= amtScrubbed)
                c.szCur <- (c.szCur - amtScrubbed))
            cacheManagerLoop c

        let setCacheSize (c:Cache) (sz:uint64) : unit =
            lock c (fun () ->
                c.szMax <- sz
                if(cacheOverflow c)
                    then Monitor.PulseAll(c))

        let init () : Cache = 
            let c = new Cache()
            (new Thread(fun () -> cacheManagerLoop c)).Start()
            c

    let private sharedCache = lazy (C.init ())

    /// Configure the LVRef shared cache size in approximate bytes.
    /// The default cache size is about eighty megabytes, although
    /// this isn't precise: the cache treats tiny resources as 200
    /// bytes, and resource size estimates aren't exact.
    ///
    /// When cache overflows, a background thread will Clear items,
    /// targeting older objects that have not been recently used. A
    /// little randomness is added to mitigate aliasing issues. But
    /// the background works until there is no more overflow. 
    ///
    /// The cache size should be large compared to the normal working
    /// set. Otherwise, the cache thrashes and performance degrades.
    /// It also shouldn't be set larger than RAM. Cached LVRefs serve
    /// as a purely functional variant of virtual memory.
    let setCacheSize (sz:uint64) : unit =
        C.setCacheSize (sharedCache.Value) sz

    /// Manage a cached resource, overflow driven (cf setCacheSize).
    ///     
    /// The cache leverages weak references to avoid interfering with
    /// .Net GC of cached items. We don't guarantee the `Clear` method
    /// will ever be called.
    ///
    /// The `touch` option logically marks a resource as recently used.
    /// This causes the data to live one step longer than unmarked items
    /// created around the same time. But it should be used sparingly.
    ///
    /// This is the same method used to cache LVRefs upon stow and load.
    /// It is exposed since it might prove useful in many other cases.
    let cache (o:Cached) (szEst:SizeEst) (touch:bool) =
        let tc = o.Usage + (if touch then System.Int32.MaxValue else 0)
        let sz = 120u + uint32 (max 80 szEst)
        let wref = System.WeakReference(o :> System.Object)
        C.cacheAdd (sharedCache.Value) (C.Obj(tc,sz,wref)) 

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
        cache (ref :> Cached) sz true
        ref

    let private loadAndCache (ref:LVRef<'V>) : 'V =
        let vref = ref.VRef
        lock ref (fun () ->
            match ref.cache with
            | None ->
                let bytes = vref.DB.Load (vref.ID)
                let v = Codec.readBytes (vref.Codec) (vref.DB) bytes
                ref.cache <- Some v
                cache (ref :> Cached) (bytes.Length) false
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


