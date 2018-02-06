namespace Stowage
open System.Threading

/// Abstract cached resource. 
///
/// A cached resource provides both an interface to clear the cache
/// and a 'usage' heuristic - usually a touch count or time stamp -
/// to delay release of a recently used resource.
type Cached =
    abstract member Usage : int
    abstract member Clear : unit -> unit

/// Concrete global cache manager.
///
/// Since stowed data is 'remote' it's convenient to preserve some
/// in local memory to avoid redundant load and parse overheads. So
/// Stowage.Data provides a cache. It is possible to set the global
/// cache size or inject `Cached` objects for management.
///
/// This cache uses a general expiration model - a heuristic combo
/// of least-recently-used and exponential-decay. This should be
/// good for most use cases.
module Cache =

    // shared cache implementation
    module private C =

        [<Struct>]
        type Obj =
            val tc  : int       // touch count upon cache
            val sz  : uint32    // cache size
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
            f.count <- (f.count + 1)

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
        // erase about 5% of total cache per frame plus however much
        // was garbage collected in the meantime.
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
        //
        // GC'd elements are identified by WeakReference, and also are
        // erased from the cache.
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

    /// Configure the global Stowage cache size. Default is eighty
    /// megabytes. Sizes aren't exact, but are used to estimate when
    /// an overflow occurs to drive background expiration of data.
    ///
    /// Cache size should be larger than the normal working set or
    /// we'll suffer cache thrashing. It shouldn't be larger than
    /// RAM because Stowage serves as a virtual memory system.
    let resize (sz:uint64) : unit =
        C.setCacheSize (sharedCache.Value) sz

    /// Manage a cached resource.
    ///
    /// Cached objects are held by weak reference, expire only
    /// upon cache overflow, and cleared by background thread.
    /// The `Clear` method is only called if the object has not
    /// been garbage collected, hence is not guaranteed.
    ///
    /// The `touch` parameter will help the newly cached object
    /// survive its first elimination round, modifying recorded
    /// Usage. More sophisticated strategies should leverage 
    /// the Clear() callback.
    let receive (o:Cached) (szEst:SizeEst) (touch:bool) =
        let tc = o.Usage + (if touch then System.Int32.MaxValue else 0)
        let sz = 120u + uint32 (max 80 szEst)
        let wref = System.WeakReference(o :> System.Object)
        C.cacheAdd (sharedCache.Value) (C.Obj(tc,sz,wref)) 


