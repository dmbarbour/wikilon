namespace Stowage
open System.Threading
open System.Threading.Tasks

/// Abstract cached resource. 
///
/// A cached resource provides both an interface to clear the cache
/// and a 'usage' heuristic - usually a touch count or time stamp -
/// to delay release of a recently used resource.
type Cached =
    abstract member Usage : int
    abstract member Clear : unit -> unit

/// Stowage Cache
///
/// Use of Stowage resources can operate a lot like virtual memory,
/// a purely functional variant where stowed elements are loaded and
/// parsed as needed. A Stowage cache exists to manage the resources,
/// simulating the virtual memory paging heuristics.
///
/// The current implementation uses a concrete heuristic strategy that 
/// combines aspects of least-recently-used with exponential-decay. It 
/// should be effective for most use cases. Resources are cleared from
/// the .Net task thread pool.
///
/// This module only provides a manager, not lookup. For cached lookups,
/// consider MCache or DCache.  
module Cache =

    type private Rsc = (struct(int * int * System.WeakReference))
    type private Frame = ResizeArray<Rsc>

    // Our simple algorithm for releasing memory.
    let private scrubFrame (rng:System.Random) (pdecay:int) (f:Frame) : struct(Frame * uint64) =
        let mutable erased = 0UL
        let newFrame = new Frame()
        for (struct(tc,sz,wref)) in f do
            match wref.Target with
            | null -> 
                erased <- (erased + uint64 sz)
            | :? Cached as c ->
                let tc' = c.Usage
                let doScrub = (tc = tc') && (rng.Next(100) < pdecay)
                if doScrub 
                    then c.Clear(); erased <- (erased + uint64 sz)
                    else newFrame.Add(struct(tc',sz,wref))
            | _ -> failwith "invalid state"
        struct(newFrame,erased)

    /// Concrete cache manager.
    ///
    /// This object manages a set of Cached resources, clearing some
    /// whenever a quota overflows (this is a soft quota). The items
    /// cleared are selected based on strategies of least-recently
    /// used and exponential decay. This should be good for most use
    /// cases, but more sophisticated strategies can be implemented
    /// by having Cached items decide what to do upon Clear.
    ///
    /// Cached items are only referenced weakly, such that GC can
    /// remove items independently from the cache manager clearing
    /// them. Due to GC, there is no guarantee Clear is called.
    type Manager =
        val mutable private szMax : uint64
        val mutable private szCur : uint64
        val mutable private ixHd  : int
        val private frames : Frame[]
        val private rngSrc : System.Random
        val private pdecay : int
        val mutable private bgtask : bool
        new(framect,pdecay,quota) =
            { szMax  = quota
              szCur  = 0UL
              ixHd   = 0
              frames = Array.init (max framect 2) (fun _ -> new Frame()) 
              rngSrc = new System.Random(0)
              pdecay = (max pdecay 1)
              bgtask = false
            }
        new(quota) = new Manager(12,60,quota)

        /// Adjust the managed quota.
        member m.Resize (quota:uint64) : unit =
            lock m (fun () ->
                m.szMax <- quota
                m.ConsiderBGScrub())

        /// Add object for management. When added, a size estimate must
        /// also be provided to count against the quota. 
        member m.Receive (c:Cached) (sz:SizeEst) : unit =
            assert(sz >= 0)
            lock m (fun () ->
                let f = m.frames.[m.ixHd]
                let tc = c.Usage + System.Int32.MinValue // logical touch
                f.Add(struct(tc,sz,System.WeakReference(c)))
                m.szCur <- (m.szCur + uint64 sz)
                m.ConsiderBGScrub())

        member private m.ConsiderBGScrub() : unit =
            if (m.bgtask || (m.szMax >= m.szCur)) then () else
            assert(Monitor.IsEntered(m))
            m.bgtask <- true
            Task.Run(fun () -> m.BGScrub()) |> ignore<Task>

        member inline private m.NextFrameIx() =
            ((m.ixHd + 1) % (m.frames.Length))

        member private m.BGScrub() : unit =
            assert(not (Monitor.IsEntered(m)))
            lock (m.frames) (fun () ->
                assert(m.bgtask)
                lock m (fun () -> m.ixHd <- m.NextFrameIx())
                let ixScrub = m.NextFrameIx()
                let f = m.frames.[ixScrub]
                let struct(f',erased) = scrubFrame (m.rngSrc) (m.pdecay) (f)
                m.frames.[ixScrub] <- f'
                lock m (fun () ->
                    assert(m.szCur >= erased)
                    m.szCur <- (m.szCur - erased)
                    m.bgtask <- false
                    m.ConsiderBGScrub()))

    /// Although there are some use-cases for multiple cache managers,
    /// it's usually best to just use a global cache manager to match
    /// our global heap and OS-provided virtual memory system.
    let defaultManager = new Manager(80_000_000UL)

    /// Configure the global Stowage cache size. Default is eighty
    /// megabytes. Sizes aren't exact, but are used to estimate when
    /// an overflow occurs to drive background expiration of data.
    ///
    /// Cache size should be larger than the normal working set or
    /// we'll suffer cache thrashing. It shouldn't be larger than
    /// RAM because Stowage serves as a virtual memory system.
    let inline resize sz = defaultManager.Resize sz

    /// Manage a cached resource.
    ///
    /// Cached objects are held by weak reference, expire only
    /// upon cache overflow, and cleared by background thread.
    /// The `Clear` method is only called if the object has not
    /// been garbage collected, hence is not guaranteed.
    ///
    /// Size estimates should include rough overheads. They don't
    /// need to be exact, only sufficient that we're not overshooting
    /// our quotas by too much. Better to err towards high estimates.
    let inline receive c sz = defaultManager.Receive c sz

