namespace Stowage
open System.Threading
open System.Threading.Tasks

/// Buffered Value References
/// 
/// Stowage represents access to high-latency data that may be on
/// disk or remote, and must be loaded and parsed. Reasonably, we
/// can assume a high cost for reads and writes. 
///
/// Buffering can mitigate these costs. On read, the buffer serves
/// as a cache and allows use of data many times from one load and
/// parse. On write, the buffer provides an extra opportunity for
/// GC of resources so we don't too eagerly write intermediate data
/// to disk or network.
///
/// The buffering in this case uses weak references and is driven
/// by size information, so we don't need to worry about how swiftly
/// data is written or interference with .Net GC.
type BVRef<'V> =
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
        | :? BVRef<'V> as y -> (x.ID = y.ID)
        | _ -> false
    interface System.IComparable with
        member x.CompareTo yobj =
            if System.Object.ReferenceEquals(x,yobj) then 0 else
            match yobj with
            | :? BVRef<'V> as y -> compare (x.ID) (y.ID)
            | _ -> invalidArg "yobj" "cannot compare values of different types"
    internal new (lvref,cache) = { lvref = lvref; cache = cache; tc = 0 }

module BVRef =

    /// Wrap an existing VRef.
    let wrap (vref:VRef<'V>) : BVRef<'V> = 
        new BVRef<'V>(lazy vref, None)

    /// Non-buffered, immediate stowage.
    let inline stow' (cV:Codec<'V>) (db:Stowage) (v:'V) : BVRef<'V> = 
        wrap (VRef.stow cV db v)

    /// Non-caching Load. 
    ///
    /// Will use cache opportunistically, but does not cause data
    /// to be buffered if it isn't already available in memory.
    let load' (ref:BVRef<'V>) : 'V =
        match ref.cache with
        | None -> VRef.load (ref.VRef)
        | Some v -> v

    /// Forcibly stow and clear cached value.
    ///
    /// This is the same operation used by the buffer's management
    /// functions.
    let clear (ref:BVRef<'V>) : unit =
            ignore (ref.lvref.Force())
            ref.cache <- None 

    // TODO: switch to size-driven buffer!

    // latency provided through .Net Task subsystem.
    let private delay (ms:int) (op:unit -> unit) : unit =
        Task.Delay(ms).ContinueWith(fun _ -> op ()) |> ignore<Task>

    let private viaWeakRef (action:'V -> unit) (wref:System.WeakReference) () : unit =
        match wref.Target with
        | null -> ()
        | :? 'V as obj -> action obj
        | _ -> failwith "incorrect weak-ref object type"

    let inline private delayWeak (ms:int) (action:'V -> unit) (obj:'V) : unit =
        let wref = System.WeakReference(obj :> System.Object)
        let action = viaWeakRef action wref
        delay ms action

    // delay operation on reference, delay another step each time it is
    // touched concurrently (i.e. by caching load). 
    let rec private refDelayOnTouch ms (action:BVRef<'V> -> unit) (tc:int) (ref:BVRef<'V>) : unit =
        if (tc = ref.tc) then action ref else
        delayWeak ms (refDelayOnTouch ms action (ref.tc)) ref

    let private delayedClear (ref:BVRef<'V>) : unit =
        delayWeak 400 (refDelayOnTouch 200 clear (ref.tc)) ref


    /// Stow a value later. Meanwhile, the data is cached. When the
    /// cache expires, or if the VRef/RscHash is required for some 
    /// operation (such as serialization), the data will be stowed
    /// to disk.
    let stow (c:Codec<'V>) (db:Stowage) (v:'V) (sz:SizeEst) : BVRef<'V> =
        let ref = new BVRef<'V>(lazy (VRef.stow c db v), Some v)
        delayedClear ref
        ref


    let private loadAndCache (ref:BVRef<'V>) : 'V =
        lock ref (fun () ->
            match ref.cache with
            | None ->
                let v = VRef.load (ref.VRef)
                ref.cache <- Some v
                delayedClear ref
                v
            | Some v -> v // someone else cached first
        )

    let inline private touch (ref:BVRef<_>) : unit =
        // race conditions are possible for updating touch counter, but
        // are irrelevant due to the already heuristic nature of caching.
        ref.tc <- (1 + ref.tc)

    /// Load a value, caching it briefly for further lookups.
    ///
    /// If the value was already cached, this extends the lifespan
    /// of the cached value by another latency cycle.
    let load (ref:BVRef<'V>) : 'V =
        touch ref
        match ref.cache with
        | Some v -> v
        | None -> loadAndCache ref



