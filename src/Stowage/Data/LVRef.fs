namespace Stowage
open System.Threading
open System.Threading.Tasks

module LVRef =

    /// Latent Value References
    /// 
    /// This type adds a short-lived cache to VRefs for both loads and
    /// the initial stowage operations. Usefully, we can potentially 
    /// avoid stowage entirely if the VRef/ID is not required and the
    /// .Net GC eliminates the reference. This can prevent intermediate
    /// data from reaching the disk.
    type Ref<'V> =
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
            | :? Ref<'V> as y -> (x.ID = y.ID)
            | _ -> false
        interface System.IComparable with
            member x.CompareTo yobj =
                if System.Object.ReferenceEquals(x,yobj) then 0 else
                match yobj with
                | :? Ref<'V> as y -> compare (x.ID) (y.ID)
                | _ -> invalidArg "yobj" "cannot compare values of different types"
        internal new (lvref,cache) = { lvref = lvref; cache = cache; tc = 0 }

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
    let rec private refDelayOnTouch ms (action:Ref<'V> -> unit) (tc:int) (ref:Ref<'V>) : unit =
        if (tc = ref.tc) then action ref else
        delayWeak ms (refDelayOnTouch ms action (ref.tc)) ref

    /// Forcibly stow and clear cached value.
    let clear (ref:Ref<'V>) : unit =
            ignore (ref.lvref.Force())
            ref.cache <- None 

    let private delayedClear (ref:Ref<'V>) : unit =
        delayWeak 400 (refDelayOnTouch 200 clear (ref.tc)) ref

    /// Wrap a lazy VRef. This won't be computed until necessary.
    let wrap (vref:VRef<'V>) : Ref<'V> = new Ref<'V>(lazy vref,None)

    /// Stow a value later. Meanwhile, the data is cached. When the
    /// cache expires, or if the VRef/RscHash is required for some 
    /// operation (such as serialization), the data will be stowed
    /// to disk.
    let stow (c:Codec<'V>) (db:Stowage) (v:'V) : Ref<'V> =
        let ref = new Ref<'V>(lazy (VRef.stow c db v), Some v)
        delayedClear ref
        ref

    /// Stow a value immediately, no delayed pending stowage cache.
    let inline stow' (c:Codec<'V>) (db:Stowage) (v:'V) : Ref<'V> = 
        wrap (VRef.stow c db v)

    let private loadAndCache (ref:Ref<'V>) : 'V =
        lock ref (fun () ->
            match ref.cache with
            | None ->
                let v = VRef.load (ref.VRef)
                ref.cache <- Some v
                delayedClear ref
                v
            | Some v -> v // someone else cached first
        )

    let inline private touch (ref:Ref<_>) : unit =
        // race conditions are possible for updating touch counter, but
        // are irrelevant due to the already heuristic nature of caching.
        ref.tc <- (1 + ref.tc)

    /// Load a value, caching it briefly for further lookups.
    ///
    /// If the value was already cached, this extends the lifespan
    /// of the cached value by another latency cycle.
    let load (ref:Ref<'V>) : 'V =
        touch ref
        match ref.cache with
        | Some v -> v
        | None -> loadAndCache ref

    /// Load a value without caching it, e.g. for immediate update.
    ///
    /// This will use an existing cached value opportunistically, but
    /// will not store loaded data into the LVRef cache.
    let load' (ref:Ref<'V>) : 'V =
        match ref.cache with
        | Some v -> v
        | None -> VRef.load (ref.VRef)


type LVRef<'V> = LVRef.Ref<'V>

