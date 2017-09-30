namespace Stowage
open System.Threading
open System.Threading.Tasks

module LVRef =

    // An LVRef has three primary states:
    //
    // * Stowing: the LVRef holds a Value but no VRef.
    // * Stowed:  the LVRef holds a VRef but no Value.
    // * Cached:  the LVRef holds both Value and VRef.
    //
    // Additionally, creation and destruction of the LVRef is important
    // to the life-cycle. The important state transitions are:
    //
    // * Create as Stowed via `wrap` operation
    // * Create as Stowing via `stow` operation
    // * Stowed to Cached via `load` operation
    // * Cached to Stowed after some latency
    // * Stowing to Stowed after some latency
    // * Stowing to Stowed forced by request
    // * Stowing to Destroyed via .Net GC
    //
    // Stowage may be forced early due to potential requests for the 
    // secure hash ID or VRef (e.g. for serialization or comparison).
    //
    // If we GC the LVRef while Stowing, nothing will be written to the
    // Stowage DB. This would be observed by the .Net WeakReference type.
    type State<'V> =
        | Stowed of VRef<'V>
        | Cached of VRef<'V> * 'V
        | Stowing of Codec<'V> * Stowage * 'V
 
    /// Latent Value References
    /// 
    /// A little latency improves VRefs. If we delay serialization for
    /// stowage, we can avoid writing short-lived intermediate structure
    /// to disk. If we cache loads, we simplify logic for operations on
    /// collection data structures. It doesn't very much latency for 
    /// significant benefits.
    ///
    /// Note: use of the VRef or ID properties, including serialization,
    /// will force initial stowage of the Ref but won't hinder further
    /// caching. Comparison or hash will also force the ID unless it is
    /// obviously equal (by reference equality).
    type Ref<'V> =
        val mutable internal S : State<'V>
        val mutable internal T : int       // simple touch counter
        member r.Codec 
            with get() : Codec<'V> = 
                match r.S with
                | Stowed (vref) -> vref.Codec
                | Cached (vref,_) -> vref.Codec
                | Stowing (c,_,_) -> c
        member r.DB 
            with get() : Stowage =
                match r.S with
                | Stowed (vref) -> vref.DB
                | Cached (vref,_) -> vref.DB
                | Stowing (_,db,_) -> db
        member r.ID with get() : RscHash = r.VRef.ID
        member r.VRef 
            with get() : VRef<'V> =
                match r.S with
                | Stowed vref -> vref
                | Cached (vref,_) -> vref
                | Stowing _ -> r.Stow()

        member internal r.Stow() : VRef<'V> =
            lock r (fun () -> 
                match r.S with
                | Cached (vref,_) ->
                    r.S <- Stowed vref
                    vref
                | Stowing (c,db,v) ->
                    let vref = VRef.stow c db v
                    r.S <- Stowed vref
                    vref
                | Stowed vref -> vref
            )

        override r.ToString() = r.VRef.ToString()
        override r.GetHashCode() = r.VRef.GetHashCode()
        override x.Equals yobj =
            // reference equality is mostly to help delay stowage
            // when performing deep structural diffs, where chance
            // of a self-comparison is reasonably good.
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

        internal new (s:State<'V>) = { S = s; T = 0 }

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
        if (tc = ref.T) then action ref else
        delayWeak ms (refDelayOnTouch ms action (ref.T)) ref

    /// Forcibly stow and clear cached value.
    let clear (ref:Ref<'V>) : unit = 
        ref.Stow() |> ignore<VRef<'V>>

    let private delayedClear (ref:Ref<'V>) : unit =
        delayWeak 400 (refDelayOnTouch 200 clear (ref.T)) ref

    /// Wrap an existing VRef.
    let wrap (vref:VRef<'V>) : Ref<'V> =
        new Ref<'V>(Stowed vref)

    /// Stow a value asynchronously, first moving the data into a
    /// pending stowage cache. Loads can access this cache. And if
    /// the reference is soon garbage collected, we might save the
    /// write to Stowage. 
    let stow (c:Codec<'V>) (db:Stowage) (v:'V) : Ref<'V> =
        let ref = new Ref<'V>(Stowing (c,db,v))
        delayedClear ref
        ref

    let private loadAndCache (ref:Ref<'V>) : 'V =
        lock ref (fun () ->
            match ref.S with
            | Stowed vref ->
                let v = VRef.load vref
                ref.S <- Cached (vref,v)
                delayedClear ref
                v 
            | Cached (_,v) -> v // cached concurrently
            | Stowing (_,_,v) -> v
            )

    let inline private touch (ref:Ref<_>) : unit =
        // race conditions are possible for updating touch counter, but
        // are irrelevant due to the already heuristic nature of caching.
        ref.T <- (1 + ref.T)

    /// Load a value, caching it briefly for further lookups.
    ///
    /// If the value was already cached, this extends the lifespan
    /// of the cached value by another latency cycle.
    let load (ref:Ref<'V>) : 'V =
        touch ref
        match ref.S with
        | Stowed _ -> loadAndCache ref
        | Cached (_,v) -> v
        | Stowing (_,_,v) -> v

    /// Load a value without caching it, e.g. for immediate update.
    ///
    /// This will use an existing cached value opportunistically, but
    /// will not store loaded data into the LVRef cache.
    let load' (ref:Ref<'V>) : 'V =
        match ref.S with
        | Stowed vref -> VRef.load vref
        | Cached (_,v) -> v
        | Stowing (_,_,v) -> v

    /// Stow a value immediately, no delayed pending stowage cache.
    let inline stow' (c:Codec<'V>) (db:Stowage) (v:'V) : Ref<'V> = 
        wrap (VRef.stow c db v)


type LVRef<'V> = LVRef.Ref<'V>

