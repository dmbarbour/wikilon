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
        | Stowing of Codec<'V> * DB * 'V
 
    /// Latent Value References
    /// 
    /// A little latency improves VRefs. If we delay serialization for
    /// stowage, we can avoid writing short-lived intermediate structure
    /// to disk. If we cache loads, we simplify logic for lookup-modify
    /// operations. It doesn't take much. A few tens of milliseconds is
    /// sufficient for most use cases.
    ///
    /// Note: use of the VRef or ID properties will force stowage of the
    /// Ref. Comparisons and serializations tend to use the ID. So you
    /// should favor VRefs if you're going to be comparing IDs anyway.
    type Ref<'V> =
        val mutable internal S : State<'V>
        member r.Codec 
            with get() : Codec<'V> = 
                match r.S with
                | Stowed (vref) -> vref.Codec
                | Cached (vref,_) -> vref.Codec
                | Stowing (c,_,_) -> c
        member r.DB 
            with get() : DB =
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
                | Stowing _ -> r.Force()

        member private r.Force() : VRef<'V> =
            lock r (fun () -> 
                match r.S with
                | Stowing (c,db,v) ->
                    let vref = VRef.stow c db v
                    r.S <- Stowed vref
                    vref
                | Stowed vref -> vref
                | Cached (vref,_) -> vref)

        override r.ToString() = r.ID.ToString()
        override r.GetHashCode() = r.ID.GetHashCode()
        override x.Equals yobj =
            match yobj with
            | :? Ref<'V> as y -> (x.ID = y.ID)
            | _ -> false
        interface System.IComparable with
            member x.CompareTo yobj =
                match yobj with
                | :? Ref<'V> as y -> compare (x.ID) (y.ID)
                | _ -> invalidArg "yobj" "cannot compare values of different types"



        internal new (s:State<'V>) = { S = s }
        new(ref:VRef<'V>) = { S = Stowed ref }

    // Latency agent via singleton module.
    module private Latency =

        // how long to sleep after receiving each batch.
        let sleepMillis = 40

        type Action = unit -> unit
        type Frame = ResizeArray<Action>
 
        // the agent operates a simple loop but will run 
        type Agent =
            val mutable private F : Frame

            member agent.Add (a:Action) : unit =
                lock agent (fun () ->
                    agent.F.Add(a)
                    Monitor.PulseAll(agent))
                
            member private agent.GetTasks() : Frame =
                lock agent (fun () ->
                    if (0 = agent.F.Count)
                        then Monitor.Wait(agent) |> ignore
                    let oldF = agent.F
                    agent.F <- new Frame()
                    oldF)

            // run tasks via Task thread-pool; don't wait
            static member private RunTasks (tasks:Frame) : unit =
                for t in tasks do 
                    Task.Run(t) |> ignore

            member private agent.Loop () : unit =
                let tasks = agent.GetTasks()
                Thread.Sleep(sleepMillis)
                Agent.RunTasks tasks
                agent.Loop()

            member private agent.Begin() : unit =
                printfn "Latency agent initialized."
                (new Thread(agent.Loop)).Start()
                
            new() as agent = 
                { F = new Frame() } 
                then agent.Begin()

        let agent = new Agent()

    let private onWeakRef (action : Ref<'V> -> unit) (wref:System.WeakReference) () : unit = 
        match wref.Target with
        | null -> ()
        | :? Ref<'V> as ref -> action ref
        | _ -> failwith "unexpected weak ref type"

    let private viaWeakRef (action : Ref<'V> -> unit) (ref : Ref<'V>) : unit -> unit =
        let wref = System.WeakReference(ref :> System.Object)
        onWeakRef action wref

    /// Force immediate stowage (only if still stowing).
    let force (ref:Ref<'V>) : unit = 
        ref.ID |> ignore

    /// Clear cached value (if any). 
    let clear (ref:Ref<'V>) : unit =
        match ref.S with 
        | Cached (vref,_) ->
            ref.S <- Stowed vref
        | _ -> ()

    /// Wrap an existing VRef.
    let wrap (vref:VRef<'V>) : Ref<'V> = new Ref<'V>(vref)

    /// Stow a value (with latency) to create a LVRef.
    let stow (c:Codec<'V>) (db:DB) (v:'V) : Ref<'V> =
        let ref = new Ref<'V>(Stowing (c,db,v))
        Latency.agent.Add(viaWeakRef force ref)
        ref

    let private cacheLoad (ref:Ref<'V>) : 'V =
        lock ref (fun () ->
            match ref.S with
            | Stowed vref ->
                let v = VRef.load vref
                ref.S <- Cached (vref,v)
                Latency.agent.Add(viaWeakRef clear ref)
                v 
            | Cached (_,v) -> v // cached concurrently
            | Stowing (_,_,v) -> v
            )

    /// Load a value, caching it briefly to support further operations.
    let load (ref:Ref<'V>) : 'V =
        match ref.S with
        | Stowed _ -> cacheLoad ref
        | Cached (_,v) -> v
        | Stowing (_,_,v) -> v
    
         

type LVRef<'V> = LVRef.Ref<'V>

