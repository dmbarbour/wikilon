namespace Stowage
open Data.ByteString
open System.Threading.Tasks

/// Abstract transactional variables.
///
/// Variables are obtained from a DB, and may only be read or written
/// in context of the originating DB and its transactional hierarchy.
type TVar<'V> = interface end

/// Abstract, transactional key-value Stowage Database.
///
/// Where Stowage offers access to remote binary resources, a key-value
/// layer simply provides a simple basis for durable roots. This permits
/// modeling of large, durable data structures with a lot of sharing and
/// lightweight logarithmic updates!
type DB =
    inherit Stowage

    /// Register a durable TVar. 
    ///
    /// The key at this layer is an arbitrary string, but it may be 
    /// escaped or mangled at the backend. If a key is registered 
    /// more than once, the same type and codec must be used every
    /// time. The DB may cache registrations and return the same 
    /// TVar each time.
    ///
    /// Registrations may be expensive (even when cached) so if the
    /// variable will be used frequently, prefer to cache explicitly.
    ///
    /// For durable TVars, values are always optional. The value None
    /// represents deletion or non-existence of the key, and is simply
    /// treated as a valid variable state.
    abstract member Register : ByteString -> Codec<'V> -> TVar<'V option>

    /// Allocate an ephemeral TVar.
    ///
    /// Ephemeral TVars are always destroyed upon process shutdown, but
    /// are still transactional and may be mixed transparently with 
    /// durable variables.
    abstract member Allocate : 'V -> TVar<'V> 

    /// Read a TVar.
    ///
    /// Reads the DB's value in context of this DB. Reads within a
    /// transaction should be snapshot consistent, such that read
    /// only transactions never fail.
    abstract member Read : TVar<'V> -> 'V

    /// Write a TVar.
    ///
    /// This will update the TVar in context of this DB and any
    /// hierarchical transactions performed after this write.
    abstract member Write : TVar<'V> -> 'V -> unit

    /// Flush writes to durable storage layer.
    ///
    /// For performance reasons, it's preferable to avoid serializing
    /// most intermediate data. Thus, writes may be buffered in memory
    /// until explicitly flushed. Flushing within a transaction will
    /// cause the DB to flush after a successful commit but before we
    /// return from Transact, so it's a convenient way to mark the 
    /// whole transaction 'durable'.
    abstract member Flush : unit -> unit

    /// Perform Hierarchical Transaction.
    ///
    /// This runs a transaction using a snapshot view of the DB. Upon
    /// normal return, the transactional update is committed. Commit
    /// may fail if a concurrent transaction has written a TVar that
    /// the transaction read at a different value. The extra boolean
    /// result reports whether a transaction commited successfully.
    ///
    /// Transactions at the DB layer should be optimistic, without 
    /// risk of deadlock. Optimistic transactions aren't appropriate
    /// for high-contention variables, but synchronization is left 
    /// to another layer of indirection.
    abstract member Transact : (DB -> 'X) -> struct('X * bool)

    /// Check Transaction for Obvious Causes of Failure
    ///
    /// Return `false` only if commit would fail. False optimism
    /// is permitted - the trivial implementation always returns
    /// `true`. Purpose is to help clients short-circuit a long
    /// running transaction where interference is possible but is
    /// rare enough that optimism is warranted.
    abstract member Check : unit -> bool

module DB =

    type Key = ByteString
    type Val = ByteString option
    type KVMap = BTree<Val>

    /// Storage models exclusive access to a binary key-value database.
    /// By "exclusive", I mean that we can assume prior reads remain 
    /// valid until we explicitly overwrite them. 
    ///
    /// Storage is weakly transactional: writes are performed in batches
    /// to model atomic updates, and `Flush` must ensure pending writes
    /// have reached the backing store.
    ///
    /// Our backend may provide a mangling function, such that every
    /// key may be rewritten exactly once (upon registration).
    type Storage =
        inherit Stowage
        abstract member Mangle     : ByteString -> Key
        abstract member Read       : Key -> Val     
        abstract member WriteBatch : KVMap -> unit 
        abstract member Flush      : unit -> unit

    module private StorageDB =
        // GOAL: Create a DB from a Storage!

        // create unique IDs for DBVars to simplify indexing.
        type UID = uint64
        type IDGen =
            val mutable private prev : UID
            new() = { prev = 0UL }
            member g.Next() = lock g (fun () -> 
                g.prev <- (1UL + g.pref) 
                g.prev) 
        let idGen = new IDGen()

        // for uniform "update" testing, I'll box all the data.
        type Data = System.Object

        // a DBVar is potentially durable. If durable, we'll have
        // a Key for Storage, and a function to serialize our data.
        //
        // Note that we never *read* a value after registration, so
        // we dont' need a full codec.
        type Durability =
            | Durable of Key * (Data -> Val)
            | Ephemeral

        let rootDur (k:Key) (cV:Codec<'V>) = 
            let s d = 
                match unbox<'V option>(d) with
                | None -> None
                | Some v -> Some (Codec.writeBytes cV v)
            Durable k s

        type DBVar = 
            val UID : UID
            val Durability : Durability
            val mutable Data : Data     // last committed data
            new(data, dur) = 
                let uid = idGen.Next()
                { UID = uid
                  Data = data 
                  Durability = dur
                }
            override v.GetHashCode() = hash (v.UID)
            override x.Equals yobj =
                match yobj with
                | :? DBVar as y -> (x.UID = y.UID)
                | _ -> false
            interface System.IComparable with
                member x.CompareTo yobj =
                    match yobj with
                    | :? DBVar as y -> compare (x.UID) (y.UID)
                    | _ -> invalidArg "yobj" "comparison of incompatible types"

        let isDurable (dbv:DBVar) : bool =
            match dbv.Durability with
            | Ephemeral -> false
            | Durable _ -> true

        type DBVar<'V>(v,dur) =
            inherit DBVar(box<'V>(v),dur)
            interface TVar<'V>

        allocEph (v:'V) = new DBVar<'V>(v,Ephemeral)
        allocRoot (db:Storage) (k:Key) (cV:Codec<'V>) : DBVar<'V option> =
            let parse b = Codec.readBytes cV (db :> Stowage) b
            let vOpt = Option.map parse (db.Read k)
            new DBVar<'V option>(vOpt, rootDur k cV)


        // For cached keys, I'll use weak references to avoid interfering
        // with GC. This slightly complicates lookups, but not too badly.
        type RootsCache = BTree<System.WeakReference>

        let tryFindObj<'O when 'O :not struct> (k:ByteString) (cache:RootsCache) : 'O option =
            match BTree.tryFind k cache with
            | None -> None
            | Some wref ->
                match wref.Target with
                | null -> None
                | :? 'O as obj -> Some obj
                | o ->
                    let msg = 
                        sprintf "root %s has incompatible type (%A vs. %A)" 
                            (BS.toString k) (o.GetType()) (typeof<'O>)
                    invalidOp msg

        // Find a root DBVar. May raise exception if type changes.
        let inline tryFindRoot<'V> k cache = 
            tryFindObj<DBVar<'V option>> k cache

        // remove dead refs from cache 
        let clearCache (cache:RootsCache) : RootsCache = 
            BTree.filter (fun _ v -> v.IsAlive) cache

        let castTVar (tv:TVar<'V>) : DBVar<'V> =
            match tv with
            | :? DBVar<'V> as dbv -> dbv
            | _ -> invalidArg "tv" "TVar from wrong DB"

        type Reads = Map<DBVar,Data>
        type Writes = Reads

        // compute serializable writes (excludes ephemerals)
        let serializeWrites (ws:Writes) : KVMap =
            let add wb (dbv:DBVar) v =
                match dbv.Durability with
                | Durable (k,s) -> BTree.add k (s v) wb
                | Ephemeral -> wb
            Map.fold add (BTree.empty) ws



        // Snapshots for transactional updates.
        //
        // To support snapshot isolation in a DB, I use a trivial,
        // brute force method: before commit, we walk all of the
        // snapshots in memory and record the current state for a
        // variable (assuming that snapshot doesn't already have
        // a prior version for the same variable).
        //
        // A single snapshot may be shared by many transactions on
        // a premise that many transactions are read-only and deep
        // hierarchical transactions should all use the same image.
        [<AllowNullLiteral>]
        type Snapshot =
            val mutable next   : Snapshot      // linked list iteration
            val mutable refct  : nativeint     // sharing of data 
            val mutable memory : Map<UID,Data> // remembered history

            new() = 
                { next  = null 
                  refct = 0n
                  memory = Map.empty
                }

            member ss.Read (dbv:DBVar) : Data = lock ss (fun () ->
                match Map.tryFind (dbv.UID) (ss.memory) with
                | Some data -> data
                | None -> dbv.Data)

            member ss.Precommit (ws:Writes) : unit = lock ss (fun () ->
                if (0n = ss.refct) then () else
                let add mem (dbv:DBVar) _ =
                    let k = dbv.UID
                    if Map.containsKey k mem then mem else
                    Map.add k (dbv.Data) mem
                ss.mem <- Map.fold add (ss.mem) ws)
        

        // Primary DB implementation.
        type RootDB =
            val store : Storage
            val mutex : System.Object
            val mutable ss : Snapshot
            val mutable roots : RootsCache
            val mutable buffer : Writes
            val mutable flushing : Lazy<unit>
            val mutable tm_clean : int

            new(s:Storage) =
                { store = s
                  mutex = new System.Object()
                  ss = new Snapshot()
                  roots = BTree.empty
                  buffer = Map.empty
                  flushing = lazy ()
                  tm_clean = 0
                }

            member inline db.Stowage with get() = ((db.store) :> Stowage)
            interface Stowage with
                member db.Load h = db.Stowage.Load h
                member db.Stow v = db.Stowage.Stow v
                member db.Incref h = db.Stowage.Incref h
                member db.Decref h = db.Stowage.Decref h

            member inline db.Read (dbv:DBVar<'V>) : 'V = unbox<'V>(dbv.Data)
            member inline db.Write (dbv:DBVar<'V>) (v:'V) : unit =
                let ws = Map.add (dbv :> DBVar) (box<'V>(v)) (Map.empty)
                let b = db.Commit (Map.empty) ws
                assert(b)

            member db.Commit (rs:Reads) (ws:Writes) : bool =
                // snapshot consistency implies read-only transactions succeed,
                // logically occurring before any change in the data.
                if Map.isEmpty ws then true else
                lock (db.mutex) (fun () ->
                    let check (dbv:DBVar) v = 
                        System.Object.ReferenceEquals((dbv.Data), v)
                    let okReads = Map.forall check rs
                    if not okReads then false else
                    db.Precommit ws
                    let commit (dbv:DBVar) v = 
                        dbv.data <- v
                        if isDurable dbv // buffer the durable writes
                            then db.buffer <- Map.add dbv v (db.buffer)
                    Map.iter commit ws
                    true)
            
            member private db.Precommit (ws:Writes) : unit =
                let rec loop ss =
                    if (null = ss.next) then () else
                    if (0n = ss.next.refct) 
                       then ss.next <- ss.next.next // drop unused
                            loop ss
                       else ss.next.Precommit ws // update in use
                            loop (ss.next)
                // introduce fresh snapshot per commit. Transactions 
                // started before next commit use the fresh snapshot.
                let ss = new Snapshot()
                ss.next <- db.ss
                db.ss <- ss
                loop (db.ss)

            // Register a durable variable.
            //
            // goals: (1) do not parse while holding DB mutex
            //        (2) always map same key to same DBVar
            //
            // This implementation permits redundant parsing if two threads
            // both try to register the same key near the same time. But it
            // shouldn't be a huge issue in practice.
            member db.Register (kRaw:ByteString) (cV:Codec<'V>) : DBVar<'V option> =
                let k = BS.trimBytes (db.store.Mangle kRaw)
                match tryFindRoot<'V> k (db.cache) with
                | Some dbv -> dbv
                | None ->
                    let dbv = allocRoot (db.store) k cV
                    lock (db.mutex) (fun () ->
                        match tryFindRoot<'V> k (db.cache) with
                        | Some dbvRaceWinner -> dbvRaceWinner
                        | None -> 
                            let wref = new System.WeakReference(dbv)
                            db.cache <- BTree.add k wref (db.cache)
                            dbv
                    )

            // extra cleanup, performed on Flush (but only if enough
            // time has passed). Time, in this case, is measured in
            // terms of GC cycles rather than milliseconds.
            member private db.Cleanup() : unit = 
                lock (db.mutex) (fun () ->
                    let tm = System.GC.CollectionCount(1)
                    if (tm = db.tm_clean) then () else
                    db.tm_clean <- tm
                    db.roots <- clearCache (db.roots)
                )

            // flush the database
            //
            // goals: concurrent flush from multiple threads should
            // leverage parallelism for serialization and may overlap
            // in time (instead of acting as sequential flushes).
            // 
            // To achieve this, I use `flushing` to ensure sequential
            // order for write-batch submissions, with each thread first
            // serializing its own writes before waiting on other threads.
            // And the backend is flushed after we've submitted in order.
            member db.Flush() : unit =
                let flush = lock (db.mutex) (fun () ->
                    let prior_flush = db.flushing
                    let wb = db.buffer
                    db.buffer <- Map.empty
                    db.flushing <- lazy (
                        let ws = serializeWrites wb // potentially concurrent with prior_flush
                        prior_flush.Force()         // wait for prior flush to submit write batch
                        db.store.WriteBatch ws      // submit the serial batch we created
                        System.GC.KeepAlive(wb))    // hold onto DBVars until updated values are visible
                    db.flushing)
                flush.Force()    // serialize and submit our write batch
                db.store.Flush() // flush write batch to durable storage
                db.Cleanup()     // potential cleanup of roots cache
            
            interface Stowage.DB with
                member db.Register k cV = upcast (db.Register k cV)
                member __.Allocate v = allocEph v
                member db.Read tv = db.Read (castTVar tv)
                member db.Write tv v = db.Write (castTVar tv) v 
                member db.Flush () = db.Flush()
                member db.Transact fn = db.Transact fn
                member db.Check () = true

            // obtain snapshot from most recent commit
            member db.AcquireSnapshot () : Snapshot =
                lock (db.mutex) (fun () ->
                    let ss = db.ss
                    ss.refct <- (ss.refct + 1n)
                    ss)

            // release snapshot when finished with it
            member db.ReleaseSnapshot (ss:Snapshot) : unit =
                lock (db.mutex) (fun () ->
                    assert(0n <> ss.refct)
                    ss.refct <- (ss.refct - 1n)
                    if (0n = ss.refct) 
                        then ss.memory <- Map.empty)

            member db.Transact (fn : DB -> 'X) : struct('X * bool) =
                let (result,rs,ws,flush) = 
                    let ss = db.AcquireSnapshot()
                    try let tx = new TX(db,ss)
                        let r = fn (tx :> DB)
                        (result, tx.rs, tx.ws, tx.flush)
                    finally db.ReleaseSnapshot ss
                let ok = db.Commit rs ws
                if (ok && flush) then db.Flush()
                struct(result,ok)
       
        and TX = 
            val db : RootDB     // for stowage, register, allocate
            val ss : Snapshot   // for new reads
            val mutex : System.Object 
            val mutable rs : Reads
            val mutable ws : Writes
            val mutable flush : bool

            new(db,ss) =
                { db = db
                  ss = ss
                  rs = Map.empty
                  ws = Map.empty
                  flush = false
                  mutex = new System.Object()
                }

            member inline tx.Stowage with get() = tx.db.Stowage
            interface Stowage with
                member tx.Load h = tx.Stowage.Load h
                member tx.Stow v = tx.Stowage.Stow v
                member tx.Incref h = tx.Stowage.Incref h
                member tx.Decref h = tx.Stowage.Decref h

            // reads on a 
            member tx.Read (dbv:DBVar<'V>) : 'V = lock (tx.mutex) (fun () ->
                let k = (dbv :> DBVar)
                match Map.tryFind k (tx.ws) with
                | Some data -> unbox<'V>(data)
                | 

                let k = (dbv :> DBVar)
                match Map.tryFind k (tx.ws) with
                | Some 

tx.ss.Read dbv
                

            interface Stowage.DB with
                member db.Register k cV = upcast (db.Register k cV)
                member __.Allocate v = allocEph v
                member db.Read tv = db.Read (castTVar tv)
                member db.Write tv v = db.Write (castTVar tv) v 
                member db.Flush () = db.Flush()
                member db.Transact fn = db.Transact fn
                member db.Check () = true

    
            interface 
    



