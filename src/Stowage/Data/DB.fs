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
    /// A registration takes a bytestring key and a codec, and creates
    /// a variable with an optional value where `None` represents the
    /// non-existence or deletion of a key. If a key is registered more 
    /// than once, an equivalent codec of the same type must be used at
    /// each registration. In return, registrations should be cached.
    ///
    /// Writes to durable TVars are not automatically durable. Instead,
    /// the DB must be explicitly flushed to serialize writes to disk.
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
    /// most intermediate data. Thus writes may be buffered in memory
    /// until explicitly flushed. Flush within a transaction is applied
    /// upon successful commit, and effectively marks the transaction
    /// durable. 
    ///
    /// Naturally, only durable TVars are preserved in durable storage.
    abstract member Flush : unit -> unit

    /// Perform Hierarchical Transaction.
    ///
    /// This creates a transactional model of the DB interface, such
    /// that read dependencies are tracked and writes are aggregated.
    /// Ideally has snapshot isolation, so read-only transactions do
    /// not fail. Commit is implicit on returning and may fail. The
    /// extra boolean result reports commit success. Any exception 
    /// will cleanly abort the transaction.
    /// 
    /// For high-contention variables, clients should use external
    /// synchronization to improve chances of transaction success.
    abstract member Transact : (DB -> 'X) -> struct('X * bool)


module DB =

    type Key = ByteString
    type Val = ByteString option
    type KVMap = BTree<Val>
    type Sync = Lazy<unit>

    /// Storage represents a simple key-value database with Stowage,
    /// such that values serve as durable stowage roots. Assuming we
    /// have exclusive write access, we may use a Storage to produce
    /// a DB.
    ///
    /// WriteBatch must be atomic and return a value to await durable
    /// synchronization with the backing store. Read must be consistent
    /// with prior WriteBatch operations even if synchronization hasn't
    /// completed.
    ///
    /// Many databases cannot support arbitrary keys, so we support
    /// a `Mangle` operation to rewrite and escape problematic keys
    /// once rather than per write. Oversized keys can be rewritten
    /// using a secure hash.
    type Storage =
        inherit Stowage
        abstract member Mangle     : ByteString -> Key
        abstract member Read       : Key -> Val     
        abstract member WriteBatch : KVMap -> Sync

    /// Flush storage (via writing empty batch). 
    let flushStorage (s:Storage) : unit =
        let sync = s.WriteBatch (BTree.empty)
        sync.Force()

    module private StorageDB =
        // GOAL: Create a DB from a Storage!

        // create unique IDs for DBVars to simplify indexing.
        //
        // I assume we won't process the whole 64-bit space, at least
        // without a few hundred years of runtime without restarting.
        type UID = uint64
        type IDGen =
            val mutable private prev : UID
            new() = { prev = 0UL }
            member g.Next() = lock g (fun () -> 
                g.prev <- (1UL + g.prev) 
                g.prev) 
        let idGen = new IDGen()

        // for uniform "update" testing, I'll box all the data.
        type Data = System.Object

        // a DBVar is potentially durable. If durable, we'll have
        // a Key for Storage, and a function to serialize our data.
        type Durability =
            | Durable of Key * (Data -> Val)
            | Ephemeral

        let serializeRoot (cV:Codec<'V>) (vOpt : 'V option) : Val =
            match vOpt with
            | Some v -> Some (Codec.writeBytes cV v)
            | None -> None

        let serializeRootData (cV:Codec<'V>) (d:Data) : Val =
            serializeRoot cV (unbox<'V option>(d))

        let rootDur (k:Key) (cV:Codec<'V>) : Durability = 
            Durable(k, serializeRootData cV)

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
            override v.GetHashCode() = 4567 * (int v.UID)
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

        let allocEph (v:'V) = new DBVar<'V>(v, Ephemeral)

        let allocRoot (db:Storage) (k:Key) (cV:Codec<'V>) : DBVar<'V option> =
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
        type Env = Reads

        // compute serializable writes (excludes ephemerals)
        let serializeWrites (ws:Writes) : KVMap =
            let add wb (dbv:DBVar) v =
                match dbv.Durability with
                | Durable (k,s) -> BTree.add k (s v) wb
                | Ephemeral -> wb
            Map.fold add (BTree.empty) ws

        // quickly merge two maps, favoring values from `a` on conflict.
        let inline leftBiasedUnion (a:Map<'K,'V>) (b:Map<'K,'V>) : Map<'K,'V> =
            if Map.isEmpty b then a else
            Map.foldBack (Map.add) a b

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
            val mutable next   : Snapshot   // linked list iteration
            val mutable refct  : nativeint  // sharing of data 
            val mutable memory : Map<UID,Data> // remembered history

            new() = 
                { next  = null 
                  refct = 0n
                  memory = Map.empty
                }

            member ss.ReadData (dbv:DBVar) : Data = lock ss (fun () ->
                match Map.tryFind (dbv.UID) (ss.memory) with
                | Some data -> data
                | None -> dbv.Data)

            // for variables we're about to write and for which we lack
            // any memory, add pre-write value (dbv.Data) to snapshot.
            member ss.Precommit (ws:Writes) : unit = lock ss (fun () ->
                if (0n = ss.refct) then () else
                let add (dbv:DBVar) _  mem =
                    let k = dbv.UID
                    if Map.containsKey k mem 
                        then mem 
                        else Map.add k (dbv.Data) mem
                ss.memory <- Map.foldBack add ws (ss.memory))

        // Primary DB implementation.
        type RootDB =
            val store : Storage
            val private mutex : System.Object
            val mutable ss : Snapshot
            val mutable roots : RootsCache
            val mutable buffer : Writes
            val mutable flushing : Lazy<Sync>
            val mutable tm_clean : int

            new(s:Storage) =
                { store = s
                  mutex = new System.Object()
                  ss = new Snapshot()
                  roots = BTree.empty
                  buffer = Map.empty
                  flushing = lazy(lazy())
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

            member private db.Commit (rs:Reads) (ws:Writes) : bool =
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
                        dbv.Data <- v
                        if isDurable dbv // buffer the durable writes
                           then db.buffer <- Map.add dbv v (db.buffer)
                    Map.iter commit ws
                    true)
            
            // precommit assumes we're holding lock
            member private db.Precommit (ws:Writes) : unit =
                let rec loop (ss:Snapshot) =
                    if (null = ss.next) then () else
                    if (0n = ss.next.refct) 
                       then ss.next <- ss.next.next // drop unused, old
                            loop ss
                       else ss.next.Precommit ws // update snapshot
                            loop (ss.next)
                // introduce fresh snapshot per commit. Transactions 
                // started before next commit use the fresh snapshot.
                let ss = new Snapshot()
                ss.next <- db.ss
                loop ss
                db.ss <- ss

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
                match tryFindRoot<'V> k (db.roots) with
                | Some dbv -> dbv
                | None ->
                    let dbv = allocRoot (db.store) k cV
                    lock (db.mutex) (fun () ->
                        match tryFindRoot<'V> k (db.roots) with
                        | Some dbvRaceWinner -> dbvRaceWinner
                        | None -> 
                            let wref = new System.WeakReference(dbv)
                            db.roots <- BTree.add k wref (db.roots)
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
                    db.Precommit (Map.empty)
                    db.roots <- clearCache (db.roots)
                )

            // flush the database
            //
            // Concurrent flush from multiple threads can parallelize
            // serialization of each write-batch, rather than run in
            // sequence. However, we must ensure flushes are ordered
            // in terms of write batches. I use the lazy 'flushing' to
            // order write batches without holding the lock too long.
            member db.Flush() : unit =
                let write = lock (db.mutex) (fun () ->
                    let wb = db.buffer
                    db.buffer <- Map.empty
                    let prior = db.flushing // potentially concurrent
                    db.flushing <- lazy (
                        let ws = serializeWrites wb         // concurrent with prior
                        prior.Force() |> ignore<Sync>       // await prior WriteBatch
                        let sync = db.store.WriteBatch ws   // apply new WriteBatch
                        System.GC.KeepAlive(wb)             // guard DBVars, VRefs
                        sync)
                    db.flushing)
                let sync = write.Force()
                sync.Force()  
                db.Cleanup()     // potential cleanup of roots cache
            
            interface DB with
                member db.Register k cV = upcast (db.Register k cV)
                member __.Allocate v = upcast (allocEph v)
                member db.Read tv = db.Read (castTVar tv)
                member db.Write tv v = db.Write (castTVar tv) v 
                member db.Flush () = db.Flush ()
                member db.Transact fn = db.Transact fn

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
                    try let tx = new TX(db,ss,Map.empty)
                        let result = fn (tx :> DB)
                        (result, tx.rs, tx.ws, tx.flush)
                    finally db.ReleaseSnapshot ss
                let ok = db.Commit rs ws
                if (ok && flush) then db.Flush()
                struct(result,ok)
       
        and TX = 
            val db : RootDB     // for stowage, register, allocate
            val ss : Snapshot   // for new reads
            val env : Reads     // modifier for parent TX writes 
            val private mutex : System.Object 
            val mutable rs : Reads
            val mutable ws : Writes
            val mutable flush : bool

            new(db,ss,env) =
                { db = db
                  ss = ss
                  env = env
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


            // reads in a transaction will look in several locations:
            //
            //  - the write set (since we might have updated it)
            //  - the read set (avoid extra lock on snapshot)
            //  - environment (our parent might have written it)
            //  - the snapshot (for snapshot-consistent data)
            //
            // Our environment represents the writes present from the
            // parent TX. Assuming the parent TX might be concurrently
            // updated, we must record dependencies at this environment.
            member tx.ReadData (dbv:DBVar) : Data =
                lock (tx.mutex) (fun () ->
                    match tx.ReadLocalData dbv with
                    | Some data -> data
                    | None -> tx.ReadEnvData dbv)

            member private tx.ReadLocalData (dbv:DBVar) : Data option =
                let inWS = Map.tryFind dbv (tx.ws)
                if Option.isSome inWS then inWS else
                Map.tryFind dbv (tx.rs)

            member private tx.ReadEnvData (dbv:DBVar) : Data =
                let data = 
                    match Map.tryFind dbv (tx.env) with
                    | Some data -> data
                    | None -> tx.ss.ReadData dbv
                tx.rs <- Map.add dbv data (tx.rs)
                data

            member inline tx.Read (dbv:DBVar<'V>) : 'V = 
                unbox<'V>(tx.ReadData (dbv :> DBVar))
                
            member tx.Write (dbv:DBVar<'V>) (v:'V) : unit =
                lock (tx.mutex) (fun () ->
                    tx.ws <- Map.add (dbv :> DBVar) (box<'V>(v)) (tx.ws))

            member inline tx.Flush() = tx.flush <- true

            interface DB with
                member tx.Register k cV = upcast (tx.db.Register k cV)
                member __.Allocate v = upcast (allocEph v)
                member tx.Read tv = tx.Read (castTVar tv)
                member tx.Write tv v = tx.Write (castTVar tv) v 
                member tx.Flush () = tx.Flush ()
                member tx.Transact fn = tx.Transact fn

            // hierarchical transactions
            member tx.Transact (fn : DB -> 'X) : struct('X * bool) =
                let env = leftBiasedUnion (tx.ws) (tx.env)
                let ctx = new TX(tx.db, tx.ss, env)
                let result = fn (ctx :> DB)
                let ok = tx.Commit (ctx.rs) (ctx.ws)
                if (ok && ctx.flush) then tx.Flush()
                struct(result,ok)

            // hierarchical transaction commit
            //
            // A transaction may fail if the parent TX was concurrently
            // updated in a manner that invalidates a read by the child.
            // If the transaction succeeds, we filter the read-set based
            // on the parent's write set (hence we logically read from 
            // the moment of commit).
            member private tx.Commit (rs:Reads) (ws:Writes) : bool =
                // snapshot consistency implies read-only transactions succeed
                if Map.isEmpty ws then true else
                lock (tx.mutex) (fun () ->
                    let check (dbv:DBVar) v =
                        match Map.tryFind dbv (tx.ws) with
                        | Some data -> System.Object.ReferenceEquals(data,v)
                        | None -> true // no conflict here
                    let okReads = Map.forall check rs
                    if not okReads then false else
                    let addReadDep dbv v m =
                        if Map.containsKey dbv (tx.ws) then m else
                        Map.add dbv v m
                    tx.rs <- Map.foldBack addReadDep rs (tx.rs)
                    tx.ws <- leftBiasedUnion ws (tx.ws)
                    true)


    /// Create a DB from a Storage.
    ///
    /// Assumes exclusive write access to the given Storage. This is
    /// a relatively convenient way to create a full Stowage DB.
    let fromStorage s = (new StorageDB.RootDB(s :> Storage)) :> DB


