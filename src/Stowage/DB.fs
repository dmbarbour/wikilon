namespace Stowage
open Data.ByteString

/// Concrete implementation of Stowage Database API.
///
/// This uses a memory mapped file database, LMDB, under the hood.
/// It is intended to serve as the primary Stowage implementation,
/// suitable for most applications.
module DB =

    // Note: I could potentially generalize this implementation to any
    // key-value database assuming exclusive control. Not sure it is a
    // worthwhile generalization, however.

    /// Obtain LMDB-layer key for any given TVar key. Normally, this
    /// just uses the utf-8 representation for a string. Problematic
    /// keys are rewritten to "#SecureHash". Modulo use of tools like
    /// mdb_dump, you can safely ignore this mangling. 
    let lmdbKey (s:string) : ByteString =
        let s_utf8 = BS.fromString s
        let bHashPrefix = byte '#'
        assert (I.minKeyLen > 0)
        let ok = (I.maxKeyLen >= s_utf8.Length) &&
                 (s_utf8.Length >= I.minKeyLen) && 
                 (s_utf8.[0] <> bHashPrefix)    
        if ok then s_utf8 else
        let h = BS.take (RscHash.size / 2) (RscHash.hash s_utf8)
        BS.cons bHashPrefix h

    [<AutoOpen>]
    module internal Impl =

        // using boxed data for most things
        type Data = System.Object

        // My back-end conflates the empty string with key deletion, while
        // my front-end conflates the `None` value with key deletion. This
        // rootValCodec will help unify the two.
        let rootValCodec (cV : Codec<'V>) =
            let cRootPrefix = byte '='
            { new Codec<'V option> with
                member __.Write vOpt dst =
                    match vOpt with
                    | Some v -> 
                        ByteStream.writeByte cRootPrefix dst 
                        Codec.write cV v dst
                    | None -> () // empty byte string
                member __.Read db src =
                    if ByteStream.eos src then None else
                    let b0 = ByteStream.readByte src
                    if (b0 <> cRootPrefix) 
                       then raise (ByteStream.ReadError)
                    let v = Codec.read cV db src
                    Some v
                member __.Compact db vOpt =
                    match vOpt with
                    | Some v ->
                        let struct(v',szV) = Codec.compactSz cV db v
                        struct(Some v', 1 + szV)
                    | None -> struct(None, 0)
            }

        // durability (using boxed codec)
        type Durability =
            | Durable of ByteString * Codec<Data>
            | Ephemeral

        type UID = uint64

        type IDGen =
            val mutable private n : uint64
            new() = { n = 0UL }
            member g.Next() : UID = lock g (fun () ->
                g.n <- (1UL + g.n); g.n)

        // produce unique process-level IDs, starting at 1
        let sharedIdGen = new IDGen()

        // DBVar has lightweight identifier for table lookups and a
        // boxed value for trivial comparisons (via ReferenceEquals).
        // Durability is recorded in a generic way. 
        type DBVar =
            val uid : UID
            val dur : Durability
            val mutable data : Data
            // consider tracking origin for better debugging?
            new(dur,data) = 
                let uid = sharedIdGen.Next()
                { uid  = uid
                  dur  = dur
                  data = data
                }
            // enable generic DBVars to be used as keys in map
            override v.GetHashCode() = hash (v.uid)
            override x.Equals yobj =
                match yobj with
                | :? DBVar as y -> (x.uid = y.uid)
                | _ -> false
            interface System.IComparable with
                member x.CompareTo yobj = 
                    match yobj with
                    | :? DBVar as y -> compare (x.uid) (y.uid)
                    | _ -> invalidArg "yobj" "comparing values of different types"

        type DBVar<'V>(dur,data) =
            inherit DBVar(dur,data)
            interface TVar<'V>

        let allocDBVar (v:'V) : DBVar<'V> =
            new DBVar<'V>(Ephemeral, box<'V>(v))

        // Durable DBVars are all cached in a BTree with weak refs.
        type RootsCache = BTree<System.WeakReference>

        let tryFindObj<'O when 'O :not struct> (k:ByteString) (cache:RootsCache) : 'O option =
            match BTree.tryFind k cache with
            | None -> None
            | Some weak ->
                match weak.Target with
                | null -> None
                | :? 'O as obj -> Some obj
                | o ->
                    let msg = 
                        sprintf "key %s bound to multiple types (%A vs. %A)" 
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
            | _ -> invalidArg "tv" "TVar is from another DB"

        // Implementing Snapshot Isolation
        //
        // To ensure snapshot isolation, the easiest means would be
        // a global read-lock. Upon commit, we wait for concurrent
        // transactions to finish before writing, then we write and
        // continue. This hurts concurrency and reentrancy.
        //
        // One alternative is to keep multiple versions of values in
        // memory, such that readers can continue to work with the
        // older values even as we write our new values. In this
        // case, we can manage up to one snapshot per TX. Commit does
        // not wait, in this case, but has a cost proportional to the
        // number of concurrent transactions in memory.
        //
        // An intermediate option is to use a bounded number of 
        // snapshots, which also bounds the number of versions for
        // each value. Each snapshot might be  shared by several 
        // transactions that start around the same time. When we
        // start new transactions, we must wait for fresh snapshots.
        //
        // For complexity, the per-TX snapshot or read-lock is far
        // simpler than shared snapshots. For now, I'll just use 
        // per-TX snapshots. If this becomes an issue in the future,
        // I should correct it then.

        type Snapshot = Map<UID, Data>
        type Reads = Map<DBVar, Data>
        type Writes = Reads
        type WritePend = (struct(Codec<Data>*Data))
        type WriteBuffer = BTree<struct(Codec<Data>*Data)>
        type OK = OK

        let serializeWrites (wb:WriteBuffer) : I.KVMap =
            let serializeWP _ (struct(cV,v):WritePend) = 
                Codec.writeBytes cV v 
            BTree.map serializeWP wb

        // Primary DB implementation.
        type DB =
            val impl : I.DB
            val mutex : System.Object
            val mutable txhd : TX
            val mutable roots : RootsCache 
            val mutable wb : WriteBuffer 
            val mutable flushing : Lazy<OK> // to limit span of DB locks
            val mutable gc_count : int // for delayed cleanup

            new(impl:I.DB) =
                { impl = impl
                  mutex = new System.Object()
                  txhd = null
                  roots = BTree.empty
                  wb = BTree.empty
                  flushing = lazy OK
                  gc_count = 0
                }

            member db.ReadKey (k:ByteString) : ByteString =
                I.withRTX (db.impl) (fun rtx -> 
                    I.dbReadKey (db.impl) rtx k)

            member db.TryLoad (h:RscHash) : ByteString option =
                I.withRTX (db.impl) (fun rtx ->
                    I.dbGetRsc (db.impl) rtx h)

            interface Stowage with
                member db.Load h =
                    match db.TryLoad h with
                    | Some v -> v
                    | None -> raise (MissingRsc ((db :> Stowage), h)) 
                member db.Stow v = I.dbStow (db.impl) v
                member db.Incref h = db.impl.ephtbl.Incref (I.rscEphID h)
                member db.Decref h = db.impl.ephtbl.Decref (I.rscEphID h)

            interface Stowage.TX with
                // singular transactions
                member db.Stowage with get() = (db :> Stowage)
                member __.Allocate (v:'V) : TVar<'V> = 
                    upcast (allocDBVar v)
                member db.Register (cV:Codec<'V>) (k:string) : TVar<'V option> = 
                    upcast (db.Register cV (lmdbKey k)) 
                member db.Write (tv:TVar<'V>) (v:'V) : unit =
                    db.Write (castTVar tv) v
                member db.Read (tv:TVar<'V>) : 'V =
                    db.Read (castTVar tv)

            interface Stowage.DB with
                member db.Stowage with get() = (db :> Stowage)
                member db.Singular with get() = (db :> Stowage.TX)
                member db.Transact fn = db.Transact fn
                member db.Flush() = db.Flush()

            member db.Dispose (disposing:bool) = 
                if disposing then (db.Flush())
                I.closeDB (db.impl)
            override db.Finalize() = db.Dispose(false)
            interface System.IDisposable with
                member db.Dispose() =
                    db.Dispose(true)
                    System.GC.SuppressFinalize(db) 

            member inline db.Read (dbv : DBVar<'V>) : 'V =
                unbox<'V>((dbv :> DBVar).data)

            member db.Write (dbv : DBVar<'V>) (v:'V) : unit =
                // singleton write, no reads
                let rs = Map.empty
                let ws = Map.add (dbv :> DBVar) (box<'V>(v)) (Map.empty)
                let b = db.Commit rs ws
                assert(b)

            member db.Commit (rs:Reads) (ws:Writes) : bool =
                // assume snapshot consistency, thus read-only succeeds
                // even when some variables were updated concurrently.
                // Only read-write can have conflicts.
                if Map.isEmpty ws then true else 
                lock (db.mutex) (fun () ->
                    let checkRead (dbv:DBVar) v = 
                        System.Object.ReferenceEquals((dbv.data), v)
                    let okReads = Map.forall checkRead rs
                    if not okReads then false else
                    let rec loop (tx:TX) = 
                        // update TX snapshots
                        if (null = tx) then () else
                        tx.Precommit ws
                        loop (tx.next)
                    loop (db.txhd)
                    let commitWrite (dbv:DBVar) v = 
                        dbv.data <- v
                        match dbv.dur with
                        | Ephemeral -> ()
                        | Durable(k,c) ->
                            db.wb <- BTree.add k (struct(c,v)) (db.wb)  
                    Map.iter commitWrite ws
                    true
                )

            member db.Register (cV:Codec<'V>) (k:ByteString) : DBVar<'V option> =
                // goals:
                //  - ensure all instances of key in memory are shared
                //  - keep potentially expensive parse outside of lock
                match tryFindRoot<'V> k (db.roots) with
                | Some dbv -> dbv
                | None -> 
                    let cRoot = Codec.boxed (rootValCodec cV)
                    let v = Codec.readBytes cRoot (db :> Stowage) (db.ReadKey k)
                    let dur = Durable (k, cRoot)
                    let dbv = new DBVar<'V option>(dur,v)
                    lock (db.mutex) (fun () ->
                        match tryFindRoot<'V> k db.roots with
                        | Some dbvRaceWinner -> dbvRaceWinner // another thread registered same key
                        | None -> 
                            let wdbv = new System.WeakReference(dbv)
                            db.roots <- BTree.add k wdbv (db.roots)
                            dbv
                    )

            // perform generic cleanup, only if enough time (in terms
            // of GC cycles) has passed since our prior cleanup.
            member db.Cleanup() : unit = 
                lock (db.mutex) (fun () ->
                    let gc_count = System.GC.CollectionCount(1)
                    if (gc_count = db.gc_count) then () else
                    db.gc_count <- gc_count
                    db.roots <- clearCache (db.roots)
                )


            // Flush will serialize pending writes concurrently with
            // other threads performing Flush, and may share the same
            // write batch. Laziness is used to shift serialization
            // outside of the DB mutex. 
            member db.Flush() : unit =
                let tcs = new System.Threading.Tasks.TaskCompletionSource<bool>()
                let flush = lock (db.mutex) (fun () ->
                    let flush_old_writes = db.flushing
                    let wb = db.wb
                    db.wb <- BTree.empty
                    db.flushing <- lazy (
                        let ws = serializeWrites wb
                        flush_old_writes.Force() |> ignore<OK>
                        I.dbCommit (db.impl) (BTree.empty, ws, tcs)
                        OK)
                    db.flushing)
                flush.Force() |> ignore<OK>
                let b = tcs.Task.Result
                assert(b)
                db.Cleanup()

            // This will use the concrete TX implementation.
            member db.Transact (fn : Stowage.TX -> 'X) : struct('X * bool) =
                // TODO!
                raise (System.NotImplementedException())

        
        and [<AllowNullLiteral>] TX = 
            val db : DB
            val mutable next : TX
            val mutable prev : TX
            val mutable sc : Snapshot
            val mutable rs : Reads
            val mutable ws : Writes

            new(db:DB) =
                { db = db
                  next = null
                  prev = null
                  sc = Map.empty
                  rs = Map.empty
                  ws = Map.empty
                }

            member tx.Precommit (ws:Writes) : unit = lock tx (fun () ->
                let add sc (dbv:DBVar) _ =
                    let k = dbv.uid
                    if Map.containsKey k sc then sc else
                    Map.add k (dbv.data) sc
                tx.sc <- Map.fold add (tx.sc) ws)
            

    /// Open or Create database in current directory.
    ///
    /// Note: This implementation assumes exclusive access to the
    /// specified directory. A lockfile is used to resist accident,
    /// but is not perfect protection. 
    ///
    /// The DB may close via IDisposable or Finalize. It's also crash
    /// safe, assuming you've flushed the relevant data. There is no
    /// implicit flushing, so consider setting up a periodic event!
    let load (path : string) (maxSizeMB : int) : Stowage.DB = 
        let impl = I.openDB path maxSizeMB
        (new Impl.DB(impl)) :> Stowage.DB


    // I'm going to strictly use the Stowage.DB APIs.

