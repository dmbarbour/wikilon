namespace Stowage
open System.IO
open System.Threading
open System.Threading.Tasks
open System.Security
open Data.ByteString
open Stowage.Internal.Memory
open Stowage.Internal.LMDB
open System.Runtime.InteropServices

/// Stowage is a key-value database that features garbage collected
/// references between binaries via secure hashes. 
///
/// Stowage is implemented above LMDB, a memory-mapped B-tree. Stowage
/// transactions are optimistic and lightweight, held in memory until
/// commit. Non-conflicting concurrent writes are batched to amortize
/// overheads for synchronization to disk. However, in case of conflict,
/// inconsistencies may be observed within the failing transactions.
///
/// The ability to reference binaries via secure hashes, together with
/// garbage collection, enables a stowage database to represent larger 
/// than memory persistent data structures in a purely functional style.
/// It also supports simple structure sharing, and should be relatively
/// easy to shard in a distributed system. This doubles as a functional
/// virtual memory model.
module DB =

    /// A Resource is identified by a secure hash (see Stowage.Hash)
    type RscHash = ByteString

    /// Stowage keys are non-empty bytestrings of at most 255 bytes.
    type Key = ByteString
    let minKeyLen : int = 1
    let maxKeyLen : int = 255
    let isValidKey (k : Key) : bool = (maxKeyLen >= k.Length)
                                   && (k.Length >= minKeyLen)

    /// Stowage values are arbitrary bytestrings. However, values are
    /// not considered entirely opaque: they may contain resource hash
    /// references to other binaries (see scanHashDeps).
    type Val = ByteString

    /// Scan a value for secure hash resource dependencies.
    ///
    /// This conservatively finds substrings that exactly match the Hash
    /// size and character set (cf. Stowage.Hash). Separated hashes using
    /// spaces, braces, punctuation, parentheses, or other characters.
    let scanHashDeps (fn : 's -> RscHash -> 's) : 's -> Val -> 's =
        let rec loop s v = 
            let hv' = Data.ByteString.dropWhile (Hash.validHashByte >> not) v
            if Data.ByteString.isEmpty hv' then s else
            let (h,v') = Data.ByteString.span (Hash.validHashByte) hv'
            let s' = if (Hash.validHashLen = h.Length) then (fn s h) else s
            loop s' v'
        loop

    /// A batch of key-values to be compared or written.
    type KVMap = Map<Key,Val>

    [< SecuritySafeCriticalAttribute >]
    module internal I =

        // fragment of hash used for stowage keys
        let stowKeyLen = Hash.validHashLen / 2
        type StowKey = ByteString // of stowKeyLen

        // I don't favor use of F# maps because they lack efficient
        // union/merge/diff and aren't ideal for large keys. But they
        // are sufficient for now. I doubt this will be a performance
        // bottleneck in any case.
        type Stowage = Map<RscHash,Val>     // recent stowage requests

        // An ephemeral roots table tracks a conservative set of elements
        // that we must preserve even if they lack persistent references.
        // This is represented by a map of hashes to reference counts.
        type EphID = uint64                 // via FNV-1a hash
        type RC = nativeint                 // reference count type
        type EphRoots = Map<EphID, RC>      // ephemeral roots

        // assumes sufficient size (stowKeyLen)
        let memEphID (p : nativeint) : EphID =
            let fnv_prime = 1099511628211UL
            let offset_basis = 14695981039346656037UL
            let mutable h = offset_basis
            for ix = 0 to (stowKeyLen - 1) do
                let b = Marshal.ReadByte(p,ix)
                h <- ((h ^^^ (uint64 b)) * fnv_prime)
            h
        let rscEphId (rsc : RscHash) : EphID =
            // reuse memEphID to guarantee same hash is computed
            assert(Hash.validHashLen = rsc.Length)
            Data.ByteString.withPinnedBytes rsc memEphID

        // Each commit consists of:
        //  a set of values read (or assumed) to validate
        //  a set of values to be written 
        //  a task completion resource to report success or failure
        type Commit = (KVMap * KVMap * TaskCompletionSource<bool>)

        // Readlock state is essentially a reader-count with an event
        // for when we reach zero readers.
        type ReadLock () = 
            let mutable rc = 0
            member this.Acquire () = lock this (fun () -> 
                rc <- rc + 1)
            member this.Release () = lock this (fun () ->
                assert (rc > 0)
                rc <- rc - 1
                if(0 = rc) then Monitor.PulseAll(this))
            member this.Wait () = lock this (fun () -> 
                while(0 <> rc) do ignore(Monitor.Wait(this)))

        type DB =
            { 
                db_env  : MDB_env     
                db_data : MDB_dbi     // key -> value
                db_stow : MDB_dbi     // resource ID -> value
                db_rfct : MDB_dbi     // resources with refct > 0
                db_zero : MDB_dbi     // resources with zero refct

                mutable db_rdlock : ReadLock        // current read-lock (updated per frame).
                mutable db_ephtbl : EphRoots        // ephemeral resource roots
                mutable db_newrsc : Stowage         // recent stowage requests
                mutable db_commit : Commit list     // pending commit requests
                mutable db_trygc  : bool            // incremental GC available

                db_fini : System.Object  // extra finalizer
            }

        // the DB will have a dedicated writer thread.
        // the DB object serves mutex and pulse/wait signal, via Monitor
        let dbCommit (db : DB) (c : Commit) : unit =
            lock db (fun () ->
                db.db_commit <- (c :: db.db_commit)
                Monitor.PulseAll(db))

        // Perform operation while holding reader TX.
        //
        // LMDB with NOLOCK is essentially a frame-buffered database.
        // At most times, we have two valid frames. During commit, we
        // drop the older frame and write the new one. Stowage aligns
        // its locking with this model, so readers never wait and the
        // writer waits only on readers that hold the lock for nearly
        // two full write frames.
        //
        // We can assume most readers only hold the lock briefly, and
        // hence our writer very rarely waits.
        let inline withRTX (db : DB) (action : MDB_txn -> 'x) : 'x =
            let rdlock = lock db (fun () -> 
                // lock prevents advanceReaderFrame during Acquire
                db.db_rdlock.Acquire() 
                db.db_rdlock)
            try let tx = mdb_rdonly_txn_begin db.db_env
                try action tx
                finally mdb_txn_commit tx // release MDB_txn memory
            finally rdlock.Release()

        let advanceReaderFrame (db : DB) : ReadLock = lock db (fun () ->
            let oldFrame = db.db_rdlock
            db.db_rdlock <- new ReadLock()
            oldFrame)

        let inline getValZC (db : DB) (rtx : MDB_txn) (k : Key) : MDB_val =
            if not (isValidKey k) then invalidArg "k" "invalid key" else
            defaultArg (mdb_getZC rtx (db.db_data) k) (MDB_val()) 

        let inline readKeyDB (db : DB) (rtx : MDB_txn) (k : Key) : Val =
            Stowage.Internal.LMDB.val2bytes (getValZC db rtx k) 

        let matchVal (vtx : Val) (vdb : MDB_val) : bool =
            if (vtx.Length <> int vdb.size) then false else
            if (0 = vtx.Length) then true else
            withPinnedBytes vtx (fun pvtx -> 
                0 = (memcmp (pvtx) (vdb.data) (vtx.Length)))

        // search for an invalid read assumption
        let findInvalidRead db rtx wb rd =
            let validAssumption k v = 
                if not (isValidKey k) then false else
                match Map.tryFind k wb with
                  | Some vwb -> (v = vwb)
                  | None -> matchVal v (getValZC db rtx k)
            let invalidAssumption k v = not (validAssumption k v)
            Map.tryFindKey invalidAssumption rd

        // utilities to work with ephemeron tables
        let ephInc (etb : EphRoots) (k : EphID) (rcu : RC) : EphRoots =
            assert (rcu > 0n)
            match Map.tryFind k etb with
              | None -> Map.add k rcu etb
              | Some rc0 -> Map.add k (rcu + rc0) etb

        let ephDec (etb : EphRoots) (k : EphID) (rcu : RC) : EphRoots =
            assert (rcu > 0n)
            match Map.tryFind k etb with
              | None -> failwith "negative refct"
              | Some rc0 ->
                    assert (rc0 >= rcu)
                    let rc' = rc0 - rcu
                    if (0n = rc') then Map.remove k etb
                                  else Map.add k rc' etb 

        // compute ephemeral roots from a value
        let accumValEphs : EphRoots -> Val -> EphRoots =
            scanHashDeps (fun r h -> ephInc r (rscEphId h) 1n)

        let inline ephAdd (upd : EphRoots) (etb0 : EphRoots) : EphRoots =
            Map.fold ephInc etb0 upd

        let inline ephRem (upd : EphRoots) (etb0 : EphRoots) : EphRoots =
            Map.fold ephDec etb0 upd

        let inline dbUpdEphRoots (db : DB) (fn : EphRoots -> EphRoots) : unit =
            lock db (fun () -> (db.db_ephtbl <- fn db.db_ephtbl))

        let dbAddEphRoots (db : DB) (upd : EphRoots) : unit =
            if(not (Map.isEmpty upd)) then dbUpdEphRoots db (ephAdd upd)

        let dbRemEphRoots (db : DB) (upd : EphRoots) : unit =
            if(not (Map.isEmpty upd)) then dbUpdEphRoots db (ephRem upd)

        // add stowage to both the newrsc pool and ephemeral root.
        let dbAddStowage (db : DB) (h : RscHash) (v : Val) : EphID =
            assert (h.Length = Hash.validHashLen)
            let k = rscEphId h
            lock db (fun () ->
                db.db_newrsc <- Map.add h v (db.db_newrsc)
                db.db_ephtbl <- ephInc (db.db_ephtbl) k 1n 
                Monitor.PulseAll(db))
            k

        let tryFindRscMDB (db:DB) (rtx:MDB_txn) (h:RscHash) : MDB_val option =
            if(h.Length <> Hash.validHashLen) then invalidArg "h" "bad resource ID" else
            let hKey = Data.ByteString.take stowKeyLen h
            let hRem = Data.ByteString.drop stowKeyLen h
            let rlen = Hash.validHashLen - stowKeyLen
            let vOpt = mdb_getZC rtx (db.db_stow) hKey 
            match vOpt with
              | None -> None
              | Some v ->
                    assert(v.size < (unativeint System.Int32.MaxValue))
                    assert((int v.size) >= rlen)
                    let matchRem = withPinnedBytes hRem (fun ra -> 
                            memcteq ra v.data rlen)
                    if not matchRem then None else
                    let size' = v.size - unativeint rlen
                    let data' = v.data + nativeint rlen
                    Some(MDB_val(size',data'))


        let dbHasWork (db:DB) : bool =
            (db.db_trygc ||
             not (List.isEmpty db.db_commit) ||
             not (Map.isEmpty db.db_newrsc))

        let rec dbWriterLoop (db:DB) (rlock:ReadLock) : unit =
            let rec waitForWork () = 
                if dbHasWork db then () else 
                ignore (Monitor.Wait(db))
                waitForWork ()
            let (newRsc,revCommitLst) = lock db (fun () -> 
                waitForWork()
                let result = (db.db_newrsc, db.db_commit)
                db.db_commit <- List.empty
                db.db_trygc <- false
                result)
            let eph = db.db_ephtbl
            let wtx = mdb_readwrite_txn_begin (db.db_env) 
            let tryCommit ((rd,ws,tcs) : Commit) wb =
                if Option.isSome (findInvalidRead db wtx wb rd)
                    then tcs.TrySetResult(false) |> ignore; wb 
                    else Map.fold (fun m k v -> Map.add k v m) wb ws
            let writes = List.foldBack tryCommit revCommitLst Map.empty
            let overwritten = Map.map (fun k _ -> readKeyDB db wtx k) writes
            
            
            let rlock' = advanceReaderFrame db
            db.db_trygc <- false // should depend on GC progress
            dbWriterLoop db rlock'


        let inline dbThreadStart (db:DB) : unit = 
            dbWriterLoop db (advanceReaderFrame db)
            

    /// Stowage database object (abstract)
    [< Struct >]
    type DB =
        val internal Impl : I.DB
        internal new(dbImpl : I.DB) = { Impl = dbImpl }

    /// Read value associated with a key in the DB.
    ///
    /// Every key has a value, defaulting to the empty string. Reads
    /// are always atomic, i.e. you'll never read a partial value.
    let readKeyDB (db : DB) (k : Key) : Val =
        I.withRTX db.Impl (fun rtx -> I.readKeyDB db.Impl rtx k)

    /// Read multiple keyed values from DB. 
    ///
    /// Guarantees snapshot consistency for reading multiple elements.
    /// That is, it's atomic for the full list of keys. 
    let readKeysDB (db : DB) (ks : Key list) : Val list =
        I.withRTX db.Impl (fun rtx -> List.map (I.readKeyDB db.Impl rtx) ks)

    /// Find first key (if any) for which associated value doesn't match.
    /// Note: This doesn't account for concurrent writes.
    let testReadAssumptions (db : DB) (reads : KVMap) : (Key option) =
        if Map.isEmpty reads then None else
        I.withRTX db.Impl (fun rtx -> 
            I.findInvalidRead db.Impl rtx Map.empty reads)

    /// verify that all read assumptions are currently valid
    let inline verifyReadAssumptions (db : DB) (reads : KVMap) : bool =
        Option.isNone (testReadAssumptions db reads)

    /// Atomic database update (asynchronous)
    /// 
    /// This delivers read assumptions and writes to a writer thread.
    /// The writer will verify the reads and, if they are valid, will
    /// perform the writes. The result is true only if all reads are
    /// valid and the writes are successfully synchronized to disk.
    ///
    /// The writer will tend to batch updates that are provided around
    /// the same time, i.e. anything provided while the writer was busy
    /// with the prior batch. This helps amortize disk synchronization
    /// overheads among concurrent writers.
    ///
    /// In case of conflict, the order of commit determines success.
    /// Thus, progress is guaranteed, but failed updates may need to
    /// retry, and fairness is not assured. Clients should control
    /// access to high-contention keys using an independent locks or
    /// queues system.
    let atomicUpdateDB_async (db : DB) (reads : KVMap) (writes : KVMap) : Task<bool> =
        let tcs = new TaskCompletionSource<bool>()
        I.dbCommit db.Impl (reads, writes, tcs)
        tcs.Task

    /// Atomic compare and update (synchronous).
    let inline atomicUpdateDB (db : DB) (reads : KVMap) (writes : KVMap) : bool =
        (atomicUpdateDB_async db reads writes).Result

    /// Access a secure hash resource from the Database
    ///
    /// A Stowage database contains a set of binary values that are
    /// referenced by secure hash. These values are garbage collected
    /// if not rooted by the key-value persistence layer or ephemeral
    /// transaction. However, it's safe to look up resources even if
    /// they aren't rooted.
    let loadRscDB (db : DB) (h : RscHash) : Val option =
        // note: I'm relying on atomic reads for reference variables.
        // This was asserted for C#, so I assume it's valid for .Net.
        let newRsc = Map.tryFind h (db.Impl.db_newrsc)
        if Option.isSome newRsc then newRsc else
        I.withRTX db.Impl (fun rtx ->
            I.tryFindRscMDB db.Impl rtx h 
              |> Option.map Stowage.Internal.LMDB.val2bytes)

    /// Zero-copy access to a secure hash resource from the DB.
    ///
    /// This is possible leveraging the memory-mapped database, LMDB.
    /// However, this is unsafe except for short-lived, read-only 
    /// actions on the data.
    let unsafeWithRscDB (db : DB) (h : RscHash) (action : nativeint -> int -> 'x) : 'x option =
        // note: I'm relying on atomic reads for reference variables.
        // This was asserted for C#, so I assume it's valid for .Net.
        match Map.tryFind h (db.Impl.db_newrsc) with
          | None -> I.withRTX db.Impl (fun rtx ->
                match I.tryFindRscMDB db.Impl rtx h with
                  | None -> None
                  | Some v -> Some (action v.data (int v.size)))
          | Some v -> withPinnedBytes v (fun vaddr ->
                Some (action vaddr v.Length))

    /// Transaction object
    ///
    /// A Stowage transaction has a set of read assumptions and pending
    /// writes in memory. Upon commit, the `atomicUpdateDB` operation
    /// is performed. Additionally, each transaction provides ephemeral 
    /// roots for new secure hash resources, providing opportunity to
    /// commit and root the new data.
    ///
    /// An important consideration is that snapshot isolation is not
    /// guaranteed. Transactions may read inconsistent data. Example:
    ///
    ///         Alice        Bob
    ///         Reads A
    ///                      Updates A,B
    ///         Reads B
    ///
    /// In this case, Alice will read B inconsistent with A. Further, any
    /// secure hash resources previously rooted by A may be concurrently 
    /// GC'd after Bob's update, causing loadRsc to fail. The state change
    /// will cause Alice's commit to fail, but before commit Alice must
    /// handle the potential inconsistency. Alternatively, design the app
    /// such that the conflict scenario simply does not occur.
    /// 
    /// A TX is not thread-safe. The normal use case is single threaded.
    /// If used from multiple threads, you must ensure exclusive access. 
    type TX =
        val         internal db : I.DB
        val mutable internal rd : KVMap
        val mutable internal ws : KVMap
        val mutable internal eph : I.EphRoots
        new (db : DB) =
            { db = db.Impl
              rd = Map.empty
              ws = Map.empty
              eph = Map.empty
            }
        member tx.DB with get () = DB (tx.db)
        member tx.Reads with get () = tx.rd
        member tx.Writes with get () = tx.ws
        member private tx.ClearEphRoots() : unit =
            I.dbRemEphRoots (tx.db) (tx.eph)
            tx.eph <- Map.empty
        override tx.Finalize() = tx.ClearEphRoots()
        interface System.IDisposable with
            member tx.Dispose() =
                tx.ClearEphRoots()
                System.GC.SuppressFinalize tx

    /// create a new transaction on the database
    let newTX (db : DB) : TX = new TX(db)

    /// deep-copy an existing transaction on the database
    let dupTX (tx : TX) : TX =
        I.dbAddEphRoots (tx.db) (tx.eph)
        let clone = newTX (tx.DB)
        clone.eph <- tx.eph
        clone.rd <- tx.rd
        clone.ws <- tx.ws
        clone

    /// Commit a transaction (asynchronous).
    ///   Essentially just calls atomicUpdateDB_async
    let inline commit_async (tx:TX) : Task<bool> =
        atomicUpdateDB_async (tx.DB) (tx.Reads) (tx.Writes)

    /// Commit a transaction (synchronous).
    let inline commit (tx:TX) : bool = (commit_async tx).Result

    /// Checkpoint a transaction (asynchronous).
    ///
    /// This performs an asynchronous commit, but also modifies the TX
    /// state to support further updates and checkpoints of the TX. It
    /// is possible for multiple checkpoints to be batched together, so
    /// that intermediate states are not written to disk.
    let checkpoint_async (tx:TX) : Task<bool> =
        let result = commit_async tx
        tx.rd <- Map.fold (fun m k v -> Map.add k v m) tx.rd tx.ws
        tx.ws <- Map.empty
        result

    /// Checkpoint a transaction (synchronous).
    let inline checkpoint (tx:TX) : bool = (checkpoint_async tx).Result

    let private readKeyOld (tx:TX) (k:Key) : Val option =
        let wv = Map.tryFind k tx.ws
        if Option.isSome wv then wv else
        Map.tryFind k tx.rd

    let private readKeyNew (tx:TX) (rtx:MDB_txn) (k:Key) : Val =
        let v = I.readKeyDB tx.db rtx k
        tx.rd <- Map.add k v tx.rd
        v

    /// Read value associated with key via TX.
    ///
    /// If the TX assumes a value due to prior read or write, that
    /// value is returned. Otherwise, this will access the DB.
    let readKey (tx:TX) (k:Key) : Val =
        match readKeyOld tx k with
          | Some v -> v
          | None -> I.withRTX tx.db (fun rtx -> readKeyNew tx rtx k)

    /// Read multiple keys from TX.
    ///
    /// Ensures snapshot isolation for keys initially read together.
    /// This is weaker than full snapshot isolation of a TX, but it
    /// is sufficient to mitigate problematic inconsistencies within
    /// a transaction. 
    let readKeys (tx:TX) (ks:Key list) : Val list =
        I.withRTX tx.db (fun rtx -> 
            let read k = 
                match readKeyOld tx k with
                  | Some v -> v
                  | None -> readKeyNew tx rtx k
            List.map read ks)

    /// Introduce a read assumption.
    ///
    /// This modifies the TX as if a value for a specific key were
    /// read. If the key has already been read and has a different
    /// value than assumed, this raises InvalidOperationException. 
    /// Usually, assumptions should be provided before any reads.
    ///
    /// Note: read assumptions don't contribute to ephemeral roots.
    let assumeKey (tx:TX) (k:Key) (v:Val) : unit =
        if not (isValidKey k) then invalidArg "k" "invalid key" else
        match Map.tryFind k tx.rd with
          | None    -> tx.rd <- Map.add k v tx.rd 
          | Some v0 -> if (v0 <> v) then invalidOp "invalid assumption for key"

    /// Write a key-value into the TX
    ///
    /// This is a trivial operation since it only writes the key into
    /// the local transaction object. Upon successful commit, the value
    /// will be persisted to disk. Until then, it's held in memory.
    ///
    /// Note: further reads on a written Key return the written value.
    let writeKey (tx : TX) (k : Key) (v : Val) : unit = 
        if not (isValidKey k) then invalidArg "k" "invalid key" else
        tx.ws <- Map.add k v tx.ws

    /// load a secure hash resource into memory (see loadRscDB)
    let inline loadRsc (tx:TX) (h:RscHash) : Val option = loadRscDB tx.DB h
    
    /// zero-copy access to secure hash resource (see unsafeWithRscDB)
    let inline unsafeWithRsc (tx:TX) (h:RscHash) (action : nativeint -> int -> 'x) : 'x option =
        unsafeWithRscDB tx.DB h action

    /// Stow a value as a secure hash resource.
    ///
    /// This moves a value to disk, providing a secure hash as a handle
    /// to later access the resource in memory. The value is not held in
    /// the TX, it is moved directly to the underlying database. The TX
    /// maintains an ephemeral root to ensure the resource is not GC'd
    /// before we have opportunity to commit. (Normally, resources that
    /// are not rooted by values in the key-value layer will be GC'd.) 
    let stowRsc (tx:TX) (v:Val) : RscHash =
        let h = Stowage.Hash.hash v
        let k = I.dbAddStowage tx.db h v
        tx.eph <- I.ephInc tx.eph k 1n
        h

    /// GC Ephemeral Roots for Secure Hash Resources
    ///
    /// This function assumes you have already written into the TX any
    /// secure hash references that must be rooted, and releases the
    /// remainder. Additionally, you may provide extra roots. This is
    /// intended for use with some long-running transactions or cases
    /// where you might simply use the TX as an ephemeral root set.
    ///
    /// Note: Unless a resource was already rooted, adding an ephemeral
    /// root does not prevent concurrent GC. 
    let clearEphRoots (tx:TX) (extraRoots:seq<RscHash>) : unit =
        let ins m (h : RscHash) = 
            if (h.Length <> Hash.validHashLen) 
                then invalidArg "h" "invalid resource hash"
                else Map.add (I.rscEphId h) 1n m
        let insM m k v = scanHashDeps ins m v
        let eph' = Map.fold insM (Seq.fold ins Map.empty extraRoots) tx.ws
        I.dbAddEphRoots tx.db eph'
        I.dbRemEphRoots tx.db tx.eph
        tx.eph <- eph'

    let inline private withDir (p : string) (op : unit -> 'R) : 'R =
        do ignore <| System.IO.Directory.CreateDirectory(p) 
        let p0 = System.IO.Directory.GetCurrentDirectory()
        try 
            do ignore <| System.IO.Directory.SetCurrentDirectory(p)
            op ()
        finally
            System.IO.Directory.SetCurrentDirectory(p0)

    let inline lockFile (fn : string) : FileStream =
        new FileStream(
                fn, 
                FileMode.OpenOrCreate, 
                FileAccess.ReadWrite, 
                FileShare.None,
                8,
                FileOptions.DeleteOnClose)

    // assuming we're in the target directory, build the database
    let inline private mkDB (maxSizeMB : int) () : DB =
        let lock = lockFile ".lock"
        let env = mdb_env_create ()
        mdb_env_set_mapsize env maxSizeMB
        mdb_env_set_maxdbs env 4
        let envFlags = MDB_NOSYNC ||| MDB_WRITEMAP ||| MDB_NOTLS ||| MDB_NOLOCK
        mdb_env_open env "." envFlags

        // introduce a DB finalizer
        let dbFini = { new System.Object() with
                        override x.Finalize() =
                            mdb_env_sync env
                            mdb_env_close env
                            lock.Dispose()
                     }

        // open named databases
        let txn = mdb_readwrite_txn_begin env
        let dbData = mdb_dbi_open txn "@" MDB_CREATE // root key-value
        let dbStow = mdb_dbi_open txn "$" MDB_CREATE // stowed resources
        let dbRfct = mdb_dbi_open txn "#" MDB_CREATE // positive refcts
        let dbZero = mdb_dbi_open txn "0" MDB_CREATE // zero refcts
        mdb_txn_commit txn
        let db : I.DB = 
            { db_env  = env
              db_data = dbData
              db_stow = dbStow
              db_rfct = dbRfct
              db_zero = dbZero
              db_rdlock = new I.ReadLock()
              db_ephtbl = Map.empty
              db_newrsc = Map.empty
              db_commit = List.empty
              db_trygc = true
              db_fini = dbFini
            }
        (new Thread(fun () -> I.dbThreadStart db)).Start()
        new DB(db)
    // TODO: 

    let load (path : string) (maxSizeMB : int) : DB = 
        withDir path (mkDB maxSizeMB)
                

    /// Obtain current reference count for a hash.
    ///   


(*

    , dupTX
    , readKey, readKeyDB
    , readKeys, readKeysDB
    , writeKey, assumeKey
    , loadRsc, loadRscDB
    , withRsc, withRscDB
    , stowRsc
    , clearRsc, clearRsc'
    , commit, commit_async
    , check
    , gcDB, gcDB_async
    , hashDeps
*)
  
    

