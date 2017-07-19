namespace Stowage
open System.IO
open System.Threading
open System.Threading.Tasks
open Data.ByteString
open Stowage.Internal.LMDB
open Microsoft.FSharp.NativeInterop

#nowarn "9" // unsafe use of NativePtr

/// Stowage is a key-value database that features garbage collected
/// references between binaries via secure hashes. 
///
/// Stowage is implemented above LMDB, a memory-mapped B-tree. Stowage
/// transactions are optimistic and lightweight, held in memory until
/// commit. Non-conflicting concurrent writes are batched to amortize
/// overheads for synchronization to disk. Read-only actions never wait.
/// Blind writes never conflict.
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

    /// Scan a value for likely resource hash dependencies. This is 
    /// conservative, allowing false positives. Hashes within a value
    /// must be separated by non-hash characters (see Stowage.Hash).
    ///
    /// This function is used for conservative reference counting GC.
    let scanHashDeps (fn : 's -> RscHash -> 's) : 's -> Val -> 's =
        let rec loop s v = 
            let hv' = Data.ByteString.dropWhile (Hash.validHashByte >> not) v
            if Data.ByteString.isEmpty hv' then s else
            let (h,v') = Data.ByteString.span (Hash.validHashByte) hv'
            let s' = if (Hash.validHashLen = h.Length) then (fn s h) else s
            loop s' v'
        loop

    module internal Internal =

        // fragment of hash used for stowage keys
        let stowKeyLen = Hash.validHashLen / 2
        type StowKey = ByteString // of stowKeyLen

        // I don't favor use of F# maps, but they're convenient to
        // help get started more swiftly. Later I might replace with
        // a CritBitTree, suitable for larger keys.
        type KVMap = Map<ByteString, ByteString>    // key-value data (read or write)
        type Stowage = Map<RscHash,ByteString>      // recent stowage requests

        // An ephemeral roots table tracks a conservative set of elements
        // that we must preserve even if they lack persistent references.
        // This is represented by a map of hashes to reference counts.
        type EphID = uint64                         // via FNV-1a hash
        type RC = nativeint                         // reference count type
        type EphRoots = Map<EphID, RC>              // ephemeral roots

        let fnv_prime = 1099511628211UL
        let fnv_offset_basis = 14695981039346656037UL
        let fnv_accum h b = ((h ^^^ (uint64 b)) * fnv_prime)
        let rscEphID (rsc : RscHash) : EphID =
            assert(Hash.validHashLen = rsc.Length)
            let stowKey = Data.ByteString.take stowKeyLen rsc
            Data.ByteString.fold fnv_accum fnv_offset_basis stowKey
        let ptrEphID (p : nativeptr<byte>) : EphID =
            let mutable h = fnv_offset_basis
            for ix = 0 to (stowKeyLen - 1) do
                let b = NativePtr.get p ix
                h <- fnv_accum h b
            h

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
                db_data : MDB_dbi     // user string -> data
                db_stow : MDB_dbi     // secure hash -> data
                db_rfct : MDB_dbi     // hashes with refct > 0
                db_zero : MDB_dbi     // hashes with zero refct

                mutable db_rdlock : ReadLock        // current read-lock (updated per frame).
                mutable db_ephtbl : EphRoots          // ephemeral hash roots
                mutable db_newrsc : Stowage         // pending stowage requests
                mutable db_commit : Commit list     // pending commit requests

                db_fini : System.Object  // extra finalizer
            }

        // the DB will have a dedicated writer thread.
        // the DB object serves mutex and pulse/wait signal, via Monitor
        let dbCommit (db : DB) (c : Commit) : unit =
            lock db (fun () ->
                db.db_commit <- (c :: db.db_commit)
                Monitor.PulseAll(db))

        // add stowage to the newrsc pool.  
        let dbAddStowage (db : DB) (h : RscHash) (v : Val) : unit =
            assert (h.Length = Hash.validHashLen)
            lock db (fun () ->
                db.db_newrsc <- Map.add h v (db.db_newrsc)
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

        // utilities to work with ephemeron tables
        let ephInc (etb : EphRoots) (k : EphID) (rcu : RC) : EphRoots =
            match Map.tryFind k etb with
              | None -> Map.add k rcu etb
              | Some rc0 -> Map.add k (rcu + rc0) etb

        let ephDec (etb : EphRoots) (k : EphID) (rcu : RC) : EphRoots =
            match Map.tryFind k etb with
              | None -> failwith "negative refct"
              | Some rc0 ->
                    assert (rc0 >= rcu)
                    let rc' = rc0 - rcu
                    if (0n = rc') then Map.remove k etb
                                  else Map.add k rc' etb 

        let inline ephAdd (upd : EphRoots) (etb0 : EphRoots) : EphRoots =
            Map.fold ephInc etb0 upd

        let inline ephRem (upd : EphRoots) (etb0 : EphRoots) : EphRoots =
            Map.fold ephDec etb0 upd

        let inline dbUpdEphRoots (db : DB) (fn : EphRoots -> EphRoots) : unit =
            lock db (fun () -> (db.db_ephtbl <- fn db.db_ephtbl))

        let inline dbInsEph (db : DB) (k : EphID) : unit =
            dbUpdEphRoots db (fun etb -> ephInc etb k 1n) 

        let inline dbAddEphRoots (db : DB) (upd : EphRoots) : unit =
            if(not (Map.isEmpty upd)) then dbUpdEphRoots db (ephAdd upd)

        let inline dbRemEphRoots (db : DB) (upd : EphRoots) : unit =
            if(not (Map.isEmpty upd)) then dbUpdEphRoots db (ephRem upd)

    /// Stowage database object (abstract)
    [< Struct >]
    type DB =
        val internal Impl : Internal.DB
        internal new(dbImpl : Internal.DB) = { Impl = dbImpl }

    /// Transaction object
    ///
    /// A transaction tracks reads, writes, and ephemeral roots for
    /// allocated secure hash resources. Reads aren't guaranteed to
    /// be snapshot consistent (see readKeys), but commit will fail
    /// if any key has been updated between time of read and commit. 
    /// Hence, transactions that successfully commit are serializable.
    /// Isolation may be reduced explicitly (via assumeKey).
    ///
    /// Stowage transactions are optimistic. When conflicts occur,
    /// progress is guaranteed for at least one transaction. But the
    /// client should control access to high-contention keys, avoid
    /// conflict where possible.
    /// 
    /// Note: a TX is not thread safe. If used from multiple threads,
    /// the client must ensure exclusive access.
    type TX =
        val         internal db : Internal.DB
        val mutable internal rd : Internal.KVMap
        val mutable internal ws : Internal.KVMap
        val mutable internal eph : Internal.EphRoots
        new (db : DB) =
            { db = db.Impl
              rd = Map.empty
              ws = Map.empty
              eph = Map.empty
            }
        member tx.DB with get () = DB (tx.db)
        member private tx.ClearEphRoots() : unit =
            Internal.dbRemEphRoots (tx.db) (tx.eph)
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
        Internal.dbAddEphRoots (tx.db) (tx.eph)
        let clone = newTX (tx.DB)
        clone.eph <- tx.eph
        clone.rd <- tx.rd
        clone.ws <- tx.ws
        clone
    
    /// Initiate background GC of Resources
    ///
    /// The writer thread performs incremental GC with every batch.
    /// So, normally, you don't need to request GC. But it may be
    /// convenient in some contexts.
    let gcDB_async (db : DB) : unit = 
        lock (db.Impl) (fun () -> Monitor.PulseAll(db.Impl))
    let gcDB (db : DB) : unit =
        let tcs = new TaskCompletionSource<bool>()
        Internal.dbCommit (db.Impl) (Map.empty,Map.empty,tcs)
        ignore tcs.Task.Result

    let inline private readKeyDB' (db : Internal.DB) (txn : MDB_txn) (k : Key) : Val =
        let vopt = mdb_get txn (db.db_data) k
        defaultArg vopt (Data.ByteString.empty)

    /// Read a Key directly from DB.
    let readKeyDB (db : DB) (k : Key) : Val =
        Internal.withRTX db.Impl (fun txn -> 
            readKeyDB' db.Impl txn k)

    /// Read multiple keys from DB. Guarantees snapshot consistency.
    let readKeysDB (db : DB) (ks : Key list) : Val list =
        Internal.withRTX db.Impl (fun txn -> 
            List.map (readKeyDB' db.Impl txn) ks)

    // todo: read key (or keys) from TX

    /// Write a Key into the TX
    ///
    /// This is a trivial operation since it only writes the key into
    /// the current transaction. Only upon commit is the value written
    /// to the DB layer. Write the empty bytestring for deletion.
    ///
    /// Note: further reads on a written Key return the written value.
    let writeKey (tx : TX) (k : Key) (v : Val) : unit =
        assert (isValidKey k)
        tx.ws <- Map.add k v tx.ws

    /// Set the Read Assumption for a Key
    ///
    /// A read assumption is a value we verify has not changed upon
    /// commit. Normally, this is adjusted upon read and commit. But
    /// explicit manipulations may be useful to weaken isolation of
    /// transactions.
    let assumeKey (tx : TX) (k : Key) (vopt : Val option) : unit =
        match vopt with
          | None -> tx.rd <- Map.remove k tx.rd
          | Some v -> tx.rd <- Map.add k v tx.rd

    /// Diagnose a transaction.
    ///
    /// Return a list of Keys for which the read assumption is invalid.
    let check (tx : TX) : Key list =
        raise (System.NotImplementedException "todo: checkTX")

    /// Commit (asynchronous)
    ///
    /// This initiates commit of a transaction, sending the data to
    /// a background writer thread and immediately returning a Task
    /// that will complete concurrently. The Task.Result will be true
    /// if commit succeeds, false if it fails for any reason. 
    ///
    /// Transactions are batched and written together, modulo conflict.
    /// This amortizes the write overheads, enabling a large number of
    /// writes so long as they occur on different volumes of keys. It
    /// is left to clients to avoid conflict, e.g. by use of channels 
    /// to control access to high-contention keys. Even when conflicts
    /// occur, progress is guaranteed. 
    ///
    /// A transaction may be committed more than once. Doing so will
    /// essentially model checkpointing, where information committed
    /// successfully is not lost. With asynchronous commit, multiple
    /// checkpoints may frequently be batched together.
    ///
    /// Read-only transactions will be verified immediately, without
    /// waiting on the writer.
    ///    
    let commitAsync (tx : TX) : Task<bool> =
        raise (System.NotImplementedException "TODO: commit")

    /// Synchronous commit.
    let inline commit (tx : TX) : bool = (commitAsync tx).Result
   


(*
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

        let db : Internal.DB = 
            { db_env  = env
              db_data = dbData
              db_stow = dbStow
              db_rfct = dbRfct
              db_zero = dbZero
              db_rdlock = new Internal.ReadLock()
              db_ephtbl = Map.empty
              db_newrsc = Map.empty
              db_commit = List.empty
              db_fini = dbFini
            }
        // TODO: init writer thread
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
  
    

