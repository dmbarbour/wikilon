namespace Stowage
open System.IO
open System.Threading
open System.Threading.Tasks
open Data.ByteString
open Stowage.Internal.LMDB

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
/// easy to shard.
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
        let private stowKeyLen = Hash.validHashLen / 2
        type StowKey = ByteString // of stowKeyLen

        let rscHashToStowKey (rsc : RscHash) : StowKey =
            assert(Hash.validHashLen = rsc.Length)
            Data.ByteString.take stowKeyLen rsc

        // I don't favor use of F# maps, but they're convenient to
        // help get started more swiftly. Later I might replace with
        // a CritBitTree, suitable for larger keys.
        type Stowage = Map<RscHash,ByteString>      // recent stowage requests
        type RC = nativeint                         // reference count type
        type EphTbl = Map<StowKey, RC>              // ephemeral roots
        type KVMap = Map<ByteString, ByteString>    // key-value data (read or write)

        // A commit operation consists of:
        //  the set of values read or assumed
        //  the set of values written 
        //  a resource for async success/failure
        type AsyncCommitResult = TaskCompletionSource<bool>
        type Commit = (KVMap * KVMap * AsyncCommitResult)

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
                db_lock : FileStream    
                db_env  : MDB_env     
                db_data : MDB_dbi     // user string -> data
                db_stow : MDB_dbi     // secure hash -> data
                db_rfct : MDB_dbi     // hashes with refct > 0
                db_zero : MDB_dbi     // hashes with zero refct

                mutable db_rdlock : ReadLock        // current read-lock (updated per frame).
                mutable db_ephtbl : EphTbl          // ephemeral hash roots
                mutable db_newrsc : Stowage         // pending stowage requests
                mutable db_commit : Commit list     // pending commit requests
            }
            // for graceful shutdown
            override db.Finalize() =
                mdb_env_sync db.db_env
                mdb_env_close db.db_env
                db.db_lock.Dispose()

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


        // Perform operation while holding read lock.
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
        let inline withReadLock (db : DB) (operation : unit -> 'x) : 'x =
                let rdlock = lock db (fun () -> 
                        // lock prevents advanceReaderFrame during Acquire
                        db.db_rdlock.Acquire() 
                        db.db_rdlock)
                try operation ()
                finally rdlock.Release()

        let advanceReaderFrame (db : DB) : ReadLock = lock db (fun () ->
                let oldFrame = db.db_rdlock
                db.db_rdlock <- new ReadLock()
                oldFrame)

        // utilities to work with ephemeron tables
        let ephUpd (etb : EphTbl) (k : StowKey) (rcu : RC) : EphTbl =
            assert((0n <> rcu) && (stowKeyLen = k.Length))
            match Map.tryFind k etb with
              | None -> Map.add k rcu etb
              | Some rc0 -> 
                    let rc' = (rcu + rc0)
                    if (0n = rc') then Map.remove k etb else
                    Map.add k rc' etb

        let ephMerge (upd : EphTbl) (etb0 : EphTbl) : EphTbl =
            Map.fold (fun etb k rcu -> ephUpd etb k rcu) etb0 upd

        let ephClear (upd : EphTbl) (etb0 : EphTbl) : EphTbl =
            Map.fold (fun etb k rcu -> ephUpd etb k (-rcu)) etb0 upd

        let inline dbUpdEphTbl (db : DB) (fn : EphTbl -> EphTbl) : unit =
            lock db (fun () -> (db.db_ephtbl <- fn db.db_ephtbl))

        let inline dbInsEph (db : DB) (k : StowKey) : unit =
            dbUpdEphTbl db (fun etb -> ephUpd etb k 1n) 

        let inline dbMergeEphTbl (db : DB) (upd : EphTbl) : unit =
            if(not (Map.isEmpty upd)) then dbUpdEphTbl db (ephMerge upd)

        let inline dbClearEphTbl (db : DB) (upd : EphTbl) : unit =
            if(not (Map.isEmpty upd)) then dbUpdEphTbl db (ephClear upd)

        type TX =
            {   tx_db           : DB
                mutable tx_rd   : KVMap
                mutable tx_ws   : KVMap
                mutable tx_eph  : EphTbl
            }
            member private tx.ClearEph() : unit = 
                dbClearEphTbl (tx.tx_db) (tx.tx_eph)
                tx.tx_eph <- Map.empty
            override tx.Finalize() = tx.ClearEph()
            interface System.IDisposable with
                member tx.Dispose() = 
                    tx.ClearEph()
                    System.GC.SuppressFinalize tx

        let newTX (db : DB) : TX =
            { tx_db = db 
              tx_rd = Map.empty
              tx_ws = Map.empty
              tx_eph = Map.empty
            }

        let dupTX (tx : TX) : TX =
            dbMergeEphTbl (tx.tx_db) (tx.tx_eph)
            { tx_db = tx.tx_db
              tx_rd = tx.tx_rd
              tx_ws = tx.tx_ws
              tx_eph = tx.tx_eph 
            }

    /// Stowage database object
    ///
    /// Operations on the DB are multi-thread safe, and non-blocking
    /// unless otherwise stated.
    [< Struct >]
    type DB =
        val internal Impl : Internal.DB
        internal new(dbImpl : Internal.DB) = { Impl = dbImpl }

    /// Transaction object
    ///
    /// A TX uses some mutable state without locking, so is safe only
    /// if used from one thread at a time. A transaction doubles as an
    /// ephemeral root for stowage resources.
    [< Struct >]
    type TX =
        val internal Impl : Internal.TX
        member tx.DB with get () = DB (tx.Impl.tx_db)
        internal new (txImpl : Internal.TX) = { Impl = txImpl }
        interface System.IDisposable with
            member tx.Dispose() = 
                (tx.Impl :> System.IDisposable).Dispose()

    /// Create a fresh transaction on the database.
    let newTX (db : DB) : TX = new TX(Internal.newTX db.Impl)

    /// Deep-clone a transaction.
    ///  Significantly, this copies the ephemeral root set.
    let dupTX (tx : TX) : TX = new TX (Internal.dupTX tx.Impl)

    /// Initiate background GC on the database (Asynchronous)
    ///
    /// The writer does incremental GC with every batch, so normally
    /// this explicit GC is unnecessary. But calling gcDB_init can
    /// force background GC when the writer is otherwise inactive.
    let gcDB_init (db : DB) : Task<bool> =
        let asyncResult = new TaskCompletionSource<bool>()
        Internal.dbCommit (db.Impl) (Map.empty,Map.empty,asyncResult)
        asyncResult.Task

    /// Synchronous GC. 
    let gcDB (db : DB) : unit =
        let t = gcDB_init db
        if not (t.Result) then failwith "Asynchronous Stowage GC failure"
   


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
        let txn = mdb_readwrite_txn_begin env
        let dbData = mdb_dbi_open txn "@" MDB_CREATE // root key-value
        let dbStow = mdb_dbi_open txn "$" MDB_CREATE // stowed resources
        let dbRfct = mdb_dbi_open txn "#" MDB_CREATE // positive refcts
        let dbZero = mdb_dbi_open txn "0" MDB_CREATE // keys with zero refct
        mdb_txn_commit txn

        let db : Internal.DB = 
            { db_lock = lock
              db_env  = env
              db_data = dbData
              db_stow = dbStow
              db_rfct = dbRfct
              db_zero = dbZero
              db_rdlock = new Internal.ReadLock()
              db_ephtbl = Map.empty
              db_newrsc = Map.empty
              db_commit = List.empty
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
  
    

