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
[< AutoOpen >]
module API =

    /// Stowage database object (abstract)
    [< Struct >]
    type DB =
        val internal Impl : I.DB
        internal new(dbImpl : I.DB) = { Impl = dbImpl }

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

    /// Read value associated with a key in the DB.
    ///
    /// Every key has a value, defaulting to the empty string. Reads
    /// are always atomic, i.e. you'll never read a partial value.
    let readKeyDB (db : DB) (k : Key) : Val =
        I.withRTX db.Impl (fun rtx -> 
            I.readKeyDB db.Impl rtx k)

    /// Read multiple keyed values from DB. 
    ///
    /// Guarantees snapshot consistency for reading multiple elements.
    /// That is, it's atomic for the full array of keys. 
    let readKeysDB (db : DB) (ks : Key[]) : Val[] =
        I.withRTX db.Impl (fun rtx -> 
            Array.map (I.readKeyDB db.Impl rtx) ks)

    /// Find first key (if any) for which associated value doesn't match.
    /// Note: This doesn't account for concurrent writes.
    let testReadAssumptions (db : DB) (reads : KVMap) : (Key option) =
        if Map.isEmpty reads then None else
        I.withRTX db.Impl (fun rtx -> 
            I.findInvalidRead db.Impl rtx Map.empty reads)

    /// verify that all read assumptions are currently valid
    let inline verifyReadAssumptions (db : DB) (reads : KVMap) : bool =
        Option.isNone (testReadAssumptions db reads)

    /// Iterate through blocks of keys within the DB.
    ///
    /// This returns a block of up to a given number of keys following
    /// the given key lexicographically whose values are defined as 
    /// non-empty bytestrings. This can be used to browse a database.
    let iterKeysDB (db : DB) (k : Key) (n : int) : Key[]
        if(n < 1) then Array.empty else
        I.withRTX db.Impl (fun rtx -> I.dbIterKeys db.Impl rtx k n)


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
        I.withRTX db.Impl (fun rtx -> I.dbGetRsc db.Impl rtx h) 

    /// Zero-copy access to a secure hash resource from the DB.
    ///
    /// This is possible leveraging the memory-mapped database, LMDB.
    /// However, this is unsafe except for short-lived, read-only 
    /// actions on the data. Long-lived readers will eventually block
    /// the writer thread. Writing to the data at the nativeint will
    /// generally corrupt the database.
    ///
    /// A potential use case is for indexed access to large resources.
    let unsafeWithRscDB (db : DB) (h : RscHash) (action : nativeint -> int -> 'x) : 'x option =
        // note: I'm relying on atomic reads for reference variables.
        // This was asserted for C#, so I assume it's valid for .Net.
        match Map.tryFind h (db.Impl.db_newrsc) with
          | None -> I.withRTX db.Impl (fun rtx ->
                match I.dbGetRscZC db.Impl rtx h with
                  | None -> None
                  | Some v -> Some (action v.data (int v.size)))
          | Some v -> withPinnedBytes v (fun vaddr ->
                Some (action vaddr v.Length))


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
    let readKeys (tx:TX) (ks:Key[]) : Val[] =
        I.withRTX tx.db (fun rtx -> 
            let read k = 
                match readKeyOld tx k with
                  | Some v -> v
                  | None -> readKeyNew tx rtx k
            Array.map read ks)

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
        if not (isValidVal v) then invalidArg "v" "invalid value" else
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
  
    

