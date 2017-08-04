namespace Stowage
open System.IO
open System.Threading
open System.Threading.Tasks
open System.Security
open Data.ByteString
open System.Runtime.InteropServices

/// Stowage Database (abstract)
///
/// Stowage is a key-value database that features garbage collected
/// references between binaries via secure hashes. That is, a binary
/// value may contain secure hashes (cf. Stowage.Hash), and we can 
/// look up binaries by secure hashes, and binaries that aren't rooted
/// will eventually be removed from the database.
///
/// A stowage database is convenient for working with larger than memory
/// data, especially in functional systems or where data persistence is
/// desired. Structure sharing (deduplication) is implicit for binaries.
/// Stowage is implemented above memory-mapped embedded database LMDB.
///
/// Security Notes: Resource secure hashes should be treated as secure
/// read capabilities - i.e. don't leak them, but also don't hesitate to
/// provide the data to anyone who can provide the hash. Stowage DB is
/// designed to resist timing attacks that leak resource hashes. Keys
/// are not protected, and should not embed sensitive information. Any
/// security for key-value lookups should be provided by the client.
/// 
/// Other Notes: Disposing of the database will simply halt the writer
/// thread. The database isn't fully closed unless Finalize'd().
[< Struct >]
type DB =
    val internal Impl : I.DB
    internal new(dbImpl : I.DB) = { Impl = dbImpl }

/// Transaction object
///
/// A Stowage transaction is simply a stateful object that tracks
/// reads, writes, and stowage of resources to simplify a lot of 
/// the surrounding operations. Upon commit, `atomicUpdateDB` is
/// performed. Upon disposal (or finalization), ephemeral roots 
/// for new secure hash resources will be automatically decref'd. 
///
/// An important consideration is that snapshot isolation is not
/// guaranteed. Transactions may read inconsistent data. Example:
///
///         Alice        Bob
///         Reads A
///                      Updates A,B
///         Reads B
///
/// In this case, Alice will read B inconsistent with A. Further, if
/// secure hash resources were previously rooted by A, they might be
/// GC'd concurrently with Alice's attempt to read them, causing some
/// loadRsc operations to fail non-deterministically.
///
/// This issue can be mitigated by reading multiple keys together or
/// by providing external concurrency control for contended keys. If
/// clients can avoid conflicts naturally, that would perform best.
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



[< AutoOpen >]
module API =

    /// Open or Create database in current directory.
    ///
    /// Note: Stowage DB assumes exclusive control by single process.
    /// A simple .lock file is used to help resist accidents. Client
    /// should avoid using Stowage with networked filesystems.
    let openDB (path : string) (maxSizeMB : int) : DB = 
        Directory.CreateDirectory(path) |> ignore
        DB (I.openDB path maxSizeMB)

    /// Close database for graceful shutdown.
    ///
    /// The normal use case for Stowage DB is to run until crash. But
    /// graceful shutdown is an option. The caller must ensure there
    /// are no concurrent operations involving the DB. This will wait
    /// for a final write and sync then properly shutdown.
    let closeDB (db : DB) : unit = I.closeDB db.Impl

    /// Read value associated with a key in the DB.
    ///
    /// Every key has a value, defaulting to the empty string. Reads
    /// are always atomic, i.e. you'll never read a partial value.
    let readKeyDB (db : DB) (k : Key) : Val =
        I.withRTX db.Impl (fun rtx -> 
            I.dbReadKey db.Impl rtx k)

    /// Read multiple keyed values from DB. 
    ///
    /// Guarantees snapshot consistency for reading multiple elements.
    /// That is, it's atomic for the full array of keys. 
    let readKeysDB (db : DB) (ks : Key[]) : Val[] =
        I.withRTX db.Impl (fun rtx -> 
            Array.map (I.dbReadKey db.Impl rtx) ks)

    /// Find first key (if any) for which associated value doesn't match.
    /// Note: This doesn't account for concurrent or asynchronous writes.
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
    /// overheads among concurrent writers. Individual writes are thus
    /// relatively lightweight.
    ///
    /// In case of conflict, the earliest commit within the batch will
    /// succeed. Thus, progress is guaranteed. But fairness is not: it
    /// is possible to compute an update many times without success if
    /// a key is under heavy contention. Clients should control access
    /// to high-contention keys, e.g. using locks or channels.
    let atomicUpdateDB_async (db : DB) (reads : KVMap) (writes : KVMap) : Task<bool> =
        // reads are validated by the writer, but sanitize writes immediately
        let validKV k v = isValidKey k && isValidVal v
        let validWS = Map.forall validKV writes
        if not validWS then invalidArg "writes" "invalid write" else
        let tcs = new TaskCompletionSource<bool>()
        I.dbCommit db.Impl (reads, writes, tcs)
        tcs.Task

    /// Atomic compare and update (synchronous).
    let inline atomicUpdateDB (db : DB) (reads : KVMap) (writes : KVMap) : bool =
        (atomicUpdateDB_async db reads writes).Result

    /// Blind Writes.
    /// 
    /// Blind writes won't conflict with any other update, but it's left
    /// to client layers to provide some form of concurrency control. 
    /// These writes are thin wrappers around atomicUpdateDB. 
    let inline writeKeyDB_async (db : DB) (k : Key) (v : Val) : Task<bool> =
        atomicUpdateDB_async db Map.empty (Map.add k v Map.empty)

    let inline writeKeyDB (db : DB) (k : Key) (v : Val) : unit =
        let r = atomicUpdateDB db Map.empty (Map.add k v Map.empty)
        assert(r)

    let inline writeKeysDB_async (db : DB) (writes : KVMap) : Task<bool> =
        atomicUpdateDB_async db Map.empty writes

    let inline writeKeysDB (db : DB) (writes : KVMap) : unit =
        let r = atomicUpdateDB db Map.empty writes
        assert(r)

    /// Synchronize the Database.
    /// 
    /// This simply waits for all prior writes and updates to complete.
    /// This is a convenient operation for graceful shutdown, or as a
    /// memory barrier of sorts in context of asynchronous writes. 
    let inline syncDB (db : DB) : unit = 
        let r = atomicUpdateDB db (Map.empty) (Map.empty)
        assert(r)
        ()

    // lookup new resource in DB
    let inline private findNewRsc (db : DB) (h : RscHash) : Val option =
        // note: I'm assuming atomic reads for reference variables. This
        // was part of the C# language spec but it should hold for F#.
        match Map.tryFind (I.rscStowKey h) (db.Impl.db_newrsc) with
        | Some(struct(_,v)) -> Some v
        | None -> None

    /// Access a secure hash resource from the Database
    ///
    /// A Stowage database contains a set of binary values that are
    /// referenced by secure hash. If the resource is not known, the
    /// database returns None. Secure hash resources will be garbage
    /// collected if not rooted by the key-value layer or ephemeral
    /// reference count, so developers cannot assume availability of
    /// the resource without careful management of roots. 
    let loadRscDB (db : DB) (h : RscHash) : Val option =
        let newRsc = findNewRsc db h
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
        match findNewRsc db h with
        | None -> I.withRTX db.Impl (fun rtx ->
            match I.dbGetRscZC db.Impl rtx h with
            | None -> None
            | Some v -> Some (action v.data (int v.size)))
        | Some v -> withPinnedBytes v (fun vaddr ->
            Some (action vaddr v.Length))

    /// Add a resource to the DB.
    ///
    /// Stowed resources are moved to disk, referenced by the returned
    /// secure hash, and accessed using loadRscDB. Using stowage can be
    /// a flexible basis for working with larger than memory data.
    ///
    /// Resources are garbage collected. To prevent immediate GC of the
    /// newly stowed resource, stowRscDB performs an implicit, atomic 
    /// increfRscDB. The client is responsible for later decrefRscDB.
    ///
    /// Note: The current implementation writes all new resources to
    /// disk, even if they'll soon be GC'd. For efficiency, clients 
    /// should avoid stowage of resources that would soon be GC'd.
    let stowRscDB (db : DB) (v : Val) : RscHash =
        if not (isValidVal v) then invalidArg "v" "value too big" else
        let h = Hash.hash v
        ignore <| I.dbStow (db.Impl) h v
        h
    
    /// Ephemeral Roots via Reference Counting
    ///
    /// The stowage DB's key-value layer provides a simple basis for
    /// persistent roots, but that leaves a requirement to track hashes
    /// in ephemeral process memory. Stowage provides a simple reference 
    /// counting model to the client, such that a decrefRscDB may be run
    /// upon a Finalize() or Dispose().
    ///
    /// The current implementation is not precise. Resources may share a
    /// reference counter, resulting in false positives without risk of
    /// false negatives. But this does mean you cannot ask for the count
    /// for a specific resource.
    let increfRscDB (db : DB) (h : RscHash) : unit =
        I.dbIncEph (db.Impl) 1L (I.rscEphID h)

    let decrefRscDB (db : DB) (h : RscHash) : unit =
        I.dbDecEph (db.Impl) 1L (I.rscEphID h)

    // idea: I could potentially introduce a notion of `Stowed<T>` nodes
    // that may hold a cached representation of the type or eventually be
    // moved into the DB after some timeout, perhaps leveraging weak refs.

    /// Iterate through blocks of keys within the DB.
    ///
    /// This returns a subset of keys with non-empty values from the DB
    /// lexicographically following a given key, or from the first key 
    /// if the previous key is None. 
    let discoverKeysDB (db : DB) (kPrev : Key option) (nMax : int) : Key[] =
        let (kMin,validKey) =
            match kPrev with
            | None -> (Data.ByteString.empty, true)
            | Some k -> (k, isValidKey k)
        if not validKey then invalidArg "kPrev" "invalid key" else
        I.withRTX db.Impl (fun rtx -> 
            Stowage.LMDB.mdb_slice_keys rtx (db.Impl.db_data) kMin nMax)

    /// Check if the value associated with a key is non-empty.
    let containsKeyDB (db : DB) (k : Key) : bool =
        I.withRTX db.Impl (fun rtx ->
            let v = I.dbReadKeyZC (db.Impl) rtx k
            (0un <> v.size))


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
    let inline commit (tx:TX) : bool = 
        (commit_async tx).Result

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
    let inline checkpoint (tx:TX) : bool = 
        (checkpoint_async tx).Result

    let private readKeyOld (tx:TX) (k:Key) : Val option =
        let wv = Map.tryFind k tx.ws
        if Option.isSome wv then wv else
        Map.tryFind k tx.rd

    let private readKeyNew (tx:TX) rtx (k:Key) : Val =
        let v = I.dbReadKey tx.db rtx k
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
    let inline loadRsc (tx:TX) (h:RscHash) : Val option = 
        loadRscDB tx.DB h
    
    /// zero-copy access to secure hash resource (see unsafeWithRscDB)
    let inline unsafeWithRsc (tx:TX) (h:RscHash) (action : nativeint -> int -> 'x) : 'x option =
        unsafeWithRscDB tx.DB h action

    /// Stow a value as a secure hash resource.
    ///
    /// This behaves as stowRscDB, except the decref will be handled when
    /// the TX is later disposed or finalized. The assumption is that any
    /// newly stowed resources should be rooted by TX commit.
    ///
    /// To hold the resource beyond the lifespan of the transaction, call
    /// `increfRscDB` explicitly on the returned resource hash. Or use the
    /// `stowRscDB` method instead.
    let stowRsc (tx:TX) (v:Val) : RscHash =
        let h = Stowage.Hash.hash v
        let k = I.dbStow tx.db h v
        tx.eph <- I.ephInc tx.eph k 1L
        h
   

