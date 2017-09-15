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
/// look up binaries by secure hashes. Binaries that are not rooted
/// at the key-value layer or with ephemeral reference counting will
/// eventually be removed from the database.
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
/// In this case, Alice will read B inconsistent with A. This will
/// be caught upon commit, but before then the inconsistency may 
/// be an issue. This can be mitigated by reading keys together or
/// by external concurrency control for keys under contention.
///
/// Note: Transactional reads will scan the value for resources and
/// protect ephemeral roots. Thus, even if Alice's view of key A is
/// outdated, Alice can readily process deep values. 
type TX =
    val         internal db : I.DB
    val mutable internal rd : KVMap
    val mutable internal ws : KVMap
    val mutable internal eph : I.EphRoots
    new (db : DB) =
        { db = db.Impl
          rd = BTree.empty
          ws = BTree.empty
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


/// Exception for missing resources.
exception MissingRsc of DB * RscHash


[< AutoOpen >]
module API =

    /// Open or Create database in current directory.
    ///
    /// Note: Stowage DB assumes exclusive control by single process.
    /// A simple .lock file is used to help resist accidents. Client
    /// should avoid using Stowage with networked filesystems.
    let openDB (path : string) (maxSizeMB : int) : DB = 
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
    /// This is a shallow read; contrast readKeyDeepDB.
    let readKeyDB (db : DB) (k : Key) : Val =
        I.withRTX db.Impl (fun rtx -> 
            I.dbReadKey db.Impl rtx k)

    /// Read multiple keyed values from DB. 
    ///
    /// Guarantees snapshot consistency for reading multiple elements.
    /// That is, it's atomic for the full array of keys. Like readKeyDB,
    /// this is a shallow operation.
    let readKeysDB (db : DB) (ks : Key[]) : Val[] =
        I.withRTX db.Impl (fun rtx -> 
            Array.map (I.dbReadKey db.Impl rtx) ks)

    /// Find first key (if any) for which associated value doesn't match.
    /// Note: This doesn't account for concurrent or asynchronous writes.
    let testReadAssumptions (db : DB) (reads : KVMap) : (Key option) =
        if BTree.isEmpty reads then None else
        I.withRTX db.Impl (fun rtx -> 
            let kvOpt = I.findInvalidRead db.Impl rtx BTree.empty reads
            Option.map fst kvOpt)

    /// verify that all read assumptions are currently valid
    let inline verifyReadAssumptions (db : DB) (reads : KVMap) : bool =
        Option.isNone (testReadAssumptions db reads)

    /// Atomic database compare and update (asynchronous)
    /// 
    /// This delivers read assumptions and writes to a writer thread.
    /// The writer will compare the read assumptions with the database
    /// and, if they are valid, will perform the writes. The result is
    /// true only if all reads match and after writes are successfully 
    /// synchronized to disk.
    ///
    /// The writer will tend to batch updates that are provided around
    /// the same time, i.e. anything provided while the writer was busy
    /// with the prior batch. This helps amortize disk synchronization
    /// overheads among concurrent writers. Individual writes are thus
    /// relatively lightweight.
    ///
    /// In case of conflict, it's first commit wins within each batch.
    /// Thus, progress is guaranteed, and it's also possible to write
    /// multiple updates each assuming success of prior writes (like 
    /// a checkpointing commit). But fairness is not guaranteed. If a
    /// key is under contention, try external concurrency control.
    let atomicUpdateDB_async (db : DB) (reads : KVMap) (writes : KVMap) : Task<bool> =
        // reads are validated by the writer, but sanitize writes immediately
        let validKV k v = isValidKey k && isValidVal v
        let validWS = BTree.forall validKV writes
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
        atomicUpdateDB_async db BTree.empty (BTree.singleton k v)

    let inline writeKeyDB (db : DB) (k : Key) (v : Val) : unit =
        let r = atomicUpdateDB db BTree.empty (BTree.singleton k v)
        assert(r)

    let inline writeKeysDB_async (db : DB) (writes : KVMap) : Task<bool> =
        atomicUpdateDB_async db BTree.empty writes

    let inline writeKeysDB (db : DB) (writes : KVMap) : unit =
        let r = atomicUpdateDB db BTree.empty writes
        assert(r)

    /// Wait for all prior writes to complete.
    let inline syncDB (db : DB) : unit = 
        let r = atomicUpdateDB db (BTree.empty) (BTree.empty)
        assert(r)

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
    let tryLoadRscDB (db : DB) (h : RscHash) : Val option =
        let newRsc = findNewRsc db h
        if Option.isSome newRsc then newRsc else
        I.withRTX db.Impl (fun rtx -> I.dbGetRsc db.Impl rtx h) 

    let inline loadRscDB db h =
        match tryLoadRscDB db h with
        | None -> raise (MissingRsc(db,h))
        | Some v -> v

    /// Zero-copy access to a secure hash resource from the DB.
    ///
    /// This is possible leveraging the memory-mapped database, LMDB.
    /// However, Stowage doesn't use LMDB locking, and any long-lived 
    /// reader may block the writer. So this should only be used if 
    /// it can be guaranteed that the read is short-lived. Also, be
    /// careful to not write to the resource, which could corrupt the
    /// database.
    ///
    /// A potential use case is for indexed access to large resources.
    let unsafeWithRscDB (db : DB) (h : RscHash) (action : nativeint -> int -> 'x) : 'x option =
        match findNewRsc db h with
        | None -> I.withRTX db.Impl (fun rtx ->
            match I.dbGetRscZC db.Impl rtx h with
            | None -> None
            | Some v -> Some (action v.data (int v.size)))
        | Some v -> BS.withPinnedBytes v (fun vaddr ->
            Some (action vaddr v.Length))

    /// Check whether a resource is known to the DB.
    let hasRscDB (db : DB) (h : RscHash) : bool =
        if Option.isSome (findNewRsc db h) then true else
        I.withRTX db.Impl (fun rtx ->
            Option.isSome (I.dbGetRscZC db.Impl rtx h))

    /// Add a resource to the DB.
    ///
    /// Stowed resources are moved to disk, referenced by the returned
    /// secure hash, and accessed using loadRscDB. Using stowage can be
    /// a flexible basis for working with larger than memory data.
    ///
    /// Resources are garbage collected. To prevent immediate GC of the
    /// newly stowed resource, stowRscDB performs an implicit, atomic 
    /// increfRscDB. The client is responsible for later decrefRscDB.
    /// (Consider use of the `Rsc` wrapper.)
    ///
    /// Note: The current implementation writes all new resources to
    /// disk, even if they could be immediately GC'd. For efficiency,
    /// clients should avoid stowage of resources that are soon GC'd.
    let stowRscDB (db : DB) (v : Val) : RscHash =
        if not (isValidVal v) then invalidArg "v" "value too big" else
        let h = Hash.hash v
        ignore <| I.dbStow (db.Impl) h v
        h
    
    /// Ephemeral Roots via Reference Counting
    ///
    /// The stowage DB's key-value layer provides a simple basis for
    /// persistent roots. To control GC for resources referenced only
    /// in ephemeral process memory, the DB also maintains a table to
    /// track in-memory reference counts.
    ///
    /// A client may explicitly incref/decref a resource hash. Also,
    /// Stowage can atomically incref during a read to prevent GC of
    /// resources read (cf readKeyDeepDB). TX manages this behavior
    /// implicitly, accepting a minor overhead per read.
    ///
    /// Note: It is safe to incref a resource unknown to the DB or to
    /// decref after closing the DB. This doesn't touch the LMDB layer.
    /// However, precision is not guaranteed, e.g. our ephemeral roots
    /// may be implemented by a counting bloom filter or similar that
    /// permits false positives when checking if resources are rooted.
    let increfRscDB (db : DB) (h : RscHash) : unit =
        I.dbIncEph (db.Impl) 1L (I.rscEphID h)

    let decrefRscDB (db : DB) (h : RscHash) : unit =
        I.dbDecEph (db.Impl) 1L (I.rscEphID h)

    // utility
    let private valEphUpd : I.EphRoots -> Val -> I.EphRoots =
        scanHashDeps (fun e h -> I.ephInc e (I.rscEphID h) 1L)
    let private valEphRoots (v : Val) : I.EphRoots = valEphUpd Map.empty v

    /// Ephemeral Roots for full Values
    /// 
    /// This essentially performs increfRscDB (or decrefRscDB) for
    /// every resource hash found in a value (cf scanHashDeps). This
    /// is mostly intended for use with readKeyDeepDB, which requires
    /// decrefValDeps after you're done with the value.
    let increfValDeps (db : DB) (v : Val) : unit =
        if(v.Length >= rscHashLen)
            then I.dbAddEphRoots (db.Impl) (valEphRoots v)

    let decrefValDeps (db : DB) (v : Val) : unit =
        if(v.Length >= rscHashLen)
            then I.dbRemEphRoots (db.Impl) (valEphRoots v)

        // TODO: performance optimizations for ephemeral roots

    /// Atomic Read and Incref.
    ///
    /// This efficiently performs readKeyDB and increfValDeps as one
    /// atomic operation, which can simplify reasoning about concurrent
    /// GC when performing read-only operations on the DB. In practice,
    /// it's likely simpler and more convenient to use TX for deep reads.
    let readKeyDeepDB (db : DB) (k : Key) : Val =
        I.withRTX db.Impl (fun rtx -> 
            let v = I.dbReadKey db.Impl rtx k
            increfValDeps db v
            v)

    /// Atomic read and incref for multiple keys.
    let readKeysDeepDB (db : DB) (ks : Key[]) : Val[] =
        I.withRTX db.Impl (fun rtx -> 
            let vs = Array.map (I.dbReadKey db.Impl rtx) ks
            Array.iter (increfValDeps db) vs
            vs)

    /// Iterate through blocks of keys within the DB.
    ///
    /// This returns a subset of keys with non-empty values from the DB
    /// lexicographically following a given key, or from the first key 
    /// if the previous key is None. 
    let discoverKeysDB (db : DB) (kPrev : Key option) (nMax : int) : Key[] =
        let (kMin,validKey) =
            match kPrev with
            | None -> (BS.empty, true)
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

    /// Assume writes have completed successfully.
    ///
    /// Writes are moved to read assumptions. Ephemeral stowage roots
    /// are released excepting resources referenced from the read set.
    /// Use together with commit to model checkpointing transactions.
    let assumeWrites (tx:TX) : unit =
        tx.rd <- BTree.fold (fun m k v -> BTree.add k v m) tx.rd tx.ws
        tx.ws <- BTree.empty
        let eph' = BTree.fold (fun e _ v -> valEphUpd e v) Map.empty tx.rd
        I.dbAddEphRoots tx.db eph'
        I.dbRemEphRoots tx.db tx.eph
        tx.eph <- eph'

    /// Checkpoint a transaction (asynchronous). 
    ///  essentially commit_async + assumeWrites
    let inline checkpoint_async (tx:TX) : Task<bool> =
        let result = commit_async tx
        assumeWrites tx
        result

    /// Checkpoint a transaction (synchronous).
    ///  essentially commit + assumeWrites if commit succeeds
    let inline checkpoint (tx:TX) : bool = 
        let result = commit tx
        if result then assumeWrites tx
        result

    let private readKeyOld (tx:TX) (k:Key) : Val option =
        let wv = BTree.tryFind k tx.ws
        if Option.isSome wv then wv else
        BTree.tryFind k tx.rd

    let private txAddRead (tx:TX) (k:Key) (v:Val) : unit =
        tx.rd <- BTree.add k v tx.rd 
        let eu = valEphRoots v
        I.dbAddEphRoots (tx.db) eu
        tx.eph <- I.ephAdd eu tx.eph

    let private readKeyNew (tx:TX) rtx (k:Key) : Val =
        let v = I.dbReadKey tx.db rtx k
        txAddRead tx k v
        v

    /// Read value associated with key via TX.
    ///
    /// If the TX assumes a value due to prior read or write, that
    /// value is returned. Otherwise, this will access the DB. Any
    /// resources referenced from the value will be protected by
    /// the transaction (similar to readKeyDeepDB). 
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
    /// This updates the TX as if a value for a specific key were
    /// read. If the key has already been read and has a different
    /// value than assumed, this raises InvalidOperationException. 
    /// Usually, assumptions should be provided before any reads.
    let assumeKey (tx:TX) (k:Key) (v:Val) : unit =
        if not (isValidKey k) then invalidArg "k" "invalid key" else
        match BTree.tryFind k tx.rd with
          | None    -> txAddRead tx k v
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
        tx.ws <- BTree.add k v tx.ws

    /// load a secure hash resource into memory (see loadRscDB)
    let inline tryLoadRsc (tx:TX) (h:RscHash) : Val option = tryLoadRscDB (tx.DB) h
    let inline loadRsc (tx:TX) (h:RscHash) : Val = loadRscDB (tx.DB) h

    /// Stow a value as a secure hash resource.
    ///
    /// This behaves as stowRscDB, except the decref will be handled when
    /// the TX is later disposed or finalized, or upon `assumeWrites` if 
    /// a resource is not rooted in the transaction data. Resources formed
    /// for a transaction should be committed with it.
    let stowRsc (tx:TX) (v:Val) : RscHash =
        let h = Stowage.Hash.hash v
        let k = I.dbStow tx.db h v
        tx.eph <- I.ephInc tx.eph k 1L
        h


