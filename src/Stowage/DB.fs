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
/// references between binaries, represented by secure hashes (see
/// Stowage.Hash). Secure hashes may be treated as secure capabilities
/// to read the data.
///
/// The motivation is to support developers in modeling very large,
/// first-class values that can readily be cached and shared in a
/// distributed system. The stowage layer can structured data that
/// is larger than memory, and can also serve as a virtual memory
/// model for purely functional computations.
///
/// GC of stowage resources integrates with .Net GC via explcit 
/// reference counting and careful use of .Net Finalizers. There
/// are some examples of this in the Stowage.Data package, which
/// provides smart pointers to Stowage (e.g. VRef, LVRef, CVRef).
/// Additionally, values at the key-value layer act as persistent
/// roots.
///
/// As a key-value database, Stowage is very simple and not heavily
/// optimized. The expectation is that root values should be small,
/// leveraging secure hashes to reference bulky data as needed. The
/// "transaction" model is a variation of compare-and-swap: we test
/// whether a given subset of keys has the values we expect, and if
/// so, we perform the requested update.
[< Struct >]
type DB =
    val internal Impl : I.DB
    internal new(dbImpl : I.DB) = { Impl = dbImpl }

/// Exception for missing resources, only when the resource is assumed
/// to be available (e.g. for 'load'). 
exception MissingRsc of DB * RscHash

module DB =

    /// Open or Create database in current directory.
    ///
    /// Note: Stowage DB assumes exclusive control by single process.
    /// A simple .lock file is used to help resist accidents. Client
    /// should avoid using Stowage with networked filesystems.
    let load (path : string) (maxSizeMB : int) : DB = 
        DB (I.openDB path maxSizeMB)

    /// Close database for graceful shutdown.
    ///
    /// The normal use case for Stowage DB is to run until crash. But
    /// graceful shutdown is an option. The caller must ensure there
    /// are no concurrent operations involving the DB. This will wait
    /// for a final write and sync then properly shutdown.
    let close (db : DB) : unit = I.closeDB db.Impl

    /// Read value associated with a key in the DB.
    ///
    /// Every key has a value, defaulting to the empty byte string.
    /// Consequently, writing the empty byte string is key deletion.
    /// Reads are atomic, i.e. you'll won't read a partial value.
    let readKey (db : DB) (k : Key) : Val =
        I.withRTX db.Impl (fun rtx -> 
            I.dbReadKey db.Impl rtx k)

    /// Atomic read of multiple keys from DB. 
    ///
    /// This read guarantees snapshot consistency, but this doesn't 
    /// guarantee the most up-to-date values. The values will probably
    /// be up-to-date if nobody has recently written them, or if the
    /// writer was synchronous and in the same thread.
    ///
    /// In this case we accept an array of keys, and the result is a
    /// fresh array mapping the values read.
    let readKeys (db : DB) (ks : Key[]) : Val[] =
        I.withRTX db.Impl (fun rtx -> 
            Array.map (I.dbReadKey db.Impl rtx) ks)

    /// Find first key (if any) for which associated value doesn't match.
    ///
    /// Like readKeys, this operation is atomic but does not guarantee
    /// the read assumptions are up-to-date. This is mostly useful for
    /// diagnostic efforts after commit failure.
    let testReadAssumptions (db : DB) (reads : KVMap) : (Key option) =
        if BTree.isEmpty reads then None else
        I.withRTX db.Impl (fun rtx -> 
            let wb = BTree.empty
            let kvOpt = I.findInvalidRead db.Impl rtx wb reads
            Option.map fst kvOpt)

    /// verify that all read assumptions are currently valid
    let inline verifyReadAssumptions (db : DB) (reads : KVMap) : bool =
        Option.isNone (testReadAssumptions db reads)

    /// Atomic database update
    /// 
    /// This writes a set of key-value pairs to the database contingent
    /// upon the database state matching a given set of read assumptions.
    /// This provides a simple basis for transactional updates, similar
    /// in structure to compare-and-swap.
    ///
    /// The update will return successfully only after the writes are
    /// committed and synchronized to disk. However, independent writes
    /// will tend to be batched together, amortizing the synchronization
    /// overheads. Writes are independent if they don't violate each
    /// other's read assumptions. 
    let update (db : DB) (rs : KVMap) (ws : KVMap) : bool =
        // reads are validated by the writer, but sanitize writes immediately
        let validKV k v = isValidKey k && isValidVal v
        let validWS = BTree.forall validKV ws
        if not validWS then invalidArg "writes" "invalid write request" else
        let tcs = new TaskCompletionSource<bool>()
        I.dbCommit db.Impl (rs, ws, tcs)
        tcs.Task.Result

    /// Blind write a batch of keys (synchronous)
    let inline writeKeys db ws =
        let r = update db (BTree.empty) ws
        assert(r)

    /// Blind write of key (synchronous)
    let inline writeKey db k v = writeKeys db (BTree.singleton k v)

    // lookup new resource in DB
    let inline private findNewRsc (db : DB) (h : RscHash) : Val option =
        match Map.tryFind (I.rscStowKey h) (db.Impl.db_newrsc) with
        | Some(struct(_,v)) -> Some v
        | None -> None

    /// Attempt to load a secure hash resource from the Database
    let tryLoadRsc (db : DB) (h : RscHash) : Val option =
        let newRsc = findNewRsc db h
        if Option.isSome newRsc then newRsc else
        I.withRTX db.Impl (fun rtx -> I.dbGetRsc db.Impl rtx h) 

    /// Load a resource or raise the MissingRsc exception.
    let inline loadRsc db h =
        match tryLoadRsc db h with
        | Some v -> v
        | None -> raise (MissingRsc(db,h))

    /// Test whether a resource is known to the DB, without copying.
    let hasRsc (db : DB) (h : RscHash) : bool =
        if Option.isSome (findNewRsc db h) then true else
        I.withRTX db.Impl (fun rtx ->
            Option.isSome (I.dbGetRscZC db.Impl rtx h))

    /// Add resource to the DB. 
    ///
    /// This tells the DB to record the binary, and returns the RscHash
    /// needed to later load the resource. The new RscHash is implicitly 
    /// incref'd (see increfRsc, decrefRsc). 
    ///
    /// Normally, you'll want to use this *indirectly* via VRef or LVRef
    /// from the Stowage.Data packages.
    let stowRsc (db : DB) (v : Val) : RscHash =
        if not (isValidVal v) then invalidArg "v" "value too large" else
        let h = Hash.hash v
        ignore <| I.dbStow (db.Impl) h v
        h
    
    /// Ephemeral Roots via Reference Counting
    ///
    /// Stowage DB's key-value layer provides persistent roots, but
    /// to track recently stowed resources, or to ensure stable data
    /// when reading keys that might be concurrently updated, we must
    /// track ephemeral references from .Net memory. This is achieved
    /// via explicit reference counting of hashes.
    ///
    /// Decref from finalizer is safe even if DB closed or finalized.
    let decrefRsc (db : DB) (h : RscHash) : unit =
        I.dbEphDec (db.Impl) (I.rscEphID h)

    let increfRsc (db : DB) (h : RscHash) : unit =
        I.dbEphInc (db.Impl) (I.rscEphID h)

    /// Ephemeral Roots for full Values
    /// 
    /// This essentially performs increfRsc or decrefRsc for every
    /// resource hash found in a value (as recognized by Stowage GC).
    let decrefValDeps (db : DB) (v : Val) : unit =
        iterHashDeps (fun h -> decrefRsc db h) v

    let increfValDeps (db : DB) (v : Val) : unit =
        iterHashDeps (fun h -> increfRsc db h) v

    /// Atomic Read and Incref.
    ///
    /// This composes readKey and increfValDeps as one atomic operation.
    /// Mostly, Discovered secure hash resources will be rooted in memory
    /// until the corresponding decrefValDeps operation.
    ///
    /// This simplifies reasoning about GC in contexts where the value
    /// might be written concurrently, especially read-only operations.
    let readKeyDeep (db : DB) (k : Key) : Val =
        I.withRTX db.Impl (fun rtx -> 
            let v = I.dbReadKey db.Impl rtx k
            increfValDeps db v
            v)

    /// Read multiple keys in one snapshot; simultaneous increfValDeps
    let readKeysDeep (db : DB) (ks : Key[]) : Val[] =
        I.withRTX db.Impl (fun rtx -> 
            let vs = Array.map (I.dbReadKey db.Impl rtx) ks
            Array.iter (increfValDeps db) vs
            vs)

    /// Iterate through blocks of keys within the DB.
    ///
    /// This returns a subset of keys with non-empty values from the DB
    /// lexicographically following a given key, or from the first key 
    /// if the previous key is None. You can use the last key in the 
    /// array to help search for more keys.
    let discoverKeys (db : DB) (kPrev : Key option) (nMax : int) : Key[] =
        let (kMin,validKey) =
            match kPrev with
            | None -> (BS.empty, true)
            | Some k -> (k, isValidKey k)
        if not validKey then invalidArg "kPrev" "invalid key" else
        I.withRTX db.Impl (fun rtx -> 
            Stowage.LMDB.mdb_slice_keys rtx (db.Impl.db_data) kMin nMax)

    /// Check whether the value associated with a key is non-empty.
    let containsKey (db : DB) (k : Key) : bool =
        I.withRTX db.Impl (fun rtx ->
            let v = I.dbReadKeyZC (db.Impl) rtx k
            (0un <> v.size))

