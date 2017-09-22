namespace Stowage
open System.IO
open System.Threading
open System.Threading.Tasks
open System.Security
open Data.ByteString
open System.Runtime.InteropServices

/// Stowage Database
///
/// Stowage is a key-value database that features garbage collected
/// references between binaries, represented by secure hashes (see
/// Stowage.Hash). Secure hashes are treated as secure capabilities
/// to read the data.
///
/// Secure hashes can reference persistent data structures that are
/// larger than memory, and even model full databases as first-class
/// values (e.g. by modeling a critbit tree or LSM-tree value, see
/// package Stowage.Data). Structure sharing is implicit. And this 
/// may double as a purely functional variant on virtual memory.
///
/// The key-value layer serves as a durable root set for GC. Values
/// accessed by key can survive process crash or power failure, and
/// resources referenced from those values are also preserved. For 
/// ephemeral roots, in-memory reference counting is supported.
///
/// The expectation is that keys have small values, no more than a
/// few hundred bytes, but those values will use secure hashes to
/// reference large data structures. This makes it easy to share 
/// structures on disk. 
///
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
    /// A simple lock file is used to resist accidents. The client
    /// should avoid using Stowage with networked filesystems.
    let load (path : string) (maxSizeMB : int) : DB = 
        DB (I.openDB path maxSizeMB)

    /// Close database for graceful shutdown.
    ///
    /// The normal use case for Stowage DB is to run until crash. But
    /// graceful shutdown is an option. The caller must ensure there
    /// are no concurrent operations involving the DB, excepting the
    /// possibility of `decrefRsc` calls. This will wait for a final
    /// write and sync then properly shutdown.
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
    /// This guarantees snapshot consistency, but it doesn't promise
    /// the values are the most up-to-date in context of asynchronous
    /// writes. Values are returned in new array corresponding to the
    /// index for the keys read.
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

    let inline private verifyKVElems (ws:KVMap) : bool = 
        let validKV k v = isValidKey k && isValidVal v
        BTree.forall validKV ws

    /// Atomic database update
    ///
    /// This creates an asynchronous task to write a set of key-value
    /// pairs (ws) contingent upon a set of read assumptions (rs). The
    /// writer atomically verifies read assumptions before performing
    /// any writes. This provides a simple foundation for optimistic
    /// transactional updates, lock-free compare-and-swap, etc.. The
    /// update returns successfully after writes are flushed to disk.
    ///
    /// This isn't spectacularly efficient, of course. But the idea is
    /// to keep values at keys relatively small by pushing most of the
    /// data into persistent data structures at the resource layer. If
    /// this assumption holds, then compare-and-swap is not expensive.
    /// Also, for keys under heavy contention, external synchronization 
    /// should be modeled.
    ///
    /// Updates for independent subsets of keys (where read assumptions
    /// aren't violated) will not conflict and may be batched together
    /// to help amortize disk synchronization overheads. Also, updates
    /// are always applied in the order received, and within each batch 
    /// only the final value for a key is written. So it is feasible to
    /// model asynchronous checkpointing transactions that take prior
    /// writes as future read assumptions.
    let updateAsync (db : DB) (rs : KVMap) (ws : KVMap) : Task<bool> =
        if not (verifyKVElems ws)
            then invalidArg "ws" "invalid write request"
        let tcs = new TaskCompletionSource<bool>()
        I.dbCommit db.Impl (rs, ws, tcs)
        tcs.Task

    /// Blind asynchronous write of multiple keys. Since this doesn't
    /// have any read conflicts, it should always succeed.
    let inline writeKeysAsync db ws = updateAsync db (BTree.empty) ws |> ignore

    /// Blind asynchronous write of a single key.
    let inline writeKeyAsync db k v = writeKeysAsync db (BTree.singleton k v)

    /// Synchronous update. Simply a synchronous update that immediately waits.
    let inline update db rs ws = (updateAsync db rs ws).Result

    /// Synchronously write a batch of keys
    let inline writeKeys db ws =
        let r = update db (BTree.empty) ws
        assert(r)

    /// Synchronous write of single key
    let inline writeKey db k v = writeKeys db (BTree.singleton k v)

    /// Wait for all pending asynchronous writes and updates to complete.
    let sync db = writeKeys db (BTree.empty)

    // lookup new resource in DB
    let inline private findNewRsc (db : DB) (h : RscHash) : Val option =
        match Map.tryFind (I.rscStowKey h) (db.Impl.newrsc) with
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
    /// In many cases, you'll want to use stowRsc indirectly via the
    /// Stowage.Data package's smart references (e.g. LVRef.stow).
    let stowRsc (db : DB) (v : Val) : RscHash =
        if not (isValidVal v) then invalidArg "v" "value too large" else
        I.dbStow (db.Impl) v
    
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
        db.Impl.ephtbl.Decref (I.rscEphID h)

    let increfRsc (db : DB) (h : RscHash) : unit =
        db.Impl.ephtbl.Incref (I.rscEphID h)

    /// Ephemeral Roots for full Values
    /// 
    /// This essentially performs increfRsc or decrefRsc for every
    /// resource hash found in a value (as recognized by Stowage GC).
    /// The primary use case is to call decrefValDeps after readKeyDeep
    let decrefValDeps (db : DB) (v : Val) : unit =
        iterHashDeps (fun h -> decrefRsc db h) v

    let increfValDeps (db : DB) (v : Val) : unit =
        iterHashDeps (fun h -> increfRsc db h) v

    /// Atomic Read and Incref.
    ///
    /// This composes readKey and increfValDeps as one atomic operation
    /// to simplify reasoning about concurrent GC. The client should use
    /// decrefValDeps when finished with the value.
    let readKeyDeep (db : DB) (k : Key) : Val =
        I.withRTX db.Impl (fun rtx -> 
            let v = I.dbReadKey db.Impl rtx k
            increfValDeps db v
            v)

    /// Snapshot consistency of readKeys + atomic incref of readKeyDeep
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
            Stowage.LMDB.mdb_slice_keys rtx (db.Impl.data) kMin nMax)

    /// Check whether the value associated with a key is non-empty.
    let containsKey (db : DB) (k : Key) : bool =
        I.withRTX db.Impl (fun rtx ->
            let v = I.dbReadKeyZC (db.Impl) rtx k
            (0un <> v.size))

