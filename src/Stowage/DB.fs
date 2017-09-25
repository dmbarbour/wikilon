namespace Stowage
open System.IO
open System.Threading
open System.Threading.Tasks
open System.Security
open Data.ByteString
open System.Runtime.InteropServices

/// Stowage Database
///
/// This is a key-value database with ACID updates that supports the
/// Stowage model of secure hash resources. Values at the key-value
/// layer serve as the durable root set for secure hash resources.
///
/// The "transaction" model in this case is simplistic and optimistic,
/// essentially a compare-and-swap except we can compare an arbitrary
/// set of key-value pairs independently from what we write. We assume
/// values are relatively small - e.g. a couple kilobytes at most - so
/// naive comparison isn't too expensive. Larger data should leverage
/// indirection through secure hash references.
///
/// For high-contention keys, clients may need to provide external
/// synchronization, e.g. some variation of key locking. Otherwise,
/// the compare-and-swap model will require too many retries.
///
type DB =
    val internal Impl : I.DB
    internal new(dbImpl : I.DB) = { Impl = dbImpl }

    member internal db.TryLoad (h:RscHash) : Val option =
        let newRsc = I.dbRecentStowage (db.Impl) h
        if Option.isSome newRsc then newRsc else
        I.withRTX (db.Impl) (fun rtx -> I.dbGetRsc db.Impl rtx h) 

    member inline db.Stowage with get() : Stowage = (db :> Stowage)
    interface Stowage with
        member db.Stow v = I.dbStow (db.Impl) v
        member db.Incref h = db.Impl.ephtbl.Incref (I.rscEphID h)
        member db.Decref h = db.Impl.ephtbl.Decref (I.rscEphID h)
        member db.Load h = 
            match db.TryLoad h with
            | Some v -> v
            | None -> raise (MissingRsc (db.Stowage,h))

    override x.Equals yobj =
        match yobj with
        | :? DB as y -> (x.Impl.env = y.Impl.env)
        | _ -> false
    override x.GetHashCode() = (int) (x.Impl.env >>> 3)
    interface System.IComparable with
        member x.CompareTo yobj =
            match yobj with
            | :? DB as y -> compare (x.Impl.env) (y.Impl.env)
            | _ -> invalidArg "yobj" "cannot compare values of different types"


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
    /// The value may be out of date due to asynchronous or concurrent
    /// writes, but the value is read atomically. See `readKeys` for
    /// snapshot isolation or `readKeyDeep` for structured data.
    ///
    /// Every key has a value, defaulting to the empty byte string.
    /// Writing the empty string is equivalent to key deletion.
    let readKey (db : DB) (k : Key) : Val =
        I.withRTX db.Impl (fun rtx -> 
            I.dbReadKey db.Impl rtx k)

    /// Atomic read of multiple keys from DB. 
    ///
    /// This guarantees snapshot isolation for the keys read together.
    /// This simplifies reasoning about consistency of data between the
    /// several keys. However, the whole snapshot may refer to an older
    /// version of the data due to concurrent update. See `readKeysDeep`
    /// for structured data.
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
    /// any writes. Essentially, this is an atomic compare-and-swap.
    /// The read-set is compared then the write-set is written.
    ///
    /// The operation returns successfully only after values written
    /// and all transitive dependencies are synchronized to disk. 
    ///
    /// Non-conflicting updates can be batched together, amortizing the
    /// synchronization overheads. Updates also are applied in the order
    /// received, so it is possible to skip writing intermediate values
    /// if asynchronous updates are used carefully.
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

    /// Synchronous write of single key.
    let inline writeKey db k v = writeKeys db (BTree.singleton k v)

    /// Wait for all pending asynchronous writes and updates to complete.
    /// This will also force pending stowage to disk.
    let sync db = writeKeys db (BTree.empty)

    /// Attempt to load a secure hash resource from the Database.
    /// This returns an optional value instead of raising MissingRsc.
    let tryLoadRsc (db : DB) (h : RscHash) : Val option = db.TryLoad h

    /// Test whether a resource is known to the DB, without copying.
    let hasRsc (db : DB) (h : RscHash) : bool =
        if Option.isSome (I.dbRecentStowage db.Impl h) then true else
        I.withRTX db.Impl (fun rtx ->
            Option.isSome (I.dbGetRscZC db.Impl rtx h))

    /// Load a resource or raise the MissingRsc exception.
    let inline loadRsc (db : DB) (h : RscHash) : Val = db.Stowage.Load h

    /// Add resource to the DB. 
    let inline stowRsc (db : DB) (v : Val) : RscHash = db.Stowage.Stow v

    /// Tune the Database stowage threshold.
    ///
    /// This is the amount of data (approximately) we'll buffer before
    /// flushing recently stowed data to disk. Stowage is also flushed
    /// upon `sync` or any key-value update, so this only affects the 
    /// construction of data between writes. The default buffer is a few
    /// megabytes, which is sufficient for most use cases.
    let setStowageBuffer (db : DB) (bytes : int) =
        if (bytes < 0)
            then invalidArg "bytes" "negative byte count"
        db.Impl.threshold <- I.dbThreshSize bytes

    let inline decrefRsc (db : DB) (h : RscHash) : unit = db.Stowage.Decref h
    let inline increfRsc (db : DB) (h : RscHash) : unit = db.Stowage.Incref h

    /// Ephemeral Roots for full Values
    /// 
    /// This essentially performs increfRsc or decrefRsc for every
    /// resource hash found in a value (as recognized by Stowage GC).
    /// The primary use case is to call decrefValDeps after readKeyDeep
    let decrefValDeps (db : DB) (v : Val) : unit = 
        RscHash.iterHashDeps (decrefRsc db) v
    let increfValDeps (db : DB) (v : Val) : unit = 
        RscHash.iterHashDeps (increfRsc db) v

    /// Atomic Read and Incref.
    ///
    /// This composes readKey and increfValDeps as one atomic operation.
    /// The motivation is to ensure resources referenced are not GC'd 
    /// due to concurrent writes. The client should call `decrefValDeps` 
    /// on the returned value when finished with it.
    let readKeyDeep (db : DB) (k : Key) : Val =
        I.withRTX db.Impl (fun rtx -> 
            let v = I.dbReadKey db.Impl rtx k
            increfValDeps db v
            v)

    /// Atomic Snapshot with Incref.
    ///
    /// This composes readKeys and readKeyDeep, atomically. You'll need
    /// to perform `decrefValDeps` for each returned value when finished.
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

    /// Check whether the value associated with a key is non-empty without
    /// copying the key's value.
    let containsKey (db : DB) (k : Key) : bool =
        I.withRTX db.Impl (fun rtx ->
            let v = I.dbReadKeyZC (db.Impl) rtx k
            (0un <> v.size))

