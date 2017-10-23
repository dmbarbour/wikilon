namespace Stowage
open System
open System.IO
open System.Threading
open System.Threading.Tasks
open System.Runtime.InteropServices
open System.Collections.Generic
open Data.ByteString
open Stowage.LMDB_FFI

/// Stowage above a memory-mapped database.
///
/// This uses the memory mapped file database LMDB under the hood.
/// LMDB is a read-optimized database, but offers effective write
/// performance - at least faster than 
module LMDB =
    type Key = DB.Key
    type Val = DB.Val
    type KVMap = DB.KVMap

    /// MDB storage will support values up to 64MB.
    let maxValLen = (64 * 1024 * 1024)

    let maxSafeKeyLen = 255
    let minSafeKeyLen = 1
    let inline safeKeyByte (b:byte) : bool = 
        ((126uy >= b) && (b >= 35uy))

    /// Safe keys are limited as follows:
    ///   contain only ASCII bytes in 35..126
    ///   size limit between 1 and 255 bytes
    /// Other keys will be mangled.
    let safeKey (k:ByteString) : bool =
        (maxSafeKeyLen >= k.Length) &&
        (k.Length >= minSafeKeyLen) &&
        BS.forall safeKeyByte k

    /// Mangle unsafe keys, rewriting to !SecureHash.
    /// Secure hash is the first 160 bits of RscHash.
    let mangle (k:ByteString) : Key =
        if safeKey k then k else
        BS.cons (byte '!') (BS.take 32 (RscHash.hash k))

    module private I =
        type WriteBatch = BTree<Val>
        
        // to guard Stowage hashes against timing attacks, we'll not
        // use the full RscHash for lookups. Instead, I use just the
        // first 180 bits, then verify the remaining 180 bits with a
        // constant time equality check.
        type StowKey = ByteString 
        let stowKeyLen = (RscHash.size / 2)
        let stowKeyRem = RscHash.size - stowKeyLen
        do assert(stowKeyRem >= stowKeyLen)
        type StowData = (struct(RscHash * ByteString))
        type StowBuff = Map<StowKey,StowData>
        let tryFindRscSB (h:RscHash) (sb:StowBuff) : ByteString option =
            match Map.tryFind (BS.take stowKeyLen h) sb with
            | Some (struct(h',v)) when (ByteString.CTEq h h') -> Some v
            | _ -> None 
        
        type EphID = uint64
        let inline skEphId (s:StowKey) : EphID = 
            assert(stowKeyLen = s.Length)
            ByteString.Hash64 s

        // Our ephemeron table both locks the refct table and allows me
        // to delay decrefs that might occur after concurrent writes so
        // we don't prematurely GC any resources.
        type Ephemerons =
            val table : RCTable.Table
            val mutable decrefs : ResizeArray<EphID>
            val mutable delay : bool
            
            new() = 
                { table = new RCTable.Table()
                  decrefs = new ResizeArray<EphID>()
                  delay = false
                }

            member this.DelayDecrefs () = lock this (fun () -> 
                this.delay <- true)
            member this.PassDecrefs () = lock this (fun () ->
                this.decrefs.ForEach(fun k -> this.table.Decref k)
                this.decrefs <- new ResizeArray<EphID>()
                this.delay <- false)
            member this.Incref (k:EphID) : unit = lock this (fun () -> 
                this.table.Incref k)
            member this.Decref (k:EphID) : unit = lock this (fun () ->
                if this.delay 
                    then this.decrefs.Add k
                    else this.table.Decref k)
            member this.Contains (k:EphID) : bool = lock this (fun () -> 
                this.table.Contains k)

        type FileLock = FileStream
        let lockFile (name : string) : FileLock =
            new FileStream(
                    name, 
                    FileMode.OpenOrCreate, 
                    FileAccess.ReadWrite, 
                    FileShare.None,
                    8,
                    FileOptions.DeleteOnClose)

        // Simply count concurrent readers. Allow wait for zero readers. 
        type ReadLock =
            val mutable rc : unativeint
            new () = { rc = 0un }
            member rdlock.Acquire() = lock rdlock (fun () ->
                rdlock.rc <- (rdlock.rc + 1un))
            member rdlock.Release() = lock rdlock (fun () ->
                assert(0un < rdlock.rc)
                rdlock.rc <- (rdlock.rc - 1un)
                if (0un = rdlock.rc)
                    then Monitor.PulseAll(rdlock))
            member rdlock.Wait() = lock rdlock (fun () ->
                while (0un < rdlock.rc) do
                    Monitor.Wait(rdlock) |> ignore<bool>)

        // Due to the entanglement with concurrency, GC, etc. I haven't 
        // found a convenient model to break this into small components.
        type Database =

            // MDB layer tables.
            val mdb_env  : MDB_env  // LMDB database
            val dbi_data : MDB_dbi  // root key-value pairs
            val dbi_stow : MDB_dbi  // binary Stowage resources
            val dbi_rfct : MDB_dbi  // table with positive refcts
            val dbi_zero : MDB_dbi  // table with zero-refct items
            val flock    : FileLock // resist accidental concurrency

            val ephtbl           : Ephemerons   // track resources in memory
            val mutable rdlock   : ReadLock     // track concurrent readers
            val mutable write    : KVMap        // new write batch
            val mutable stow     : StowBuff     // new stowage resources
            val mutable sbsz     : int          // ~ data in stowage buffer

            val wlock            : Object       // one writer at a time!
            val mutable writing  : KVMap        // flushing write batch
            val mutable stowing  : StowBuff     // flushing stowed data

            // stats for feedback
            val mutable ct_stow  : uint64       // resources stowed
            val mutable ct_del   : uint64       // resources deleted
           
            new (path:string, maxSizeMB:int) =
                Directory.CreateDirectory(path) |> ignore
                let flock = lockFile (Path.Combine(path,"lock"))
                let env = mdb_env_create ()
                mdb_env_set_mapsize env maxSizeMB
                mdb_env_set_maxdbs env 4
                let envFlags = MDB_NOSYNC ||| MDB_WRITEMAP ||| 
                               MDB_NOTLS ||| MDB_NOLOCK
                mdb_env_open env path envFlags
                let tx = mdb_readwrite_txn_begin env
                let dbi_data = mdb_dbi_open tx "/"
                let dbi_stow = mdb_dbi_open tx "$"
                let dbi_rfct = mdb_dbi_open tx "#"
                let dbi_zero = mdb_dbi_open tx "0"
                mdb_txn_commit tx
                { mdb_env  = env
                  dbi_data = dbi_data
                  dbi_stow = dbi_stow
                  dbi_rfct = dbi_rfct
                  dbi_zero = dbi_zero
                  flock    = flock
                  ephtbl   = new Ephemerons()
                  rdlock   = new ReadLock()
                  wlock    = new System.Object()
                  write    = BTree.empty
                  writing  = BTree.empty
                  stow     = Map.empty
                  sbsz     = 0
                  stowing  = Map.empty
                  ct_stow  = 0UL
                  ct_del   = 0UL
                }

            member db.Close() = 
                mdb_env_sync (db.mdb_env)
                mdb_env_close (db.mdb_env)
                db.flock.Dispose()

        let withRTX (db : Database) (action : MDB_txn -> 'x) : 'x =
            let rdlock = lock db (fun () ->
                db.rdlock.Acquire()
                db.rdlock)
            try let tx = mdb_rdonly_txn_begin (db.mdb_env)
                try action tx
                finally mdb_txn_commit tx
            finally rdlock.Release()

        // locate resource in database, if it is available. This will search
        // recently buffered stowage before the LMDB layer. Uses constant time
        // to compare stowKeyRem bytes of RscHash to resist timing attacks.
        let tryLoadRsc (db : Database) (h : RscHash) : ByteString option =
            if (RscHash.size <> h.Length) 
                then invalidArg "h" "invalid resource hash"
            let struct(sb0,sb1) = lock db (fun () -> 
                struct(db.stow, db.stowing))
            let inSB0 = tryFindRscSB h sb0
            if Option.isSome inSB0 then inSB0 else
            let inSB1 = tryFindRscSB h sb1
            if Option.isSome inSB1 then inSB1 else
            let vOpt = withRTX db (fun tx -> 
                mdb_get tx (db.dbi_stow) (BS.take stowKeyLen h))
            match vOpt with
            | Some rv when (ByteString.CTEq (BS.drop stowKeyLen h) (BS.take stowKeyRem rv)) ->
                Some (BS.drop stowKeyRem rv)
            | _ -> None

        let inline dbReadKey (db:Database) (rtx:MDB_txn) (k : Key) =
            mdb_get rtx (db.dbi_data) k

        // read a recent value for a key. If there was a write to the key
        // earlier in the smae thread, that value or a later one is read.
        let readKey (db : Database) (k : Key) : Val =
            let struct(wb0,wb1) = lock db (fun () -> 
                struct(db.write, db.writing))
            match BTree.tryFind k wb0 with
            | Some v -> v
            | None ->
                match BTree.tryFind k wb1 with
                | Some v -> v
                | None -> withRTX db (fun rtx -> dbReadKey db rtx k)

        let inline leftBiasedUnion (a:BTree<'x>) (b:BTree<'x>) : BTree<'x> =
            if BTree.isEmpty b then a else
            BTree.foldBack (BTree.add) a b

        let safeWrite (k:Key) (vOpt:Val) =
            // testing key against LMDB limits here
            let okKey = (511 >= k.Length) && (k.Length >= 1)
            if not okKey then false else
            match vOpt with
            | Some v -> (maxValLen >= v.Length) 
            | None -> true

        // write a batch of updates. Does not flush updates.
        let writeBatch (db : Database) (wb : KVMap) : unit =
            if not (BTree.forall safeWrite wb)
                then invalidArg "wb" "invalid write batch"
            lock db (fun () ->
                db.write <- leftBiasedUnion wb (db.write))

        // reduce sizes to prevent overflow issues, together with
        // maximum lengths this shouldn't be a problem.
        let inline sbSize (bytes:int) : int = (3 + (bytes >>> 7))

        // Add to stowage batch for next flush operation. Does not
        // flush automatically, but records rough size of stowage.
        let stowRsc (db : Database) (v : ByteString) : RscHash =
            if (v.Length > maxValLen)
                then invalidArg "v" "oversized value"
            let h = RscHash.hash v
            let sk = BS.take stowKeyLen h
            db.ephtbl.Incref (skEphId sk)
            lock db (fun () -> 
                db.stow <- Map.add sk (struct(h,v)) (db.stow)
                db.sbsz <- db.sbsz + (sbSize (v.Length)))
            h

        // reference counts are natural numbers, encoded using EncVarNat.
        // However, zero counts are recorded implicitly in separate table
        // (dbi_zero) for incremental GC and ephemeral roots.
        type RC = uint64
        let refctBytes (rc:RC) : ByteString =
            ByteStream.write (fun dst ->
                ByteStream.reserve 16 dst
                EncVarNat.write rc dst)

        // read a durable reference count
        let dbGetRefct (db:Database) (rtx:MDB_txn) (sk:StowKey) : RC =
            assert(stowKeyLen = sk.Length)
            match mdb_get rtx (db.dbi_rfct) sk with
            | Some v -> ByteStream.read (EncVarNat.read) v
            | None -> 0UL

        // delete a resource and its reference counts. It's possible
        // that this is a false resource (due to conservative GC) or
        // a locally unknown resource. But we delete when there are 
        // no references remaining.
        let dbDelRsc (db:Database) (wtx:MDB_txn) (sk:StowKey) : unit =
            assert(stowKeyLen = sk.Length)
            mdb_del wtx (db.dbi_rfct) sk |> ignore<bool>
            mdb_del wtx (db.dbi_zero) sk |> ignore<bool>
            let bDropped = mdb_del wtx (db.dbi_stow) sk 
            if bDropped then db.ct_del <- (db.ct_del + 1UL)

        let dbSetRefct (db:Database) (wtx:MDB_txn) (sk:StowKey) (rc:RC) : unit =
            assert(stowKeyLen = sk.Length)
            if (0UL = rc) then
                mdb_del wtx (db.dbi_rfct) sk |> ignore<bool>
                mdb_put wtx (db.dbi_zero) sk (BS.empty)
            else
                mdb_del wtx (db.dbi_zero) sk |> ignore<bool>
                mdb_put wtx (db.dbi_rfct) sk (refctBytes rc)

        // add resource to stowage table (doesn't touch refcts)
        let dbAddRsc (db:Database) (wtx:MDB_txn) (h:RscHash) (v:ByteString) : unit =
            assert(RscHash.size = h.Length)
            let sk = BS.take stowKeyLen h
            let rem = BS.drop stowKeyLen h
            let dst = mdb_reserve wtx (db.dbi_stow) sk (rem.Length + v.Length)
            Marshal.Copy(rem.UnsafeArray, rem.Offset, dst, rem.Length)
            Marshal.Copy(v.UnsafeArray, v.Offset, (dst + nativeint rem.Length), v.Length)
            db.ct_stow <- (db.ct_stow + 1UL)

        // write a key-value pair; write `None` to delete the key.
        // an empty value is distinct from non-existent.
        let dbWriteKeyVal (db:Database) (wtx:MDB_txn) (k:Key) (vOpt:Val) : unit =
            match vOpt with
            | Some v -> mdb_put wtx (db.dbi_data) k v
            | None -> mdb_del wtx (db.dbi_data) k |> ignore<bool>

        let inline writePending (db:Database) : bool =
            let inactive = BTree.isEmpty (db.writing) 
                        && Map.isEmpty (db.stowing)
            not inactive

        // The GC task is sophisticated enough to have its own object.
        // 
        // Durable reference counts are held in the Database, and we must
        // also delay GC based on ephemeral references (Ephemerons table)
        // or incremental GC quotas (so we don't delay writers too much).
        // To avoid fine-grained updates, all reference counts should also
        // be computed in memory and serialized in one batch.
        //
        // GC assumes that, once an object reaches 0 refct within a batch,
        // it will not incref again. Ephemeral resources might incref in a
        // future batch, but not this one. Consequently, increfs should run
        // before anything else.
        //
        // The current GC strategy is essentially breadth-first, but will
        // delete recursively within the limits of quotas.
        type GC =
            val db  : Database
            val wtx : MDB_txn
            val mutable rfct  : Map<StowKey,RC> 
            val queue : Queue<StowKey>    // pending GC targets
            val mutable quota : int       // GC effort quota
            new(db,wtx) = 
              { db = db
                wtx = wtx
                rfct = Map.empty
                queue = new Queue<StowKey>()
                quota = GC.DefaultQuota
              }

            // The amount of incremental GC is based on a flat quota,
            // plus a small quota per incref to ensure GC can keep up
            // with a busy writer.
            static member DefaultQuota = 2000
            static member IncrefQuota = 2
            static member NewRscQuota = 2

            // add a deletion task for a given StowKey. Constrained
            // by quota and ephemeral resources. Deletion is delayed
            // until the `RunToCompletion` loop.
            member gc.AddDeleteTask (sk:StowKey) : unit = 
                let blockGC = (0 = gc.quota)
                           || (gc.db.ephtbl.Contains (skEphId sk)) 
                if blockGC then () else
                gc.quota <- (gc.quota - 1)
                gc.queue.Enqueue sk

            member gc.SetRefct (sk:StowKey) (rc:RC) : unit =
                assert(stowKeyLen = sk.Length)
                gc.rfct <- Map.add sk rc (gc.rfct)
                if (0UL = rc) then gc.AddDeleteTask sk

            member gc.GetRefct (sk:StowKey) : RC =
                match Map.tryFind sk (gc.rfct) with
                | Some rc -> rc
                | None -> dbGetRefct (gc.db) (gc.wtx) sk

            member gc.NewRsc (sk:StowKey) : unit = 
                // do not modify refct, but ensure one is written
                gc.quota <- (gc.quota + GC.NewRscQuota)
                gc.SetRefct sk (gc.GetRefct sk)

            member gc.Incref (h:RscHash) : unit =
                gc.quota <- (gc.quota + GC.IncrefQuota)
                assert(RscHash.size = h.Length)
                let sk = BS.take stowKeyLen h
                let rc = gc.GetRefct sk 
                gc.SetRefct sk (rc + 1UL)

            member gc.Decref (h:RscHash) : unit =
                assert(RscHash.size = h.Length)
                let sk = BS.take stowKeyLen h
                let rc = gc.GetRefct sk
                if(0UL = rc) then failwith "negative refct" else
                gc.SetRefct sk (rc - 1UL)

            member inline gc.AddVal (v:Val) : unit =
                match v with
                | Some s -> RscHash.iterHashDeps (gc.Incref) s
                | None -> () 

            member inline gc.RemVal (v:Val) : unit =
                match v with
                | Some s -> RscHash.iterHashDeps (gc.Decref) s
                | None -> ()

            member gc.ScanPending() : unit =
                // scan dbi_zero table for items we may delete.
                let ks = mdb_keys_from (gc.wtx) (gc.db.dbi_zero) (BS.empty)
                use e = ks.GetEnumerator()
                while((0 < gc.quota) && (e.MoveNext())) do
                    let sk = e.Current
                    let tryGC = not (Map.containsKey sk (gc.rfct))
                    if tryGC then gc.AddDeleteTask sk

            member gc.Delete (sk:StowKey) : unit =
                assert(0UL = (gc.GetRefct sk))
                gc.rfct <- Map.remove sk (gc.rfct)
                mdb_get (gc.wtx) (gc.db.dbi_stow) sk
                    |> Option.map (BS.drop stowKeyRem)
                    |> gc.RemVal
                dbDelRsc (gc.db) (gc.wtx) sk

            member gc.RunToCompletion () : unit =
                // perform GC until no new tasks are enqueued.
                while(0 < gc.queue.Count) do
                    gc.Delete (gc.queue.Dequeue())

            member gc.FlushRefcts () : unit = 
                // write final reference counts to the database
                Map.iter (dbSetRefct (gc.db) (gc.wtx)) (gc.rfct)

            member gc.Perform () : unit =
                gc.ScanPending()
                gc.RunToCompletion()
                gc.FlushRefcts()

        // flush pending writes and stowage data, and performs GC.
        // The `gcFlush` flag can force flush when no write is pending.
        let dbFlush (db:Database) (gcFlush:bool) : unit = lock (db.wlock) (fun () ->
            assert(not (Monitor.IsEntered(db)))
            if writePending db 
                then failwith "invalid DB state: prior write failed!"

            // delay decrefs during flush to simplify reasoning about
            // correctness. This must occur before we access
            db.ephtbl.DelayDecrefs()
            use onExitPassDecrefs =
                { new System.IDisposable with
                    member __.Dispose() = db.ephtbl.PassDecrefs()
                }

            // write buffers are held in Database until commit, but
            // are immediately separated from new incoming writes
            lock db (fun () ->
                db.writing <- db.write
                db.stowing <- db.stow
                db.write <- BTree.empty
                db.stow <- Map.empty
                db.sbsz <- 0)

            // potential for early escape
            let performFlush = gcFlush || writePending db 
            if not performFlush then () else

            let wtx = mdb_readwrite_txn_begin (db.mdb_env)

            // Write our new roots. Remember old roots for GC purposes.
            let overwriting = BTree.map (fun k _ -> dbReadKey db wtx k) (db.writing)
            BTree.iter (dbWriteKeyVal db wtx) (db.writing)

            // For stowage, filter known resources. Write the remainder.
            let isNewRsc sk _ = not (mdb_contains wtx (db.dbi_stow) sk) 
            db.stowing <- Map.filter isNewRsc (db.stowing)
            Map.iter (fun _ (struct(h,v)) -> dbAddRsc db wtx h v) (db.stowing)

            // update reference counts and perform GC.
            let gc = new GC(db,wtx)
            BTree.iter (fun _ v -> gc.AddVal v) (db.writing)
            Map.iter (fun _ (struct(_,v)) -> gc.AddVal (Some v)) (db.stowing)
            BTree.iter (fun _ v -> gc.RemVal v) (overwriting)
            Map.iter (fun sk _ -> gc.NewRsc sk) (db.stowing)
            gc.Perform()

            // write and flush the transaction
            mdb_txn_commit wtx
            let oldReaders = lock db (fun () ->
                let oldReadLock = db.rdlock
                db.rdlock <- new ReadLock()
                oldReadLock)
            mdb_env_sync (db.mdb_env) // flush to disk
            oldReaders.Wait() // wait on readers of old frame
            lock db (fun () -> 
                db.writing <- BTree.empty
                db.stowing <- Map.empty)
            ) // unlock db.wlock

        // flush only if we're past a given threshold.
        let inline sbFlush (thresh:int) (db:Database) : unit =
            let sbFull = (db.sbsz) > (sbSize thresh)
            if sbFull then dbFlush db false

    /// LMDB based Storage.
    ///
    /// A new Storage must be given the path and the memory map size
    /// in megabytes, which also determines the maximum file size and
    /// address space consumption. 
    ///
    /// New stowage is buffered in memory until a threshold is reached
    /// or explicitly flushed with other writes. The motivation for this
    /// is to amortize synchronization costs across a larger write batch.
    /// Default threshold is a couple megabytes, but it may be configured.
    type Storage =
        val private db : I.Database
        val mutable private sbthresh : int
        new(path:string,maxSizeMB:int) = 
            { db = new I.Database(path,maxSizeMB) 
              sbthresh = 2_000_000
            }
        
        /// Configure Stowage Threshold (in bytes).
        member this.SetStowageThreshold (bytes:int) : unit =
            this.sbthresh <- bytes
            this.FlushStowage()

        // flush only if we've surpassed our threshold
        member private this.FlushStowage() = 
            I.sbFlush (this.sbthresh) (this.db)

        /// Force a Stowage GC step. 
        ///
        /// GC is normally perfomed incrementally with writes upon Flush.
        /// This operation acts as a false write-flush just to perform 
        /// some GC. This may collect a couple thousand items in one step.
        /// You can use StowageStats if you need feedback about the amount
        /// deleted. 
        member this.GC() : unit = I.dbFlush (this.db) true

        /// Trivial Statistics
        /// 
        /// This returns a pair of integers indicating a count of resources
        /// (stowed * deleted) by the current process. Only includes actual
        /// writes or deletions at the LMDB layer. 
        member this.StowageStats() : (uint64 * uint64) =
            lock (this.db.wlock) (fun () ->
                (this.db.ct_stow, this.db.ct_del))


        interface Stowage with
            member this.Load h =
                match I.tryLoadRsc (this.db) h with
                | Some v -> v
                | None -> raise (MissingRsc (this :> Stowage, h))
            member this.Stow v =
                let h = I.stowRsc (this.db) v
                this.FlushStowage()
                h
            member this.Incref h = 
                assert (RscHash.size = h.Length)
                let sk = BS.take (I.stowKeyLen) h
                this.db.ephtbl.Incref (I.skEphId sk)
            member this.Decref h =
                assert (RscHash.size = h.Length)
                let sk = BS.take (I.stowKeyLen) h
                this.db.ephtbl.Decref (I.skEphId sk)

        interface DB.Storage with
            member this.Mangle k = mangle k
            member this.Read k = I.readKey (this.db) k
            member this.WriteBatch wb = I.writeBatch (this.db) wb
            member this.Flush () = I.dbFlush (this.db) false

    /// Open an LMDB storage as a Stowage Database.
    /// Composes `new LMDB.Storage` and `DB.fromStorage`
    let inline openDB (path:string) (maxSizeMB:int) : DB =
        let s = new Storage(path,maxSizeMB) 
        DB.fromStorage (s :> DB.Storage)

