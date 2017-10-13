namespace Stowage
open System
open System.IO
open System.Threading
open System.Threading.Tasks
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
        
        type EphID = uint64
        let inline ephId (s:ByteString) : EphID =
            ByteString.Hash64 (BS.take 16 s)

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
            val mutable rc : nativeint
            new () = { rc = 0n }
            member rdlock.Acquire() = lock rdlock (fun () ->
                rdlock.rc <- (rdlock.rc + 1n))
            member rdlock.Release() = lock rdlock (fun () ->
                assert(0n <> rdlock.rc)
                rdlock.rc <- (rdlock.rc - 1n)
                if (0n = rdlock.rc)
                    then Monitor.PulseAll(rdlock))
            member rdlock.Wait() = lock rdlock (fun () ->
                while (0n <> rdlock.rc) do
                    Monitor.Wait(rdlock))


        // Due to the entanglement of of concurrency issues, I haven't found
        // a convenient model to break this into smaller components. OTOH, 
        // it isn't critical to do so.
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
            val mutable stow     : StowBuff     // batch of stowage resources
            val mutable sbsz     : unativeint   // stowage buffer approx size
            val mutable sbthresh : unativeint   // max sbsz before flush

            val wlock            : Object       // single writer lock
            val mutable writing  : KVMap        // flushing write batch
            val mutable stowing  : StowBuff     // recently stowed data

            // consider: stats for how much data is processed.
           
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
                  sbsz     = 0un
                  sbthresh = unativeint (4 * 1000 * 1000)
                }

            member db.Close() = 
                mdb_env_sync (db.env)
                mdb_env_close (db.env)

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
        // to compare the last 40 bytes of RscHash to resist timing attacks.
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

        // read a recent value for a key. If there was a write to the key
        // earlier in the smae thread, that value or a later one is read.
        let readKey (db : Database) (k : Key) : Val =
            let struct(wb0,wb1) = lock db (fun () ->
                struct(db.write, db.writing))
            match BTree.find k wb0 with
            | Some v -> v
            | None ->
                match BTree.find k wb1 with
                | Some v -> v
                | None -> withRTX db (fun tx -> mdb_get tx (db.dbi_data) k)

        let inline leftBiasedUnion (a:BTree<'x>) (b:BTree<'x>) : BTree<'x> =
            if BTree.isEmpty b then a else
            BTree.foldBack (BTree.add) a b

        let safeWrite k vOpt =
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

        // Add to stowage batch, returning incref'd RscHash. Actual write
        // is delayed until flush, but maintains db.sbsz so we may decide
        // to auto-flush whenever we have a fair amount of buffered data.
        let stowRsc (db : Database) (v : ByteString) : RscHash =
            if (v.Length > maxValLen)
                then invalidArg "v" "oversized value"
            let h = RscHash.hash v
            db.ephtbl.Incref h
            lock db (fun () ->
                db.stow <- Map.add (BS.take stowKeyLen h) (struct(h,v))
                db.sbsz <- unativeint (200 + v.Length) + db.sbsz) 
            h

        // positive reference counts are encoded using EncVarNat.
        // zero counts are encoded in a separate table for iteration.
        type RC = uint64
        let refctBytes (rc:RC) : ByteString =
            ByteStream.write (fun dst ->
                ByteStream.reserve (EncVarNat.size rc) dst
                EncVarNat.write rc dst)

        // read a durable reference count
        let dbGetRefct (db:Database) (rtx:MDB_txn) (sk:StowKey) : RC =
            match mdb_get rtx (db.dbi_rfct) sk with
            | Some v -> ByteStream.read (EncVarNat.read) v
            | None -> 0UL

        // set durable reference count. Ensures that stowkey appears
        // in either the rfct or zero database, but not both.
        let dbSetRefct (db:Database) (wtx:MDB_txn) (sk:StowKey) (rc:RC) : unit =
            if (0UL = rc) then
                mdb_del wtx (db.dbi_rfct) sk |> ignore<bool>
                mdb_put wtx (db.dbi_zero) sk (BS.empty)
            else
                mdb_del wtx (db.dbi_zero) sk |> ignore<bool>
                mdb_put wtx (db.dbi_rfct) sk (refctBytes rc)

        let inline dbHasRsc (db:Database) (rtx:MDB_txn) (h:RscHash) : bool =
            mdb_contains rtx (db.dbi_stow) (BS.take stowKeyLen h)

        // delete a resource and its reference counts
        let dbDelRsc (db:Database) (wtx:MDB_txn) (sk:StowKey) : unit =
            assert(stowKeyLen = sk.Length)
            mdb_del wtx (db.dbi_rfct) sk |> ignore<bool>
            mdb_del wtx (db.dbi_zero) sk |> ignore<bool>
            mdb_del wtx (db.dbi_stow) sk |> ignore<bool>
        
        // add resource to stowage table (doesn't touch refcts)
        let dbAddRsc (db:Database) (wtx:MDB_txn) (h:RscHash) (v:ByteString) : unit =
            assert(RscHash.size = h.Length)
            let sk = BS.take stowKeyLen h
            let rem = BS.drop stowKeyLen h
            let dst = mdb_reserve wtx (db.stow) sk (rem.Length + v.Length)
            Marshal.Copy(rem.UnsafeArray, rem.Offset, dst, rem.Length)
            Marshal.Copy(v.UnsafeArray, v.Offset, (dst + nativeint rem.Length), v.Length)

        // write a key-value pair; write `None` to delete the key.
        // an empty value is distinct from non-existent.
        let dbWriteKeyVal (db:Database) (wtx:MDB_txn) (k:Key) (vOpt:Val) : unit =
            match vOpt with
            | Some v -> mdb_put wtx (db.dbi_data) k v
            | None -> mdb_del wtx (db.dbi_data) k |> ignore<bool>

        
        // Perform pending writes and GC as needed.
        // Assumes and requires the write lock. 
        let dbWriteFrame (db:Database) : unit = 
            // some assumptions
            assert(Monitor.IsEntered(db.wlock)) // require write lock
            assert(not (Monitor.IsEntered(db))) // forbid database lock
            assert(BTree.isEmpty (db.writing) && Map.isEmpty (db.stowing))



