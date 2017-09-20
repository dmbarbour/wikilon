
namespace Stowage

open System
open System.IO
open System.Threading
open System.Threading.Tasks
open System.Security
open System.Runtime.InteropServices
open Data.ByteString

open Stowage.Hash
open Stowage.Memory
open Stowage.LMDB

#nowarn "9" 

[< SecuritySafeCriticalAttribute >]
module internal I =

    // The underlying database tables uses only half RscHash for lookup
    // or reference count management. The latter half is verified in 
    // constant time to resist leaking of hashes via timing attacks.
    let stowKeyLen = rscHashLen / 2

    [< Struct >]
    type StowKey =
        // a simple wrapper type to resist accidents
        val v : ByteString
        new( s : ByteString ) = assert(s.Length = stowKeyLen); { v = s }

    let inline rscStowKey (h:RscHash) : StowKey =
        assert(h.Length = rscHashLen)
        StowKey(BS.take stowKeyLen h)

    // Recent stowage table. Uses half the key to resist timing attacks.
    // Just using Map since prefix sharing is improbable with hashes and
    // won't affect performance much. 
    type Stowage = Map<StowKey, struct(RscHash * Val)>     // recent stowage requests

    type RC = int64                     // sufficient for RC
    let maxRCLen = 18                   // length in bytes
    let maxRC = 999_999_999_999_999_999L // what can we squeeze into 18 digits?
    type RCU = Map<StowKey, RC>         // pending refct updates

    type EphID = nativeint                 // hash of stowKey
    let inline skEphID(h:StowKey) : EphID = nativeint (ByteString.Hash64(h.v))
    let inline rscEphID(h:RscHash) : EphID = skEphID (rscStowKey h)


    // Each commit consists of:
    //  a set of values read (or assumed) to validate
    //  a set of values to be written 
    //  a task completion resource to report success or failure
    type Commit = (KVMap * KVMap * TaskCompletionSource<bool>)

    // Readlock state is essentially a reader-count with an event
    // for when we reach zero readers.
    type ReadLock = 
        val mutable private rc : RC
        new () = { rc = 0L }
        member this.Acquire () = lock this (fun () -> 
            this.rc <- this.rc + 1L)
        member this.Release () = lock this (fun () ->
            assert (this.rc > 0L)
            this.rc <- this.rc - 1L
            if(0L = this.rc) then Monitor.PulseAll(this))
        member this.Wait () = lock this (fun () -> 
            while(0L <> this.rc) do 
                ignore(Monitor.Wait(this)))

    [<CustomEquality; CustomComparison>]
    type DB =
        { 
            db_env  : MDB_env     
            db_data : MDB_dbi     // key -> value
            db_stow : MDB_dbi     // resource ID -> value
            db_rfct : MDB_dbi     // resources with refct > 0
            db_zero : MDB_dbi     // resources with zero refct
            db_flock : FileStream
            db_ephtbl : EphTbl.Table    // ephemeral resource roots

            mutable db_rdlock : ReadLock        // current read-lock (updated per frame).
            mutable db_newrsc : Stowage         // recent stowage requests
            mutable db_commit : Commit list     // pending commit requests
            mutable db_halt   : bool            // halt the writer
        }
        // reference comparison and equality on the stable MDB_env pointer.
        override x.Equals(yobj) =
            match yobj with
            | :? DB as y -> x.db_env = y.db_env
            | _ -> false
        override db.GetHashCode() = (int)(db.db_env >>> 4)
        interface System.IComparable with
            member x.CompareTo yobj =
                match yobj with
                | :? DB as y -> compare (x.db_env) (y.db_env)
                | _ -> invalidArg "yobj" "cannot compare values of different types"

    let inline dbEphInc (db : DB) (k : EphID) : unit = EphTbl.incref (db.db_ephtbl) k
    let inline dbEphDec (db : DB) (k : EphID) : unit = EphTbl.decref (db.db_ephtbl) k
    let inline dbHasEph (db : DB) (k : EphID) : bool = EphTbl.contains (db.db_ephtbl) k

    // add commit request to the database
    let dbCommit (db : DB) (c : Commit) : unit =
        lock db (fun () ->
            db.db_commit <- (c :: db.db_commit)
            Monitor.PulseAll(db))

    // signal writer to perform a step (via dummy commit)
    let dbSignal (db : DB) : unit =
        let tcs = new TaskCompletionSource<bool>() // result ignored
        dbCommit db (BTree.empty, BTree.empty, tcs)

    // add stowage request and ephemeral root to database, returns root ID
    let dbStow (db : DB) (h : RscHash) (v : Val) : EphID =
        let sk = rscStowKey h
        let ephID = skEphID sk
        dbEphInc db ephID
        lock db (fun () ->
            db.db_newrsc <- Map.add sk (struct(h,v)) (db.db_newrsc)
            Monitor.PulseAll(db))
        ephID


    // Our reference count tables simply have data of type [1-9][0-9]*.
    // Reasonably, reference counts shouldn't be larger than 18 digits.
    let dbGetRefct (db:DB) (rtx:MDB_txn) (k:StowKey) : RC =
        let vOpt = mdb_getZC rtx (db.db_rfct) k.v
        match vOpt with
        | None -> 0L
        | Some v -> 
            assert((unativeint maxRCLen) >= v.size)
            let rec loop a p len = 
                if (0 = len) then a else
                let b = Marshal.ReadByte(p)
                assert(((byte '9') >= b) && (b >= (byte '0')))
                let a' = (10L * a) + (int64 (b - (byte '0')))
                loop a' (1n + p) (len - 1)
            loop 0L v.data (int v.size)

    let refctBytes (rc:RC) : ByteString =
        assert((maxRC >= rc) && (rc > 0L))
        let data = Array.zeroCreate maxRCLen
        let rec write ix rc =
            if(0L = rc) 
                then BS.unsafeCreate data ix (data.Length - ix)
                else data.[ix - 1] <- (byte '0' + byte (rc % 10L))
                     write (ix - 1) (rc / 10L)
        write data.Length rc

    let dbSetRefct (db:DB) (wtx:MDB_txn) (k:StowKey) (rc:RC) : unit =
        let validRefct = (maxRC >= rc) && (rc >= 0L)
        if not validRefct then invalidArg "rc" "refct out of range" else
        if (0L = rc)
            then mdb_del wtx (db.db_rfct) (k.v) |> ignore
                 mdb_put wtx (db.db_zero) (k.v) (BS.empty)
            else mdb_del wtx (db.db_zero) (k.v) |> ignore
                 mdb_put wtx (db.db_rfct) (k.v) (refctBytes rc)

    let dbGetRscZC (db:DB) (rtx:MDB_txn) (h:RscHash) : MDB_val option =
        if(h.Length <> rscHashLen) then invalidArg "h" "bad resource ID" else
        let hKey = BS.take stowKeyLen h
        let hRem = BS.drop stowKeyLen h
        let vOpt = mdb_getZC rtx (db.db_stow) hKey 
        match vOpt with
        | None -> None
        | Some v ->
            let rlen = rscHashLen - stowKeyLen
            assert((unativeint (rlen + maxValLen) >= v.size) && 
                   (v.size >= unativeint rlen)                 )
            let matchRem = BS.withPinnedBytes hRem (fun ra -> 
                    Memory.memcteq ra v.data rlen)
            if not matchRem then None else
            let size' = v.size - unativeint rlen
            let data' = v.data + nativeint rlen
            Some(MDB_val(size',data'))

    let inline dbGetRsc (db:DB) (rtx:MDB_txn) (h:RscHash) : Val option =
        dbGetRscZC db rtx h |> Option.map Stowage.LMDB.val2bytes

    // deletes resource, returns bytestring removed for refct GC
    let dbDelRsc (db:DB) (wtx:MDB_txn) (k:StowKey) : Val =
        //printf "Deleting Resource %s...\n" (toString k.v)
        let vOpt = mdb_get wtx (db.db_stow) (k.v)
        mdb_del wtx (db.db_stow) (k.v) |> ignore
        mdb_del wtx (db.db_zero) (k.v) |> ignore
        mdb_del wtx (db.db_rfct) (k.v) |> ignore
        match vOpt with
        | None -> BS.empty
        | Some v -> 
            let rlen = rscHashLen - stowKeyLen
            assert(v.Length >= rlen)
            BS.drop rlen v

    let dbPutRsc (db:DB) (wtx:MDB_txn) (h:RscHash) (v:Val) : unit =
        if(h.Length <> rscHashLen) then invalidArg "h" "invalid resource ID" else
        if not (isValidVal v) then invalidArg "v" "invalid resource value" else
        //printf "Writing Resource %s\n" (toString h)
        let hKey = BS.take stowKeyLen h
        let hRem = BS.drop stowKeyLen h
        let dst = mdb_reserve wtx (db.db_stow) hKey (hRem.Length + v.Length)
        Marshal.Copy(hRem.UnsafeArray, hRem.Offset, dst, hRem.Length)
        Marshal.Copy(v.UnsafeArray, v.Offset, (dst + nativeint hRem.Length), v.Length)

    let dbReadKeyZC (db:DB) (rtx:MDB_txn) (k:Key) : MDB_val =
        if(not (isValidKey k)) then invalidArg "k" "invalid key" else
        let vOpt = mdb_getZC rtx (db.db_data) k
        match vOpt with
        | Some v -> assert(0un <> v.size); v
        | None -> MDB_val()

    let inline dbReadKey (db:DB) (rtx:MDB_txn) (k:Key) : Val =
        Stowage.LMDB.val2bytes (dbReadKeyZC db rtx k)

    let dbWriteKey (db:DB) (wtx:MDB_txn) (k:Key) (v:Val) : unit =
        if(not (isValidKey k)) then invalidArg "k" "invalid key" else
        if(not (isValidVal v)) then invalidArg "v" "invalid value" else
        //printf "Writing Key %s\n" (toString k)
        if(BS.isEmpty v) 
            // empty value corresponds to deletion
            then mdb_del wtx (db.db_data) k |> ignore
            else mdb_put wtx (db.db_data) k v

    let acquireReadLock (db:DB) : ReadLock =
        // lock to prevent advanceReaderFrame while acquiring
        lock db (fun () ->
            db.db_rdlock.Acquire()
            db.db_rdlock)
            
    let advanceReaderFrame (db : DB) : ReadLock = 
        lock db (fun () ->
            let oldFrame = db.db_rdlock
            db.db_rdlock <- new ReadLock()
            oldFrame)

    // Perform operation while holding reader TX.
    //
    // LMDB with NOLOCK is essentially a frame-buffered database.
    // At most times, we have two valid frames. During commit, we
    // drop the older frame and write the new one. 
    //
    // Stowage aligns its locking with this frame-buffer model. The
    // writer waits for readers to advance to the new frame before it 
    // computes the next. Readers, OTOH, never need to wait. And if
    // read transactions are short-lived, the writer won't wait long.
    let inline withRTX (db : DB) (action : MDB_txn -> 'x) : 'x =
        let rdlock = acquireReadLock db
        try let tx = mdb_rdonly_txn_begin db.db_env
            try action tx
            finally mdb_txn_commit tx // release MDB_txn memory
        finally rdlock.Release()

    let matchVal (vtx : Val) (vdb : MDB_val) : bool =
        if (vtx.Length <> int vdb.size) then false else
        if (0 = vtx.Length) then true else
        BS.withPinnedBytes vtx (fun pvtx -> 
            0 = (Memory.memcmp (pvtx) (vdb.data) (vtx.Length)))

    // search for an invalid read assumption
    let findInvalidRead db rtx wb rd =
        let inline validAssumption k v = 
            if not (isValidKey k) then false else
            match BTree.tryFind k wb with
            | Some vwb -> (v = vwb) // compare with pending update
            | None -> matchVal v (dbReadKeyZC db rtx k) // compare with DB
        let invalidAssumption ((k,v)) = not (validAssumption k v)
        Seq.tryFind invalidAssumption (BTree.toSeq rd)

    let inline dbHasRsc (db:DB) (rtx:MDB_txn) (k:StowKey) : bool =
        mdb_contains rtx (db.db_stow) (k.v)

    let dbHasWork (db:DB) : bool =
        (not (List.isEmpty db.db_commit) ||
         not (Map.isEmpty db.db_newrsc))

    // Reference Counts Management
    let updRCU (rc:RC) (u:RCU) (k:StowKey) : RCU =
        match Map.tryFind k u with
        | None -> Map.add k rc u
        | Some rc0 -> Map.add k (rc0 + rc) u

    // utilities
    let valRCU (rc:RC) (u0:RCU) (v:Val) : RCU =
        foldHashDeps (fun u h -> updRCU rc u (rscStowKey h)) u0 v
    let kvmRCU (rc:RC) (m:KVMap) (u0:RCU) : RCU =
        BTree.fold (fun u _ v -> valRCU rc u v) u0 m
    let rscRCU (m:Stowage) (u0:RCU) : RCU =
        Map.fold (fun u sk (struct(h,v)) -> updRCU 0L (valRCU 1L u v) sk) u0 m


    let addToWriteBatch (ws:KVMap) (wb:KVMap) : KVMap =
        if BTree.isEmpty wb then ws else
        BTree.fold (fun wb' k v -> BTree.add k v wb') wb ws

    // Write Frame.
    //
    // Write Goals:
    //  - first commit within the batch always succeeds
    //  - batch multiple updates on same key if possible
    //  - new resources observable while write incomplete
    //  - minimize writer wait on reader
    //
    // Commits are aggregated in a list, and we'll simply process the full
    // list in each write loop. New resources will be held in the db_newrsc
    // map until fully written and synchronized to disk. 
    //
    // We minimize the writer wait on the reader by advancing the reader frame
    // after commit but before sync, then waiting on readers after sync. This
    // ensures we only gain latency from readers that require longer than sync. 
    //
    // GC Goals: 
    //  - readers can atomically increment ephemeral roots without waiting
    //  - GC of balanced trees or deep lists should be about the same
    //  - limit GC effort per write frame, but keep up with busy writer
    //
    // The first point means we cannot GC any RscHash that a reader might
    // be concurrently reading via rooted keys. Conservatively, we can just
    // delay GC for anything the writer decrefs within the current frame.
    // 
    // OTOH, since I want efficient GC for deep lists (less than one write
    // frame per node), we cannot extend this beyond the rooted values and
    // we must follow deep structure of values during GC.
    //
    // Incremental GC involves limited write effort per frame, but a constant
    // amount of effort may prove unable to keep up with a very busy writer. 
    // So our GC effort must also be proportional (roughly) to write effort.

    let rec dbWriterLoop (db:DB) : unit =
        // wait for writer tasks
        let (stowedRsc,commitList,halt) = lock db (fun () ->
            if not (dbHasWork db) 
                then ignore(Monitor.Wait(db))
            let r = (db.db_newrsc, db.db_commit, db.db_halt)
            db.db_commit <- List.empty
            r)

        let wtx = mdb_readwrite_txn_begin (db.db_env) 

        // Compute write batch, overwritten data, newly stowed resources
        let tryCommit ((rd,ws,tcs) : Commit) wb =
            if Option.isSome (findInvalidRead db wtx wb rd)
                then tcs.TrySetResult(false) |> ignore; wb 
                else addToWriteBatch ws wb
        let writes = List.foldBack tryCommit commitList BTree.empty 
        let overwrites = BTree.map (fun k _ -> dbReadKey db wtx k) writes
        let writeRsc = Map.filter (fun sk _ -> not (dbHasRsc db wtx sk)) stowedRsc 

        // Write keys and new resources.
        BTree.iter (dbWriteKey db wtx) writes
        Map.iter (fun _ struct(h,v) -> dbPutRsc db wtx h v) writeRsc

        // Root refcts modified based on recent writes
        let rcRoots = 
            Map.empty 
              |> kvmRCU (-1L) overwrites
              |> kvmRCU 1L writes
              |> rscRCU writeRsc
              |> Map.map (fun sk rc -> rc + dbGetRefct db wtx sk)

        // GC effort must be roughly proportional to write effort
        // otherwise GC might fall behind when writer is very busy
        let gcLo = 100 + (2 * (Map.count rcRoots)) 
        let gcHi = 500 + (5 * gcLo)

        // prevent GC for a subset of resources
        let blockGC sk = Map.containsKey sk rcRoots // resources touched this frame
                      || (dbHasEph db (skEphID sk)) // ephemeral root

        let updRefct (struct(nGC, rcTbl, rcu)) sk n = 
            let rc0 = 
                match Map.tryFind sk rcTbl with
                | None -> dbGetRefct db wtx sk
                | Some rc -> rc
            let rc' = (rc0 + n)
            assert(rc' >= 0L) // sanity check
            let doGC = (0L = rc') && (nGC < gcHi) && not (blockGC sk)
            if not doGC then struct(nGC, (Map.add sk rc' rcTbl), rcu) else
            let vDel = dbDelRsc db wtx sk // we GC as we go
            let nGC' = nGC + 1 + (vDel.Length / 4096)
            let rcu' = valRCU (-1L) rcu vDel
            struct(nGC', (Map.remove sk rcTbl), rcu') 

        let rec gcLoop nGC rcTbl rcu =
            // breadth-first GC (albeit limited by gcHi)
            if(Map.isEmpty rcu) then (nGC,rcTbl) else
            let struct(nGC',rcTbl',rcu') = 
                Map.fold updRefct (struct(nGC,rcTbl,Map.empty)) rcu
            gcLoop nGC' rcTbl' rcu'

        // initial GC from db_zero table, filtering protected resources
        let gcCand = 
            LMDB.mdb_keys_from wtx (db.db_zero) (BS.empty)
                |> Seq.filter (fun k -> not (blockGC (StowKey k)))
                |> Seq.truncate gcLo
                |> Seq.fold (fun m k -> Map.add (StowKey k) 0L m) Map.empty

        let (nGC,rcTbl) = gcLoop 0 rcRoots gcCand

        Map.iter (dbSetRefct db wtx) rcTbl  // write final refcts
        if(nGC >= gcLo) // heuristically continue incremental GC
            then dbSignal db 

        // finalize write frame        
        mdb_txn_commit wtx // overwrite old frame with new frame
        let oldReaders = advanceReaderFrame db // move new readers to new frame
        mdb_env_sync (db.db_env) // sync updates to disk (EXPENSIVE)
        oldReaders.Wait() // wait on readers of old frame

            // note: it is feasible to delay oldReaders.Wait() until just before
            // the next txn_commit, but the benefits are minor. Sync is the most
            // expensive step. And it adds complexity, and requires delaying GC 
            // for another frame (oldReaders have longer to add ephemeral roots).
            // It's simple and effective to just use two frames. 
       
        // signal completion of updates only after sync to disk
        let signalSuccess ((_,_,tcs) : Commit) = 
            tcs.TrySetResult(true) |> ignore 
        List.iter signalSuccess commitList

        // clear stowage requests processed this write frame
        lock db (fun () -> 
            db.db_newrsc <- Map.fold (fun m sk _ -> Map.remove sk m) 
                                db.db_newrsc stowedRsc)
        //printf "Written: %d keys, %d rscs (%d in queue, %d collected)\n" (Map.count writes) (Map.count writeRsc) (Map.count db.db_newrsc) nGC

        // begin next write frame unless we're halted
        if (halt) then () else dbWriterLoop db

    let dbThread (db:DB) () : unit = 
        dbSignal db // force initial write step
        dbWriterLoop db 

    let lockFile (fn : string) : FileStream =
        new FileStream(
                fn, 
                FileMode.OpenOrCreate, 
                FileAccess.ReadWrite, 
                FileShare.None,
                8,
                FileOptions.DeleteOnClose)

    // assuming we're in the target directory, build the database
    let openDB (path : string) (maxSizeMB : int) : DB =
        Directory.CreateDirectory(path) |> ignore
        let flock = lockFile (Path.Combine(path,".lock"))
        let env = mdb_env_create ()
        mdb_env_set_mapsize env maxSizeMB
        mdb_env_set_maxdbs env 4
        let envFlags = MDB_NOSYNC ||| MDB_WRITEMAP ||| MDB_NOTLS ||| MDB_NOLOCK
        mdb_env_open env path envFlags

        // open named databases
        let txn = mdb_readwrite_txn_begin env
        let dbData = mdb_dbi_open txn "@" // root key-value
        let dbStow = mdb_dbi_open txn "$" // stowed resources
        let dbRfct = mdb_dbi_open txn "#" // positive refcts
        let dbZero = mdb_dbi_open txn "0" // zero refcts
        mdb_txn_commit txn

        let db : DB = 
            { db_env  = env
              db_data = dbData
              db_stow = dbStow
              db_rfct = dbRfct
              db_zero = dbZero
              db_flock = flock
              db_ephtbl = new EphTbl.Table()
              db_rdlock = new ReadLock()
              db_newrsc = Map.empty
              db_commit = List.empty
              db_halt = false
            }
        (new Thread(dbThread db)).Start()
        db

    let closeDB (db : DB) : unit =
        let tcs = new TaskCompletionSource<bool>()
        lock db (fun () ->
            if(db.db_halt) then invalidOp "DB already closed" else
            db.db_halt <- true
            db.db_commit <- (BTree.empty,BTree.empty,tcs)::(db.db_commit)
            Monitor.PulseAll(db))
        tcs.Task.Wait()
        assert(tcs.Task.Result)
        mdb_env_close (db.db_env) // gracefully close the DB
        db.db_flock.Dispose()     // release the file lock


