
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
        StowKey(Data.ByteString.take stowKeyLen h)

    // the db_stow table uses a half key
    type Stowage = Map<StowKey, struct(RscHash * Val)>     // recent stowage requests

    type RC = int64                     // sufficient for RC
    let maxRCLen = 18                   // length in bytes
    let maxRC = 999_999_999_999_999_999L // what can we squeeze into 18 digits?
    type RCU = Map<StowKey, RC>         // pending refct updates

    type EphID = uint64                 // hash of stowKey
    type EphRoots = Map<EphID,RC>       // ephemeral root set
    let inline skEphID(h:StowKey) : EphID = ByteString.Hash64(h.v)
    let inline rscEphID(h:RscHash) : EphID = skEphID (rscStowKey h)

    // utilities to work with ephemeron tables
    let ephInc (etb : EphRoots) (k : EphID) (rc : RC) : EphRoots =
        assert (rc > 0L)
        match Map.tryFind k etb with
        | None -> Map.add k rc etb
        | Some rc0 -> Map.add k (rc + rc0) etb

    let ephDec (etb : EphRoots) (k : EphID) (rc : RC) : EphRoots =
        assert (rc > 0L)
        match Map.tryFind k etb with
        | None -> failwith "negative refct"
        | Some rc0 ->
            assert (rc0 >= rc)
            let rc' = rc0 - rc
            if (0L = rc') then Map.remove k etb
                          else Map.add k rc' etb 

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

    type DB =
        { 
            db_env  : MDB_env     
            db_data : MDB_dbi     // key -> value
            db_stow : MDB_dbi     // resource ID -> value
            db_rfct : MDB_dbi     // resources with refct > 0
            db_zero : MDB_dbi     // resources with zero refct
            db_flock : FileStream

            mutable db_rdlock : ReadLock        // current read-lock (updated per frame).
            mutable db_ephtbl : EphRoots        // ephemeral resource roots
            mutable db_newrsc : Stowage         // recent stowage requests
            mutable db_commit : Commit list     // pending commit requests
            mutable db_halt   : bool            // halt the writer
        }

    // add commit request to the database
    let dbCommit (db : DB) (c : Commit) : unit =
        lock db (fun () ->
            db.db_commit <- (c :: db.db_commit)
            Monitor.PulseAll(db))

    // signal writer to perform a step (via dummy commit)
    let dbSignal (db : DB) : unit =
        let tcs = new TaskCompletionSource<bool>() // result ignored
        dbCommit db (Map.empty, Map.empty, tcs)

    // add stowage request and ephemeral root to database, returns root ID
    let dbStow (db : DB) (h : RscHash) (v : Val) : EphID =
        let k = rscEphID h
        lock db (fun () ->
            db.db_newrsc <- Map.add (rscStowKey h) (struct(h,v)) (db.db_newrsc)
            db.db_ephtbl <- ephInc (db.db_ephtbl) k 1L
            Monitor.PulseAll(db))
        k

    // reference count management
    let inline dbUpdEph (db : DB) (fn : EphRoots -> EphRoots) : unit =
        lock db (fun () -> (db.db_ephtbl <- fn db.db_ephtbl))
    let inline dbIncEph (db : DB) (rc : RC) (k : EphID) : unit = dbUpdEph db (fun eph -> ephInc eph k rc)
    let inline dbDecEph (db : DB) (rc : RC) (k : EphID) : unit = dbUpdEph db (fun eph -> ephDec eph k rc)
    let inline ephAdd (upd : EphRoots) (etb0 : EphRoots) : EphRoots = Map.fold ephInc etb0 upd
    let inline ephRem (upd : EphRoots) (etb0 : EphRoots) : EphRoots = Map.fold ephDec etb0 upd
    let dbAddEphRoots (db : DB) (upd : EphRoots) : unit =
        if(not (Map.isEmpty upd)) then dbUpdEph db (ephAdd upd)
    let dbRemEphRoots (db : DB) (upd : EphRoots) : unit =
        if(not (Map.isEmpty upd)) then dbUpdEph db (ephRem upd)

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
                then Data.ByteString.unsafeCreate data ix (data.Length - ix)
                else data.[ix - 1] <- (byte '0' + byte (rc % 10L))
                     write (ix - 1) (rc / 10L)
        write data.Length rc

    let dbSetRefct (db:DB) (wtx:MDB_txn) (k:StowKey) (rc:RC) : unit =
        let validRefct = (maxRC >= rc) && (rc >= 0L)
        if not validRefct then invalidArg "rc" "refct out of range" else
        if (0L = rc)
            then mdb_del wtx (db.db_rfct) (k.v) |> ignore
                 mdb_put wtx (db.db_zero) (k.v) (Data.ByteString.empty)
            else mdb_del wtx (db.db_zero) (k.v) |> ignore
                 mdb_put wtx (db.db_rfct) (k.v) (refctBytes rc)

    let dbGetRscZC (db:DB) (rtx:MDB_txn) (h:RscHash) : MDB_val option =
        if(h.Length <> rscHashLen) then invalidArg "h" "bad resource ID" else
        let hKey = Data.ByteString.take stowKeyLen h
        let hRem = Data.ByteString.drop stowKeyLen h
        let vOpt = mdb_getZC rtx (db.db_stow) hKey 
        match vOpt with
        | None -> None
        | Some v ->
            let rlen = rscHashLen - stowKeyLen
            assert((unativeint (rlen + maxValLen) >= v.size) && 
                   (v.size >= unativeint rlen)                 )
            let matchRem = withPinnedBytes hRem (fun ra -> 
                    Memory.memcteq ra v.data rlen)
            if not matchRem then None else
            let size' = v.size - unativeint rlen
            let data' = v.data + nativeint rlen
            Some(MDB_val(size',data'))

    let inline dbGetRsc (db:DB) (rtx:MDB_txn) (h:RscHash) : Val option =
        dbGetRscZC db rtx h |> Option.map Stowage.LMDB.val2bytes

    // deletes resource, returns bytestring removed for refct GC
    let dbDelRsc (db:DB) (wtx:MDB_txn) (k:StowKey) : Val =
        let vOpt = mdb_get wtx (db.db_stow) (k.v)
        mdb_del wtx (db.db_stow) (k.v) |> ignore
        mdb_del wtx (db.db_zero) (k.v) |> ignore
        mdb_del wtx (db.db_rfct) (k.v) |> ignore
        match vOpt with
        | None -> Data.ByteString.empty
        | Some v -> 
            let rlen = rscHashLen - stowKeyLen
            assert(v.Length >= rlen)
            Data.ByteString.drop rlen v

    let dbPutRsc (db:DB) (wtx:MDB_txn) (h:RscHash) (v:Val) : unit =
        if(h.Length <> rscHashLen) then invalidArg "h" "invalid resource ID" else
        if not (isValidVal v) then invalidArg "v" "invalid resource value" else
        let hKey = Data.ByteString.take stowKeyLen h
        let hRem = Data.ByteString.drop stowKeyLen h
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
        if(Data.ByteString.isEmpty v) 
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
    // drop the older frame and write the new one. Stowage aligns
    // its locking with this model so readers never wait and the
    // writer waits only on readers that hold the lock for nearly
    // two full write frames. Short-lived readers won't interfere
    // with the writer at all.
    //
    // We can assume most readers only hold the lock briefly, and
    // hence our writer very rarely waits.
    let inline withRTX (db : DB) (action : MDB_txn -> 'x) : 'x =
        let rdlock = acquireReadLock db
        try let tx = mdb_rdonly_txn_begin db.db_env
            try action tx
            finally mdb_txn_commit tx // release MDB_txn memory
        finally rdlock.Release()

    let matchVal (vtx : Val) (vdb : MDB_val) : bool =
        if (vtx.Length <> int vdb.size) then false else
        if (0 = vtx.Length) then true else
        withPinnedBytes vtx (fun pvtx -> 
            0 = (Memory.memcmp (pvtx) (vdb.data) (vtx.Length)))

    // search for an invalid read assumption
    let findInvalidRead db rtx wb rd =
        let validAssumption k v = 
            if not (isValidKey k) then false else
            match Map.tryFind k wb with
            | Some vwb -> (v = vwb)
            | None -> matchVal v (dbReadKeyZC db rtx k)
        let invalidAssumption k v = not (validAssumption k v)
        Map.tryFindKey invalidAssumption rd

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
        scanHashDeps (fun u h -> updRCU rc u (rscStowKey h)) u0 v
    let kvmRCU (rc:RC) (m:KVMap) (u0:RCU) : RCU =
        Map.fold (fun u _ v -> valRCU rc u v) u0 m
    let rscRCU (m:Stowage) (u0:RCU) : RCU =
        Map.fold (fun u sk (struct(h,v)) -> updRCU 0L (valRCU 1L u v) sk) u0 m



    // approach to GC...
    //
    // I need to do incremental GC, and I don't want it to degrade much based
    // on whether we GC long lists vs. balanced tree structures. So I'll try 
    // to compute a deep reference count update with each frame. Additionally, 
    // I'll take a few candidates for GC from the db_zero pool to continue GC.
    // And I'll add to the db_zero pool after I have enough candidates.
    
    // candidates for GC from the db_zero table. This might touch a number of
    // elements proportional to the quota plus the db_ephtbl size.
    let dbContinueGC (db:DB) (rtx:MDB_txn) (eph : EphRoots) (quota : int) : RCU =
        LMDB.mdb_keys_from rtx (db.db_zero) (Data.ByteString.empty) 
            |> Seq.filter (fun k -> not (Map.containsKey (skEphID (StowKey(k))) eph))
            |> Seq.truncate quota 
            |> Seq.fold (fun m k -> Map.add (StowKey(k)) 0L m) Map.empty

    let rec dbWriterLoop (db:DB) (rlock:ReadLock) : unit =
        // wait for writer tasks
        let (stowedRsc,ephRoots,commitList,halt) = lock db (fun () ->
            if not (dbHasWork db) 
                then ignore(Monitor.Wait(db))
            let r = (db.db_newrsc, db.db_ephtbl, db.db_commit, db.db_halt)
            db.db_commit <- List.empty
            r)

        let wtx = mdb_readwrite_txn_begin (db.db_env) 

        // Compute write batch, overwritten data, newly stowed resources
        let tryCommit ((rd,ws,tcs) : Commit) wb =
            if Option.isSome (findInvalidRead db wtx wb rd)
                then tcs.TrySetResult(false) |> ignore; wb 
                else Map.fold (fun m k v -> Map.add k v m) wb ws
        let writes = List.foldBack tryCommit commitList Map.empty 
        let overwrites = Map.map (fun k _ -> dbReadKey db wtx k) writes
        let writeRsc = Map.filter (fun sk _ -> not (dbHasRsc db wtx sk)) stowedRsc 

        // Write keys and resources.
        Map.iter (dbWriteKey db wtx) writes
        Map.iter (fun _ struct(h,v) -> dbPutRsc db wtx h v) writeRsc

        // compute refct updates and perform incremental GC.
        // GC is the most complicated part of the writer loop.
        let (gcLo,gcHi) = (100,1000) // effort quotas for incremental GC
        let updRefct (struct(nGC,rcTbl,rcu)) sk n = 
            let rc0 = 
                match Map.tryFind sk rcTbl with
                | None -> dbGetRefct db wtx sk
                | Some rc -> rc
            let rc' = (rc0 + n)
            assert(rc' >= 0L) // sanity check
            let doGC = (0L = rc') && (nGC < gcHi) &&
                       not (Map.containsKey (skEphID sk) ephRoots)
            if not doGC then struct(nGC, (Map.add sk rc' rcTbl), rcu) else
            let vDel = dbDelRsc db wtx sk // we GC as we go
            let rcu' = valRCU (-1L) rcu vDel
            struct((1+nGC), (Map.remove sk rcTbl), rcu') 
        let rec gcLoop nGC rcTbl rcu =
                if(Map.isEmpty rcu) then (nGC,rcTbl) else
                let struct(nGC',rcTbl',rcu') = 
                    Map.fold updRefct (struct(nGC,rcTbl,Map.empty)) rcu
                gcLoop nGC' rcTbl' rcu'
        let rcu0 = // initial refct updates
            dbContinueGC db wtx ephRoots gcLo
              |> kvmRCU 1L writes 
              |> kvmRCU (-1L) overwrites
              |> rscRCU writeRsc
        let (nGC,rcTbl) = gcLoop 0 Map.empty rcu0

        Map.iter (dbSetRefct db wtx) rcTbl
        if(nGC >= gcLo) 
            then dbSignal db // heuristically continue GC next frame

        // finalize write frame        
        rlock.Wait() // wait for readers of old frame to finish
        mdb_txn_commit wtx // overwrite old frame with new frame
        let rlock' = advanceReaderFrame db // advance readers to new frame(*)
            // (*) race condition for new readers between commit and advance
            //     is safe, at worst increases latency for next rlock.Wait().
        mdb_env_sync (db.db_env) // sync updates to disk
        
        // signal completion of updates only after sync to disk
        let signalSuccess ((_,_,tcs) : Commit) = 
            tcs.TrySetResult(true) |> ignore 
        List.iter signalSuccess commitList

        // clear stowage requests processed this write frame
        lock db (fun () -> 
            db.db_newrsc <- Map.fold (fun m h _ -> Map.remove h m) db.db_newrsc stowedRsc)

        // begin next write frame unless we're halted
        if (halt) then () else dbWriterLoop db rlock'

    let dbThread (db:DB) () : unit = 
        dbSignal db // force initial write step
        dbWriterLoop db (advanceReaderFrame db)

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
              db_rdlock = new ReadLock()
              db_ephtbl = Map.empty
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
            db.db_commit <- (Map.empty,Map.empty,tcs)::(db.db_commit)
            Monitor.PulseAll(db))
        let ok = tcs.Task.Result  // wait for final write & sync
        assert(ok)
        mdb_env_close (db.db_env) // gracefully close the DB
        db.db_flock.Dispose()     // release the file lock


