
namespace Stowage

open System.IO
open System.Threading
open System.Threading.Tasks
open System.Security
open System.Runtime.InteropServices
open Data.ByteString

open Stowage.Types
open Stowage.Hash
open Stowage.Memory
open Stowage.LMDB

#nowarn "9" 

[< SecuritySafeCriticalAttribute >]
module internal I =

    // the db_stow table uses a half key
    type Stowage = Map<RscHash,Val>     // recent stowage requests

    // The underlying database tables uses only half RscHash for lookup
    // or reference count management. The latter half is verified in 
    // constant time to resist leaking of hashes via timing attacks.
    let stowKeyLen = rscHashLen / 2
    type StowKey = ByteString           // of stowKeyLen

    type RC = int64                     // sufficient for RC
    let maxRCLen = 18                   // length in bytes
    let maxRC = 999_999_999_999_999_999L // what can we squeeze into 18 digits?
    type RCU = Map<StowKey, RC>         // pending refct updates

    type EphID = uint64                 // hash of stowKey
    type EphRoots = Map<EphID,RC>       // ephemeral root set
    let inline rscStowKey (h:RscHash) : StowKey =
        assert(h.Length = rscHashLen)
        Data.ByteString.take stowKeyLen h

    // Each commit consists of:
    //  a set of values read (or assumed) to validate
    //  a set of values to be written 
    //  a task completion resource to report success or failure
    type Commit = (KVMap * KVMap * TaskCompletionSource<bool>)

    // Readlock state is essentially a reader-count with an event
    // for when we reach zero readers.
    type ReadLock () = 
        let mutable rc = 0
        member this.Acquire () = lock this (fun () -> 
            rc <- rc + 1)
        member this.Release () = lock this (fun () ->
            assert (rc > 0)
            rc <- rc - 1
            if(0 = rc) then Monitor.PulseAll(this))
        member this.Wait () = lock this (fun () -> 
            while(0 <> rc) do ignore(Monitor.Wait(this)))

    type DB =
        { 
            db_env  : MDB_env     
            db_data : MDB_dbi     // key -> value
            db_stow : MDB_dbi     // resource ID -> value
            db_rfct : MDB_dbi     // resources with refct > 0
            db_zero : MDB_dbi     // resources with zero refct

            mutable db_rdlock : ReadLock        // current read-lock (updated per frame).
            mutable db_ephtbl : EphRoots        // ephemeral resource roots
            mutable db_newrsc : Stowage         // recent stowage requests
            mutable db_commit : Commit list     // pending commit requests
            mutable db_trygc  : bool            // incremental GC available

            db_fini : System.Object  // extra finalizer
        }

    // Our reference count tables simply have data of type [1-9][0-9]*.
    // Reasonably, reference counts shouldn't be larger than 18 digits.
    let dbGetRefct (db:DB) (rtx:MDB_txn) (k:StowKey) : RC =
        assert(k.Length = stowKeyLen)
        let vOpt = mdb_getZC rtx (db.db_rfct) k 0
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

    let inline refctBytes (rc:RC) : ByteString =
        assert((maxRC >= rc) && (rc > 0L))
        let data = Array.zeroCreate maxRClen
        let rec write ix rc =
            if(0 = rc) 
                then Data.ByteString.unsafeCreate data ix (data.Length - ix)
                else data.[ix - 1] <- (byte '0' + byte (rc % 10L))
                     write (ix - 1) (rc / 10L)
        write data.Length rc

    let dbSetRefct (db:DB) (wtx:MDB_txn) (k:StowKey) (rc:RC) : unit =
        let validRefct = (maxRC >= rc) && (rc >= 0L)
        if not validRefct then invalidArg "rc" "refct out of range" else
        if (k.Length <> stowKeyLen) then invalidArg "k" "refct via stowage key" else
        if (0L = rc)
            then mdb_del wtx (db.db_rfct) k |> ignore
                 mdb_put wtx (db.db_zero) k (Data.ByteString.empty)
            else mdb_del wtx (db.db_zero) k |> ignore
                 mdb_put wtx (db.db_rfct) k (refctBytes rc)


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
            let matchRem = withPinnedBytes hRem (fun ra -> memcteq ra v.data rlen)
            if not matchRem then None else
            let size' = v.size - unativeint rlen
            let data' = v.data + nativeint rlen
            Some(MDB_val(size',data'))

    let inline dbGetRsc (db:DB) (rtx:MDB_txn) (h:RscHash) : Val option =
        dbGetRscZC db rtx h |> Option.map Stowage.LMDB.val2bytes



    let dbDelRsc (db:DB) (wtx:MDB_txn) (k:StowKey) : unit =
        if (k.Length <> stowKeyLen) then invalidArg "k" "invalid stowage key" else
        mdb_del wtx (db.db_stow) k |> ignore
        mdb_del wtx (db.db_zero) k |> ignore
        mdb_del wtx (db.db_rfct) k |> ignore                

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
        | Some v -> v
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

    // the DB will have a dedicated writer thread.
    // the DB object serves mutex and pulse/wait signal, via Monitor
    let dbCommit (db : DB) (c : Commit) : unit =
        lock db (fun () ->
            db.db_commit <- (c :: db.db_commit)
            Monitor.PulseAll(db))

    // Perform operation while holding reader TX.
    //
    // LMDB with NOLOCK is essentially a frame-buffered database.
    // At most times, we have two valid frames. During commit, we
    // drop the older frame and write the new one. Stowage aligns
    // its locking with this model, so readers never wait and the
    // writer waits only on readers that hold the lock for nearly
    // two full write frames.
    //
    // We can assume most readers only hold the lock briefly, and
    // hence our writer very rarely waits.
    let inline withRTX (db : DB) (action : MDB_txn -> 'x) : 'x =
        let rdlock = lock db (fun () -> 
            // lock prevents advanceReaderFrame during Acquire
            db.db_rdlock.Acquire() 
            db.db_rdlock)
        try let tx = mdb_rdonly_txn_begin db.db_env
            try action tx
            finally mdb_txn_commit tx // release MDB_txn memory
        finally rdlock.Release()

    let advanceReaderFrame (db : DB) : ReadLock = lock db (fun () ->
        let oldFrame = db.db_rdlock
        db.db_rdlock <- new ReadLock()
        oldFrame)


    let matchVal (vtx : Val) (vdb : MDB_val) : bool =
        if (vtx.Length <> int vdb.size) then false else
        if (0 = vtx.Length) then true else
        withPinnedBytes vtx (fun pvtx -> 
            0 = (memcmp (pvtx) (vdb.data) (vtx.Length)))

    // search for an invalid read assumption
    let findInvalidRead db rtx wb rd =
        let validAssumption k v = 
            if not (isValidKey k) then false else
            match Map.tryFind k wb with
            | Some vwb -> (v = vwb)
            | None -> matchVal v (getValZC db rtx k)
        let invalidAssumption k v = not (validAssumption k v)
        Map.tryFindKey invalidAssumption rd

    // utilities to work with ephemeron tables
    let ephInc (etb : EphRoots) (k : StowKey) (rcu : RC) : EphRoots =
        assert (rcu > 0L)
        match Map.tryFind k etb with
        | None -> Map.add k rcu etb
        | Some rc0 -> Map.add k (rcu + rc0) etb

    let ephDec (etb : EphRoots) (k : StowKey) (rcu : RC) : EphRoots =
        assert (rcu > 0L)
        match Map.tryFind k etb with
        | None -> failwith "negative refct"
        | Some rc0 ->
            assert (rc0 >= rcu)
            let rc' = rc0 - rcu
            if (0L = rc') then Map.remove k etb
                          else Map.add k rc' etb 

    // compute ephemeral roots from a value
    let accumValEphs : EphRoots -> Val -> EphRoots =
        scanHashDeps (fun r h -> ephInc r (rscEphId h) 1L)

    let inline ephAdd (upd : EphRoots) (etb0 : EphRoots) : EphRoots =
        Map.fold ephInc etb0 upd

    let inline ephRem (upd : EphRoots) (etb0 : EphRoots) : EphRoots =
        Map.fold ephDec etb0 upd

    let inline dbUpdEphRoots (db : DB) (fn : EphRoots -> EphRoots) : unit =
        lock db (fun () -> (db.db_ephtbl <- fn db.db_ephtbl))

    let dbAddEphRoots (db : DB) (upd : EphRoots) : unit =
        if(not (Map.isEmpty upd)) then dbUpdEphRoots db (ephAdd upd)

    let dbRemEphRoots (db : DB) (upd : EphRoots) : unit =
        if(not (Map.isEmpty upd)) then dbUpdEphRoots db (ephRem upd)

    // add stowage to both the newrsc pool and ephemeral root.
    let dbAddStowage (db : DB) (rtx:MDB_txn) (h : RscHash) (v : Val) : EphID =
        assert (h.Length = rscHashLen)
        let k = rscEphId h
        lock db (fun () ->
            db.db_newrsc <- Map.add h v (db.db_newrsc)
            db.db_ephtbl <- ephInc (db.db_ephtbl) k 1L
            Monitor.PulseAll(db))
        k

    let inline hasRscMDB (db:DB) (rtx:MDB_txn) (h:RscHash) : bool =
        mdb_contains rtx (db.db_stow) (rscHashToStowKey h)

    let dbHasWork (db:DB) : bool =
        (db.db_trygc ||
         not (List.isEmpty db.db_commit) ||
         not (Map.isEmpty db.db_newrsc))

    // Reference Counts Management
    type RC = int64                 // type for reference counts or deltas
    type RCU = Map<StowKey, RC>     // proposed updates to reference counts
    let updRCU (rc:RC) (u:RCU) (h:RscHash) : RCMap =
        assert(h.Length = rscHashLen)
        let sk = Data.ByteString.take stowKeyLen h
        match Map.tryFind sk u with
        | None -> Map.add sk rc u
        | Some rc0 -> Map.add sk (rc0 + rc) u

    // reference counts within the database are simply [1-9][0-9]* texts.
    // I assume/assert a 64-bit integer is sufficient.
    let dbGetRefct (db:DB) (rtx:MDB_txn) (sk:StowKey) : RC =
        assert(stowKeyLen = sk.Length)
        let vOpt = mdb_getZC rtx (db.db_refct) sk
        match vOpt with
        | None -> 0L
        | Just v -> 
            assert(18L >= v.size); // 64 bits sufficient?
            let rec parseNat acc ptr len =
                if(0 = len) then acc else
                let b = Marshal.ReadByte(ptr)  
                assert((byte '9' >= b) && (b >= byte '0'))
                let acc' = (10L * acc) + (int64 (b - (byte '0'))
                loop acc' (1n + ptr) (len - 1)
            parseNat 0L (v.data) (int (v.size))

    // utilities
    let valRCU (rc:RC) (u:RCU) (v:Val) : RCU =
        scanHashDeps (updRCU rc) u v
    let kvmRCU (rc:RC) (m:KVMap) (u0:RCU) : RCU =
        Map.fold (fun u _ v -> valRCU rc u v) u0 m
    let rscRCU (m:Stowage) (u0:RCU) : RCU =
        Map.fold (fun u k v -> updRCU 0L (valRCU 1L u v) h)

    // seek a set of keys for which pending GC may continue
    let dbGCPend (db:DB) (rtx:MDB_txn) (hold:EphRoots) (quota:int) : RCU =
        raise (NotImplementedException())

    let rec dbWriterLoop (db:DB) (rlock:ReadLock) : unit =
        let rec waitForWork () = 
            if dbHasWork db then () else 
            ignore (Monitor.Wait(db))
            waitForWork ()
        let (stowedRsc,commitList) = lock db (fun () -> 
            waitForWork()
            let result = (db.db_newrsc, db.db_commit)
            db.db_commit <- List.empty
            db.db_trygc <- false
            result)
        let ephRoots : EphRoots = 
            // current db_ephtbl + recently stowed resources
            let addStowed e k _ = Map.add (rscEphId k) 1L e
            let initHold = db.db_ephtbl // assuming atomic read
            Map.fold addStowed initHold stowedRsc

        let wtx = mdb_readwrite_txn_begin (db.db_env) 

        // Compute write batch, overwritten data, newly stowed resources
        let tryCommit ((rd,ws,tcs) : Commit) wb =
            if Option.isSome (findInvalidRead db wtx wb rd)
                then tcs.TrySetResult(false) |> ignore; wb 
                else Map.fold (fun m k v -> Map.add k v m) wb ws
        let writes = List.foldBack tryCommit commitList Map.empty 
        let overwrites = Map.map (fun k _ -> readKeyDB db wtx k) writes 
        let writeRsc = Map.filter (fun k _ -> not (hasRscMDB db wtx k)) stowedRsc 

        // compute updated root reference counts.
        let writeEffort = Map.count writes + Map.count writeRsc
        let maxExtraGC = 50 + (2 * writeEffort) // 
        let maxTotalGC = 5 * maxExtraGC 
        
        let posRC = 
            Map.empty 
              |> kvmRCU 1L writes 
              |> rscRCU writeRsc
              |> Map.map (fun sk delta -> (delta + (dbGetRefct db wtx sk)))

        


        
        
        // 

        

        
       
        let rlock' = advanceReaderFrame db
        db.db_trygc <- false // should depend on GC progress
        dbWriterLoop db rlock'


    let inline dbThreadStart (db:DB) : unit = 
        dbWriterLoop db (advanceReaderFrame db)


    let inline private withDir (p : string) (op : unit -> 'R) : 'R =
        do ignore <| System.IO.Directory.CreateDirectory(p) 
        let p0 = System.IO.Directory.GetCurrentDirectory()
        try 
            do ignore <| System.IO.Directory.SetCurrentDirectory(p)
            op ()
        finally
            System.IO.Directory.SetCurrentDirectory(p0)

    let inline lockFile (fn : string) : FileStream =
        new FileStream(
                fn, 
                FileMode.OpenOrCreate, 
                FileAccess.ReadWrite, 
                FileShare.None,
                8,
                FileOptions.DeleteOnClose)

    // assuming we're in the target directory, build the database
    let inline private mkDB (maxSizeMB : int) () : DB =
        let lock = lockFile ".lock"
        let env = mdb_env_create ()
        mdb_env_set_mapsize env maxSizeMB
        mdb_env_set_maxdbs env 4
        let envFlags = MDB_NOSYNC ||| MDB_WRITEMAP ||| MDB_NOTLS ||| MDB_NOLOCK
        mdb_env_open env "." envFlags

        // introduce a DB finalizer
        let dbFini = { new System.Object() with
                        override x.Finalize() =
                            mdb_env_sync env
                            mdb_env_close env
                            lock.Dispose()
                     }

        // open named databases
        let txn = mdb_readwrite_txn_begin env
        let dbData = mdb_dbi_open txn "@" // root key-value
        let dbStow = mdb_dbi_open txn "$" // stowed resources
        let dbRfct = mdb_dbi_open txn "#" // positive refcts
        let dbZero = mdb_dbi_open txn "0" // zero refcts
        mdb_txn_commit txn
        let db : I.DB = 
            { db_env  = env
              db_data = dbData
              db_stow = dbStow
              db_rfct = dbRfct
              db_zero = dbZero
              db_rdlock = new I.ReadLock()
              db_ephtbl = Map.empty
              db_newrsc = Map.empty
              db_commit = List.empty
              db_trygc = true
              db_fini = dbFini
            }
        (new Thread(fun () -> I.dbThreadStart db)).Start()
        new DB(db)
    // TODO: 


