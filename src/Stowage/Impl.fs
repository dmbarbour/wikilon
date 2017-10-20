
namespace Stowage

open System
open System.IO
open System.Threading
open System.Threading.Tasks
open System.Security
open System.Runtime.InteropServices
open Data.ByteString

open Stowage.Memory
open Stowage.LMDB

#nowarn "9" 

[< SecuritySafeCriticalAttribute >]
module internal I =

    // Reference Counts Management
    let updRCU (rc:RC) (u:RCU) (k:StowKey) : RCU =
        match Map.tryFind k u with
        | None -> Map.add k rc u
        | Some rc0 -> Map.add k (rc0 + rc) u

    // utilities
    let valRCU (rc:RC) (u0:RCU) (v:Val) : RCU =
        RscHash.foldHashDeps (fun u h -> updRCU rc u (rscStowKey h)) u0 v
    let kvmRCU (rc:RC) (m:KVMap) (u0:RCU) : RCU =
        BTree.fold (fun u _ v -> valRCU rc u v) u0 m
    let rscRCU (m:Stowage) (u0:RCU) : RCU =
        Map.fold (fun u sk (struct(h,v)) -> updRCU 0L (valRCU 1L u v) sk) u0 m


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
    //  - resources referenced from asynchronous writes are protected
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
    //
    // To protect asynchronous writes, I currently freeze decrefs within each
    // write frame (by shunting them instead to a list). 
    //
    // TODO: enable GC of newly stowed resources, such that they're never
    // written to the database layer. This would improve performance in some
    // cases where a lot of intermediate data is constructed, especially when
    // the stowage buffer is large and writes are infrequent.
    let dbWriteFrame db commitList stowedRsc =

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
        let gcLo = 10 * (100 + (Map.count rcRoots)) 
        let gcHi = 4 * gcLo

        // prevent GC for a subset of resources
        let blockGC sk = Map.containsKey sk rcRoots // resources touched this frame
                      || (db.ephtbl.Contains (skEphID sk)) // ephemeral root

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
            let nGC' = nGC + 1 + (vDel.Length >>> 10)
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
            LMDB.mdb_keys_from wtx (db.zero) (BS.empty)
                |> Seq.filter (fun k -> not (blockGC (StowKey k)))
                |> Seq.truncate gcLo
                |> Seq.fold (fun m k -> Map.add (StowKey k) 0L m) Map.empty
        let (nGC,rcTbl) = gcLoop 0 rcRoots gcCand
        Map.iter (dbSetRefct db wtx) rcTbl  // write final refcts

        // if GC is making good progress, continue GC in next frame.
        // (but don't bother if it's just a few items GC'd)
        let gcContinueThresh = 100
        if(nGC >= gcContinueThresh)
            then dbSignal db 
                 // printfn "%d resources deleted" nGC
                 

        // finalize write frame        
        mdb_txn_commit wtx // overwrite old frame with new frame
        let oldReaders = advanceReaderFrame db // move new readers to new frame
        mdb_env_sync (db.env) // sync updates to disk (EXPENSIVE)

        // signal successful updates after sync to disk
        let signalSuccess ((_,_,tcs) : Commit) = 
            tcs.TrySetResult(true) |> ignore 
        List.iter signalSuccess commitList

        // Wait for readers of the previous frame to finish, such that
        // all concurrent readers are operating on the current frame.
        oldReaders.Wait()


       
    let inline dbHasWork (db:DB) : bool =
        (not (List.isEmpty db.commit))
            || (db.buffer > db.threshold)

    let inline dbAwaitWork (db:DB) : unit =
        lock db (fun () ->
            if not (dbHasWork db)
                then ignore (Monitor.Wait(db)))

    let rec dbWriterLoop (db:DB) : unit =
        dbAwaitWork db
        db.ephtbl.DelayDecrefs() // protect asynch writes
        let (commit,stow,halt) = lock db (fun () ->
            // commit and halt MUST be read together for closeDB.
            let state = (db.commit, db.stowing, db.halt)
            db.commit <- List.empty
            db.writing <- db.stowing
            db.stowing <- Map.empty
            db.buffer <- 0
            state)
        try dbWriteFrame db commit stow
        finally 
            db.ephtbl.PassDecrefs() 
            db.writing <- Map.empty
        if halt then () else dbWriterLoop db

    let dbThread (db:DB) () : unit = 
        dbSignal db // force initial write step
        dbWriterLoop db 

    // assuming we're in the target directory, build the database
    let openDB (path : string) (maxSizeMB : int) : DB =
        Directory.CreateDirectory(path) |> ignore
        let flock = lockFile (Path.Combine(path,"lock"))
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
            { env  = env
              data = dbData
              stow = dbStow
              rfct = dbRfct
              zero = dbZero
              flock = flock
              ephtbl = new EphTable()
              rdlock = new ReadLock()
              writing = Map.empty
              stowing = Map.empty
              buffer = 0
              threshold = dbThreshDefault
              commit = List.empty
              halt = false
            }
        (new Thread(dbThread db)).Start()
        db


