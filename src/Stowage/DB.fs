namespace Stowage
open Stowage.Internal.LMDB
open System.IO

/// Stowage is a key-value database that features garbage collected
/// references between binaries via secure hashes.
///
/// The ability to reference binaries via secure hashes enables the
/// stowage database to contain larger-than-memory persistent data
/// structures in a purely functional style. It also enables a high
/// degree of structure sharing. Garbage collection is important to
/// easily manipulate this data.
///
/// Stowage is implemented above LMDB, a memory-mapped B-tree. Stowage
/// transactions are optimistic and lightweight, held in memory until
/// commit, and small concurrent transactions may be batched together
/// insofar as there are no conflicts.
module DB =

    type DB = 
        { 
            db_lock : FileStream    
            db_env  : MDB_env     
            db_data : MDB_dbi     // user string -> data
            db_stow : MDB_dbi     // secure hash -> data
            db_rfct : MDB_dbi     // hashes with refct > 0
            db_zero : MDB_dbi     // hashes with zero refct
        }  

    // still needed: Task data and synchronization primitives?
    // or maybe a separate thread with Monitor?
 
    // fragment of hash used for stowage keys
    let private stowKeyLen = Hash.validHashLen / 2

    let inline private withDir (p : string) (op : unit -> 'R) : 'R =
        do ignore <| System.IO.Directory.CreateDirectory(p) 
        let p0 = System.IO.Directory.GetCurrentDirectory()
        try 
            do ignore <| System.IO.Directory.SetCurrentDirectory(p)
            op ()
        finally
            System.IO.Directory.SetCurrentDirectory(p0)

    // assuming we're in the target directory, build the database
    let inline private mkDB (maxSizeMB : int) () : DB =
        let flock = new System.IO.FileStream(".lock", FileMode.OpenOrCreate, FileAccess.ReadWrite, FileShare.None)
        let maxSizeBytes = (1024UL * 1024UL) * uint64 (max 1 maxSizeMB)
        raise (System.NotImplementedException "Stowage DB load")

    let load (path : string) (maxSizeMB : int) : DB = 
        withDir path (mkDB maxSizeMB)
                


(*

    , newTX, txDB, dupTX
    , readKey, readKeyDB
    , readKeys, readKeysDB
    , writeKey, assumeKey
    , loadRsc, loadRscDB
    , withRsc, withRscDB
    , stowRsc
    , clearRsc, clearRsc'
    , commit, commit_async
    , check
    , gcDB, gcDB_async
    , hashDeps
    , FilePath
    , ByteString
    , Hash
    
*)
  
    

