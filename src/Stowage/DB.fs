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
/// commit. The writer operates in a separate Task.
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

    /// Load or create database.
    let load (path : string) (maxSizeMB : int) : DB =
        do ignore <| System.IO.Directory.CreateDirectory(path)
        raise (System.NotImplementedException "Stowage DB load")



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
  
    

