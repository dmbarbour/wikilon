#nowarn "9" // disable warning for StructLayoutAttribute

namespace Stowage.Internal
open System.Runtime.InteropServices


// slice of LMDB API and FFI used for Stowage
//  I could use the LightningDB package, but I'd still mostly use
module LMDB =
    type MDB_env = nativeint
    type MDB_txn = nativeint
    type MDB_dbi = uint32
    type MDB_cursor = nativeint
    type MDB_cursor_op = int
    type mdb_mode_t = int
    type size_t = nativeint
    type pvoid = nativeint

    [< StructLayoutAttribute(LayoutKind.Sequential) >]
    type MDB_val =
        struct
            val mv_size : size_t
            val mv_data : pvoid 
        end

    let defaultMode : mdb_mode_t = 0o660

    // Environment Flags
    let MDB_NOSYNC   =  0x10000     // writer handles sync explicitly
    let MDB_WRITEMAP =  0x80000     // avoid extra malloc/copy on write
    let MDB_NOTLS    = 0x200000     // using lightweight threads
    let MDB_NOLOCK   = 0x400000     // specialized locking model
    
    // Database Flags
    let MDB_CREATE  = 0x40000   // create DB if it doesn't exist

    // Write Flags
    let MDB_RESERVE = 0x10000   // reserve space for writing data
    let MDB_NOOVERWRITE = 0x10  // return error on overwrite

    // Cursor Ops
    let MDB_FIRST = 0           // first item in table
    let MDB_NEXT  = 8           // next cursor item

    // API native methods
    //
    // Note: I'm not happy with .Net's conversion of strings. But I don't 
    // use anything outside of ASCII within Stowage. 
    [< DllImport("lmdb", CallingConvention = CallingConvention.Cdecl) >] 
    extern int mdb_env_create(MDB_env& env);

    [< DllImport("lmdb", CallingConvention = CallingConvention.Cdecl) >] 
    extern int mdb_env_open(MDB_env env, string path, uint32 flags, mdb_mode_t mode);

    [< DllImport("lmdb", CallingConvention = CallingConvention.Cdecl) >] 
    extern int mdb_env_set_mapsize(MDB_env env, size_t size);

    [< DllImport("lmdb", CallingConvention = CallingConvention.Cdecl) >] 
    extern int mdb_env_set_maxdbs(MDB_env env, MDB_dbi dbs);

    [< DllImport("lmdb", CallingConvention = CallingConvention.Cdecl) >] 
    extern int mdb_txn_begin(MDB_env env, MDB_txn parent, uint32 flags, MDB_txn& txn);

    [< DllImport("lmdb", CallingConvention = CallingConvention.Cdecl) >] 
    extern int mdb_txn_commit(MDB_txn txn);

    [< DllImport("lmdb", CallingConvention = CallingConvention.Cdecl) >] 
    extern int mdb_dbi_open(MDB_txn txn, string name, uint32 flags, MDB_dbi& dbi); 

    [< DllImport("lmdb", CallingConvention = CallingConvention.Cdecl) >] 
    extern int mdb_get(MDB_txn txn, MDB_dbi dbi, MDB_val& key, MDB_val& data);

    [< DllImport("lmdb", CallingConvention = CallingConvention.Cdecl) >] 
    extern int mdb_put(MDB_txn txn, MDB_dbi dbi, MDB_val& key, MDB_val& data, uint32 flags);

    [< DllImport("lmdb", CallingConvention = CallingConvention.Cdecl) >] 
    extern int mdb_del(MDB_txn txn, MDB_dbi dbi, MDB_val& key, MDB_val* data);

    [< DllImport("lmdb", CallingConvention = CallingConvention.Cdecl) >]
    extern int mdb_cursor_open(MDB_txn txn, MDB_dbi dbi, MDB_cursor& cursor);

    [< DllImport("lmdb", CallingConvention = CallingConvention.Cdecl) >]
    extern void mdb_cursor_close(MDB_cursor cursor);

    [< DllImport("lmdb", CallingConvention = CallingConvention.Cdecl) >]
    extern int mdb_cursor_get(MDB_cursor cursor, MDB_val& key, MDB_val& data, MDB_cursor_op op);

    // I don't need cursor put or del at this time.



