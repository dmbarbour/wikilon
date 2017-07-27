#nowarn "9" // disable warning for StructLayoutAttribute

namespace Stowage
open Stowage.Memory

open System
open System.Runtime.InteropServices
open System.Security
open Data.ByteString


// slice of LMDB API and FFI used for Stowage
//  I could use the LightningDB package, but I'd still mostly use
[< SecuritySafeCriticalAttribute >]
module internal LMDB =
    type MDB_env = nativeint    // opaque MDB_env*
    type MDB_txn = nativeint    // opaque MDB_txn*
    type MDB_dbi = uint32       // database ids are simple ints
    type MDB_cursor = nativeint // opaque MDB_cursor*
    type MDB_cursor_op = int    // an enumeration
    type mdb_mode_t = int       // Unix-style permissions

    [< Struct; StructLayoutAttribute(LayoutKind.Sequential) >]
    type MDB_val =
        val size : size_t
        val data : nativeint
        new(s,d) = { size = s; data = d }

    let defaultMode : mdb_mode_t = 0o660

    // Environment Flags
    let MDB_NOSYNC   =  0x10000u    // writer handles sync explicitly
    let MDB_WRITEMAP =  0x80000u    // avoid extra malloc/copy on write
    let MDB_NOTLS    = 0x200000u    // using lightweight threads
    let MDB_NOLOCK   = 0x400000u    // specialized locking model
    
    // Database Flags
    let MDB_CREATE  = 0x40000u  // create DB if it doesn't exist

    // Write Flags
    let MDB_RESERVE = 0x10000u  // reserve space for writing data

    // Cursor Ops
    let MDB_NEXT  = 8           // next cursor item
    let MDB_SET_RANGE = 17      // next key after request

    // Transaction Flags
    let MDB_RDONLY = 0x20000u

    // Specially handled Error codes
    let MDB_NOTFOUND = (-30798)

    module Native = 
        // API native methods
        //
        // Note: I'm not happy with .Net's conversion of strings. But I don't 
        // use anything outside of ASCII within Stowage. 
        [< DllImport("lmdb", CallingConvention = CallingConvention.Cdecl) >]
        extern nativeint mdb_strerror(int errno);

        [< DllImport("lmdb", CallingConvention = CallingConvention.Cdecl) >] 
        extern int mdb_env_create([<Out>] MDB_env& env);

        [< DllImport("lmdb", CallingConvention = CallingConvention.Cdecl) >] 
        extern int mdb_env_open(MDB_env env, string path, uint32 flags, mdb_mode_t mode);

        [< DllImport("lmdb", CallingConvention = CallingConvention.Cdecl) >] 
        extern void mdb_env_close(MDB_env env);

        [< DllImport("lmdb", CallingConvention = CallingConvention.Cdecl) >] 
        extern int mdb_env_sync(MDB_env env, int force); 

        [< DllImport("lmdb", CallingConvention = CallingConvention.Cdecl) >] 
        extern int mdb_env_set_mapsize(MDB_env env, size_t size);

        [< DllImport("lmdb", CallingConvention = CallingConvention.Cdecl) >] 
        extern int mdb_env_set_maxdbs(MDB_env env, MDB_dbi dbs);

        [< DllImport("lmdb", CallingConvention = CallingConvention.Cdecl) >] 
        extern int mdb_txn_begin(MDB_env env, MDB_txn parent, uint32 flags, [<Out>] MDB_txn& txn);

        [< DllImport("lmdb", CallingConvention = CallingConvention.Cdecl) >] 
        extern int mdb_txn_commit(MDB_txn txn);

        [< DllImport("lmdb", CallingConvention = CallingConvention.Cdecl) >] 
        extern int mdb_dbi_open(MDB_txn txn, string name, uint32 flags, [<Out>] MDB_dbi& dbi); 

        [< DllImport("lmdb", CallingConvention = CallingConvention.Cdecl) >] 
        extern int mdb_get(MDB_txn txn, MDB_dbi dbi, [<In>] MDB_val& key, [<Out>] MDB_val& data);

        [< DllImport("lmdb", CallingConvention = CallingConvention.Cdecl) >] 
        extern int mdb_put(MDB_txn txn, MDB_dbi dbi, [<In>] MDB_val& key, MDB_val& data, uint32 flags);

        [< DllImport("lmdb", CallingConvention = CallingConvention.Cdecl) >] 
        extern int mdb_del(MDB_txn txn, MDB_dbi dbi, [<In>] MDB_val& key, nativeint nullData);
            // note: not using DUPSORT in Stowage; data argument should be 0n

        [< DllImport("lmdb", CallingConvention = CallingConvention.Cdecl) >]
        extern int mdb_cursor_open(MDB_txn txn, MDB_dbi dbi, MDB_cursor& cursor);

        [< DllImport("lmdb", CallingConvention = CallingConvention.Cdecl) >]
        extern void mdb_cursor_close(MDB_cursor cursor);

        [< DllImport("lmdb", CallingConvention = CallingConvention.Cdecl) >]
        extern int mdb_cursor_get(MDB_cursor cursor, MDB_val& key, nativeint nullData, MDB_cursor_op op);
            // currently using cursors only to browse available keys

        // I don't need cursor put or cursor del at this time.
        // I also don't currently close or dispose of a DB.

    // Stowage shouldn't be seeing errors, so I won't waste complexity
    // budget on sophisticated error handling. 
    exception LMDBError of int

    let private reportError (e : int) : unit =
        let msgPtr = Native.mdb_strerror(e)
        let msgStr = 
            if (IntPtr.Zero = msgPtr) then "(null)" else
            Marshal.PtrToStringAnsi(msgPtr)
        printf "LMDB Error %d: %s" e msgStr
        raise (LMDBError e)

    let inline private check (e : int) : unit =
        if (0 <> e) then reportError(e) 

    let inline private mdb_assertions (env : MDB_env) : unit =
        assert((2 * Marshal.SizeOf<nativeint>()) = Marshal.SizeOf<MDB_val>())
        

    let mdb_env_create () : MDB_env =
        let mutable env = IntPtr.Zero
        check(Native.mdb_env_create(&env))
        assert (IntPtr.Zero <> env)
        mdb_assertions env
        env

    /// note: give size here in megabytes, not bytes
    let mdb_env_set_mapsize (env : MDB_env) (maxSizeMB : int) : unit =
        assert(maxSizeMB > 0)
        let maxSizeBytes = (1024UL * 1024UL) * (uint64 maxSizeMB)
        check(Native.mdb_env_set_mapsize(env, unativeint maxSizeBytes))

    let mdb_env_set_maxdbs (env : MDB_env) (maxDBs : int) : unit =
        assert (maxDBs > 0)
        check(Native.mdb_env_set_maxdbs(env, uint32 maxDBs))

    let mdb_env_open (env : MDB_env) (path : string) (flags : uint32) =
        check(Native.mdb_env_open(env,path,flags,defaultMode))

    let mdb_env_close (env : MDB_env) : unit =
        Native.mdb_env_close(env)

    let mdb_env_sync (env : MDB_env) : unit =
        check(Native.mdb_env_sync(env,1))

    let mdb_readwrite_txn_begin (env : MDB_env) : MDB_txn =
        let mutable txn = IntPtr.Zero
        check(Native.mdb_txn_begin(env, IntPtr.Zero, 0u, &txn))
        assert(IntPtr.Zero <> txn)
        txn

    let mdb_rdonly_txn_begin (env : MDB_env) : MDB_txn =
        let mutable txn = IntPtr.Zero
        check(Native.mdb_txn_begin(env, IntPtr.Zero, MDB_RDONLY, &txn))
        assert(IntPtr.Zero <> txn)
        txn

    /// MDB transactions always succeed. 
    /// We just can't have more than one writer transaction at a time.
    let mdb_txn_commit (txn : MDB_txn) : unit =
        check(Native.mdb_txn_commit(txn))

    let mdb_dbi_open (txn : MDB_txn) (db : string) : MDB_dbi =
        let mutable dbi = 0u
        check(Native.mdb_dbi_open(txn, db, MDB_CREATE, &dbi))
        dbi

    // zero-copy get (copy neither key nor result)
    let mdb_getZC (txn : MDB_txn) (db : MDB_dbi) (key : ByteString) : MDB_val option =
        Data.ByteString.withPinnedBytes key (fun kAddr -> 
            let mutable mdbKey = MDB_val(unativeint key.Length, kAddr)
            let mutable mdbVal = MDB_val()
            let e = Native.mdb_get(txn, db, &mdbKey, &mdbVal)
            if(MDB_NOTFOUND = e) then None else
            check e
            Some v)
    
    // check to see if a key already exists in the database
    // (lookup but don't copy!)
    let inline mdb_contains (txn : MDB_txn) (db : MDB_dbi) (key : ByteString) : bool =
        mdb_getZC txn db key |> Option.isSome

    // copy an unmanaged MDB_val to a managed byte array
    let copyVal (v : MDB_val) : byte[] =
        if(0un = v.size) then Array.empty else
        let len = int (v.size)
        assert ((0 < len) && ((unativeint len) = v.size))
        let arr : byte[] = Array.zeroCreate len
        Marshal.Copy(v.data, arr, 0, len)
        arr

    let inline val2bytes (v : MDB_val) : ByteString =
        Data.ByteString.unsafeCreateA (copyVal v) 
    
    // lookup a value in the database, copies the bytes
    let inline mdb_get (txn : MDB_txn) (db : MDB_dbi) (key : ByteString) : ByteString option =
        mdb_getZC txn db key |> Option.map val2bytes

    // allocate space to write a value
    let mdb_reserve (txn : MDB_txn) (db : MDB_dbi) (key : ByteString) (len : int) : nativeint =
        Data.ByteString.withPinnedBytes key (fun keyAddr ->
            let mutable mdbKey = MDB_val(unativeint key.Length, keyAddr)
            let mutable mdbVal = MDB_val(unativeint len, IntPtr.Zero)
            check(Native.mdb_put(txn, db, &mdbKey, &mdbVal, MDB_RESERVE))
            assert(IntPtr.Zero <> mdbVal.data)
            mdbVal.data)

    // write or modify a value 
    let inline mdb_put (txn : MDB_txn) (db : MDB_dbi) (key : ByteString) (v : ByteString) : unit =
        let dst = mdb_reserve txn db key v.Length
        Marshal.Copy(v.UnsafeArray, v.Offset, dst, v.Length)

    // delete key-value pair, returns whether item exists.
    let mdb_del (txn : MDB_txn) (db : MDB_dbi) (key : ByteString) : bool =
        Data.ByteString.withPinnedBytes key (fun keyAddr ->
            let mutable mdbKey = MDB_val(unativeint key.Length, keyAddr)
            let e = Native.mdb_del(txn,db,&mdbKey,IntPtr.Zero)
            if(MDB_NOTFOUND = e) then false else check(e); true)

    // cursors are somewhat challenging to generalize,
    // but I can provide a few utility functions.

    let mdb_cursor_open (txn : MDB_txn) (dbi : MDB_dbi) : MDB_cursor =
        let mutable crs = IntPtr.Zero
        check(Native.mdb_cursor_open(txn, dbi, &crs))
        assert(IntPtr.Zero <> crs)
        crs

    let inline mdb_cursor_close (crs : MDB_cursor) : unit =
        Native.mdb_cursor_close(crs)

    let mdb_enum_keys (txn : MDB_txn) 
                      (dbi : MDB_dbi) 
                      (from : ByteString option) 
                      : System.Collections.Generic.IEnumerator<ByteString> =
        
                      

    // seek for key equal or greater than the argument, if any.
    let mdb_cursor_seek_key_ge (crs : MDB_cursor) (key : ByteString) : (ByteString option) =
        Data.ByteString.withPinnedBytes key (fun keyAddr ->
            let mutable mdbKey = MDB_val(unativeint key.Length, keyAddr)
            let e = Native.mdb_cursor_get(crs, &mdbKey, IntPtr.Zero, MDB_SET_RANGE)
            if(MDB_NOTFOUND = e) then None else
            check(e)
            Some(val2bytes mdbKey)

    // seek to next key in iterator. 
    let mdb_cursor_next_key (crs : MDB_cursor) : (ByteString option) =
        let mutable mdbKey = MDB_val()
        let e = Native.mdb_cursor_get(crs, &mdbKey, IntPtr.Zero, MDB_NEXT)
        if(MDB_NOTFOUND = e) then None else
        check(e)
        Some(val2bytes mdbKey)

    // seek to first key strictly greater than the argument.
    let mdb_cursor_seek_key_gt (crs : MDB_cursor) (key : ByteString) : (ByteString option) =
        let kOpt = mdb_cursor_seek_key_ge crs key 
        match kOpt with
        | None -> None
        | Just keyFound ->
            if keyFound = key 
                then mdb_cursor_next_key crs
                else assert(keyFound > key); kOpt

    // find up to a quota of keys strictly greater than given argument.
    let mdb_browse_keys txn dbi key0 maxResults : ByteString[] =
        let crs = mdb_cursor_open txn dbi
        try let dst = new ResizeArray<ByteString>(maxResults)
                if((
        finally mdb_cursor_close crs 


