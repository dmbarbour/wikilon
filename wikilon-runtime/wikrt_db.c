
#include "futil.h"
#include "lmdb/lmdb.h"
#include "wikrt.h"
#include <string.h>
#include <stdio.h>
#include <assert.h>

/** Support for persistence and transactions. */ 
struct wikrt_db { 
    int                 lockfile;  
    MDB_env            *env;       
    MDB_dbi             memory; // address → value
    MDB_dbi             caddrs; // hash → [address]
    MDB_dbi             keyval; // key → (txn, address)
    MDB_dbi             refcts; // address → number
    MDB_dbi             refct0; // address → unit
    stowaddr            last_gc; 
    stowaddr            last_alloc;
};

bool wikrt_lockfile_init(int* pfd, char const* dirPath) {
    size_t const dplen = strlen(dirPath);
    size_t const fplen = dplen + 20;
    char lockFileName[fplen];
    sprintf(lockFileName, "%s/dblock", dirPath);
    bool const r = lockfile(pfd, lockFileName, WIKRT_FILE_MODE);
    return r;
}

// prepare LMDB database
bool wikrt_db_init(wikrt_db** pdb, char const* dp, uint32_t dbMaxMB) {
    (*pdb) = NULL;

    wikrt_db* db = calloc(1, sizeof(wikrt_db)); 
    if(NULL == db) { return false; }

    // ensure "" and "." have same meaning as working directory
    char const* dirPath = (dp[0] ? dp : ".");
    size_t const dbMaxBytes = (size_t)dbMaxMB * (1024 * 1024);

    // create our directory if necessary
    if(!mkdirpath(dirPath, WIKRT_DIR_MODE)) {
        fprintf(stderr, "Failed to create directory: %s\n", dirPath);
        goto onError;
    }

    // to populate on success:
    int db_lockfile;
    MDB_env* pLMDB;
    MDB_dbi db_memory, db_keyval, db_caddrs, db_refcts, db_refct0;
    stowaddr db_last_alloc;

    // relevant constants
    // TODO: Implement my own transaction thread-lock for STM and VCache-style
    //  batched transactions. But until then, we'll use LMDB's transaction model.
    int const mdbFlags = /* MDB_NOLOCK | */ MDB_NOTLS | MDB_NOMEMINIT;
    int const f_memory = MDB_CREATE | MDB_INTEGERKEY;
    int const f_keyval = MDB_CREATE;
    int const f_caddrs = MDB_CREATE | MDB_INTEGERKEY | MDB_DUPSORT | MDB_DUPFIXED | MDB_INTEGERDUP;
    int const f_refcts = MDB_CREATE | MDB_INTEGERKEY;
    int const f_refct0 = MDB_CREATE | MDB_INTEGERKEY;

    if(!wikrt_lockfile_init(&db_lockfile, dirPath)) {
        fprintf(stderr, "Failed to create or obtain lockfile in %s\n", dirPath);
        goto onError;
    }

    if( (0 != mdb_env_create(&pLMDB)))
        goto onErrCreateDB;

    MDB_txn* pTxn;
    if( (0 != mdb_env_set_mapsize(pLMDB, dbMaxBytes)) ||
        (0 != mdb_env_set_maxdbs(pLMDB, 5)) ||
        (0 != mdb_env_open(pLMDB, dirPath, mdbFlags, WIKRT_FILE_MODE)) ||
        (0 != mdb_txn_begin(pLMDB, NULL, MDB_NOSYNC, &pTxn)))
        goto onErrInitDB;

    // open our databases
    MDB_cursor* pCursor;
    if( (0 != mdb_dbi_open(pTxn, "@", f_memory, &db_memory)) ||
        (0 != mdb_dbi_open(pTxn, "/", f_keyval, &db_keyval)) ||
        (0 != mdb_dbi_open(pTxn, "#", f_caddrs, &db_caddrs)) ||
        (0 != mdb_dbi_open(pTxn, "^", f_refcts, &db_refcts)) ||
        (0 != mdb_dbi_open(pTxn, "%", f_refct0, &db_refct0)) ||
        (0 != mdb_cursor_open(pTxn, db_memory, &pCursor))      )
        goto onErrTxn;

    // obtain last allocated address
    MDB_val last_key;
    if(0 == mdb_cursor_get(pCursor, &last_key, NULL, MDB_LAST)) {
        assert(sizeof(stowaddr) == last_key.mv_size);
        db_last_alloc = *((stowaddr*)last_key.mv_data);
    } else {
        // default to allocations just past the 
        // maximum normal address for a resource
        db_last_alloc = ((stowaddr)1) << 32;
    }
    mdb_cursor_close(pCursor);

    // commit the transaction
    if(0 != mdb_txn_commit(pTxn)) 
        goto onErrInitDB;

    // if we reach this point, we've succeeded.
    db->env = pLMDB;
    db->lockfile = db_lockfile;
    db->memory = db_memory;
    db->caddrs = db_caddrs;
    db->keyval = db_keyval;
    db->refcts = db_refcts;
    db->refct0 = db_refct0;
    db->last_alloc = db_last_alloc;
    db->last_gc = 0;
    (*pdb) = db;

    return true;   

    // we might jump to error handling below
 onErrTxn:
    mdb_txn_abort(pTxn);
 onErrInitDB:
    mdb_env_close(pLMDB);
 onErrCreateDB: 
    close(db_lockfile);
 onError:
    free(db);
    return false;
}

void wikrt_db_destroy(wikrt_db* db) {
    mdb_env_close(db->env);
    close(db->lockfile);
    free(db);
}

void wikrt_db_flush(wikrt_db* db) {
    int const force_flush = 1;
    mdb_env_sync(db->env, force_flush);
}


