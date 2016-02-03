
#include "futil.h"
#include "wikrt.h"
#include <string.h>
#include <stdio.h>
#include <assert.h>

bool wikrt_lockfile_init(int* pfd, char const* dirPath) {
    size_t const dplen = strlen(dirPath);
    size_t const fplen = dplen + 6; // "/lock" + NUL
    char lockFileName[fplen];
    sprintf(lockFileName, "%s/lock", dirPath);
    bool const r = lockfile(pfd, lockFileName, WIKRT_FILE_MODE);
    return r;
}

// prepare LMDB database
bool wikrt_db_init(wikrt_env* e, char const* dp, uint32_t dbMaxMB) {

    // ensure "" and "." have same meaning as local directory
    char const* dirPath = (dp[0] ? dp : ".");
    size_t const dbMaxBytes = (size_t)dbMaxMB * (1024 * 1024);

    // create our directory if necessary
    if(!mkdirpath(dirPath, WIKRT_DIR_MODE)) {
        fprintf(stderr, "Failed to create directory: %s\n", dirPath);
        return false;
    }

    // to populate on success:
    int db_lockfile;
    MDB_env* pLMDB;
    MDB_dbi db_memory, db_keyval, db_caddrs, db_refcts, db_refct0;
    stowaddr db_last_alloc;

    // relevant constants
    int const mdbFlags = MDB_NOTLS | MDB_NOLOCK | MDB_NOMEMINIT;
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
    e->db_env = pLMDB;
    e->db_lockfile = db_lockfile;
    e->db_memory = db_memory;
    e->db_caddrs = db_caddrs;
    e->db_keyval = db_keyval;
    e->db_refcts = db_refcts;
    e->db_refct0 = db_refct0;
    e->db_last_alloc = db_last_alloc;
    e->db_last_gc = 0;

    return true;   

    // we might jump to error handling below
 onErrTxn:
    mdb_txn_abort(pTxn);
 onErrInitDB:
    mdb_env_close(pLMDB);
 onErrCreateDB: 
    close(db_lockfile);
 onError:
    return false;
}

void wikrt_db_destroy(wikrt_env* e) {
    if(e->db_enable) {
        mdb_env_close(e->db_env);
        close(e->db_lockfile);
    }
}


