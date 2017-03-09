
#include <stdlib.h>
#include <stdio.h>
#include <errno.h>
#include <string.h>
#include <assert.h>
#include <pthread.h>
#include <sys/mman.h>


#include "b64.h"
#include "futil.h"
#include "wikrt_private.h"

/* Some concerns:
 *
 * While LMDB is great in a lot of ways, there are some caveats.
 *
 * - memory fragmentation: LMDB does not provide means to operate
 *   without malloc/free, and hence is subject to fragmentation.
 *   Performance may easily degrade in a long-running process.
 *
 * - reader limits: readers are tracked in an external lock table
 *   that is not readily resized. The number of parallel readers
 *   is limited. And there is no means to 'wait' for a reader slot
 *   to become available (except to poll).
 */

void wikrt_new_ephid(char* ephid);
bool wikrt_load_ephid(wikrt_db* db, char const* dirPath);
bool wikrt_init_shm(wikrt_db* db);
bool wikrt_load_eph_table(wikrt_db* db, char const* dirPath);
void wikrt_release_eph_table(wikrt_db* db);
bool wikrt_load_lmdb(wikrt_db* db, char const* dirPath, size_t);

void wikrt_db_sync(wikrt_env* e)
{
    // tell LMDB to tell the OS to flush pending writes to disk.
    if(NULL != e->db) {
        int const synchronous = 1;
        mdb_env_sync(e->db->mdb, synchronous);
    }
}

bool wikrt_db_copy(wikrt_env* e, char const* copyPath)
{
    if(NULL == e->db) {
        errno = ENOENT;
        return false;
    }
    if(!mkdirpath(copyPath, WIKRT_DIR_MODE)) {
        fprintf(stderr, "%s failed to create directory `%s`\n"
            , __FUNCTION__, copyPath);
        return false;
    }
    int const st = mdb_env_copy(e->db->mdb, copyPath);
    if(0 != st) {
        fprintf(stderr, "%s: copy failed at LMDB layer (%s)\n"
            , __FUNCTION__, mdb_strerror(errno));
        return false;
    }
    return true;
}


bool wikrt_load_lmdb(wikrt_db* db, char const* dirPath, size_t dbMaxSize)
{
    if(0 != mdb_env_create(&(db->mdb))) {
        fprintf(stderr, "%s: could not create LMDB (%s)\n"
            , __FUNCTION__, mdb_strerror(errno));
        return false;
    }

    int const nDatabaseCount = 4;
    int const nMaxReaders = 250;
    int const mdbFlags = MDB_NOTLS;
    mdb_mode_t const mdbMode = WIKRT_FILE_MODE;
    MDB_txn* tx;

    // initialize database environment
    if( (0 != mdb_env_set_mapsize(db->mdb, dbMaxSize)) ||
        (0 != mdb_env_set_maxdbs(db->mdb, nDatabaseCount)) ||
        (0 != mdb_env_set_maxreaders(db->mdb, nMaxReaders)) ||
        (0 != mdb_env_open(db->mdb, dirPath, mdbFlags, mdbMode)) ||
        (0 != mdb_txn_begin(db->mdb, NULL, 0, &tx)) )
    {
        fprintf(stderr, "%s: could not initialize LMDB environment (%s)\n"
            , __FUNCTION__, mdb_strerror(errno));
        mdb_env_close(db->mdb);
        return false;
    }

    // create the four databases
    //  at the moment I don't need any special flags
    int const f_roots  = MDB_CREATE;
    int const f_memory = MDB_CREATE;
    int const f_refcts = MDB_CREATE;
    int const f_refupd = MDB_CREATE;

    if( (0 != mdb_dbi_open(tx, "/", f_roots,  &(db->roots ))) ||
        (0 != mdb_dbi_open(tx, "$", f_memory, &(db->memory))) ||
        (0 != mdb_dbi_open(tx, "#", f_refcts, &(db->refcts))) ||
        (0 != mdb_dbi_open(tx, "+", f_refupd, &(db->refupd))) ||
        (0 != mdb_txn_commit(tx))                               )
    {
        fprintf(stderr, "%s: could not create LMDB databases (%s)\n"
            , __FUNCTION__, mdb_strerror(errno));
        mdb_env_close(db->mdb);
        return false;
    }

    // no data initialization is required
    return true;
}



bool wikrt_db_open(wikrt_env* e, char const* _dirPath, size_t dbMaxSize)
{
    char const* const dirPath = (_dirPath[0] ? _dirPath : ".");
    size_t const dpLen = strlen(dirPath);
    char ephid[dpLen + 12];
    sprintf(ephid, "%s/wikrt-eph", dirPath);

    if(NULL != e->db) { errno = EEXIST; goto onError; }
    e->db = calloc(1, sizeof(wikrt_db));
    if(NULL == e->db) { goto onError; }

    // ensure "" and "." have same meaning as working directory
    if(!mkdirpath(dirPath, WIKRT_DIR_MODE)) {
        fprintf(stderr, "%s: failed to create directory `%s`\n",
            __FUNCTION__, dirPath);
        goto onError;
    }

    // initialize ephemeron table via file in target directory
    e->eph = wikrt_eph_open(ephid, WIKRT_FILE_MODE);
    if(NULL == e->eph) { goto onError; }

    if(!wikrt_load_lmdb(e->db, dirPath, dbMaxSize)) {
        fprintf(stderr, "%s: failed to load LMDB at directory `%s`\n"
            , __FUNCTION__, dirPath);
        goto onError;
    }
    return true;

// error cleanup logic
onError:
    free(e->db); e->db = NULL;
    if(NULL != e->eph) { wikrt_eph_close(e->eph); }
    e->eph = NULL;
    return false;
}


void wikrt_db_close(wikrt_env* e)
{
    if(NULL == e->db) { 
        return; 
    }

    mdb_env_sync(e->db->mdb, 1);   // final synchronization
    mdb_env_close(e->db->mdb);     // close the database
    free(e->db); 
    e->db = NULL;
    wikrt_eph_close(e->eph); 
    e->eph = NULL;
}


