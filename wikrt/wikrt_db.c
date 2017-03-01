
#include <stdlib.h>
#include <stdio.h>
#include <errno.h>
#include <string.h>
#include <assert.h>
#include <sys/mman.h>

#include "b64.h"
#include "futil.h"
#include "wikrt_private.h"

void wikrt_get_entropy(size_t const amt, uint8_t* const out);
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
        return false;
    }
    fprintf(stderr, "%s todo: tell LMDB to copy database!", __FUNCTION__);
    return false;
}

// entropy is currently used only to create an ephemeron table
// name within shared volatile memory. This is a one-time event
// when initially opening an environment, so performance is not
// a significant concern. 
void wikrt_get_entropy(size_t const amt, uint8_t* const out)
{
    char const* const random_source = "/dev/random";
    FILE* const f = fopen(random_source, "rb");
    if(NULL == f) {
        fprintf(stderr, "%s could not open %s for reason %s\n"
            , __FUNCTION__, random_source, strerror(errno));
        abort();
    } 
    size_t const rd = fread(out, 1, amt, f);
    fclose(f);
    if(amt != rd) {
        fprintf(stderr, "%s could only read %d (of %d) bytes from %s\n"
            , __FUNCTION__, (int)rd, (int)amt, random_source);
        abort();
    }
}

// create a (probably) unique ID for ephemeron table shared memory.
void wikrt_new_ephid(char* ephid)
{
    _Static_assert((sizeof(uint8_t) == sizeof(char))
        , "unsafe cast between uint8_t* and char*");
    _Static_assert((WIKRT_EPH_ID_LEN >= 32)
        , "insufficient buffer for ephid");

    size_t const entropy_amt = 15;
    uint8_t entropy_buff[entropy_amt];
    wikrt_get_entropy(entropy_amt, entropy_buff);

    size_t const entropy_b64_len = ((entropy_amt * 4) / 3);
    uint8_t entropy_b64[entropy_b64_len + 1];
    b64_encode(entropy_buff, entropy_amt, entropy_b64);
    entropy_b64[entropy_b64_len] = 0;

    // should be exactly 32 bytes
    //   /wikrt-eph-        11 bytes
    //   entropy_b64        20 bytes
    //   NUL terminal        1 byte 
    size_t const written = snprintf(ephid, WIKRT_EPH_ID_LEN
        , "/wikrt-eph-%s"
        , (char const*) entropy_b64
        );
    assert((20 == entropy_b64_len) 
        && (31 == written));
}

bool wikrt_load_ephid(wikrt_db* db, char const* dirPath)
{
    size_t const dplen = strlen(dirPath);
    char ephid_file_name[dplen + 12];
    sprintf(ephid_file_name, "%s/ephid", dirPath);

    db->ephid_fd = open(ephid_file_name, O_CREAT | O_RDWR, WIKRT_FILE_MODE);
    if((-1) == db->ephid_fd) {
        fprintf(stderr, "%s: failed to open file `%s`\n"
            , __FUNCTION__, ephid_file_name); 
        return false; 
    }

    flock(db->ephid_fd, LOCK_EX); 

    ssize_t amt = read(db->ephid_fd, db->ephid, WIKRT_EPH_ID_LEN);
    bool name_ok = (amt > 1) && ('/' == db->ephid[0]) 
                && (amt < WIKRT_EPH_ID_LEN);
    if(name_ok) {
        db->ephid[amt] = 0;
    } else {
        lseek(db->ephid_fd, 0, SEEK_SET);
        int const st_size = ftruncate(db->ephid_fd, 0);
        if(0 != st_size) {
            fprintf(stderr, "%s: failed to truncate `%s`\n"
                , __FUNCTION__, ephid_file_name);
            close(db->ephid_fd);
            return false;
        }

        wikrt_new_ephid(db->ephid);
        size_t const len = strlen(db->ephid);
        assert(len < WIKRT_EPH_ID_LEN);
        amt = write(db->ephid_fd, db->ephid, len);
        if(len != (size_t)amt) {
            close(db->ephid_fd);
            fprintf(stderr, "%s: failed to record shm name `%s` to `%s`\n"
                , __FUNCTION__, db->ephid, ephid_file_name);
            return false;
        }
    }
    flock(db->ephid_fd, LOCK_UN);
    return true;
}

bool wikrt_init_shm(wikrt_db* db)
{
    // assumes valid db->ephid and exclusive lock on db->ephid_fd
    // We'll protect the reference count via the db->ephid_fd.

    int const shm_fd = shm_open(db->ephid, O_CREAT | O_RDWR, WIKRT_FILE_MODE);
    if((-1) == shm_fd) { 
        fprintf(stderr, "%s: failed to open shm `%s`\n"
            , __FUNCTION__, db->ephid);
        return false; 
    }

    int const st_size = ftruncate(shm_fd, sizeof(wikrt_eph));
    if(0 != st_size) {
        fprintf(stderr, "%s: failed to size shm `%s` to %d\n"
            , __FUNCTION__, db->ephid, (int)sizeof(wikrt_eph));
        return false;
    }

    db->eph = mmap(NULL, sizeof(wikrt_eph)
                  , PROT_READ | PROT_WRITE
                  , MAP_SHARED, shm_fd, 0);
    close(shm_fd);
    if(MAP_FAILED == db->eph) { 
        fprintf(stderr, "%s: failed to mmap shared memory %s\n"
            , __FUNCTION__, db->ephid); 
        return false; 
    }
    if(0 == db->eph->refct) {
        pthread_mutexattr_t a;
        pthread_mutexattr_init(&a);
        pthread_mutexattr_setrobust(&a, PTHREAD_MUTEX_ROBUST);
        int const st_mutex = pthread_mutex_init(&(db->eph->mutex), &a);
        pthread_mutexattr_destroy(&a);
        if(0 != st_mutex) {
            fprintf(stderr, "%s: failed to initialize robust mutex\n"
                , __FUNCTION__);
            munmap(db->eph, sizeof(wikrt_eph));
            return false;
        }
    }
    ++(db->eph->refct);
    return true;
}

// load the ephemeron table
bool wikrt_load_eph_table(wikrt_db* db, char const* dirPath)
{
    // obtain a unique identifier for the ephemeron table
    if(!wikrt_load_ephid(db, dirPath)) { 
        fprintf(stderr, "%s: failed to load ephid\n", __FUNCTION__);
        return false; 
    }

    flock(db->ephid_fd, LOCK_EX);
    if(!wikrt_init_shm(db)) {
        fprintf(stderr, "%s: failed to init shm\n", __FUNCTION__);
        close(db->ephid_fd);
        return false;
    }
    flock(db->ephid_fd, LOCK_UN);
    return true;
}

bool wikrt_load_lmdb(wikrt_db* db, char const* dirPath, size_t dbMaxSize)
{
    // NOTE: LMDB supports multiple processes via shared lock files,
    // but supports only a small finite number of parallel readers.
    // 
    // Unfortunately, LMDB also requires malloc/free unless I plan
    // to permanently consume reader slots using mdb_txn_reset and
    // mdb_txn_renew. I cannot precisely control use of memory within
    // LMDB. This may become a memory fragmentation risk.
    //
    //
    // It's up to Wikilon runtime to ensure that read transactions are
    // short-lived and only constitute a small fraction of context ops.
    // But we can aim to support a reasonably large number of parallel
    // readers. We can also try to batch multiple reads into a larger
    // transaction.
    //
    // Note: we might end up modeling read-only transactions at the
    // wikrt_env* layer, or wikrt_cx* layer to avoid repeated malloc
    // and free of transaction objects.
    //
    // Wikilon runtime does not use TLS, though we also shouldn't return
    // in the middle of a transaction, so MDB_NOTLS is optional. I set it
    // here.
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
        fprintf(stderr, "%s: could not initialize LMDB (%s)\n"
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
        (0 != mdb_dbi_open(tx, "+", f_refupd, &(db->refupd)))   )
    {
        fprintf(stderr, "%s: could not create LMDB databases (%s)\n"
            , __FUNCTION__, mdb_strerror(errno));
        mdb_txn_abort(tx);
        mdb_env_close(db->mdb);
        return false;
    }

    // no data initialization is required
    return true;
}



bool wikrt_db_open(wikrt_env* e, char const* _dirPath, size_t dbMaxSize)
{
    // ensure "" and "." have same meaning as working directory
    char const* const dirPath = (_dirPath[0] ? _dirPath : ".");

    if(NULL != e->db) {
        errno = EEXIST;
        return false;
    }

    if(!mkdirpath(dirPath, WIKRT_DIR_MODE)) {
        fprintf(stderr, "%s: failed to create directory `%s`\n",
            __FUNCTION__, dirPath);
        return false;
    }

    wikrt_db* const db = calloc(1, sizeof(wikrt_db));
    if(NULL == db) { 
        return false; 
    }

    if(!wikrt_load_eph_table(db, dirPath)) {
        fprintf(stderr, "%s: failed to load shared memory ephemeron table\n",
            __FUNCTION__);
        free(db);
        return false;
    }

    if(!wikrt_load_lmdb(db, dirPath, dbMaxSize)) {
        fprintf(stderr, "%s: failed to load LMDB at directory `%s`\n"
            , __FUNCTION__, dirPath);
        wikrt_release_eph_table(db);
        free(db);
        return false;
    }

    e->db = db;
    return true;
}

void wikrt_release_eph_table(wikrt_db* db)
{
    flock(db->ephid_fd, LOCK_EX);
    --(db->eph->refct);
    if(0 == db->eph->refct) {
        // recycle shared memory resources
        shm_unlink(db->ephid);
        pthread_mutex_destroy(&(db->eph->mutex));
    }
    munmap(db->eph, sizeof(wikrt_eph));
    close(db->ephid_fd);
}


void wikrt_db_close(wikrt_env* e)
{
    if(NULL == e->db) { return; }
    mdb_env_sync(e->db->mdb, 1);   // final synchronization
    mdb_env_close(e->db->mdb);     // close the database
    wikrt_release_eph_table(e->db); 
    free(e->db);
    e->db = NULL;
}


