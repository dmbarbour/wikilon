
#include <stdlib.h>
#include <stdio.h>
#include <errno.h>
#include <string.h>
#include <assert.h>
#include <sys/mman.h>

#include "b64.h"
#include "futil.h"
#include "wikrt_private.h"

void wikrt_db_sync(wikrt_env* e)
{
    // tell LMDB to tell the OS to flush pending writes to disk.
    if(NULL != e->db) {
        int const synchronous = 1;
        mdb_env_sync(e->db->mdb, synchronous);
    }
}

void wikrt_db_close(wikrt_env* e)
{
    wikrt_db_sync(e);
    if(NULL != e->db) {
        fprintf(stderr, "%s todo: close database\n", __FUNCTION__);
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
// a major concern. 
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
    assert(written == (WIKRT_EPH_ID_LEN - 1));
}

// load the ephemeron table
bool wikrt_load_eph(wikrt_db* db, char const* dirPath)
{
    // ugh, filesystem code is always ugly
    size_t const dplen = strlen(dirPath);
    char ephid_file_name[dplen + 12];
    sprintf(ephid_file_name, "%s/ephid", dirPath);

    int const ephid_fd = open(ephid_file_name, O_CREAT | O_RDWR, WIKRT_FILE_MODE);
    if((-1) == ephid_fd) { return false; }

    // use file lock to guard allocation of shared memory
    flock(ephid_fd, LOCK_EX); 
    ssize_t amt = read(ephid_fd, db->ephid, WIKRT_EPH_ID_LEN - 1);
    bool name_ok = (amt > 1) && ('/' == ephid[0]);
    if(name_ok) {
        db->ephid[amt] = 0;
    } else {
        wikrt_new_ephid(db->ephid);
        size_t const len = strlen(ephid);
        lseek(fd, 0, SEEK_SET);
        amt = write(ephid_fd, db->ephid, len);
        if(len != (size_t)amt) {
            close(ephid_fd);
            return false;
        }
    }
    int shm_fd = shm_open(db->ephid, 
            O_CREAT | O_RDWR | O_EXCL, 
            WIKRT_FILE_MODE);

    bool const init_shm = ((-1) != shm_fd);
    if(!init_shm) { 
        // try to open existing memory
        shm_fd = shm_open(db->ephid, O_RDWR); 
        if((-1) == shm_fd) {
            close(ephid_fd);
            return false;
        }
    }
    db->eph = mmap(NULL, sizeof(wikrt_eph)
                  , PROT_READ | PROT_WRITE
                  , MAP_SHARED, shm_fd, 0);


    if((-1) == shm_fd) {
        shm_fd = shm_open(db->ephid, O_RDWR);
        db->eph = (shm_fd == (-1)) ? MAP_FAILED :
            mmap(NULL, sizeof(wikrt_eph)
                , PROT_READ | PROT_WRITE
                , MAP_SHARED, shm_fd, 0);
        if(MAP_FAILED != db->eph) {
            ++(db->eph->refct); // via 
            
        }

        close(shm_fd);
        if(MAP_FAILED == db->eph) { return false; }
        pthread_mutex_lock(&(db->eph->mutex));
        ++(db->eph->

    }

    bool const init_shm = ((-1) != shm_fd);
    if(!init_shm) { 
        close
        shm_fd = shm_open(db->ephid, O_RDWR); 
}

    if((-1) == shm_fd) { 
        close(ephid_fd);
        return false;


    else {
        ftruncate(shm_fd, sizeof(wikrt

    if(init_shm)


        ftruncate(shm_fd, sizeof(wikrt_eph));
        db->eph = mmap(NULL, sizeof(wikrt_eph)
            , PROT_READ

        close(ephid_fd);
    } else {
        close(ephid_fd);
        shm_fd = shm_open(db->ephid, O_RDWR);
        if((-1) == shm_fd) { return false; }

        close(shm_fd);
    } 
    if(((-1) == shm_fd) && (EEXIST == errno)) {
        shm_fd = shm_open(db->ephid, O_RDWR);
    }
     
    
    


    close(ephid_fd);




    ssize_t amt = read(fd, ephid, WIKRT_EPH_ID_LEN - 1);
    bool ok = (amt > 1) && ('/' == ephid[0]) && (0 != ephid[1]);
    if(ok) { 
        ephid[amt] = 0;
    } else {
        wikrt_new_ephid(ephid);
        size_t const len = strlen(ephid);
        lseek(fd, 0, SEEK_SET);
        amt = write(fd, ephid, len);
        ok = ((size_t) amt) == len;
    }
    close(fd); // implicitly unlocks ephid file
    return ok;
}

bool wikrt_db_open(wikrt_env* e, char const* dirPath, size_t dbMaxSize)
{
    if(NULL != e->db) {
        errno = EEXIST;
        return false;
    }

    if(!mkdirpath(dirPath, WIKRT_DIR_MODE)) {
        fprintf(stderr, "%s: failed to create directory %s\n",
            __FUNCTION__, dirPath);
        return false;
    }

    wikrt_db* const db = calloc(1, sizeof(wikrt_db));
    if(NULL == db) { return false; }

    if(!wikrt_load_eph(db, dirPath)) {
        fprintf(stderr, "%s: failed to allocate ephemeron table ID\n",
            __FUNCTION__);
        free(db);
        return false;
    }

    // todo: load shared memory
    //       load LMDB

    // note: it might be easiest to combine allocation  lock on ephid
    //       when initially allocating shared memory, too
        
    return false;
}


