#include <stdlib.h>
#include <stdio.h>
#include <pthread.h>
#include <errno.h>
#include <string.h>
#include <assert.h>
#include <sys/mman.h>
#include <sys/file.h>

#include "b64.h"
#include "futil.h"
#include "wikrt_private.h"


// data elements are simply a uint64_t:
//   upper 44 bits for the hash value
//   lower 20 bits for the ref count
//
// Empty slots have 0 refct and come in two flavors:
//   if the hash value is also zero, it's never been used
//   if hash value is non-zero, it's a "used" slot
//
// Linear probing is used to fill the table, with a fill max of 
// about 70%. A difference is that linear probing might need to
// scan past a used slot.
typedef uint64_t wikrt_eph_d;
#define WIKRT_EPH_MAX_COUNT ((1<<20)-1)
#define WIKRT_EPH_D(H,C) (H<<20 | (C & WIKRT_EPH_MAX_COUNT))
#define WIKRT_EPH_H(D) (D >> 20)
#define WIKRT_EPH_C(D) (D & WIKRT_EPH_MAX_COUNT)
#define WIKRT_EPH_NULL 0
#define WIKRT_EPH_USED WIKRT_EPH_D(1,0)
#define WIKRT_EPH_HASH_PRIME 1234789

#define WIKRT_EPH_VER_DATE 20170302 
#define WIKRT_EPH_VER  ((uint64_t)WIKRT_EPH_HASH_PRIME * \
                        (uint64_t)WIKRT_EPH_VER_DATE     )
#define WIKRT_EPH_NAME_LEN 32

// stable header
typedef struct wikrt_eph_h {
    uint64_t        ver;    // magic number for multi-process sharing
    uint64_t        refct;  // references to table overall
    pthread_mutex_t mutex;  // must be a shared, robust mutex
    uint64_t        elem;   // filled slots (non-zero refct)
    uint64_t        fill;   // non-empty slots (filled + used)
    uint64_t        size;   // total slot count (filled + used + empty)
    uint64_t        resize; // for failure recovery on resize
} wikrt_eph_h;

#define WIKRT_EPH_BLOCK (1<<9)

struct wikrt_eph {
    char            name[WIKRT_EPH_NAME_LEN]; // shm name
    int             shm;       // shm file descriptor
    int             file;       // identity file for locking
    wikrt_eph_h*    head;       // header 
    wikrt_eph_d*    data;       // data pointer
    uint64_t        mmsz;       // mapped data slots
};

void wikrt_get_entropy(size_t const amt, uint8_t* const out);
void wikrt_new_ephid(char* ephid);
bool wikrt_eph_open_file(wikrt_eph* eph, char const* file);
bool wikrt_eph_open_shm(wikrt_eph* eph);

wikrt_eph* wikrt_eph_open(char const* file)
{
    wikrt_eph* const eph = calloc(1,sizeof(wikrt_eph));
    if(NULL == eph) { return NULL; }

    if(!wikrt_eph_open_file(eph, file)) {
        free(eph);
        return NULL;
    }

    // use file locks to isolate initialization to a single process
    flock(eph->file, LOCK_EX);

    // create the shared memory
    if(!wikrt_eph_open_shm(eph)) {
        fprintf(stderr, "%s: failed to open shm `%s`\n"
            , __FUNCTION__, eph->name);
        close(eph->file);
        free(eph);
        return NULL;
    }

    flock(eph->file, LOCK_UN);

    return eph;
}

// updates eph->file, eph->name.
bool wikrt_eph_open_file(wikrt_eph* eph, char const* file) 
{
    eph->file = open(file, O_CREAT|O_RDWR, WIKRT_FILE_MODE);
    if((-1) == eph->file) {
        fprintf(stderr, "%s: failed to open file `%s` (%s)\n"
            , __FUNCTION__, file, strerror(errno));
        return false;
    }


    flock(eph->file, LOCK_EX);

    ssize_t amt = read(eph->file, eph->name, WIKRT_EPH_NAME_LEN);
    bool const name_ok = (amt > 1) && ('/' == (eph->name[0])) 
                      && (amt < WIKRT_EPH_NAME_LEN);
    if(name_ok) { 
        eph->name[amt] = 0; 
    } else if(0 == amt) {
        // write empty file only
        wikrt_new_ephid(eph->name);
        size_t const len = strlen(eph->name);
        assert(len < WIKRT_EPH_NAME_LEN);
        amt = write(eph->file, eph->name, len);
        if(len != (size_t)amt) {
            fprintf(stderr, "%s: failed to write ephid `%s` to file `%s` (%s)\n"
                , __FUNCTION__, eph->name, file, strerror(errno));
            close(eph->file);
            return false;
        } 
    } else {
        // don't overwrite existing data!
        fprintf(stderr, "%s: invalid file `%s`; consider removing\n"
            , __FUNCTION__, file);
        close(eph->file);
        return false;
    }

    flock(eph->file, LOCK_UN);
    return true;
}

// load eph->head, initialize if needed
bool wikrt_eph_open_shm(wikrt_eph* eph)
{
    eph->shm = shm_open(eph->name, O_CREAT | O_RDWR, WIKRT_FILE_MODE);
    if((-1) == eph->shm) {
        fprintf(stderr, "%s: could not open shared memory (%s)\n"
            , __FUNCTION__, strerror(errno));
        return false;
    }

    struct stat shm_stat;
    int const st_stat = fstat(eph->shm, &shm_stat);
    if(0 != st_stat) {
        fprintf(stderr, "%s: could not read shared memory status (%s)\n"
            , __FUNCTION__, strerror(errno));
        close(eph->shm);
        return false;
    }

    bool const init_mem = (0 == shm_stat.st_size);
    if(init_mem) {
        _Static_assert((sizeof(wikrt_eph_h) < WIKRT_EPH_BLOCK), 
            "require ephemeron table header fits in one block");
        if(0 != ftruncate(eph->shm, WIKRT_EPH_BLOCK)) {
            fprintf(stderr, "%s: could not allocate shared memory (%s)\n"
                , __FUNCTION__, strerror(errno));
            close(eph->shm);
            return false;
        }
    }

    eph->head = mmap(NULL, sizeof(wikrt_eph_h)
                         , PROT_READ|PROT_WRITE
                         , MAP_SHARED, eph->shm, 0);

    if(MAP_FAILED == eph->head) {
        fprintf(stderr, "%s: could not map shared memory (%s)\n"
            , __FUNCTION__, strerror(errno));
        close(eph->shm);
        return false;
    }

    if(init_mem) {
        eph->head->ver = WIKRT_EPH_VER;

        pthread_mutexattr_t a;
        pthread_mutexattr_init(&a);
        pthread_mutexattr_setpshared(&a, PTHREAD_PROCESS_SHARED);
        pthread_mutexattr_setrobust(&a, PTHREAD_MUTEX_ROBUST);
        int const st_mutex = pthread_mutex_init(&(eph->head->mutex), &a);
        int const err_mutex = errno;
        pthread_mutexattr_destroy(&a);

        if(0 != st_mutex) {
            fprintf(stderr, "%s: could not initialize shared, robust mutex (%s)\n"
                , __FUNCTION__, strerror(err_mutex));
            munmap(eph->head, sizeof(wikrt_eph_h)); 
            return false;
        }
    }

    bool const match_ver = (WIKRT_EPH_VER == eph->head->ver);
    if(!match_ver) {
        fprintf(stderr, "%s: version match failure (%llu vs %llu)\n"
            , __FUNCTION__, (long long unsigned int)eph->head->ver
                          , (long long unsigned int)WIKRT_EPH_VER   );
        munmap(eph->head, sizeof(wikrt_eph_h));
        return false;
    }

    ++(eph->head->refct);

    // I'll leave initialization of eph->data to the resize logic.
    return true;
}

void wikrt_eph_close(wikrt_eph* eph)
{
    flock(eph->file, LOCK_EX);
    --(eph->head->refct);
    if(0 == eph->head->refct) {
        shm_unlink(eph->name);
        pthread_mutex_destroy(&(eph->head->mutex));
    }
    munmap(eph->head, sizeof(wikrt_eph_h));
    if(0 != eph->mmsz) {
        size_t const bytes = eph->mmsz * sizeof(wikrt_eph_d);
        munmap(eph->data, bytes);
    }
    close(eph->shm);
    close(eph->file);
    free(eph);
}

void wikrt_get_entropy(size_t const amt, uint8_t* const out)
{
    char const* const random_source = "/dev/random";
    FILE* const f = fopen(random_source, "rb");
    if(NULL == f) {
        fprintf(stderr, "%s: could not open %s for reason %s\n"
            , __FUNCTION__, random_source, strerror(errno));
        abort();
    } 
    size_t const rd = fread(out, 1, amt, f);
    fclose(f);
    if(amt != rd) {
        fprintf(stderr, "%s: could only read %d (of %d) bytes from %s\n"
            , __FUNCTION__, (int)rd, (int)amt, random_source);
        abort();
    }
}

// create a unique shared memory ID
void wikrt_new_ephid(char* ephid)
{
    _Static_assert((sizeof(uint8_t) == sizeof(char))
        , "unsafe cast between uint8_t* and char*");
    _Static_assert((WIKRT_EPH_NAME_LEN >= 32)
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
    size_t const written = snprintf(ephid, WIKRT_EPH_NAME_LEN
        , "/wikrt-eph-%s"
        , (char const*) entropy_b64
        );
    assert((20 == entropy_b64_len) 
        && (31 == written));
}

// TODO: 
//  lock/unlock with process failure recovery
//  add, remove, test...

#if 0

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


bool wikrt_load_ephid(wikrt_db* db, char const* dirPath)
{
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
#endif

