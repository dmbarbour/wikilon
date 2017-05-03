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
#include "wikrt_eph.h"


// data elements are simply a uint64_t:
//   upper 44 bits for the hash value
//   lower 20 bits for the ref count
// 
// Empty slots are always 0 (zero hash, zero refct). When an
// element is removed, we'll repair the table so we don't need
// any "marked deleted" slots.
//
// Linear probing is used to fill the table, with a fill max of 
// about 2/3. A difference is that linear probing might need to
// scan past a used slot.
typedef uint64_t wikrt_eph_d;
#define WIKRT_EPH_RC_BITS 20
#define WIKRT_EPH_RC_MAX (( ((wikrt_eph_d)1) << WIKRT_EPH_RC_BITS ) - 1)
#define WIKRT_EPH_HASH_MAX  (UINT64_MAX >> WIKRT_EPH_RC_BITS)
#define WIKRT_EPH_HASH_PRIME 1235789
#define WIKRT_EPH_SIZE_INIT 4000

#define WIKRT_EPH_H(D) (D >> WIKRT_EPH_RC_BITS)
#define WIKRT_EPH_RC(D) (D & WIKRT_EPH_RC_MAX)
#define WIKRT_EPH_D(H,RC) ((H << WIKRT_EPH_RC_BITS) | RC)

#define WIKRT_EPH_VER 20170302 
#define WIKRT_EPH_NAME_LEN 32

static inline uint64_t wikrt_eph_hash(uint64_t id) { 
    // distribute adjacent identifiers
    return ((id * WIKRT_EPH_HASH_PRIME) & WIKRT_EPH_HASH_MAX); 
}

// stable header
typedef struct wikrt_eph_h {
    uint64_t        ver;    // magic number for multi-process sharing
    uint64_t        refct;  // references to table overall
    pthread_mutex_t mutex;  // must be a shared, robust mutex
    uint64_t        fill;   // filled slots
    uint64_t        size;   // total slots count
    uint64_t        resize; // for failure recovery on resize
} wikrt_eph_h;

struct wikrt_eph {
    char            name[WIKRT_EPH_NAME_LEN]; // shm name
    int             shm;    // shm file descriptor
    int             file;   // identity file for locking
    wikrt_eph_h*    head;   // mapped header only
    void*           mmd;    // full memory mapped file
    size_t          mmsz;   // memory mapped data size
    wikrt_eph_d*    data;   // data pointer
    bool            error;  // resize or recovery error
};

static void wikrt_get_entropy(size_t const amt, uint8_t* const out);
static void wikrt_new_ephid(char* ephid);
static bool wikrt_eph_open_file(wikrt_eph* eph, char const* file, int mode);
static bool wikrt_eph_open_shm(wikrt_eph* eph, int mode);
static void wikrt_eph_repair(wikrt_eph* eph);

wikrt_eph* wikrt_eph_open(char const* file, int mode)
{
    wikrt_eph* const eph = calloc(1,sizeof(wikrt_eph));
    if(NULL == eph) { return NULL; }

    if(!wikrt_eph_open_file(eph, file, mode)) {
        free(eph);
        return NULL;
    }

    // use file locks to isolate initialization to a single process
    flock(eph->file, LOCK_EX);

    // create the shared memory
    if(!wikrt_eph_open_shm(eph, mode)) {
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
static bool wikrt_eph_open_file(wikrt_eph* eph, char const* file, int mode) 
{
    eph->file = open(file, O_CREAT|O_RDWR, mode);
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
static bool wikrt_eph_open_shm(wikrt_eph* eph, int mode)
{
    eph->shm = shm_open(eph->name, O_CREAT | O_RDWR, mode);
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
        if(0 != ftruncate(eph->shm, sizeof(wikrt_eph_h))) {
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
        eph->head->resize = WIKRT_EPH_SIZE_INIT;

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
    assert(0 < eph->head->refct);
    --(eph->head->refct);
    if(0 == eph->head->refct) {
        shm_unlink(eph->name);
        pthread_mutex_destroy(&(eph->head->mutex));
    }
    munmap(eph->head, sizeof(wikrt_eph_h));
    if(0 != eph->mmsz) { 
        munmap(eph->mmd, eph->mmsz);
    }
    close(eph->shm);
    close(eph->file);
    free(eph);
}

static void wikrt_get_entropy(size_t const amt, uint8_t* const out)
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
static void wikrt_new_ephid(char* ephid)
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

bool wikrt_eph_err(wikrt_eph const* eph) { return eph->error; }

void wikrt_eph_lock(wikrt_eph* eph) 
{
    if(eph->error) { return; }
    int const st = pthread_mutex_lock(&(eph->head->mutex));
    bool const locked = (0 == st) || (EOWNERDEAD == st);
    if(!locked) { eph->error = true; return; }
    pthread_mutex_consistent(&(eph->head->mutex)); // crash robust mutex
    wikrt_eph_repair(eph); // always repair
}

void wikrt_eph_unlock(wikrt_eph* eph)
{
    if(eph->error) { return; }
    pthread_mutex_unlock(&(eph->head->mutex));
}

static size_t wikrt_eph_index(wikrt_eph const* eph, uint64_t const h) 
{
    size_t const sz = eph->head->resize;
    size_t ix = (h * 4567) % sz;
    do {
        wikrt_eph_d const entry = eph->data[ix];
        bool const match = (0 == entry) || (h == WIKRT_EPH_H(entry));
        if(match) { return ix; }
        ix = (ix + 1) % sz;
    } while(1);
}

bool wikrt_eph_test(wikrt_eph const* eph, uint64_t elem)
{
    if(eph->error) { return true; } // a false positive
    uint64_t const h = wikrt_eph_hash(elem);
    size_t const ix = wikrt_eph_index(eph, h);
    return (0 != eph->data[ix]);
}

static bool wikrt_eph_prealloc(wikrt_eph* eph, size_t amt) 
{
    // max fill is 2/3
    if(eph->error) { return false; }
    wikrt_eph_h* const eh = eph->head;
    size_t const new_fill = amt + eh->fill;
    bool const oversized = (new_fill * 3) > (eh->size * 2);
    if(!oversized) { return true; }

    // grow size so fill reduces from 2/3 to 1/3
    eh->resize = new_fill * 3;
    wikrt_eph_repair(eph); // repair handles resize
    return !(eph->error); 
}

void wikrt_eph_add(wikrt_eph* eph, uint64_t elem) 
{
    if(!wikrt_eph_prealloc(eph,1)) { return; }

    uint64_t const h = wikrt_eph_hash(elem);
    size_t const ix = wikrt_eph_index(eph, h);

    wikrt_eph_d* const pd = ix + eph->data;
    if(0 == *pd) { ++(eph->head->fill); }
    uint64_t const rc = WIKRT_EPH_RC(*pd);
    if(WIKRT_EPH_RC_MAX == rc) { return; }
    (*pd) = WIKRT_EPH_D(h, (1 + rc));
}

static void wikrt_eph_delete(wikrt_eph* eph, size_t ix)
{
    if(0 == eph->data[ix]) { return; }
    assert(0 < eph->head->fill);
    --(eph->head->fill);
    eph->data[ix] = 0;
    size_t const sz = eph->head->size;

    // local repair for collisions.
    do {
        ix = (ix + 1) % sz;
        wikrt_eph_d const d = eph->data[ix];
        if(0 == d) { return; }

        size_t const new_ix = wikrt_eph_index(eph, WIKRT_EPH_H(d));
        if(new_ix != ix) {
            eph->data[new_ix] = d;
            eph->data[ix] = 0;
        }
    } while(1);
}

void wikrt_eph_rem(wikrt_eph* eph, uint64_t elem)
{
    if(eph->error) { return; }

    uint64_t const h = wikrt_eph_hash(elem);
    size_t ix = wikrt_eph_index(eph, h);
    wikrt_eph_d* pd = ix + eph->data;
    
    uint64_t const rc = WIKRT_EPH_RC(*pd);
    if(WIKRT_EPH_RC_MAX == rc) { return; } // cannot decref
    else if(rc > 1) { (*pd) = WIKRT_EPH_D(h, (rc - 1)); }
    else { wikrt_eph_delete(eph, ix); }
}

static inline size_t wikrt_eph_data_size(wikrt_eph const* eph) 
{
    size_t const sz = (size_t) eph->head->size;
    size_t const rsz = (size_t) eph->head->resize;
    return (sz > rsz) ? sz : rsz;
}

static bool wikrt_eph_resize_shm(wikrt_eph* eph)
{
    size_t const mmsz = sizeof(wikrt_eph_h) + (wikrt_eph_data_size(eph) * sizeof(wikrt_eph_d));
    if(eph->mmsz >= mmsz) { return true; }

    // to keep it simple, always clear the old map. 
    if(0 != eph->mmsz) { munmap(eph->mmd, eph->mmsz); }
    eph->mmd  = 0;
    eph->mmsz = 0;
    eph->data = 0;

    // ensure the shared memory 'file' is sized as we expect
    int const st = ftruncate(eph->shm, (off_t)mmsz);
    if(0 != st) { return false; }

    // remap the file
    void* const mmd = mmap(0, mmsz, PROT_READ|PROT_WRITE, MAP_SHARED, eph->shm, 0);
    if(MAP_FAILED == mmd) { return false; }

    eph->mmsz = mmsz;
    eph->mmd  = mmd;
    eph->data = (wikrt_eph_d*)((uintptr_t)mmd + sizeof(wikrt_eph_h));
    return true;
}

static void wikrt_eph_repair(wikrt_eph* eph)
{
    assert(!(eph->error));

    // try to resize the SHM or memory mapping
    if(!wikrt_eph_resize_shm(eph)) {
        // This is the primary condition for eph->error.
        // We also must unlock upon error.
        eph->error = true;
        pthread_mutex_unlock(&(eph->head->mutex));
        return;
    }

    // resort data if needed. 
    size_t const sz = eph->head->size;
    if(sz == eph->head->resize) { return; }
    assert(eph->head->fill < eph->head->resize);

    for(size_t ix = 0; ix < sz; ++ix) {
        wikrt_eph_d* const pd = ix + eph->data;
        if(0 == *pd) { continue; }
        size_t const new_ix = wikrt_eph_index(eph, WIKRT_EPH_H(*pd));
        if(new_ix == ix) { continue; }
        eph->data[new_ix] = (*pd);
        (*pd) = 0;
    } 

    // resort succeeded (no crash, kill, etc.) 
    eph->head->size = eph->head->resize;
}


