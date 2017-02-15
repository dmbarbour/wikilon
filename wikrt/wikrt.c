
#include <blake2.h>
#include <time.h>
#include <stdlib.h>
#include <stdio.h>
#include <errno.h>
#include <string.h>
#include "b64.h"
#include "wikrt_private.h"

_Static_assert((sizeof(wikrt_v) == 4) || (sizeof(wikrt_v) == 8), 
    "expecting 32-bit or 64-bit words");
_Static_assert((sizeof(wikrt_v) >= sizeof(size_t)),
    "expecting to store sizes in a single word");
_Static_assert((sizeof(uint8_t) == sizeof(char)), 
    "expecting uint8_t* aligns with char*");
_Static_assert(sizeof(wikrt_ws) == (4*sizeof(wikrt_v)),
    "flexible array members don't work the way I think they should");

uint32_t wikrt_api_ver() 
{ 
    _Static_assert(WIKRT_API_VER < UINT32_MAX, "bad value for WIKRT_API_VER");
    return WIKRT_API_VER; 
}

uint64_t wikrt_thread_time()
{
    struct timespec tm;
    int const st = clock_gettime(CLOCK_THREAD_CPUTIME_ID, &tm);
    if(0 != st) {
        fprintf(stderr, "%s cannot get time\n", __FUNCTION__);
        abort();
    }
    uint64_t const usec_sec = ((uint64_t)tm.tv_sec) * (1000 * 1000);
    uint64_t const usec_nsec = ((uint64_t)tm.tv_nsec) / 1000;
    return (usec_sec + usec_nsec);
}

void wikrt_get_entropy(size_t const amt, uint8_t* const out)
{
    // Obtaining entropy is not efficient, but Wikilon doesn't do
    // this frequently, usually just to initialize a unique ID for
    // the runtime database.
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

void wikrt_hash(char* const h, uint8_t const* const data, size_t const data_size)
{
    #define WIKRT_HASH_BYTES ((WIKRT_HASH_SIZE * 3) / 4)
    _Static_assert((0 == (WIKRT_HASH_SIZE % 4)), "hash size should be exact in base64");
    _Static_assert((WIKRT_HASH_BYTES <= 64), "hash size should be in valid range for BLAKE2b");
    _Static_assert((sizeof(char) == sizeof(uint8_t)), "casting from char* to uint8_t*");

    uint8_t hbytes[WIKRT_HASH_BYTES];
    int const st = blake2b(hbytes, data, NULL, WIKRT_HASH_BYTES, data_size, 0);
    if(0 != st) {
        fprintf(stderr, "%s invalid hash operation\n", __FUNCTION__);
        abort();
    }
    b64_encode(hbytes, WIKRT_HASH_BYTES, (uint8_t*) h);
};


wikrt_env* wikrt_env_create()
{
    wikrt_env* const e = calloc(1, sizeof(wikrt_env));
    if(NULL == e) { return NULL; }
    e->mutex = (pthread_mutex_t)PTHREAD_MUTEX_INITIALIZER;
    e->work_available = (pthread_cond_t)PTHREAD_COND_INITIALIZER;
    e->workers_halted = (pthread_cond_t)PTHREAD_COND_INITIALIZER;
    return e;
}

void wikrt_env_destroy(wikrt_env* e)
{
    wikrt_halt_threads(e);

    // We require that no contexts exist when this is called.
    bool const env_inactive = (NULL == e->cx) && (NULL == e->cxw);
    if(!env_inactive) {
        fprintf(stderr, "%s environment in use\n", __FUNCTION__);
        abort();
    }

    wikrt_db_close(e);
    pthread_cond_destroy(&(e->work_available));
    pthread_cond_destroy(&(e->workers_halted));
    pthread_mutex_destroy(&(e->mutex));
    free(e);
}

void wikrt_worker_loop(wikrt_env* const e)
{
    pthread_mutex_lock(&(e->mutex));
    do {
        // perform available work continuously, rotating through contexts
        fprintf(stderr, "%s todo: perform some work\n", __FUNCTION__);

        // wait for more work to become available        
        pthread_cond_wait(&(e->work_available), &(e->mutex));
    } while(e->workers_max >= e->workers_alloc);
    
    // halt the worker
    --(e->workers_alloc);
    if(0 == e->workers_alloc) {
        pthread_cond_signal(&(e->workers_halted));
    } 
    pthread_mutex_unlock(&(e->mutex));
    pthread_cond_signal(&(e->work_available)); 
        // in case another thread needs to halt
}

void* wikrt_worker_behavior(void* e)
{
    wikrt_worker_loop((wikrt_env*)e);
    return NULL;
}

void wikrt_halt_threads(wikrt_env* e)
{
    pthread_mutex_lock(&(e->mutex));
    e->workers_max = 0;
    while(0 != e->workers_alloc) {
        pthread_cond_signal(&(e->work_available));
        int const st = pthread_cond_wait(&(e->workers_halted), &(e->mutex));
        if(0 != st) {
            fprintf(stderr, "%s failed to safely halt worker threads\n", __FUNCTION__);
            abort();
        }
    }
    pthread_mutex_unlock(&(e->mutex));
}

void wikrt_db_sync(wikrt_env* e)
{
    // tell LMDB to tell the OS to
    // flush pending writes to disk.
    if(NULL != e->db) {
        int const synchronous_flush = 1;
        mdb_env_sync(e->db->mdb, synchronous_flush);
    }
}

void wikrt_db_close(wikrt_env* e)
{
    wikrt_db_sync(e);
    if(NULL != e->db) {
        fprintf(stderr, "%s todo: close database\n", __FUNCTION__);
    }
}

void wikrt_env_threadpool(wikrt_env* e, uint32_t ct)
{
    pthread_mutex_lock(&(e->mutex));
    e->workers_max = ct;

    // Allocate workers only if we're done.
    if(e->workers_alloc < e->workers_max) {
        pthread_attr_t a;
        pthread_attr_init(&a);
        pthread_attr_setdetachstate(&a, PTHREAD_CREATE_DETACHED);
        pthread_attr_setstacksize(&a, WIKRT_WORKER_STACK_SIZE);
        
        uint32_t alloc = (e->workers_max - e->workers_alloc);
        do {
            pthread_t tid; // dropped; threads are not signaled directly.
            int const st = pthread_create(&tid, &a, &wikrt_worker_behavior, e);
            if(0 == st) { ++(e->workers_alloc); }
            --alloc;
        } while(alloc > 0);

        pthread_attr_destroy(&a);
    } else if(e->workers_alloc > e->workers_max) {
        // Otherwise signal workers to die asynchronously
        pthread_cond_signal(&(e->work_available));
    }
    pthread_mutex_unlock(&(e->mutex));
}



