
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

void wikrt_get_entropy(size_t const amt_req, uint8_t* const out)
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
    size_t const amt_read = fread(out, 1, amt_req, f);
    fclose(f);
    if(amt_read != amt_req) {
        fprintf(stderr, "%s could only read %d bytes from %s\n"
            , __FUNCTION__, (int)amt_read, random_source);
        abort();
    }
}

void wikrt_hash(char* h, uint8_t const* data, size_t data_size)
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
    wikrt_env* const env = calloc(1, sizeof(wikrt_env));
    if(NULL == env) { return NULL; }
    env->mutex = (pthread_mutex_t)PTHREAD_MUTEX_INITIALIZER;

    return env;
}

void wikrt_env_destroy(wikrt_env* const e)
{
    if(NULL == e) { errno = EINVAL; return; }

    // We require that no contexts exist when this is called.
    bool const env_inactive = (NULL == e->cx) && (NULL == e->cxw);
    if(!env_inactive) {
        fprintf(stderr, "%s environment in use\n", __FUNCTION__);
        abort();
    }

    // todo: flush and close the database
    // wikrt_db_sync(env);

    pthread_mutex_destroy(&(e->mutex));
    free(e);
}




