
#include <blake2.h>
#include <time.h>
#include <stdlib.h>
#include <stdio.h>
#include <errno.h>
#include <string.h>
#include <assert.h>

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

void wikrt_add_cx(wikrt_cx** plist, wikrt_cx* cx);
void wikrt_rem_cx(wikrt_cx** plist, wikrt_cx* cx);
void wikrt_cx_signal_work_available(wikrt_cx*);
void wikrt_cx_interrupt_work(wikrt_cx*);
void wikrt_cx_set_dict_name(wikrt_cx* cx, char const* const dict_name);

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

// blacklist is @#[]()<>{}\/,;|&='", SP, C0 (0-31), and DEL
// and any bytes not permitted in UTF-8 (192, 193, 245-255)
//
// BLOCK                                                    VALUE
//  0-31:   forbid everything                               0
//  32-63:  forbid 32 34 35 38 39 40 41 44 47 59 60 61 62   ~(0x780093cd)
//  64-95:  forbid 64 91 92 93                              ~(0x38000001)
//  96-127: forbid 123 124 125 127                          ~(0xb8000000)
//  128-159: allow everything                               ~0
//  160-191: allow everything                               ~0
//  192-223: forbid 192 193                                 ~3
//  224-255: forbid 245-255                                 ~(0xffe00000)
//
// Scanning for valid word bytes does not ensure we have valid
// UTF-8, but does ensure we recognize any word stop characters.
uint32_t const valid_word_char_bits[8] = 
    {  0, ~(0x780093cd), ~(0x38000001), ~(0xb8000000)
    , ~0, ~0,            ~3,            ~(0xffe00000) };
static inline bool is_valid_word_byte(uint8_t u) 
{
    return (0 != (valid_word_char_bits[ (u >> 5) ] & (1 << (u & 0x1F))));
}
static inline size_t valid_word_len(uint8_t const* const src, size_t maxlen)
{
    uint8_t const* const end = src + maxlen;
    uint8_t const* iter = src;
    while((iter < end) && is_valid_word_byte(*iter)) { ++iter; }
    return (iter - src);
}

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
    bool const env_inactive = (NULL == e->cxs) && (NULL == e->cxw);
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

wikrt_env* wikrt_cx_env(wikrt_cx* cx) { return cx->env; }

void wikrt_worker_loop(wikrt_env* const e)
{
    pthread_mutex_lock(&(e->mutex));
    while(e->workers_max >= e->workers_alloc) {
        // perform available work continuously, rotating through contexts
        fprintf(stderr, "%s todo: perform work!\n", __FUNCTION__);

        // wait for more work to become available        
        pthread_cond_wait(&(e->work_available), &(e->mutex));
    }

    // halt the worker
    assert(0 < e->workers_alloc);
    --(e->workers_alloc);
    if(0 == e->workers_alloc) {
        pthread_cond_signal(&(e->workers_halted));
    } 
    pthread_mutex_unlock(&(e->mutex));
}

void* wikrt_worker_behavior(void* e)
{
    wikrt_worker_loop((wikrt_env*)e);
    return NULL;
}

void wikrt_report_work_available(wikrt_cx* cx)
{
    pthread_mutex_lock(&(cx->env->mutex));
}

void wikrt_halt_threads(wikrt_env* e)
{
    pthread_mutex_lock(&(e->mutex));
    e->workers_max = 0;
    pthread_cond_broadcast(&(e->work_available));
    if(0 != e->workers_alloc) {
        pthread_cond_wait(&(e->workers_halted), &(e->mutex));
        assert(0 == e->workers_alloc);
    }
    pthread_mutex_unlock(&(e->mutex));
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

void wikrt_add_cx_list(wikrt_cx** plist, wikrt_cx* cx) 
{
    // addend to a circular linked list
    // assumes exclusive access to *plist
    if(NULL == *plist) {
        cx->cxp = cx;
        cx->cxn = cx;
    } else {
        cx->cxp = (*plist)->cxp;
        cx->cxn = (*plist);
    }
    cx->cxp->cxn = cx;
    cx->cxn->cxp = cx;
    (*plist) = cx;
}
void wikrt_rem_cx_list(wikrt_cx** plist, wikrt_cx* const cx)
{
    // extract from circular linked list
    if(cx == cx->cxn) {
        assert(cx == (*plist));
        (*plist) = NULL;
    } else {
        cx->cxp->cxn = cx->cxn;
        cx->cxn->cxp = cx->cxp;
        if(cx == (*plist)) {
            (*plist) = (*plist)->cxn;
        }
        assert(cx != (*plist));
    }
    cx->cxp = cx;
    cx->cxn = cx;
}

void wikrt_cx_move_to_env_worklist(wikrt_cx* cx)
{
    _Static_assert(WIKRT_ENV_HAS_TWO_CONTEXT_LISTS, "expecting in cxs or cxw");
    if(!(cx->in_env_worklist)) {
        wikrt_rem_cx_list(&(cx->env->cxs), cx);
        wikrt_add_cx_list(&(cx->env->cxw), cx);
        cx->in_env_worklist = true;
    } 
}

void wikrt_cx_signal_work_available(wikrt_cx* cx)
{
    // TODO: try to reduce synchronization involved here
    pthread_mutex_lock(&(cx->env->mutex));
    wikrt_cx_move_to_env_worklist(cx);
    pthread_mutex_unlock(&(cx->env->mutex));
    pthread_cond_broadcast(&(cx->env->work_available));
}

void wikrt_cx_remove_from_env_worklist(wikrt_cx* cx)
{
    _Static_assert(WIKRT_ENV_HAS_TWO_CONTEXT_LISTS, "expecting in cxs or cxw");
    if(cx->in_env_worklist) {
        wikrt_rem_cx_list(&(cx->env->cxw), cx);
        wikrt_add_cx_list(&(cx->env->cxs), cx);
        cx->in_env_worklist = false; 
    }
}

void wikrt_cx_interrupt_work(wikrt_cx* cx)
{
    // must not be called from a worker thread
    pthread_mutex_lock(&(cx->env->mutex));
    // signal worker threads to halt (checked on GC)
    cx->workers_halt = true;        
    if(0 != cx->worker_count) {
        // wait for worker threads to leave this thread
        pthread_cond_wait(&(cx->workers_done), &(cx->env->mutex));
        assert(0 == cx->worker_count);
    }
    // clear the signal
    cx->workers_halt = false;
    wikrt_cx_remove_from_env_worklist(cx);
    pthread_mutex_unlock(&(cx->env->mutex));
}


wikrt_cx* wikrt_cx_create(wikrt_env* const env, char const* dict_name, size_t size)
{
    _Static_assert( ((9 * sizeof(wikrt_cx)) < WIKRT_CX_MIN_SIZE),
        "insufficient minimum context size");

    bool const ok_args = (NULL != env) 
                      && (size >= WIKRT_CX_MIN_SIZE);
    if(!ok_args) { 
        errno = EINVAL; 
        return NULL; 
    }
    wikrt_cx* const cx = malloc(size);
    if(!cx) { return NULL; }

    // minimally initialize context header
    (*cx) = (wikrt_cx){0};
    cx->env = env;
    cx->size = size;
    cx->mutex = (pthread_mutex_t)PTHREAD_MUTEX_INITIALIZER;
    cx->workers_done = (pthread_cond_t)PTHREAD_COND_INITIALIZER;

    // add context to environment
    pthread_mutex_lock(&(cx->env->mutex));
    wikrt_add_cx_list(&(cx->env->cxs), cx);
    pthread_mutex_unlock(&(cx->env->mutex));

    // use wikrt_cx_reset to initialize allocators, etc.
    wikrt_cx_reset(cx, dict_name);

    return cx;
}

void wikrt_cx_destroy(wikrt_cx* cx)
{
    // may survive destruction if frozen
    pthread_mutex_lock(&(cx->mutex));
    if(cx->refct > 0) {
        assert(cx->frozen);
        --(cx->refct);
        pthread_mutex_unlock(&(cx->mutex));
        return; 
    }
    pthread_mutex_unlock(&(cx->mutex));

    // clear data, halting activity if any
    cx->frozen = false; // unfreeze to destroy
    wikrt_cx_reset(cx, NULL); 

    // remove context from environment
    pthread_mutex_lock(&(cx->env->mutex));
    wikrt_rem_cx_list(&(cx->env->cxs), cx);
    pthread_mutex_unlock(&(cx->env->mutex));

    // recycle allocated context memory
    free(cx);
}


void wikrt_cx_set_dict_name(wikrt_cx* cx, char const* const dict_name)
{
    _Static_assert((sizeof(cx->dict_name) > WIKRT_HASH_SIZE), 
        "insufficient dictionary name size");

    // Use dict a stable name for the dictionary.
    size_t const name_len = (NULL == dict_name) ? 0 : strlen(dict_name);
    size_t const valid_name_len = valid_word_len((uint8_t const*) dict_name, name_len);
    bool const name_ok = (name_len == valid_name_len) && (name_len <= WIKRT_HASH_SIZE);
    if(name_ok) {
        // preserve given name, potentially empty name
        cx->dict_name_len = name_len;
        memcpy(cx->dict_name, dict_name, name_len);
    } else {
        // alias requested name to a secure hash
        cx->dict_name_len = WIKRT_HASH_SIZE;
        wikrt_hash((char*)(cx->dict_name), (uint8_t const*)dict_name, name_len);
    }
    cx->dict_name[cx->dict_name_len] = 0;
    //fprintf(stderr, "context with dictionary `%s`\n", (char const*) cx->dict_name); 
}

wikrt_z wikrt_gc_bitfield_size(wikrt_z alloc_space) 
{
    wikrt_z const bits = alloc_space / WIKRT_CELLSIZE;      // one bit per cell we mark
    wikrt_z const bytes = WIKRT_LNBUFF_POW2(bits, 8) / 8;   // round up to bytes
    return wikrt_cellbuff(bytes);                           // round up to cells
}

wikrt_z wikrt_compute_alloc_space(wikrt_z const space_total)
{
    // We must reserve space in our context for three GC bitfields.
    // 
    // The GC reserve space is about 3/(128+3) of the total space on a 64 bit
    // system, or 3/(64+3) on a 32 bit system. But we actually need to round
    // up a bit to ensure each individual buffer is aligned to the cell size.
    wikrt_z const denom = (8 * WIKRT_CELLSIZE) + 3;
    wikrt_z const field = wikrt_cellbuff((space_total + (denom - 1)) / denom);
    wikrt_z const alloc = (space_total - (3 * field));

    // safety and sanity check
    assert(space_total >= (alloc + (3 * wikrt_gc_bitfield_size(alloc))));
    return alloc;
}

void wikrt_cx_alloc_reset(wikrt_cx* cx)
{
    cx->main = cx->memory = (wikrt_thread){0};
    cx->main.cx = cx->memory.cx = cx;

        // allocations must be aligned to a cell buffer  
    cx->memory.start = wikrt_cellbuff( ((wikrt_a)cx) + sizeof(wikrt_cx) );  
    cx->memory.end   = ((wikrt_a)cx) + cx->size; // exact
    cx->memory.gen   = cx->memory.start; // no elder survivors yet
    cx->memory.alloc = cx->memory.start; // bump pointer allocation

        // limit memory use (.stop) based on GC requirements
    wikrt_z const max_cell_count = (cx->memory.end - cx->memory.start) / WIKRT_CELLSIZE;
    wikrt_z const max_usable_space = max_cell_count * WIKRT_CELLSIZE;
    wikrt_z const alloc_space = wikrt_compute_alloc_space(max_usable_space);
    cx->memory.stop = cx->memory.start + alloc_space;

    // allocation of cx->main from cx->memory will apply lazily
}

void wikrt_cx_reset(wikrt_cx* cx, char const* const dict_name)
{
    wikrt_cx_interrupt_work(cx);
    if(cx->frozen) {
        fprintf(stderr, "%s: a frozen context cannot be reset\n", __FUNCTION__);
        abort();
    }

    // clear prototype from freeze-copy
    if(NULL != cx->proto) {
        wikrt_cx_destroy(cx->proto);
        cx->proto = NULL;
    }

    // reset the root values
    cx->dict_ver[0] = 0;   
    cx->words_table = 0;
    cx->writes_list = 0;
    cx->trace       = 0;
    cx->streams     = 0;
    wikrt_cx_alloc_reset(cx);
    wikrt_cx_set_dict_name(cx, dict_name);

    // set an initial effort quota
    wikrt_set_effort(cx, WIKRT_CX_DEFAULT_EFFORT);
}

void wikrt_set_effort(wikrt_cx* cx, uint32_t cpu_usec)
{
    // I still need to work out how to best track effort.
    // Maybe in terms of rewrites, instead of CPU time?
    //
    // For now, just record the available effort.
    pthread_mutex_lock(&(cx->mutex));
    cx->effort = cpu_usec;
    pthread_mutex_unlock(&(cx->mutex));
}







