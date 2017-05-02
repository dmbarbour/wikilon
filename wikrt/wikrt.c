
#include <blake2.h>
#include <time.h>
#include <stdlib.h>
#include <stdio.h>
#include <errno.h>
#include <string.h>
#include <assert.h>

#include "b64.h"
#include "utf8.h"
#include "wikrt_private.h"

_Static_assert((sizeof(wikrt_v) == 4) || (sizeof(wikrt_v) == 8), 
    "expecting 32-bit or 64-bit words");
_Static_assert((sizeof(wikrt_v) >= sizeof(size_t)),
    "expecting to store sizes in a single word");
_Static_assert((sizeof(uint8_t) == sizeof(char)), 
    "expecting uint8_t* aligns with char*");

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
    uint64_t const usec_usec = ((uint64_t)tm.tv_nsec) / 1000;
    return (usec_sec + usec_usec);
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
    bool const inactive = (NULL == e->cxs);
    if(!inactive) {
        fprintf(stderr, "%s environment destroyed still has undestroyed contexts\n"
            , __FUNCTION__);
        abort();
    }

    wikrt_db_close(e);
    pthread_cond_destroy(&(e->work_available));
    pthread_cond_destroy(&(e->workers_halted));
    pthread_mutex_destroy(&(e->mutex));
    free(e);
}

wikrt_env* wikrt_cx_env(wikrt_cx* cx) 
{ 
    return cx->env; 
}

void wikrt_worker_loop(wikrt_env* const e)
{

    pthread_mutex_lock(&(e->mutex));
    do {
        // perform available work continuously, rotating through contexts
        //fprintf(stderr, "%s todo: perform work!\n", __FUNCTION__);

        // Note: when work is available, only a single worker is signaled.
        // So we'll need that worker to signal yet another if yet more work
        // is available in the context, perhaps heuristically limited based
        // on available effort and space.


        // when we're done, either halt or wait for more work
        if(e->workers_max < e->workers_alloc) { break; }
        pthread_cond_wait(&(e->work_available), &(e->mutex)); 
    } while(1);

    // halt the worker
    assert(0 < e->workers_alloc);
    --(e->workers_alloc);
    if(0 == e->workers_alloc) {
        pthread_cond_signal(&(e->workers_halted));
    } 
    pthread_mutex_unlock(&(e->mutex));
}

static void* wikrt_worker_behavior(void* e)
{
    wikrt_worker_loop((wikrt_env*)e);
    return NULL;
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

    if(e->workers_alloc < e->workers_max) {
        // allocate workers 
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
        pthread_cond_broadcast(&(e->work_available));
    }
    pthread_mutex_unlock(&(e->mutex));
}

static void wikrt_cx_env_add(wikrt_cx* cx) 
{
    pthread_mutex_lock(&(cx->env->mutex));
    if(NULL == cx->env->cxs) {
        // first in list
        cx->cxn = cx;
        cx->cxp = cx;
        cx->env->cxs = cx;
    } else {
        // add to end of list
        cx->cxn = cx->env->cxs;
        cx->cxp = cx->cxn->cxp;
        cx->cxn->cxp = cx;
        cx->cxp->cxn = cx;
    }
    pthread_mutex_unlock(&(cx->env->mutex));
}

static void wikrt_cx_env_del(wikrt_cx* cx)
{
    pthread_mutex_lock(&(cx->env->mutex));
    if(cx == cx->cxp) {
        assert(cx == cx->env->cxs);
        cx->env->cxs = NULL;
    } else {
        cx->cxn->cxp = cx->cxp;
        cx->cxp->cxn = cx->cxn;
        if(cx == cx->env->cxs) {
            cx->env->cxs = cx->cxn;
        }
    }
    cx->cxn = NULL;
    cx->cxp = NULL;
    pthread_mutex_unlock(&(cx->env->mutex));
}

void wikrt_cx_signal_work(wikrt_cx* cx)
{
    // Note: No work queue. Workers that are awake scan all contexts
    // for work, and return to sleep only if no work was available. 
    // This has a moderate overhead for scanning inactive contexts.
    // OTOH, it simplifies shared state and synchronization. 
    //
    // When a worker cannot find any work, it sleeps on a condvar. We
    // may signal that condvar in case all workers are sleeping, to
    // get a worker back on task ASAP.
    pthread_cond_signal(&(cx->env->work_available));
}


void wikrt_api_interrupt(wikrt_cx* cx)
{
    // Tell worker threads to leave, then wait. This need to interrupt
    // is the main complication that discourages use of work queues.
    cx->interrupt = true;
    if(0 != cx->worker_count) {
        pthread_cond_wait(&(cx->workers_done), &(cx->mutex));
    }
    assert(0 == cx->worker_count);
    cx->interrupt = false;
}

wikrt_cx* wikrt_cx_create(wikrt_env* const env, char const* dict_name, size_t size)
{
    _Static_assert( ((50 * sizeof(wikrt_cx)) < WIKRT_CX_MINSIZE),
        "assuming context header is small relative to smallest context");

    bool const ok_args = (NULL != env) && (size >= WIKRT_CX_MINSIZE);
    if(!ok_args) { errno = EINVAL; return NULL; }

    wikrt_cx* const cx = malloc(size);
    if(!cx) { return NULL; }

    // minimally initialize context header
    (*cx) = (wikrt_cx){0};
    cx->env = env;
    cx->size = size;
    cx->mutex = (pthread_mutex_t)PTHREAD_MUTEX_INITIALIZER;
    cx->workers_done = (pthread_cond_t)PTHREAD_COND_INITIALIZER;
    wikrt_cx_reset(cx, dict_name);  // initialize context state
    wikrt_cx_env_add(cx);           // make visible to workers
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

    // clear the context, hide it from workers
    wikrt_cx_env_del(cx);
    cx->frozen = false; 
    wikrt_cx_reset(cx, NULL); 

    // release POSIX resources as needed
    pthread_cond_destroy(&(cx->workers_done));
    pthread_mutex_destroy(&(cx->mutex));

    // recycle allocated context memory
    free(cx);
}


static void wikrt_cx_reset_dict(wikrt_cx* cx, char const* const dict_name)
{
    _Static_assert((sizeof(cx->dict_name) > WIKRT_HASH_SIZE), 
        "insufficient dictionary name size");
    _Static_assert((sizeof(char) == sizeof(uint8_t)),
        "unsafe casts between uint8_t* and char*");

    // Compute a stable name for the dictionary. A secure hash for the
    // dictionary name is used unless it's a short, valid Awelon word.
    // Aliasing is resisted for access control security reasons.
    size_t const name_len = (NULL == dict_name) ? 0 : strlen(dict_name);
    size_t const valid_name_len = wikrt_word_len((uint8_t const*) dict_name, 
        utf8_strlen((uint8_t const*) dict_name, name_len));
    bool const name_ok = (name_len == valid_name_len) && (name_len < WIKRT_HASH_SIZE);
    if(name_ok) {
        // preserve given name, potentially empty name
        cx->dict_name_len = name_len;
        memcpy(cx->dict_name, dict_name, name_len);
    } else {
        // reference requested name indirectly, via secure hash
        cx->dict_name_len = WIKRT_HASH_SIZE;
        wikrt_hash((char*)(cx->dict_name), (uint8_t const*)dict_name, name_len);
    }
    cx->dict_name[cx->dict_name_len] = 0;
    cx->dict_ver[0] = 0; // clear version information

    //fprintf(stderr, "context with dictionary `%s`\n", (char const*) cx->dict_name); 
}

wikrt_z wikrt_gc_bitfield_size(wikrt_z alloc_space) 
{
    wikrt_z const bits = alloc_space / WIKRT_CELLSIZE;      // one bit per full cell
    wikrt_z const bytes = WIKRT_LNBUFF_POW2(bits, 8) / 8;   // round up to full bytes
    return wikrt_cellbuff(bytes);                           // round up to full cells
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
    assert(field >= wikrt_gc_bitfield_size(alloc)); // safety and sanity check
    return alloc;
}

void wikrt_cx_reset(wikrt_cx* cx, char const* const dict_name)
{
    wikrt_cx* old_proto; // for latent destruction

    wikrt_api_enter(cx);
    wikrt_api_interrupt(cx);

    if(cx->frozen) {
        fprintf(stderr, "%s: a frozen context cannot be reset\n", __FUNCTION__);
        abort();
    }
    assert(0 == cx->refct);

    // latent destruction of prototype (to simplify thread safety)
    old_proto = cx->proto;
    cx->proto = NULL;

    // TODO
    // Clear ephemeron references (via wikrt_eph_rem).
    // This should include cx->dict_ver (if determined).

    // reset memory and registers 
    cx->trace_enable    = false;
    cx->prof_enable     = false;
    cx->words           = 0;
    cx->effort          = WIKRT_CX_DEFAULT_EFFORT;
    cx->rtb             = (wikrt_rtb){0};
    cx->memory          = (wikrt_thread){0};
    cx->memory.cx       = cx;
    cx->memory.start    = wikrt_cellbuff( ((wikrt_a)cx) + sizeof(wikrt_cx) );  
    cx->memory.alloc    = cx->memory.start; // bump pointer allocation
    cx->memory.end      = ((wikrt_a)cx) + cx->size; // exact
    wikrt_z const max_cell_count = (cx->memory.end - cx->memory.start) / WIKRT_CELLSIZE;
    wikrt_z const max_usable_space = max_cell_count * WIKRT_CELLSIZE;
    wikrt_z const alloc_space = wikrt_compute_alloc_space(max_usable_space);
    cx->memory.stop = cx->memory.start + alloc_space;

    wikrt_rtb_prealloc(cx, 16);
    wikrt_cx_reset_dict(cx, dict_name);

    wikrt_api_exit(cx);

    if(NULL != old_proto) { 
        wikrt_cx_destroy(old_proto); 
    }
}

void wikrt_cx_gc(wikrt_cx* cx)
{
    wikrt_api_enter(cx);
    wikrt_api_gc(cx, WIKRT_Z_MAX);
    wikrt_api_exit(cx);
}

void wikrt_api_gc(wikrt_cx* cx, wikrt_z _amt)
{
    wikrt_api_interrupt(cx); // stop worker threads
    // perform GC, use _amt as an optional GC hint for major vs. minor
    // for now, perhaps perform major GC every time
}

void wikrt_thread_poll_waiting(wikrt_thread* thread)
{
    wikrt_v* waitlist = &(thread->waiting);
    while(0 != *waitlist) 
    {
        wikrt_v const tv = *waitlist;
        wikrt_task* const t = (wikrt_task*)wikrt_v2a(tv);
        assert(wikrt_is_task(tv) && wikrt_is_task(t->wait));
        wikrt_task const* const w = (wikrt_task*)wikrt_v2a(t->wait);
        if(WIKRT_OTYPE_TASK_COMPLETE == w->o) {
            t->wait = 0; // this task is no longer waiting
            (*waitlist) = t->next; // remove from waitlist
            t->next = thread->ready; // add to ready list
            thread->ready = tv;
        } else {
            waitlist = &(t->next);  // poll next item
        }
    }
}

void wikrt_set_effort(wikrt_cx* cx, uint32_t effort)
{
    wikrt_api_enter(cx);
    // forcibly halt evaluation if effort is 0. 
    if(0 == effort) { wikrt_api_interrupt(cx); }
    cx->effort = effort;
    wikrt_api_exit(cx);
}

void wikrt_debug_trace(wikrt_cx* cx, bool enable)
{
    wikrt_api_enter(cx);
    cx->trace_enable = enable;
    if(!enable) { cx->memory.trace = 0; }
    wikrt_api_exit(cx);
}


bool wikrt_debug_trace_move(wikrt_cx* cx, wikrt_r dst)
{   
    wikrt_api_enter(cx);
    if(!wikrt_api_prealloc(cx, 1, WIKRT_REG_WRITE_PREALLOC)) {
        wikrt_api_exit(cx);
        errno = ENOMEM;
        return false;
    }

    // addend trace log to specified register
    wikrt_reg_write(cx, dst, cx->memory.trace);
    cx->memory.trace = 0;
    wikrt_api_exit(cx);
    return true;
}
   

void wikrt_prof_stack(wikrt_cx* cx, bool enable)
{
    wikrt_api_enter(cx);
    cx->prof_enable = enable;
    if(!enable) { cx->memory.prof = 0; }
    wikrt_api_exit(cx);
}

bool wikrt_prof_stack_move(wikrt_cx* cx, wikrt_r dst)
{
    wikrt_api_enter(cx);
    if(!wikrt_api_prealloc(cx, 1, WIKRT_REG_WRITE_PREALLOC)) {
        wikrt_api_exit(cx);
        errno = ENOMEM;
        return false;
    } 
    wikrt_reg_write(cx, dst, cx->memory.prof);
    cx->memory.prof = 0;
    wikrt_api_exit(cx);
    return true;
}

static inline wikrt_z wikrt_write_frag_prealloc(wikrt_z const amt) 
{
    // binary + ptype wrapper + reg write
    return wikrt_binary_size(amt)
         + (WIKRT_CELLSIZE + WIKRT_REG_WRITE_PREALLOC);
}

static inline wikrt_z wikrt_write_bytes_needed(size_t amt) 
{
    // total cost for write
    wikrt_z needed = 0;
    while(amt > 0) {
        wikrt_z const frag_size = (amt > WIKRT_O_DATA_MAX) ? WIKRT_O_DATA_MAX : amt;
        // max overhead for binary fragment, PTYPE, and register write 
        needed += wikrt_write_frag_prealloc(frag_size);
        amt    -= frag_size;
    }
    return needed;
}

wikrt_v wikrt_alloc_binary(wikrt_thread* t, uint8_t const* const data, wikrt_z const amt)
{
    wikrt_a const a = wikrt_thread_alloc(t, wikrt_cellbuff(sizeof(wikrt_o) + amt));
    *((wikrt_o*)a) = (amt << WIKRT_O_DATA_OFF) | WIKRT_OTYPE_BINARY;
    memcpy((void*)(a + sizeof(wikrt_o)), data, amt);
    return (WIKRT_VOBJ | a);
}

static inline void wikrt_write_frag(wikrt_cx* cx, wikrt_r r, uint8_t const* const data, wikrt_z const amt)
{
    // write a single binary fragment into context memory 
    assert(WIKRT_O_DATA_MAX >= amt);
    wikrt_v const binary  = wikrt_alloc_binary(&(cx->memory), data, amt);
    wikrt_v const raw = WIKRT_OBJ | wikrt_alloc_cell(&(cx->memory), 
        wikrt_new_ptype_hdr(WIKRT_PTYPE_BINARY_RAW), binary);
    wikrt_reg_write(cx, r, raw);
}

bool wikrt_write(wikrt_cx* cx, wikrt_r r, uint8_t const* data, size_t amt) 
{
    // if write is size zero, then it's irrelevant.
    if(0 == amt) { return true; } 

    // to avoid overflow cases, don't bother if the binary is very large.
    if(amt > (WIKRT_Z_MAX / 2)) { errno = ENOMEM; return false; }
    wikrt_z const space_needed = wikrt_write_bytes_needed(amt);

    wikrt_api_enter(cx);
    if(!wikrt_api_prealloc(cx, 1, space_needed)) {
        wikrt_api_exit(cx);
        errno = ENOMEM;
        return false;
    }

    while(amt > 0) {
        wikrt_z const frag_size = (amt > WIKRT_O_DATA_MAX) ? WIKRT_O_DATA_MAX : amt;
        wikrt_write_frag(cx, r, data, frag_size);
        data += frag_size;
        amt  -= frag_size;
    }
    wikrt_api_exit(cx);

    return true;
}

// read is in wikrt_read.c

bool wikrt_is_empty(wikrt_cx* cx, wikrt_r r)
{
    pthread_mutex_lock(&(cx->mutex));
    wikrt_v const v = wikrt_reg_get(cx, r);
    pthread_mutex_unlock(&(cx->mutex));
    return (0 == v);
}

void wikrt_clear(wikrt_cx* cx, wikrt_r r)
{
    if(cx->frozen) {
        fprintf(stderr, "cannot clear a frozen context's register\n");
        abort();
    }
    pthread_mutex_lock(&(cx->mutex));
    wikrt_reg_set(cx, r, 0);
    pthread_mutex_unlock(&(cx->mutex));
}




void wikrt_cx_freeze(wikrt_cx* cx)
{
    // final cleanup
    wikrt_eval_parallel(cx);
    wikrt_cx_gc(cx);

    // perform a full GC to compact memory
    wikrt_api_enter(cx);
    cx->memory.alloc = cx->memory.end; // prevent further allocation
    cx->frozen = true;                 // prevent further GC

    // TODO: mark all objects in context as shared
    //  (this might be similar to a GC pass?)

    wikrt_api_exit(cx);
}

bool wikrt_eval_parallel(wikrt_cx* cx)
{
    errno = ENOSYS;
    return false;
}






