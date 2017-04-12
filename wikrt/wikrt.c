
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
    bool const inactive = (0 == e->cxct);
    if(!inactive) {
        fprintf(stderr, "%s environment destroyed still has %d contexts\n"
            , __FUNCTION__, (int) e->cxct);
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

void* wikrt_worker_behavior(void* e)
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

static void wikrt_wq_add(wikrt_cx* cx) 
{
    if(NULL != cx->wqp) { 
        // already in queue (NOP)
        assert(NULL != cx->wqn);
    } else if(NULL == cx->env->wq) {
        // first in queue
        cx->wqn = cx;
        cx->wqp = cx;
        cx->env->wq = cx;
    } else {
        // addend to queue
        cx->wqn = cx->env->wq;
        cx->wqp = cx->wqn->wqp;
        cx->wqp->wqn = cx;
        cx->wqn->wqp = cx;
    }
}

void wikrt_cx_signal_work(wikrt_cx* cx)
{
    // add context to work queue, then signal workers.
    pthread_mutex_lock(&(cx->env->mutex));
    wikrt_wq_add(cx);
    pthread_mutex_unlock(&(cx->env->mutex));
    pthread_cond_signal(&(cx->env->work_available));
}

static void wikrt_wq_rem(wikrt_cx* const cx)
{
    if(NULL == cx->wqp) {
        // not in queue (NOP)
        assert(NULL == cx->wqn);
    } else if(cx == cx->wqn) {
        assert((cx == cx->wqp) && (cx == cx->env->wq));
        cx->env->wq = NULL;
    } else {
        assert((cx != cx->wqn) && (cx != cx->wqp));
        assert((cx == cx->wqn->wqp) && (cx == cx->wqp->wqn));
        cx->wqn->wqp = cx->wqp;
        cx->wqp->wqn = cx->wqn;
        if(cx == cx->env->wq) {
            cx->env->wq = cx->wqn;
        }
    }
    cx->wqp = NULL;
    cx->wqn = NULL;
}

void wikrt_api_interrupt(wikrt_cx* cx)
{
    // tell all worker threads to leave ASAP.
    cx->interrupt = true;
    pthread_mutex_unlock(&(cx->mutex));

    pthread_mutex_lock(&(cx->env->mutex));
    // wait until all worker threads have left.
    if(0 != cx->worker_count) {
        pthread_cond_wait(&(cx->workers_done), &(cx->env->mutex));
    }
    wikrt_wq_rem(cx);

    // ensure exclusive control for the caller.
    pthread_mutex_lock(&(cx->mutex));
    pthread_mutex_unlock(&(cx->env->mutex));
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

    // add record of context to environment. 
    // At the moment, just a context count.
    pthread_mutex_lock(&(cx->env->mutex));
    ++(cx->env->cxct);
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
    // most logic here is in wikrt_cx_reset.
    cx->frozen = false; // can't reset a frozen context
    wikrt_cx_reset(cx, NULL); 

    // remove record of context from environment
    pthread_mutex_lock(&(cx->env->mutex));
    assert(cx->env->cxct > 0);
    --(cx->env->cxct);
    pthread_mutex_unlock(&(cx->env->mutex));

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

    // Compute a stable name for the dictionary, given arbitrary text.
    // If the text corresponds to a valid Awelon word no larger than a
    // secure hash, we will use that directly. Otherwise, I'll replace
    // the dictionary name by its secure hash.
    size_t const name_len = (NULL == dict_name) ? 0 : strlen(dict_name);
    size_t const valid_name_len = wikrt_word_len((uint8_t const*) dict_name, 
        utf8_strlen((uint8_t const*) dict_name, name_len));
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
    cx->memory       = (wikrt_thread){0};
    cx->memory.cx    = cx;
    cx->memory.start = wikrt_cellbuff( ((wikrt_a)cx) + sizeof(wikrt_cx) );  
    cx->memory.end   = ((wikrt_a)cx) + cx->size; // exact
    cx->memory.elder = cx->memory.start; // no elder survivors yet
    cx->memory.young = cx->memory.start; // no young survivors yet
    cx->memory.alloc = cx->memory.start; // bump pointer allocation

    wikrt_z const max_cell_count = (cx->memory.end - cx->memory.start) / WIKRT_CELLSIZE;
    wikrt_z const max_usable_space = max_cell_count * WIKRT_CELLSIZE;
    wikrt_z const alloc_space = wikrt_compute_alloc_space(max_usable_space);
    cx->memory.stop = cx->memory.start + alloc_space;
}

void wikrt_cx_reset(wikrt_cx* cx, char const* const dict_name)
{
    // halt existing operations ASAP
    wikrt_set_effort(cx,0);

    if(cx->frozen) {
        fprintf(stderr, "%s: a frozen context cannot be reset\n", __FUNCTION__);
        abort();
    }
    assert(0 == cx->refct);

    // clear frozen prototype
    if(NULL != cx->proto) {
        wikrt_cx_destroy(cx->proto);
        cx->proto = NULL;
    }

    // TODO
    // Clear ephemeron references (via wikrt_eph_rem).
    // This should include cx->dict_ver (once determined).

    // reset data and roots
    cx->trace_enable    = false;
    cx->prof_enable     = false;
    cx->words           = 0;
    cx->rtb             = 0;
    cx->dict_ver[0]     = 0;
    wikrt_cx_reset_dict(cx, dict_name);
    wikrt_cx_alloc_reset(cx);

    // set an initial effort quota
    wikrt_set_effort(cx, WIKRT_CX_DEFAULT_EFFORT);
}

void wikrt_cx_gc(wikrt_cx* cx)
{
    wikrt_api_enter(cx);
    wikrt_api_gc(cx);
    wikrt_api_exit(cx);
}

void wikrt_api_gc(wikrt_cx* cx)
{
    wikrt_api_interrupt(cx); // stop worker threads
    // todo: perform a full context GC.
    // (not much point bothering with minor GC yet.)
}

bool wikrt_cx_should_invite_workers(wikrt_cx* cx)
{
    // 
    return (0 != cx->memory.ready) && (0 != cx->memory.effort);
}

void wikrt_api_exit(wikrt_cx* cx)
{
    bool const invite = wikrt_cx_should_invite_workers(cx);
    pthread_mutex_unlock(&(cx->mutex));

    // signal work after every API operation, if potentially useful.
    // It doesn't hurt much if the work we can perform is limited.
    if(invite) { wikrt_cx_signal_work(cx); }
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
    if(0 == effort) { wikrt_api_interrupt(cx); }
    cx->memory.effort = effort;
    wikrt_api_exit(cx);
}

void wikrt_debug_trace(wikrt_cx* cx, bool enable)
{
    pthread_mutex_lock(&(cx->mutex));
    cx->trace_enable = enable;
    if(!enable) { cx->memory.trace = 0; }
    pthread_mutex_unlock(&(cx->mutex));
}


bool wikrt_debug_trace_move(wikrt_cx* cx, wikrt_r dst)
{   
    wikrt_api_enter(cx);
    if(!wikrt_api_prealloc(cx, WIKRT_REG_WRITE_PREALLOC)) {
        wikrt_api_exit(cx);
        errno = ENOMEM;
        return false;
    }

    // addend trace to existing register
    wikrt_reg_write(cx, dst, cx->memory.trace);
    cx->memory.trace = 0;
    wikrt_api_exit(cx);
    return true;
}
   

void wikrt_prof_stack(wikrt_cx* cx, bool enable)
{
    pthread_mutex_lock(&(cx->mutex));
    cx->prof_enable = enable;
    if(!enable) { cx->memory.prof = 0; }
    pthread_mutex_unlock(&(cx->mutex));
}

bool wikrt_prof_stack_move(wikrt_cx* cx, wikrt_r dst)
{
    wikrt_api_enter(cx);
    if(!wikrt_api_prealloc(cx, WIKRT_REG_WRITE_PREALLOC)) {
        wikrt_api_exit(cx);
        errno = ENOMEM;
        return false;
    }
    wikrt_reg_write(cx, dst, cx->memory.prof);
    cx->memory.prof = 0;
    wikrt_api_exit(cx);
    return true;
}


#if 0
bool wikrt_alloc_binary_temp(wikrt_cx* cx, uint8_t const* data, size_t amt)
{
    // Allocate and copy the binary (via temp register)
    wikrt_a addr;
    if(!wikrt_alloc(cx, &addr, (WIKRT_CELLSIZE + amt))) { return false; }
    cx->temp = WIKRT_VOBJ | addr;

    wikrt_binary* const b = (wikrt_binary*)addr;
    b->otype_binary = WIKRT_OTYPE_BINARY;
    b->size = amt;
    memcpy(b->data, data, amt);
    return true;
}
#endif

bool wikrt_write(wikrt_cx* cx, wikrt_r r, uint8_t const* data, size_t amt) 
{
    if(0 == amt) { return true; } // irrelevant write
    errno = ENOSYS;
    return false;
}

size_t wikrt_read(wikrt_cx* cx, wikrt_r r, uint8_t* const buff, size_t const max)
{
    // todo: 
    //   rewrite specified register to binary fragment stream
    //   process output stream in chunks.
    //
    // for now, I can make this a single operation.
    return 0;
}

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













