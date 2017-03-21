
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
    uint64_t const usec_nsec = ((uint64_t)tm.tv_nsec) / 1000;
    return (usec_sec + usec_nsec);
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
    bool const env_inactive = (NULL == e->cxs) 
                           && (NULL == e->cxw);
    if(!env_inactive) {
        fprintf(stderr, "%s environment destroyed still has contexts\n", __FUNCTION__);
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
        // is available in the context (perhaps heuristically limited based
        // on available effort and space).


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
        pthread_cond_broadcast(&(e->work_available));
    }
    pthread_mutex_unlock(&(e->mutex));
}

void wikrt_add_cx_list(wikrt_cx** plist, wikrt_cx* cx) 
{
    // may not already be part of any list
    assert((NULL == cx->cxp) 
        && (NULL == cx->cxn));

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
        if(cx == (*plist)) {
            (*plist) = cx->cxn;
        }
        cx->cxp->cxn = cx->cxn;
        cx->cxn->cxp = cx->cxp;
    }

    // indicate not part of any list
    cx->cxp = NULL;
    cx->cxn = NULL;
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

void wikrt_cx_signal_work(wikrt_cx* cx)
{
    // Note: we'll signal one thread only, but each worker may
    // signal another and so on based on heuristic decisions.    
    pthread_mutex_lock(&(cx->env->mutex));
    wikrt_cx_move_to_env_worklist(cx);
    pthread_mutex_unlock(&(cx->env->mutex));
    pthread_cond_signal(&(cx->env->work_available));
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
    cx->frozen = false; // can't reset a frozen context
    wikrt_cx_reset(cx, NULL); 

    // remove context from environment
    pthread_mutex_lock(&(cx->env->mutex));
    wikrt_rem_cx_list(&(cx->env->cxs), cx);
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
    assert(wikrt_cx_unshared(cx));
    cx->memory = (wikrt_thread){0};
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
    wikrt_cx_interrupt_work(cx);

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

    // reset data and roots
    cx->trace_enable    = false;
    cx->prof_enable     = false;
    cx->words           = 0;
    cx->temp            = 0;
    cx->reg             = (wikrt_rtb){0};
    cx->dict_ver[0]     = 0;
    wikrt_cx_reset_dict(cx, dict_name);
    wikrt_cx_alloc_reset(cx);

    // set an initial effort quota
    wikrt_set_effort(cx, WIKRT_CX_DEFAULT_EFFORT);
}

void wikrt_thread_poll_waiting(wikrt_thread* thread)
{
    wikrt_v* waitlist = &(thread->waiting);
    while(0 != *waitlist) 
    {
        wikrt_v const tv = *waitlist;
        wikrt_task* const t = (wikrt_task*)wikrt_v2a(tv);
        assert(wikrt_is_task(tv) && wikrt_is_task(t->wait));
        wikrt_task const* const w = (wikrt_task const*)wikrt_v2a(t->wait);
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
    if(0 == effort) {
        wikrt_cx_interrupt_work(cx);
        assert(0 == cx->worker_count);
        cx->memory.effort = 0;
    } else {
        pthread_mutex_lock(&(cx->mutex));
        cx->memory.effort = effort;
        wikrt_thread_poll_waiting(&(cx->memory));
        bool const try_work = (0 != cx->memory.ready);
        pthread_mutex_unlock(&(cx->mutex));

        // signal work available to continue background
        // parallelism if effort is set positive and at
        // least a little work is available.
        if(try_work) {
            wikrt_cx_signal_work(cx);
        }
    }
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
    // TODO
    //  probably move memory.trace to temp then write temp.
    return false;
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
    // TODO
    return false;
}



// obtain an index where we would write to a register.
bool wikrt_get_register_index(wikrt_cx* cx, wikrt_r r, wikrt_n* ix)
{
    // non-destructively obtain register index
    if(0 == cx->registers.ids) { 
        return false;
    }
    wikrt_n const* const ixs = (wikrt_n const*) (1 + wikrt_v2p(cx->registers.

bool wikrt_get_register_index(wikrt_cx* cx, wikrt_r r, wikrt_n* ix) 
    
    
}

bool wikrt_grow_registers(wikrt_cx* cx)
{
    // grow register table monotonically, as needed.
    //   
}

// allocate index where we would write to a register
bool wikrt_alloc_register_index(wikrt_cx* cx, wikrt_r r, wikrt_n* ix)
{
    if(wikrt_get_register_index(cx, r, ix)) { return true; }
    if(!wikrt_grow_registers(cx)) { return false; }
    return wikrt_get_register_index(cx, r, ix);   
}

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


bool wikrt_write(wikrt_cx* cx, wikrt_r r, uint8_t const* data, size_t amt) 
{
    if(0 == total_amt) { return true; } // irrelevant write
    if(0 == r) { errno = EBADF; return false; } // cannot write NULL stream

    if(!wikrt_alloc_binary_temp(cx, data, amt)) { return false; }
    return wikrt_reg_write_temp(cx, r);
}

size_t wikrt_read(wikrt_cx* cx, wikrt_r fd, uint8_t* const buff, size_t const max)
{
    return 0;
}

bool wikrt_is_empty(wikrt_cx* cx, wikrt_r fd)
{
    // test if stream is empty
    return true;
}

void wikrt_clear(wikrt_cx* cx, wikrt_r fd)
{
    
}













