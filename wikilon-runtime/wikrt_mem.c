
#include "wikrt.h"
#include <string.h>
#include <assert.h>
//#include <stdio.h>

typedef struct wikrt_fb {
    wikrt_size size;
    wikrt_flst next;
} wikrt_fb;

static inline wikrt_fb* wikrt_pfb(void* mem, wikrt_flst addr) {
    return (wikrt_fb*) (addr + (char*)mem); 
}


// basic strategies without fallback resources
static bool wikrt_fl_alloc_ff(void* mem, wikrt_fl* fl, wikrt_sizeb sz, wikrt_addr* addr);
static wikrt_sc wikrt_size_class_ff(wikrt_size const sz);

static inline wikrt_sc wikrt_size_class(wikrt_size const sz) {
    return (sz <= WIKRT_QFSIZE) 
         ? (wikrt_sc) WIKRT_QFCLASS(sz) 
         : wikrt_size_class_ff(sz);
}

static wikrt_sc wikrt_size_class_ff(wikrt_size const sz) {
    _Static_assert((WIKRT_CELLSIZE == sizeof(wikrt_fb)), "invalid free-block size");
    _Static_assert((WIKRT_FLCT_FF > 0), "code assumes WIKRT_FLCT_FF > 0");
    wikrt_sc sc = (WIKRT_FLCT - 1);
    wikrt_size szt = WIKRT_FFMAX;
    while(szt >= sz) {
        szt = szt >> 1;
        sc = sc - 1;
    }
    return sc;
}

static inline wikrt_flst wikrt_flst_singleton(void* mem, wikrt_sizeb sz, wikrt_addr a) 
{
    wikrt_fb* const pa = wikrt_pfb(mem, a);
    pa->size = sz;
    pa->next = a;
    return a;
}

/* merge two circular free-lists in constant time.
 *
 * The resulting list will allocate the `a` elements before the `b` elements. 
 */
static inline wikrt_flst wikrt_flst_join(void* mem, wikrt_flst a, wikrt_flst b) 
{
    if(0 == a) { return b; }
    if(0 == b) { return a; }
    wikrt_fb* const pa = wikrt_pfb(mem, a);
    wikrt_fb* const pb = wikrt_pfb(mem, b);
    wikrt_flst const hd = pa->next;
    pa->next = pb->next;
    pb->next = hd;
    return b;
}

/* Add to head of free-list. No coalescing adjacent memory. */
void wikrt_fl_free(void* mem, wikrt_fl* fl, wikrt_sizeb sz, wikrt_addr addr) 
{
    fl->free_bytes += sz;
    fl->frag_count += 1;
    wikrt_addr* const l = fl->size_class + wikrt_size_class(sz);
    (*l) = wikrt_flst_join(mem, wikrt_flst_singleton(mem, sz, addr), (*l));
}


/* For small allocations, we'll simply double the allocation if we couldn't
 * find an exact match. This should reduce fragmentation. Large allocations
 * will use first-fit. We'll always fall back on first-fit for completeness.
 *
 * Note: We do not coalesce automatically. Any decision to coalesce memory
 * will be deferred to our caller.
 */
bool wikrt_fl_alloc(void* mem, wikrt_fl* fl, wikrt_sizeb sz, wikrt_addr* addr)
{
    _Static_assert((WIKRT_CELLSIZE == sizeof(wikrt_fb)), "free-block should match minimum allocation");
    if(sz <= WIKRT_QFSIZE) {
        wikrt_flst* const l = fl->size_class + WIKRT_QFCLASS(sz);
        if(0 != (*l)) {
            wikrt_fb* const hd = wikrt_pfb(mem, (*l));
            (*addr) = hd->next;
            if((*addr) == (*l)) { (*l) = 0; }
            else { hd->next = wikrt_pfb(mem, (*addr))->next; }
            fl->frag_count -= 1;
            fl->free_bytes -= sz;
            return true;
        } else if(wikrt_fl_alloc(mem, fl, (sz << 1), addr)) {
            // double sized allocation, then free latter half.
            // we cannot assume l empty (first-fit leftovers)
            wikrt_fl_free(mem, fl, sz, sz + (*addr));
            return true;
        }
    }
    // fallback on first-fit
    return wikrt_fl_alloc_ff(mem, fl, sz, addr);
}

/* allocate using a first-fit strategy. 
 *
 * This will also rotate our free list such that we always match the
 * first item in the list. This rotation isn't necessarily optimal,
 * but it also shouldn't hurt much due to segregation of free lists.
 */
static bool wikrt_fl_alloc_ff(void* mem, wikrt_fl* fl, wikrt_sizeb sz, wikrt_addr* addr) {
    wikrt_sc sc = wikrt_size_class(sz);
    do {
        wikrt_flst* const l = fl->size_class + (sc++); // increment included
        wikrt_flst const l0 = (*l);
        if(0 == l0) { continue; }
        wikrt_fb* pl = wikrt_pfb(mem, l0);
        do {
            wikrt_flst const a = pl->next;
            wikrt_fb* const pa = wikrt_pfb(mem, a);
            if(pa->size >= sz) {
                (*addr) = a;
                if(pa == pl) { (*l) = 0; }
                else { pl->next = pa->next; }
                fl->free_bytes -= pa->size;
                fl->frag_count -= 1;
                if(pa->size > sz) {
                    wikrt_fl_free(mem, fl, (pa->size - sz), (a + sz));
                }
                return true;
            } else { (*l) = a; pl = pa; }
        } while(l0 != (*l));
    } while(sc < WIKRT_FLCT);
    return false; 
}

// Optimization: Is growing allocations in place worthwhile?
//
// It's a bunch of duplicate code. It only triggers in rare cases,
// which damages robustness. It probably won't work nicely with
// rapid allocations, small increases, or multi-threading. If we
// cannot easily predict or control an optimization, it seems an
// unnecessary source of frustration...
//
// Resolution: leave it out. 

// join segregated free-lists into a single large free-list.
static inline wikrt_flst wikrt_fl_flatten(void* const mem, wikrt_fl* const fl) {
    wikrt_flst r = 0;
    for(wikrt_sc sc = 0; sc < WIKRT_FLCT; ++sc) {
        r = wikrt_flst_join(mem, r, fl->size_class[sc]);
    }
    return r;
}

// break a circular free-list into a non-circular linked list.
static inline wikrt_addr wikrt_flst_open(void* mem, wikrt_flst a) {
    if(0 == a) { return 0; } // empty list
    wikrt_fb* const pa = wikrt_pfb(mem, a);
    wikrt_addr const hd = pa->next;
    pa->next = 0;
    return hd;
}

static void wikrt_fl_split(void* const mem, wikrt_addr const hd, wikrt_addr* const a,
                           wikrt_size const sza, wikrt_addr* const b) 
{
    // I assume sza is valid (no larger than the list)
    (*a) = hd;
    wikrt_addr* tl = a;
    wikrt_size ct = 0;
    while(ct++ != sza) { 
        tl = &(wikrt_pfb(mem, (*tl))->next); 
    }
    // at this point 'tl' points to the location of the split.
    (*b) = (*tl); // split remainder of list into 'b'.
    (*tl) = 0;    // 'a' now terminates where 'b' starts.
}

/* Sort free nodes by ascending address, using an in-place mergesort. */
static void wikrt_fl_mergesort(void* const mem, wikrt_addr* hd, wikrt_size const count)
{
    // base case: list of size zero or one is fully sorted
    if(count < 2) { return; }

    wikrt_size const sza = count / 2;
    wikrt_size const szb = count - sza;
    wikrt_addr a, b;

    // split list in two and sort each half
    wikrt_fl_split(mem, (*hd), &a, sza, &b);
    wikrt_fl_mergesort(mem, &a, sza);
    wikrt_fl_mergesort(mem, &b, szb);

    // merge sublists 'a' and 'b'.
    wikrt_addr* tl = hd;
    do {
        if(a < b) {
            (*tl) = a;
            tl = &(wikrt_pfb(mem, a)->next);
            a = (*tl); 
            if(0 == a) { (*tl) = b; return; }
        } else {
            (*tl) = b;
            tl = &(wikrt_pfb(mem, b)->next);
            b = (*tl);
            if(0 == b) { (*tl) = a; return; }
        } 
    } while(true);
}

/* Combine all adjacent free addresses. 
 *
 * This is O(N*lg(N)) with the number of fragments. It uses an
 * in-place linked list merge sort of the fragments. 
 */
void wikrt_fl_coalesce(void* mem, wikrt_fl* fl) 
{
    wikrt_size const frag_count_init = fl->frag_count;
    wikrt_size const free_bytes_init = fl->free_bytes;

    // obtain the coalesced, address-sorted list of free nodes.
    wikrt_addr a = wikrt_flst_open(mem, wikrt_fl_flatten(mem, fl));
    wikrt_fl_mergesort(mem, &a, frag_count_init);

    // reset the free list.
    (*fl) = (wikrt_fl){0}; 

    // coalesce adjacent nodes and add back to our free-lists.
    while(0 != a) {
        wikrt_fb* const pa = wikrt_pfb(mem, a);

        // coalesce adjacent nodes
        while((a + pa->size) == pa->next) {
            wikrt_fb* const pn = wikrt_pfb(mem, pa->next);
            pa->size += pn->size;
            pa->next = pn->next;
        }
        wikrt_addr const a_next = pa->next;
        pa->next = a; // close singleton flst (circular)

        // recompute free list stats
        fl->free_bytes += pa->size;
        fl->frag_count += 1;

        wikrt_sc const sc = wikrt_size_class(pa->size);
        wikrt_flst* const l = fl->size_class + sc;

        // update free-list, adding new node to end of list.
        (*l) = wikrt_flst_join(mem, (*l), a);
        a = a_next;
    }
    
    // weak validation
    assert( (free_bytes_init == fl->free_bytes) &&
            (frag_count_init >= fl->frag_count) );

}

/** Combine two free-lists, moving nodes from 'src' into 'dst'.
 *
 * Performed in constant time via circular linked list joins. I'll favor
 * nodes from `dst` before nodes from `src` in the result, though this is
 * entirely arbitrary.
 *
 * The `src` list is zeroed during this process.
 */
void wikrt_fl_merge(void* const mem, wikrt_fl* const src, wikrt_fl* const dst)
{
    assert(src != dst);
    // a merge in approximately constant time
    dst->free_bytes += src->free_bytes;
    dst->frag_count += src->frag_count;
    src->free_bytes = 0;
    src->frag_count = 0;
    for(wikrt_sc sc = 0; sc < WIKRT_FLCT; ++sc) {
        wikrt_flst* const lsrc = src->size_class + sc;
        wikrt_flst* const ldst = dst->size_class + sc;
        (*ldst) = wikrt_flst_join(mem, (*ldst), (*lsrc));
        (*lsrc) = 0;
    }
}

#if 0
static void wikrt_fl_print(FILE* out, void* mem, wikrt_fl* fl)
{
    fprintf(out, "wikrt_fl: frags=%d, bytes=%d\n", (int)fl->frag_count, (int)fl->free_bytes);
    for(wikrt_sc sc = 0; sc < WIKRT_FLCT; ++sc) {
        wikrt_flst const l0 = fl->size_class[sc];
        if(0 == l0) { continue; }
        fprintf(out, "\t[%d]: ", (int)sc);
        wikrt_flst iter = l0;
        do {
            iter = wikrt_pfb(mem, iter)->next;
            fprintf(out, " %d(%d)", (int)iter, (int)wikrt_pfb(mem, iter)->size);
        } while(iter != l0);
        fprintf(out, "\n");
    }
}
#endif
 

/** attempt to acquire a quantity of space, accepting fragmented memory.
 *
 * On success, this may overshoot the request by most of one fragment. So
 * it shouldn't be unless we know there is no single fragment large enough
 * to fulfill the request. This favors larger fragments.
 */
static bool wikrt_move_frags(void* mem, wikrt_fl* src, wikrt_fl* dst, wikrt_size amt)
{
    assert(src != dst);
    wikrt_sc sc = WIKRT_FLCT;

    while(sc-- > 0) {
        wikrt_flst* const s = src->size_class + sc;
        wikrt_flst* const d = dst->size_class + sc;
        while(0 != (*s)) {
            wikrt_fb* const ps = wikrt_pfb(mem, (*s));
            wikrt_flst const f = ps->next;
            wikrt_fb* const pf = wikrt_pfb(mem, f);
            wikrt_size const sz = pf->size;

            // remove fragment from src
            if(ps == pf) { (*s) = 0; }
            else { ps->next = pf->next; }

            // add fragment to end of dst list
            pf->next = f;
            (*d) = wikrt_flst_join(mem, (*d), f);

            // track changes in statistics.            
            dst->free_bytes += sz;  
            dst->frag_count += 1;
            src->free_bytes -= sz;  
            src->frag_count -= 1;

            // manage allocation goals
            if(sz >= amt) { return true; }
            else { amt -= sz; }
        }
    }
    return false;
}

static inline bool wikrt_acquire_shm(wikrt_cx* cx, wikrt_sizeb sz) 
{
    // assuming cxm lock is held
    wikrt_addr block;
    if(wikrt_fl_alloc(cx->memory, &(cx->cxm->fl), sz, &block)) {
        wikrt_fl_free(cx->memory, &(cx->fl), sz, block);
        return true;
    }
    return false;
}

static void wikrt_acquire_shared_memory(wikrt_cx* cx, wikrt_size sz) 
{

    // I want a simple, predictable heuristic strategy that is very
    // fast for smaller computations (the majority of Wikilon ops).
    //
    // Current approach:
    // 
    // - allocate a single slab directly, if feasible.
    // - otherwise: merge, coalesce, retry once.
    // - final fallback: accept fragmented memory.
    //
    // The final case is near to 'thrashing'. Before we get this far,
    // we should probably also try to recover memory (e.g. stowage).
    //
    wikrt_cxm* const cxm = cx->cxm;
    void* const mem = cx->memory;
    wikrt_cxm_lock(cxm); {
        if(!wikrt_acquire_shm(cx, sz)) {
            wikrt_size const f0 = cxm->fl.frag_count;
            wikrt_fl_merge(mem, &(cx->fl), &(cxm->fl));
            wikrt_fl_coalesce(mem, &(cxm->fl));
            wikrt_size const ff = cxm->fl.frag_count;

            // track fragmentation of memory
            cx->fragmentation += ((ff < f0) ? 0 : (ff - f0));

            if(!wikrt_acquire_shm(cx, sz)) {
                wikrt_move_frags(mem, &(cxm->fl), &(cx->fl), sz);
            }
        }
    } wikrt_cxm_unlock(cxm);
}

static inline bool wikrt_alloc_local(wikrt_cx* cx, wikrt_sizeb sz, wikrt_addr* addr) 
{
    if(wikrt_fl_alloc(cx->memory, &(cx->fl), sz, addr)) {
        cx->ct_bytes_alloc += sz;
        return true;
    }
    return false;
}

/** Currently using a simple allocation strategy.
 *
 * I'll try to allocate locally, if feasible. Coalescing locally could
 * help in cases like copying stacks and vectors, where I perform slab
 * allocations, and otherwise doesn't hurt much (because our local free
 * list has no more than WIKRT_FREE_THRESH bytes.)
 *
 * If that doesn't work, we'll try to allocate from the shared space.
 */
bool wikrt_alloc(wikrt_cx* cx, wikrt_size sz, wikrt_addr* addr) 
{
    sz = WIKRT_CELLBUFF(sz);

    // allocate locally if feasible.
    if(cx->fl.free_bytes >= sz) {
        if(wikrt_alloc_local(cx, sz, addr)) { return true; }

        // coalesce and retry
        wikrt_fl_coalesce(cx->memory, &(cx->fl));
        if(wikrt_alloc_local(cx, sz, addr)) { return true; }
    }

    // otherwise try to use external memory resources.
    wikrt_acquire_shared_memory(cx, WIKRT_PAGEBUFF(sz));
    return wikrt_alloc_local(cx, sz, addr); 
}

/** Free locally. If we overflow, dump everything. */
void wikrt_free(wikrt_cx* cx, wikrt_size sz, wikrt_addr addr) 
{
    sz = WIKRT_CELLBUFF(sz);
    cx->ct_bytes_freed += sz;
    wikrt_fl_free(cx->memory, &(cx->fl), sz, addr);

    // Don't allow a context to 'own' too much unused space.
    if(WIKRT_FREE_THRESH < cx->fl.free_bytes) {
        wikrt_release_mem(cx);
    }

    // thoughts: it might be wortwhile to free larger blocks of memory
    // directly to our shared space. But I'm not sure what impact this
    // would have on fragmentation.
}

/* Release working memory back to the root. */
void wikrt_release_mem(wikrt_cx* cx) 
{
    // Estimate exposed memory fragmentation.
    wikrt_fl_coalesce(cx->memory, &(cx->fl)); 
    cx->fragmentation += cx->fl.frag_count;

    // Release memory fragments to the commons.
    wikrt_cxm* const cxm = cx->cxm;
    wikrt_cxm_lock(cxm); {
        wikrt_fl_merge(cxm->memory, &(cx->fl), &(cxm->fl));
    } wikrt_cxm_unlock(cxm);
}

bool wikrt_realloc(wikrt_cx* cx, wikrt_size sz0, wikrt_addr* addr, wikrt_size szf)
{
    sz0 = WIKRT_CELLBUFF(sz0);
    szf = WIKRT_CELLBUFF(szf);
    if(sz0 == szf) {
        // no buffered size change 
        return true; 
    } else if(szf < sz0) { 
        // free up a little space at the end of the buffer
        wikrt_free(cx, (sz0 - szf), ((*addr) + szf)); 
        return true; 
    } else {
        // As an optimization, in-place growth is unreliable and
        // unpredictable. So, Wikilon runtime doesn't bother. We'll
        // simply allocate, shallow-copy, and free the original.
        wikrt_addr const src = (*addr);
        wikrt_addr dst;
        if(!wikrt_alloc(cx, szf, &dst)) {
            return false;
        }
        void* const pdst = (void*) wikrt_pval(cx, dst);
        void const* const psrc = (void*) wikrt_pval(cx, src);
        memcpy(pdst, psrc, sz0);
        wikrt_free(cx, sz0, src);
        (*addr) = dst;
        return true;
    }
}

