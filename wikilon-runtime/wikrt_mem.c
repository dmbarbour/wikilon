
#include "wikrt.h"
#include <string.h>
#include <assert.h>

typedef int wikrt_sc;

typedef struct wikrt_fb {
    wikrt_size size;
    wikrt_addr next;
} wikrt_fb;

static inline wikrt_fb* wikrt_pfb(void* mem, wikrt_addr a) {
    return (wikrt_fb*)(a + ((char*)mem));
}

wikrt_sc wikrt_size_class_ff(wikrt_size const sz) {
    _Static_assert(WIKRT_FLCT_FF > 0, "code assumes WIKRT_FLCT_FF > 0");
    int sc = (WIKRT_FLCT - 1);
    wikrt_size szt = WIKRT_FFMAX;
    while(szt >= sz) {
        szt = szt >> 1;
        sc = sc - 1;
    }
    return sc;
}


static inline wikrt_sc wikrt_size_class(wikrt_size const sz) {
    return (sz <= WIKRT_QFSIZE) 
         ? (wikrt_sc) WIKRT_QFCLASS(sz) 
         : wikrt_size_class_ff(sz);
}


/* Adding to head of free-list. No coalescing adjacent memory. */
void wikrt_fl_free(void* mem, wikrt_fl* fl, wikrt_sizeb sz, wikrt_addr addr) 
{
    wikrt_fb* const pfb = wikrt_pfb(mem, addr);
    wikrt_sc const sc = wikrt_size_class(sz);
    wikrt_flst* const l = fl->size_class + sc;
    
    pfb->size = sz;
    pfb->next = l->head;
    if(0 == l->head) { l->tail = addr; }
    l->head = addr;

    fl->free_bytes += sz;
    fl->frag_count += 1;
}

bool wikrt_fl_alloc_ff(void* mem, wikrt_fl* fl, wikrt_sizeb sz, wikrt_addr* addr);

/* For small allocations, we'll simply double the allocation if we couldn't
 * find an exact match. This should reduce fragmentation. Large allocations
 * will use first-fit. We'll always fall back on first-fit for completeness.
 *
 * Note: We do not coalesce automatically. Any decision to coalesce memory
 * will be deferred to our caller.
 */
bool wikrt_fl_alloc(void* mem, wikrt_fl* fl, wikrt_sizeb sz, wikrt_addr* addr)
{
    _Static_assert(WIKRT_CELLSIZE == sizeof(wikrt_fb), "free-block should match minimum allocation");
    if(sz <= WIKRT_QFSIZE) {
        wikrt_flst* const l = fl->size_class + WIKRT_QFCLASS(sz);
        if(0 != l->head) {
            (*addr) = l->head;
            l->head = wikrt_pfb(mem, l->head)->next;
            fl->frag_count -= 1;
            fl->free_bytes -= sz;
            return true;
        } else if(wikrt_fl_alloc(mem, fl, (sz << 1), addr)) {
            // double sized allocation, free half
            wikrt_fl_free(mem, fl, sz, ((*addr) + sz));
            return true;
        } 
    }
    // fallback on first-fit
    return wikrt_fl_alloc_ff(mem, fl, sz, addr);
}

/* allocate using a first-fit strategy. */
bool wikrt_fl_alloc_ff(void* mem, wikrt_fl* fl, wikrt_sizeb sz, wikrt_addr* addr) {
    wikrt_sc sc = wikrt_size_class(sz);
    do {
        wikrt_flst* const l = fl->size_class + sc;
        wikrt_addr prior = 0; // to repair 'l->tail'
        wikrt_addr* pa = &(l->head);
        while(0 != (*pa)) {
            wikrt_val const a = (*pa);
            wikrt_fb* const pfb = wikrt_pfb(mem, a);
            if(pfb->size >= sz) {
                (*addr) = a;
                (*pa) = pfb->next;
                if(a == l->tail) { l->tail = prior; }
                fl->free_bytes -= pfb->size;
                fl->frag_count -= 1;
                if(pfb->size > sz) { 
                    wikrt_fl_free(mem, fl, (pfb->size - sz), (a + sz));
                }
                return true;
            } else { pa = &(pfb->next); }
        }
        sc = sc + 1;
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

// join segregated free-list nodes into a single list
// this takes roughly constant time via tail pointers
wikrt_addr wikrt_fl_flatten(void* const mem, wikrt_fl* const fl) {
    wikrt_addr r = 0;
    for(wikrt_sc sc = 0; sc < WIKRT_FLCT; ++sc) {
        wikrt_flst* const l = fl->size_class + sc;
        if(0 != l->head) {
            wikrt_pfb(mem, l->tail)->next = r;
            r = l->head;
        }
    }
    return r;
}

void wikrt_fl_split(void* const mem, wikrt_addr const hd, wikrt_addr* const a, wikrt_size const sza, wikrt_addr* const b) 
{
    // I assume sza is valid (no larger than the list)
    (*a) = hd;
    wikrt_addr* tl = a;
    wikrt_size ct = 0;
    while(ct != sza) {
        ct = ct + 1;
        tl = &(wikrt_pfb(mem, (*tl))->next);
    }
    // at this point 'tl' points to the location of the split.
    (*b) = (*tl); // split remainder of list into 'b'.
    (*tl) = 0;    // 'a' now terminates where 'b' starts.
}

/* After we flatten our free-list, we perform a merge-sort by address.
 *
 * The output is an address-ordered permutatation of the input list, no
 * coalescing is performed. The sort itself uses in-place mutation. 
 *
 * The smallest free address will be placed at the head of the list.
 */
void wikrt_fl_mergesort(void* const mem, wikrt_addr* const hd, wikrt_size const count)
{
    // base case: any list of size zero or one is sorted
    if(count < 2) { return; }

    wikrt_size const sza = count / 2;
    wikrt_size const szb = count - sza;
    wikrt_addr a, b;

    // split list in two and sort each half
    wikrt_fl_split(mem, *hd, &a, sza, &b);
    wikrt_fl_mergesort(mem, &a, sza);
    wikrt_fl_mergesort(mem, &b, szb);

    wikrt_addr* tl = hd;
    while ((a != 0) && (b != 0)) {
        if(a < b) {
            (*tl) = a;
            tl = &(wikrt_pfb(mem, a)->next);
            a = (*tl); 
        } else {
            (*tl) = b;
            tl = &(wikrt_pfb(mem, b)->next);
            b = (*tl);
        } 
    }
    (*tl) = (a != 0) ? a : b;
}


/* Combine all adjacent free addresses. */
void wikrt_fl_coalesce(void* mem, wikrt_fl* fl) 
{
    wikrt_size const frag_count_init = fl->frag_count;
    wikrt_size const free_bytes_init = fl->free_bytes;

    // obtain an address-sorted list of all nodes
    wikrt_addr a = wikrt_fl_flatten(mem, fl);
    wikrt_fl_mergesort(mem, &a, frag_count_init);

    // zero the free lists
    (*fl) = (wikrt_fl){0}; 

    // add each element of lst to our free-list
    // adding to tail, in this case, to preserve address-order.
    while(0 != a) {
        wikrt_fb* const pa = wikrt_pfb(mem, a);

        // coalesce adjacent nodes
        while((a + pa->size) == pa->next) {
            wikrt_fb* const pn = wikrt_pfb(mem, pa->next);
            pa->size += pn->size;
            pa->next = pn->next;
        }

        // recomputing stats from scratch
        fl->free_bytes += pa->size;
        fl->frag_count += 1;

        wikrt_sc const sc = wikrt_size_class(pa->size);
        wikrt_addr const next = pa->next;
        pa->next = 0; // new end of list

        wikrt_flst* const l = fl->size_class + sc;
        if(0 == l->head) { l->head = a; }
        else { wikrt_pfb(mem, l->tail)->next = a; }
        l->tail = a;

        a = next;
    }
    
    // weak validation
    assert( (free_bytes_init == fl->free_bytes) &&
            (frag_count_init >= fl->frag_count) );

}

static inline void wikrt_flst_merge(void* mem, wikrt_flst* src, wikrt_flst* dst) 
{
    // prepending src onto dst
    if(0 != src->head) {
        if(0 == dst->head) { dst->tail = src->tail; }
        else { wikrt_pfb(mem, src->tail)->next = dst->head; }
        dst->head = src->head;
    }
}

void wikrt_fl_merge(void* mem, wikrt_fl* src, wikrt_fl* dst)
{
    // a merge in approximately constant time
    dst->free_bytes += src->free_bytes;
    dst->frag_count += src->frag_count;
    for(wikrt_sc sc = 0; sc < WIKRT_FLCT; ++sc) {
        wikrt_flst_merge(mem, src->size_class + sc, dst->size_class + sc);
    }
}


