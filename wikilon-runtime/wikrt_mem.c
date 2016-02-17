
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


/* coalescing is deferred; wikrt_free is O(1) in the normal case and
 * only touches the free list and the head of the deleted object.
 */
void wikrt_fl_free(void* mem, wikrt_fl* fl, wikrt_sizeb sz, wikrt_addr addr) 
{
    wikrt_fb* const pfb = wikrt_pfb(mem, addr);
    wikrt_sc const sc = wikrt_size_class(sz);
    pfb->size = sz;
    pfb->next = fl->size_class[sc];
    fl->size_class[sc] = addr;
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
        wikrt_sc const sc = WIKRT_QFCLASS(sz);
        wikrt_addr const a = fl->size_class[sc];
        if(0 != a) {
            (*addr) = a;
            fl->size_class[sc] = wikrt_pfb(mem, a)->next;
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
        wikrt_addr* pa = fl->size_class + sc;
        while(0 != *pa) {
            wikrt_val  const a    = (*pa);
            wikrt_fb*  const pfb  = wikrt_pfb(mem, a);
            if(pfb->size >= sz) {
                (*addr) = a;
                (*pa) = pfb->next;
                fl->free_bytes -= pfb->size;
                fl->frag_count -= 1;
                if(pfb->size > sz) {
                    wikrt_fl_free(mem, fl, (a + sz), (pfb->size - sz));
                }
                return true;
            } else {
                pa = &(pfb->next);
            }
        }
        sc = sc + 1;
    } while(sc < WIKRT_FLCT);
    return false;
}

bool wikrt_fl_grow_inplace(void* mem, wikrt_sizeb memsz, 
        wikrt_fl* fl, wikrt_sizeb sz0, wikrt_addr a, wikrt_sizeb szf) 
{
    wikrt_addr const tgt = (a + sz0);
    if(tgt >= memsz) { 
        // fail fast: cannot grow past end of memory
        return false; 
    }

    wikrt_fb const tgtfb = *(wikrt_pfb(mem, tgt));
    wikrt_sizeb const growsz = (szf - sz0);
    if( (tgtfb.size != WIKRT_CELLBUFF(tgtfb.size)) || 
        (tgtfb.next != wikrt_vaddr(tgtfb.next))    ||
        (tgtfb.size < growsz)                      )
    {
        // fail fast, tgt is invalid free-block or too small
        return false; 
    }
    
    // at this point, the potential to grow in-place exists.
    wikrt_sc const sc = wikrt_size_class(tgtfb.size);
    wikrt_addr* pa = fl->size_class + sc;
    while((0 != *pa) && (tgt != *pa)) { 
        pa = &(wikrt_pfb(mem, *pa)->next); 
    }

    if(0 == (*pa)) { 
        // tgt not found in free-list
        return false;
    }

    (*pa) = tgtfb.next; // remove target from free-list
    fl->frag_count -= 1;
    fl->free_bytes -= tgtfb.size;
    if(tgtfb.size > growsz) {
        wikrt_fl_free(mem, fl, (tgtfb.size - growsz), (tgt + growsz));
    }
    return true;
}


// join segregated free-list nodes into a single list
wikrt_addr wikrt_fl_flatten(void* const mem, wikrt_fl* const fl) {
    wikrt_addr r = fl->size_class[0];
    for(wikrt_sc sc = 1; sc < WIKRT_FLCT; ++sc) {
        wikrt_addr* tl = fl->size_class + sc;
        while(0 != (*tl)) { tl = &(wikrt_pfb(mem, (*tl))->next); }
        (*tl) = r;              // addend prior free-list
        r = fl->size_class[sc]; // take new head
    }
    return r;
}

void wikrt_fl_split(void* const mem, wikrt_addr const hd, wikrt_addr* const a, wikrt_size const sza, wikrt_addr* const b) 
{
    // I assume sza is valid
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
    wikrt_addr lst = wikrt_fl_flatten(mem, fl);
    wikrt_fl_mergesort(mem, &lst, frag_count_init);

    // zero the free lists
    (*fl) = (wikrt_fl){0}; 

    // to preserve address-order, we'll addend to tail of free list
    wikrt_fltl fltl;
    for(wikrt_sc sc = 0; sc < WIKRT_FLCT; ++sc) {
        fltl.size_class[sc] = fl->size_class + sc;
    }

    while(0 != lst) {
        wikrt_fb* const pl = wikrt_pfb(mem, lst);

        // coalesce adjacent nodes
        while((lst + pl->size) == pl->next) {
            wikrt_fb* const pn = wikrt_pfb(mem, pl->next);
            pl->size += pn->size;
            pl->next = pn->next;
        }

        // insert at tail of list.
        wikrt_sc const sc = wikrt_size_class(pl->size);
        *(fltl.size_class[sc]) = lst;
        lst = pl->next;
        pl->next = 0;
        fltl.size_class[sc] = &(pl->next); // new tail

        fl->free_bytes += pl->size;
        fl->frag_count += 1;
    }
    
    // weak validation
    assert( (free_bytes_init == fl->free_bytes) &&
            (frag_count_init >= fl->frag_count) );

}

void wikrt_fl_tails(void* mem, wikrt_fl* fl, wikrt_fltl* fltl)
{
    for(wikrt_sc sc = 0; sc < WIKRT_FLCT; ++sc) {
        wikrt_addr* tl = fl->size_class + sc;
        while(0 != (*tl)) { tl = &(wikrt_pfb(mem, (*tl))->next); }
        fltl->size_class[sc] = tl;
    }
}
void wikrt_fl_merge(wikrt_fl* srcHead, wikrt_fltl* srcTail, wikrt_fl* dst)
{
    dst->free_bytes += srcHead->free_bytes;
    dst->frag_count += srcHead->frag_count;
    for(wikrt_sc sc = 0; sc < WIKRT_FLCT; ++sc) {
        *(srcTail->size_class[sc]) = dst->size_class[sc];
        dst->size_class[sc] = srcHead->size_class[sc];
    }
}


