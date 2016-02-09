
#include "wikrt.h"
#include <assert.h>

bool wikrt_alloc_b(wikrt_cx*, wikrt_fl*, wikrt_addr*, wikrt_size);
bool wikrt_alloc_ff(wikrt_cx*, wikrt_fl*, wikrt_addr*, wikrt_size);
bool wikrt_alloc_qf(wikrt_cx*, wikrt_fl*, wikrt_addr*, wikrt_size);

wikrt_sc wikrt_size_class_ff(wikrt_size const sz) {
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
 *
 * Note: With a separate multi-threaded pool, we may use some heuristics
 * to decide whether to push some free space back into the shared pool.
 */
void wikrt_free_b(wikrt_cx* const cx, wikrt_fl* const fl, wikrt_addr const v, wikrt_size const szb)
{
    // assume valid parameters at this layer
    wikrt_val* const pv = wikrt_pval(cx,v);
    wikrt_sc const sc = wikrt_size_class(szb);
    pv[0] = szb; 
    pv[1] = fl->size_class[sc];
    fl->size_class[sc] = v;
    fl->free_bytes += szb;
    fl->frag_count += 1;
    // TODO: if our free bytes are high, we might want to
    // heuristically defrag.
}

void wikrt_free(wikrt_cx* cx, wikrt_fl* fl, wikrt_addr v, wikrt_size sz)
{
    wikrt_free_b(cx, fl, v, WIKRT_CELLBUFF(sz));
}

/* allocate using a first-fit strategy from a given object size. just
 * checking every element of the free list and selecting the first fit.
 * When applied to the quick-fit sizes, we can guarantee that any match
 * is valid, so there is no need to search past the first item, but we
 * won't necessarily choose an appropriate size to reduce fragmentation.
 *
 * Fragmentation issues should be mitigated by the fact that Wikilon 
 * mostly uses very small value objects. However, we still want to 
 * avoid fragmentation due to cache-line and locality concerns (i.e.
 * filling gaps between large objects is unlikely to be cache optimal).
 */
bool wikrt_alloc_ff(wikrt_cx* const cx, wikrt_fl* const fl, wikrt_addr* const v, wikrt_size const szb) {
    wikrt_sc sc = wikrt_size_class(szb);
    do {
        wikrt_addr* p = fl->size_class + sc;
        while(0 != *p) {
            wikrt_val  const a    = (*p);
            wikrt_val* const pa   = wikrt_pval(cx,a);
            wikrt_size const sza  = pa[0];
            wikrt_addr* const pn  = (pa + 1); // next p
            if(sza >= szb) {
                // first-fit success, address 'a'.
                (*v) = a;
                fl->free_bytes -= sza;
                fl->frag_count -= 1;
                (*p) = (*pn); // remove fragment from list.
                if(sza > szb) {
                    // free remaining bytes from block
                    wikrt_free_b(cx,fl,(a + szb), (sza - szb));
                }
                return true;
            } else { 
                p = pn; // try next block in linked list
            }
        }
        sc = sc + 1; // try next size class
    } while(sc < WIKRT_FLCT);
    return false;
}

/* For small allocations, we'll simply double the allocation if we couldn't
 * find an exact match. This should reduce fragmentation. Large allocations
 * will use first-fit.
 */
bool wikrt_alloc_b(wikrt_cx* const cx, wikrt_fl* const fl, wikrt_addr* const v, wikrt_size const szb)
{
    if(szb <= WIKRT_QFSIZE) {
        wikrt_sc const sc = WIKRT_QFCLASS(szb);
        wikrt_val const r = fl->size_class[sc];
        if(0 != r) { 
            // optimal case, size matched
            (*v) = r;
            fl->size_class[sc] = wikrt_pval(cx,r)[1];
            fl->frag_count -= 1;
            fl->free_bytes -= szb;
            return true;
        } else if(wikrt_alloc_b(cx,fl,v, (szb << 1))) {
            // double sized alloc, then free latter half
            wikrt_free_b(cx, fl, (*v)+szb, szb);
            return true;
        } else {
            // fall back to global first-fit
            return wikrt_alloc_ff(cx, fl, v, szb);
        }
    } else if(wikrt_alloc_ff(cx, fl, v, szb)) {
        // basic first-fit for larger allocations
        return true;
    } else if(wikrt_coalesce_maybe(cx, fl, szb)) {
        // retry after coalescing data
        return wikrt_alloc_b(cx, fl, v, szb);
    } else {
        return false;
    }
}

bool wikrt_alloc(wikrt_cx* cx, wikrt_fl* fl, wikrt_addr* v, wikrt_size sz)
{
    // to avoid recursively re-aligning target size...
    return wikrt_alloc_b(cx,fl,v, WIKRT_CELLBUFF(sz));
}

bool wikrt_coalesce_maybe(wikrt_cx* const cx, wikrt_fl* const fl, wikrt_size sz) {
    if((fl->free_bytes < (2 * sz)) || (fl->frag_count == fl->frag_count_df))
        return false;
    wikrt_size const fc0 = fl->frag_count;
    wikrt_coalesce(cx,fl);
    return (fc0 != fl->frag_count);
}

// join segregated free-list nodes into a single list
wikrt_addr wikrt_fl_flatten(wikrt_cx* cx, wikrt_fl* fl) {
    wikrt_addr r = fl->size_class[0];
    for(wikrt_sc sc = 1; sc < WIKRT_FLCT; ++sc) {
        wikrt_addr* tl = fl->size_class + sc;
        while(0 != (*tl)) { 
            // each element in free-list is (size,next) pair
            wikrt_val* const pv = wikrt_pval(cx,*tl);
            tl = &(pv[1]);
        }
        (*tl) = r;
        r = fl->size_class[sc];
    }
    return r;
}

void wikrt_fl_split(wikrt_cx* const cx, wikrt_addr const hd, wikrt_addr* const a, wikrt_size const sza, wikrt_addr* const b) 
{
    // I assume size is valid for list, that we won't reach a null address.
    (*a) = hd;
    wikrt_addr* tl = a;
    wikrt_size ct = 0;
    while(ct < sza) {
        tl = wikrt_pval(cx, (*tl)) + 1;
        ct = ct + 1;
    }
    // at this point 'tl' points to the location of the split.
    (*b) = (*tl); // split remainder of list into 'b'.
    (*tl) = 0;    // 'a' now terminates where 'b' starts.
}

// after we flatten our free-list, we'll perform a merge-sort.
// merge-sort is an easy sort for singly linked lists, and it
// requires at worst some constant amount of space for a context
// limited to 32-bits.
//
// This is just a normal merge-sort. The coalescing of blocks
// waits for wikrt_coalesce.
void wikrt_fl_mergesort(wikrt_cx* const cx, wikrt_addr* const hd, wikrt_size const count)
{
    // base case: any list of size zero or one is sorted
    if(count < 2) { return; }

    wikrt_size const sza = count / 2;
    wikrt_size const szb = count - sza;
    wikrt_addr a, b;

    // split list in two and sort each half
    wikrt_fl_split(cx, *hd, &a, sza, &b);
    wikrt_fl_mergesort(cx, &a, sza);
    wikrt_fl_mergesort(cx, &b, szb);

    // merge the two lists with lowest addresses at head
    wikrt_addr* tl = hd;
    while ((0 != a) && (0 != b)) {
        if(a < b) {
            (*tl) = a;
            tl = wikrt_pval(cx, a) + 1;
            a = (*tl); 
        } else {
            (*tl) = b;
            tl = wikrt_pval(cx, b) + 1;
            b = (*tl);
        }
    }
    (*tl) = (0 != a) ? a : b;
}

// combine fragments of free lists
void wikrt_coalesce(wikrt_cx* cx, wikrt_fl* fl)
{
    // obtain an address-sorted list of all nodes
    wikrt_size const flsz = fl->frag_count;
    wikrt_addr lst = wikrt_fl_flatten(cx,fl);
    wikrt_fl_mergesort(cx, &lst, flsz);

    // reset old free list
    (*fl) = (wikrt_fl){0}; 

    // free each node back, coalescing as we go.
    while(0 != lst) {
        wikrt_val* const pv = wikrt_pval(cx,lst);
        wikrt_size szb = pv[0];
        wikrt_addr nxt = pv[1];
        // coalesce adjacent nodes from sorted list
        while((lst + szb) == nxt) {
            wikrt_val* const pn = wikrt_pval(cx,nxt);
            szb = szb + pn[0];
            nxt = pn[1];
        }
        wikrt_free_b(cx, fl, lst, szb);
        lst = nxt;
    }

    // data for heuristic coalesce
    fl->frag_count_df = fl->frag_count;

    // THOUGHTS:
    // This implementation of 'coalesce' isn't optimal if we coalesce
    // into fragment sizes that we do not use. I think this should
    // not be a major issue in practice, though. And it could be
    // handled, eventually, by 
}

