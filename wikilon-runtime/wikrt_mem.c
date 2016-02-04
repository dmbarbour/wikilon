
#include "wikrt.h"
#include <assert.h>

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

// coalescing is deferred; wikrt_free is O(1)
void wikrt_free(wikrt_cx* const cx, wikrt_fl* const fl, wikrt_addr const v, wikrt_size const sz)
{
    // assume valid parameters at this layer
    wikrt_size const szb = WIKRT_CELLBUFF(sz);
    wikrt_val* const pv = wikrt_pval(cx,v);
    pv[0] = szb;
    wikrt_sc const sc = wikrt_size_class(szb);
    pv[1] = fl->size_class[sc];
    fl->size_class[sc] = v;
    fl->free_bytes += szb;
    fl->frag_count += 1;
}

// allocate specifically from the first-fit free-lists
bool wikrt_alloc_ff(wikrt_cx* const cx, wikrt_fl* const fl, wikrt_addr* const v, wikrt_size const sz) {
    wikrt_sc sc = wikrt_size_class_ff(sz);
    do {
        wikrt_addr* p = fl->size_class + sc;
        while(0 != *p) {
            wikrt_val  const a    = (*p);
            wikrt_val* const pa   = wikrt_pval(cx,a);
            wikrt_size const sza  = pa[0];
            wikrt_addr* const pn  = (pa + 1); // next p
            if(sza >= sz) {
                // first-fit success, address 'a'.
                (*v) = a;
                fl->free_bytes -= sza;
                fl->frag_count -= 1;
                (*p) = (*pn); // remove fragment from list.
                if(sza > sz) {
                    // free remaining bytes from block
                    wikrt_free(cx,fl,(a + sz), (sza - sz));
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

bool wikrt_alloc(wikrt_cx* const cx, wikrt_fl* const fl, wikrt_addr* const v, wikrt_size const sz)
{
    wikrt_size const szb = WIKRT_CELLBUFF(sz);
    if(szb <= WIKRT_QFSIZE) {
        wikrt_sc const sc = WIKRT_QFCLASS(sz);
        wikrt_val const r = fl->size_class[sc];
        if(0 != r) { 
            // optimal case, size matched
            (*v) = r;
            fl->size_class[sc] = wikrt_pval(cx,r)[1];
            fl->frag_count -= 1;
            fl->free_bytes -= szb;
            return true;
        } else if(wikrt_alloc(cx,fl,v, (szb << 1))) {
            // double sized alloc, then free latter half
            wikrt_free(cx,fl,(*v)+szb, szb);
            return true;
        } else {
            return false;
        }
    } else if(wikrt_alloc_ff(cx,fl,v,szb)) {
        // basic first-fit succeeded
        return true;
    } else {
        // TODO: heuristically decide whether to coalesce & retry
        //   might need to track 'fragments after last coalesce' 
        //   for the heuristic decision
        // TODO (multi-threaded): alloc from context root if necessary
        return false;
    }
}

