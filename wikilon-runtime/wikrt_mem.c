
#include "wikrt.h"

#if 0
typedef struct wikrt_freelist {
    wikrt_val size_class[WIKRT_FLCT]; // (size, next) linked lists, size-segregated
} wikrt_freelist;

bool wikrt_alloc(wikrt_cx*, wikrt_freelist*, wikrt_val*, wikrt_size);
void wikrt_free(wikrt_cx*, wikrt_freelist*, wikrt_val, wikrt_size);
#endif

inline int wikrt_size_class(wikrt_size sz) {
    // size classes; for now, just using fibonacci sequence
    wikrt_size szp = 0;
    wikrt_size szl = (wikrt_size) (2 * sizeof(wikrt_val));
    int sc = 0;
    wikrt_size szt;
    do {
        szt = szp + szl;
        if(szt >= sz) return sc;
        szp = szl;
        szl = szt;
        sc = sc + 1;
    } while(sc < WIKRT_FLCT);
    return (WIKRT_FLCT - 1);
}


bool wikrt_alloc(wikrt_cx* cx, wikrt_freelist* fl, wikrt_val* v, wikrt_size sz)
{
    // we need to do some scary pointer casting...

#if 0
    wikrt_val* const mc = (void*) cx->memory;
    int const sc = wikrt_size_class(sz);
    wikrt_val* p = &(fl->size_class[sc]);
    wikrt_val  n = *p;
#endif

    
    return false;
}
void wikrt_free(wikrt_cx* cx, wikrt_freelist* fl, wikrt_val v, wikrt_size sz)
{
    
}

