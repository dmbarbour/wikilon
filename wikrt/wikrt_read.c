/** Reading Binaries
 *
 * Ideally, reading for large binaries is incremental. This is achieved
 * by translating large values into compositions of smaller values and
 * converting only the head of the composition list to a binary for the
 * read operation, repeating until the binary is full.
 *
 * For large value stowage, I'll need to similarly write code into a 
 * binary format, so it's worth considering how to refactor this. But
 * for now, I need to just make it work.
 */
#include "wikrt_private.h"
#include <stdlib.h>
#include <stdio.h>
#include <assert.h>


size_t wikrt_read(wikrt_cx* cx, wikrt_r r, uint8_t* const buff, size_t const max)
{
    size_t const step_prealloc = 32 * 1024; // a reasonable write buffer
    uint8_t* iter = buff;
    uint8_t* const end = buff + max;
    int e = 0;

    wikrt_api_enter(cx);
    do {
        // ensure sufficient memory for lightweight operations
        if(!wikrt_api_prealloc(cx, 1, step_prealloc)) { e = ENOMEM; break; }
        wikrt_v v = wikrt_reg_get(cx,r);

        // trivial case: empty register
        if(0 == v) { e = 0; break; }

        // normalize input to composition
        if(WIKRT_COMP != wikrt_vtype(v0)) {
            v = WIKRT_COMP | wikrt_thread_alloc_cell(&(cx->memory), v, 0);
            wikrt_reg_set(cx, v);
        }

        // now we certainly have a composition, so try to process it.
        wikrt_v const hd = wikrt_v2p(v)[0];
        switch(wikrt_vtype(hd)) {
        case WIKRT_OBJ: { switch(wikrt_o_type(hd)) {
          }} break;
        case WIKRT_COMP: {
          } break;
        case WIKRT_SMALL: {
          } break;
        case WIKRT_CONS: {
          } break;
        default: {
            // major error
            fprintf(stderr, "%s: unrecognized value type (%d)\n", __FUNCTION__, (int)wikrt_vtype(hd));
            abort();
          }
        }
    } while(1);
    wikrt_api_exit(cx);
    errno = e;
    return (iter - buff);
}

