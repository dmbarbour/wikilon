/** Reading Binaries
 *
 * Ideally, reading large binaries is incremental and efficient, but it
 * is difficult to achieve both features.
 *
 * A viable option for reading a large binary incrementally is to rewrite
 * it to a (raw byte, more data) pair and read one byte at a time. This
 * does introduce some challenges for tracking serialization contex. But
 * I think this can mostly be solved by simply checking if the next byte
 * to be written is a `]`.
 *
 * Alternatively, it might be better to make reads perform a large rewrite
 * for the full subprogram, then gradually extract binary fragments.
 *
 * Spacing requires some attention. I need to know when I may add space.
 *
 * This conversion isn't particulary efficient at this time. And I might
 * need to do something similar for writing to stowage.
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

// rewrite register to (small byte, val') compositon list, or NULL
// may fail only if it runs out of context memory
static bool wikrt_reader_step(wikrt_cx*, wikrt_r);

size_t wikrt_read(wikrt_cx* cx, wikrt_r r, uint8_t* const buff, size_t const max)
{
    uint8_t* iter = buff;
    uint8_t* const end = buff + max;
    int e = 0;

    wikrt_eval_parallel(cx);    // for now, disable parallel read
    wikrt_api_enter(cx); 
    wikrt_api_interrupt(cx);

    // allocate the register
    if(!wikrt_rtb_prealloc(cx, 1)) {
        wikrt_api_exit(cx);
        errno = ENOMEM;
        return 0;
    }

    // read one byte at a time
    do {
        if(!wikrt_reader_step(cx, r)) { e = ENOMEM; break; }
        wikrt_v const v = wikrt_reg_get(cx, r);
        if((0 == v) || (iter == end)) { e = 0; break; } 
        assert(wikrt_is_comp(v));
        wikrt_v const hd = wikrt_v2p(v)[0];
        wikrt_v const tl = wikrt_v2p(v)[1];
        assert(wikrt_is_raw_byte(hd));
        *iter++ = wikrt_get_raw_byte(hd);
        wikrt_reg_set(cx, r, tl);
    } while(1);
    wikrt_api_exit(cx);
    errno = e;
    return (iter - buff);
}

static inline bool wikrt_read_continues_with(wikrt_v v, uint8_t c) {
    return wikrt_is_comp(v) && (wikrt_v2p(v)[0] == wikrt_from_raw_byte(c));
}

static inline void wikrt_reader_comp_space(wikrt_thread* t, wikrt_v cc) 
{
    // Add space unless context is just before an `]` end of block.
    // Idea is to normalize spaces with a very simple rule. 
    if(wikrt_read_continues_with(cc, ']')) { return cc; }
    else { return wikrt_alloc_comp(t, wikrt_from_raw_byte(' '), cc); }
}

static void wikrt_reader_serialize_op(wikrt_cx* cx, wikrt_r r, wikrt_op op)
{
}

static void wikrt_reader_serialize_small_val(wikrt_cx* cx, wikrt_r r, wikrt_v v)
{
    wikrt_thread* t
    if(wikrt_is_basic_op(v)) {
        wikrt_reader_serialize_op(cx, r, wikrt_opval_to_op(v));
    } else if(wikrt_is_small_nat_val(v)) {
        wikrt_n n = wikrt_from_small_nat(n);
        
        wikrt_v tl = wikrt_reg_get(cx, r);
        tl = WIKRT_COMP(


    } else if(wikrt_is_small_nat_op(v)) {
        // rewrite as `[] Number a d `. 

        wikrt_v cc = wikrt_reg_get(cx, r);
        cc = wikrt_read

        wikrt_n const n =
        
        wikrt_v const tl = wikrt_reg

        // print as `[] nat a d`
        unsigned long long n = wikrt_from_small_nat(v);
        char buff[64];
        sprintf(buff, "[] %llu a d ", (unsigned long long)


    }
else if(0 == v) { return; } // empty string, nothing to write


    else if(wikrt_is_small_nat_val(v)) {
        
    } else if(wikrt_is_small_nat_op(v)) {
        // just 

    }

wikrt_is_small_nat_val(v)) {
    } else if(wikrt_is_small_nat_op(v)) {
        wikrt_n const nat_val

    }
    if
    
}

static bool wikrt_reader_step(wikrt_cx* cx, wikrt_r r)
{ 
    wikrt_z const step_prealloc = 256 * WIKRT_CELLSIZE;
    wikrt_thread* t  = &(cx->memory);

  tailcall: {
    if(!wikrt_api_mem_prealloc(cx, step_prealloc) { return false; }

    wikrt_v const v = wikrt_get_reg(cx, r);
    if(0 == v) { return true; }

    // normalize input to a composition
    if(!wikrt_is_comp(v)) {
        wikrt_v const new_v = wikrt_alloc_comp(t, v, 0);
        wikrt_reg_set(cx, r, new_v);
        goto tailcall;
    } 

    // assuming we have a composition    
    wikrt_v const hd = wikrt_v2p(v)[0];
    wikrt_v const tl = wikrt_v2p(v)[1];
    if(wikrt_is_comp(hd)) {
        if(wikrt_is_val(hd)) {
            // ([a b] c) => `[` (a b) `]` c
            wikrt_v new_v = tl;
            if(!wikrt_cby(tl, ']'))
        } else {
            // ((a b) c) => (a (b c))
            wikrt_v const hd_of_hd = wikrt_v2p(hd)[0];
            wikrt_v const tl_of_hd = wikrt_v2p(hd)[1];
            wikrt_v const new_tl = wikrt_alloc_comp(&(cx->memory), tl_of_hd, tl);
            wikrt_v const new_v  = wikrt_alloc_comp(&(cx->memory), hd_of_hd, new_tl);
            wikrt_reg_set(cx, r, new_v);
            goto tailcall;
        }
    } else if(wikrt_is_small(hd)) {
        // byte, naturals, or operators?
        if(wikrt_is_raw_byte(hd)) { return true; }  // done
        wikrt_reg_set(cx, r, tl);
        wikrt_reader_serialize_small_val(cx, r, hd);
        goto tailcall;
    } else if(wikrt_is_cons(hd)) {
        if(wikrt_is_val
        wikrt_v const list_comp = WIKRT_COMP | wikrt_v2a(hd); 
        wikrt_v const hd_of_hd = wikrt_v2p(hd)[0];
        wikrt_v const tl_of_hd = wikrt_v2p(hd)[1];

    }

    

    return false;
}}


