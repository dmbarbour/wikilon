/** Reading Binaries
 *
 * Ideally, reading large binaries is incremental and efficient, but it
 * is difficult to achieve both features. For now, it's just incremental.
 *
 * A viable option for reading a large binary incrementally is to rewrite
 * it to a (raw byte, more data) pair and read one byte at a time. 
 * 
 * This does introduce some challenges for tracking serialization context.
 * I currently just peek to avoid adding spaces just before `]` in program 
 * output. For texts, I use dedicated object types during serialization,
 * treating a partially serialized text as a raw binary variant.
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

    // allocate the register for read/write
    if(!wikrt_api_prealloc(cx, 1, WIKRT_CELLSIZE * 1024)) {
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

static inline wikrt_v wikrt_reader_push_byte(wikrt_thread* t, uint8_t chr, wikrt_v cc)
{
    return wikrt_alloc_comp(t, wikrt_from_raw_byte(chr), cc);
}

static inline wikrt_v wikrt_reader_push_bytes(wikrt_thread* t, uint8_t const* data, size_t amt, wikrt_v cc)
{
    while(amt > 0) { cc = wikrt_reader_push_byte(t, data[--amt], cc); }
    return cc;
}

static inline wikrt_v wikrt_push_cstring(wikrt_thread* t, char const* s, wikrt_v cc)
{
    _Static_assert((sizeof(char) == sizeof(uint8_t)), "casting char* to uint8*");
    return wikrt_reader_push_bytes(t, (uint8_t const*)s, strlen(s), cc);
}

static wikrt_v wikrt_reader_push_space(wikrt_thread* t, wikrt_v cc) 
{
    // Add space unless context is just before an `]` end of block.
    // Idea is to normalize spaces with simple and predictable rules. 
    bool const at_end_of_block = wikrt_is_comp(cc) 
                   && (wikrt_from_raw_byte(']') == wikrt_v2p(cc)[0]);
    return at_end_of_block ? cc 
         : wikrt_reader_push_byte(t, ' ', cc);
}


static wikrt_v wikrt_reader_inline_v(wikrt_thread* t, wikrt_v v, wikrt_v cc)
{
    // v →  [] v a d
    // cost: 8 cells
    cc = wikrt_reader_push_space(t,cc);
    cc = wikrt_reader_push_cstring(t, "a d", cc);
    cc = wikrt_alloc_comp(t, v, cc);
    cc = wikrt_reader_push_cstring(t, "[] ", cc);
    return cc;
}

static wikrt_v wikrt_reader_wrap_block_v(wikrt_thread* t, wikrt_v v, wikrt_v cc)
{
    // v -> [v]
    // cost: 4 cells
    cc = wikrt_reader_push_space(t, cc);
    cc = wikrt_reader_push_byte(t, ']', cc);
    cc = wikrt_alloc_comp(t, v, cc);
    cc = wikrt_reader_push_byte(t, '[', cc);
    return cc;
}

static inline wikrt_v wikrt_reader_push_op(wikrt_thread* t, wikrt_op op, wikrt_v cc)
{
    if(0 == op) { return cc; }
    cc = wikrt_reader_push_space(t, cc);

    // todo: push operator

}

static inline wikrt_v wikrt_reader_push_nat(wikrt_thread* t, wikrt_n n, wikrt_v cc)
{
    // no more than one cell per digit, plus the space 
    //  result is under 20 cells on a 64-bit system
    cc = wikrt_reader_push_space(t, cc);
    do {
        cc = wikrt_reader_push_byte(t, ('0' + (n % 10)), cc);
        n = n / 10;
    } while(0 != n);
    return cc;
}

static wikrt_v wikrt_reader_push_small(wikrt_thread* t, wikrt_v v, wikrt_v cc)
{
    // ASSUMES sufficient space (~32 cells at most)
    if(wikrt_is_basic_op(v)) {
        return wikrt_reader_serialize_op(t, wikrt_opval_to_op(v), cc);
    } else if(wikrt_is_basic_op(wikrt_value_to_action(v))) {
        return wikrt_reader_wrap_block_v(t, wikrt_value_to_action(v), cc);
    } else if(wikrt_is_small_nat_val(v)) {
        return wikrt_reader_push_nat(t, wikrt_from_small_nat(v), cc);
    } else if(wikrt_is_small_nat_op(v)) {
        return wikrt_reader_inline_v(t, wikrt_action_to_value(v), cc);
    } else {
        fprintf(stderr, "%s: unhandled small value type (%d)\n"
            , __FUNCTION__, (int)(0xFF & v));
        abort();
    }
}

static wikrt_v wikrt_reader_push_comp(wikrt_thread* t, wikrt_v v, wikrt_v cc)
{
    if(wikrt_is_value(v)) {
        return wikrt_reader_wrap_block_v(t, wikrt_value_to_action(v), cc);
    } else {
        // ((a b) c) => (a (b c))
        cc = wikrt_alloc_comp(t, wikrt_v2p(v)[1], cc);
        cc = wikrt_alloc_comp(t, wikrt_v2p(v)[0], cc);
        return cc;
    }
}

static wikrt_v wikrt_reader_push_cons(wikrt_thread* t, wikrt_v v, wikrt_v cc)
{
    if(wikrt_is_value(v)) {
        return wikrt_reader_wrap_block_v(t, wikrt_value_to_action(v), cc);
    } else {
        // (hd tl) => hd tl :
        cc = wikrt_reader_push_space(t, cc);
        cc = wikrt_reader_push_byte(t, ':', cc);
        cc = wikrt_alloc_comp(t, wikrt_v2p(v)[1], cc);
        cc = wikrt_alloc_comp(t, wikrt_v2p(v)[0], cc);
        return cc;
    }
}

static bool wikrt_reader_step(wikrt_cx* cx, wikrt_r r)
{ 
    wikrt_z const step_prealloc = 64 * WIKRT_CELLSIZE;
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
    if(wikrt_is_small(hd)) {
        // byte, naturals, or operators?
        if(wikrt_is_raw_byte(hd)) { return true; }  // success!
        wikrt_reg_set(cx, r, wikrt_reader_push_small(t, hd, tl));
        goto tailcall;
    } else if(wikrt_is_comp(hd)) {
        wikrt_reg_set(cx, r, wikrt_reader_push_comp(t, hd, tl));
        goto tailcall;
    } else if(wikrt_is_obj(hd)) {
        wikrt_reg_set(cx, r, wikrt_reader_push_obj(t, hd, tl));
        goto tailcall;
    } else if(wikrt_is_cons(hd)) {
        wikrt_reg_set(cx, r, wikrt_reader_push_cons(t, hd, tl));
        goto tailcall;
    } else {
        fprintf(stderr, "%s: unrecognized object type (%d)\n"
            , __FUNCTION__, (int)(0xFF & hd));
        abort();
    }
}}


