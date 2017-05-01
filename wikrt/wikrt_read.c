/** Reading Binaries
 *
 * Ideally, reading large binaries is incremental and efficient.
 * But in the interest of making it work before making it fast,
 * it's currently just incremental. In each step, I'll allocate
 * a (raw byte, value') composition then take the raw byte.
 *
 * Feasible efficiency improvements 
 * - allocate temporary 'registers' to avoid common allocations
 *  - e.g. raw byte, focus value, list slice, list addend, next
 *  - this should eliminate about 80% of dynamic allocations
 * - temporary 'thread' structure to further localize memory
 * 
 * This does introduce some challenges for tracking serialization context.
 * I'll use a little reflection - peek ahead - to avoid extra spaces within
 * a block. Text processing or ensuring only one `(array)` or `(binary)` 
 * annotation per element may require special object types. 
 */
#include "wikrt_private.h"
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <assert.h>

// Enough space to guarantee a reader step succeeds.
// (this will overestimate most of the time, but that's okay)
#define WIKRT_READER_STEP_PREALLOC (64 * WIKRT_CELLSIZE)

// rewrite register to (small byte, val') compositon list, or NULL.
// returns true on success, false if it runs out of memory, aborts
// for any other failure case (like unhandled types).
static bool wikrt_reader_step(wikrt_cx*, wikrt_r);

size_t wikrt_read(wikrt_cx* cx, wikrt_r r, uint8_t* const buff, size_t const max)
{
    uint8_t* iter = buff;
    uint8_t* const end = buff + max;
    int e = 0;

    wikrt_eval_parallel(cx);    // for now, disable parallel read
    wikrt_api_enter(cx); 
    wikrt_api_interrupt(cx);

    // allocate register and a lightweight thread 
    if(!wikrt_api_prealloc(cx, 1, 32 * WIKRT_READER_STEP_PREALLOC)) {
        wikrt_api_exit(cx);
        errno = ENOMEM;
        return 0;
    }

    // read one byte at a time (for now)
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

static inline wikrt_v wikrt_reader_push_cstring(wikrt_thread* t, char const* s, wikrt_v cc)
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

static char const* const wikrt_op_str[WIKRT_OP_COUNT] =
{ [OP_NOP] = ""

// primitives
, [OP_a] = "a", [OP_b] = "b", [OP_c] = "c", [OP_d] = "d"

// arity annotations
, [OP_ANNO_a2] = "(a2)", [OP_ANNO_a3] = "(a3)", [OP_ANNO_a4] = "(a4)"
, [OP_ANNO_a5] = "(a5)", [OP_ANNO_a6] = "(a6)", [OP_ANNO_a7] = "(a7)"
, [OP_ANNO_a8] = "(a8)", [OP_ANNO_a9] = "(a9)"

// substructural annotations
, [OP_ANNO_nc] = "(nc)", [OP_ANNO_nd] = "(na)"

// evaluation annotations
, [OP_ANNO_error] = "(error)"
, [OP_ANNO_trace] = "(trace)"
, [OP_ANNO_par] = "(par)"
, [OP_ANNO_eval] = "(eval)"
, [OP_ANNO_memo] = "(memo)"
, [OP_ANNO_stow] = "(stow)"
, [OP_ANNO_trash] = "(trash)"

// type and representation annotations
, [OP_ANNO_nat] = "(nat)"
, [OP_ANNO_int] = "(int)"
, [OP_ANNO_dec] = "(dec)"
, [OP_ANNO_text] = "(text)"
, [OP_ANNO_binary] = "(binary)"
, [OP_ANNO_array] = "(array)"
, [OP_ANNO_bool] = "(bool)"
, [OP_ANNO_opt] = "(opt)"
, [OP_ANNO_sum] = "(sum)"
, [OP_ANNO_cond] = "(cond)"

// accelerators
, [OP_w] = "w"
, [OP_i] = "i"
, [OP_z] = "z"
, [OP_if] = "if"

, [OP_int] = "int"

// EXT ops are for optimized intermediate code
// character '/' isn't allowed in Awelon words
, [OP_EXT_RETURN] = "/return"
, [OP_EXT_RETURN_ad] = "/return-ad"
, [OP_EXT_RETURN_i] = "/return-i"
, [OP_EXT_RPUSH] = "/push"
, [OP_EXT_RPOP] = "/pop"

};
// check hand count of above items
_Static_assert((42 == WIKRT_OP_COUNT), "missing some operators");

static inline wikrt_v wikrt_reader_push_op(wikrt_thread* t, wikrt_op op, wikrt_v cc)
{
    if(OP_NOP == op) { return cc; }

    // cost: op size + 1 cells at most (about 16 cells)
    char const* const opstr = wikrt_op_str[op];
    assert(NULL != opstr);
    cc = wikrt_reader_push_space(t, cc);
    cc = wikrt_reader_push_cstring(t, opstr, cc);
    return cc;
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

static inline wikrt_v wikrt_reader_push_int_op(wikrt_thread* t, wikrt_i i, wikrt_v cc)
{
    // e.g. `-3` => `0 3 int`
    assert((WIKRT_SMALLINT_MIN <= i) && (i <= WIKRT_SMALLINT_MAX));
    wikrt_n const pos = (i > 0) ? (wikrt_n)i : 0;
    wikrt_n const neg = (i < 0) ? (wikrt_n)(-i) : 0;
    cc = wikrt_alloc_comp(t, wikrt_op_to_opval(OP_int), cc);
    cc = wikrt_alloc_comp(t, wikrt_to_small_nat_val(neg), cc);
    cc = wikrt_alloc_comp(t, wikrt_to_small_nat_val(pos), cc);
    return cc;
}

static wikrt_v wikrt_reader_push_small(wikrt_thread* t, wikrt_v v, wikrt_v cc)
{
    // ASSUMES sufficient space (~32 cells at most)
    // NOTES:
    //  - excepting naturals, most 'small' items are actions (operators, etc.)
    //  - an applied natural becomes inlined - `[] nat a d` - for simplicity.
    //  - integers are written in expanded form `[42 0 int]`, at least for now. 
    if(wikrt_is_value(v)) {
        if(wikrt_is_small_nat_val(v)) { 
            return wikrt_reader_push_nat(t, wikrt_from_small_nat(v), cc);
        } else { // write value as block
            return wikrt_reader_wrap_block_v(t, wikrt_value_to_action(v), cc);
        }
    } else if(wikrt_is_basic_op(v)) {
        return wikrt_reader_push_op(t, wikrt_opval_to_op(v), cc);
    } else if(wikrt_is_small_nat_op(v)) {
        return wikrt_reader_inline_v(t, wikrt_action_to_value(v), cc);
    } else if(wikrt_is_small_int_op(v)) {
        return wikrt_reader_push_int_op(t, wikrt_from_small_int(v), cc);
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
    } 
    // ((a b) c) => (a (b c))
    cc = wikrt_alloc_comp(t, wikrt_v2p(v)[1], cc);
    cc = wikrt_alloc_comp(t, wikrt_v2p(v)[0], cc);
    return cc;
}

static wikrt_v wikrt_reader_push_cons(wikrt_thread* t, wikrt_v v, wikrt_v cc)
{
    if(wikrt_is_value(v)) {
        return wikrt_reader_wrap_block_v(t, wikrt_value_to_action(v), cc);
    } 
    // (hd tl) => hd tl :
    cc = wikrt_reader_push_space(t, cc);
    cc = wikrt_reader_push_byte(t, ':', cc);
    cc = wikrt_alloc_comp(t, wikrt_v2p(v)[1], cc);
    cc = wikrt_alloc_comp(t, wikrt_v2p(v)[0], cc);
    return cc;
}

static wikrt_v wikrt_reader_push_obj(wikrt_thread* t, wikrt_v obj, wikrt_v cc)
{ switch(wikrt_o_type(obj)) {
    case WIKRT_OTYPE_PAIR: {
    } break;
    case WIKRT_OTYPE_QUAD: {
    } break;
    case WIKRT_OTYPE_BLOCK: {
    } break;
    case WIKRT_OTYPE_ARRAY: {
    } break;
    case WIKRT_OTYPE_BINARY: {
    } break;
    case WIKRT_OTYPE_WORD: {
    } break;
    default: {
        fprintf(stderr, "%s: unrecognized object type (%d)\n"
            , __FUNCTION__, (int)wikrt_o_type(obj));
        abort();
    }
}}

static bool wikrt_reader_step(wikrt_cx* cx, wikrt_r r)
{ wikrt_thread* const t = &(cx->memory);
  tailcall: {
    // prior to each step, ensure sufficient space
    if(!wikrt_api_mem_prealloc(cx, WIKRT_READER_STEP_PREALLOC)) { return false; }

    wikrt_v const v = wikrt_reg_get(cx, r);
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


