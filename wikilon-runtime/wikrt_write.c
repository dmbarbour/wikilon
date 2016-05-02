// This file is just for `wikrt_block_to_text` and any variants.
// The primary way to ser


#include <string.h>
#include <assert.h>
#include <stdio.h>
#include "wikrt.h"

// For the moment, this conversion to text is eager. Lazy streams will
// be desirable in the future, e.g. if I want to expand stowed values
// rather than merely reference them. 
//
// Our processing stack consists of the following pieces:
//
//   (1) the current list of operators we're processing
//   (2) a stack corresponding to `] continuation` for quoted blocks 
//   (3) a stack of 'text chunks' generated for output (in reverse order)
//
// This ordering also corresponds to the expected frequency of access.
// So our context can provide this as a stack of three items, i.e.
// (ops * (blocks * (texts * e))). The `blocks` element includes some
// substructural information for any `kf` suffix or the like.
//
// The `ops` list will also be used to break down complex values, spreading
// them into multiple operations. This can grow memory requirements.
//
// A particular concern is blocks with WIKRT_OPVAL_LAZYKF, which require
// I introduce substructure of a contained value. prior to continuing the
// computation. If a quoted value contains an affine block, then the block
// it is quoted into also becomes affine. I'll need to track the attributes
// of each block, and handle them properly when pushing and popping blocks.
//
// In addition, I have a buffer that I'll be filling for the next text
// chunk. This buffer is limited by the maximum text chunk size (0xFFFF)
// but may be smaller (e.g. 40k). It should be large enough that I rarely
// need multiple output buffers except in unusual circumstances.
//

// A reasonable chunk size for our texts.
#define WIKRT_WRITE_BUFFSZ (40 * 1000)
typedef struct wikrt_writer_state 
{
    // maybe include a wikrt_err status?
    bool       lazykf;
    bool       relevant;
    bool       affine;
    wikrt_size depth;
    wikrt_size buffsz;
    wikrt_size charct;
    uint8_t    buff[WIKRT_WRITE_BUFFSZ];
} wikrt_writer_state;

// Note: All of these strings should be ASCII.
// op2abc_table contains zero for accelerators 
#define OP(X) [OP_##X] = ABC_##X
static wikrt_abc wikrt_op2abc_table[OP_COUNT] =
 { OP(SP), OP(LF)
 , OP(PROD_ASSOCL), OP(PROD_ASSOCR)
 , OP(PROD_W_SWAP), OP(PROD_Z_SWAP)
 , OP(PROD_INTRO1), OP(PROD_ELIM1)
 , OP(SUM_ASSOCL), OP(SUM_ASSOCR)
 , OP(SUM_W_SWAP), OP(SUM_Z_SWAP)
 , OP(SUM_INTRO0), OP(SUM_ELIM0)
 , OP(COPY), OP(DROP)
 , OP(APPLY), OP(COMPOSE), OP(QUOTE), OP(REL), OP(AFF)
 , OP(NUM)
 , OP(D1), OP(D2), OP(D3), OP(D4), OP(D5)
 , OP(D6), OP(D7), OP(D8), OP(D9), OP(D0)
 , OP(ADD), OP(MUL), OP(NEG), OP(DIV), OP(GT)
 , OP(CONDAP), OP(DISTRIB), OP(FACTOR), OP(MERGE), OP(ASSERT)
 };
#undef OP

wikrt_err writer_TAILCALL(wikrt_cx* cx, wikrt_writer_state* w);
wikrt_err writer_INLINE(wikrt_cx* cx, wikrt_writer_state* w);
wikrt_err writer_PROD_SWAP(wikrt_cx* cx, wikrt_writer_state* w);
wikrt_err writer_INTRO_UNIT(wikrt_cx* cx, wikrt_writer_state* w);
wikrt_err writer_SUM_SWAP(wikrt_cx* cx, wikrt_writer_state* w);
wikrt_err writer_INTRO_VOID(wikrt_cx* cx, wikrt_writer_state* w);

typedef wikrt_err (*wikrt_writer)(wikrt_cx*, wikrt_writer_state*);
#define OP(X) [ACCEL_##X] = writer_##X
static wikrt_writer wikrt_accel2abc_table[OP_COUNT] =
 { OP(TAILCALL), OP(INLINE)
 , OP(PROD_SWAP), OP(INTRO_UNIT)
 , OP(SUM_SWAP), OP(INTRO_VOID)
 };
#undef OP

// (block * e) → (opslist * e)
static bool wikrt_writer_unwrap_block(wikrt_cx* cx) 
{
    if(wikrt_p(cx->val)) {
        wikrt_val* const v = wikrt_pval(cx, cx->val);
        if(wikrt_o(*v)) {
            wikrt_val* const pv = wikrt_pval(cx, (*v));
            if(wikrt_otag_block(*pv)) {
                (*v) = pv[1];
                return WIKRT_OK;
            }
        }
    }
    return WIKRT_TYPE_ERROR;
}

static wikrt_err wikrt_writer_init(wikrt_cx* cx, wikrt_writer_state* w) 
{
    // (block * e) → (ops * e)
    wikrt_err const stUnwrap = wikrt_writer_unwrap_block(cx);
    if(WIKRT_OK != stUnwrap) { wikrt_drop(cx, NULL); return stUnwrap; }

    // space for `texts` and `stack`
    wikrt_size const szAlloc = 2 * WIKRT_CELLSIZE;
    if(!wikrt_mem_reserve(cx, szAlloc)) { wikrt_drop(cx, NULL); return WIKRT_CXFULL; }

    wikrt_intro_r(cx, WIKRT_UNIT_INR);  // initial texts
    wikrt_intro_r(cx, WIKRT_UNIT);      // initial stack
    wikrt_zswap(cx); wikrt_wswap(cx);   // swizzle ops to top

    w->lazykf   = false;
    w->relevant = false;
    w->affine   = false;

    w->depth  = 0; // empty block stack
    w->buffsz = 0; // empty buffer
    w->charct = 0;

    return WIKRT_OK;
}

static bool wikrt_writer_flush(wikrt_cx* cx, wikrt_writer_state* w)
{
    _Static_assert((WIKRT_WRITE_BUFFSZ <= 0xFFFF), "write buffer too large to trivially flush");
    if(0 == w->buffsz) { return true; }

    // sanity check
    assert((w->buffsz <= WIKRT_WRITE_BUFFSZ)
        && (w->charct <= w->buffsz)
        && (w->buffsz <= (UTF8_MAX_CP_SIZE * w->charct)));
    
    wikrt_sizeb const szBuff  = wikrt_cellbuff(w->buffsz);
    wikrt_sizeb const szHdr   = 2 * WIKRT_CELLSIZE;
    wikrt_sizeb const szAlloc = szHdr + szBuff;
    if(!wikrt_mem_reserve(cx, szAlloc)) { return false; }

    wikrt_addr const addr_buff = wikrt_alloc_r(cx, szBuff);
    memcpy(wikrt_paddr(cx, addr_buff), w->buff, w->buffsz);

    // hdr is (OTAG_TEXT, next, (size-chars, size-bytes), buffer).
    wikrt_addr const addr_hdr = wikrt_alloc_r(cx, szHdr);
    wikrt_val* const phdr = wikrt_paddr(cx, addr_hdr);
    phdr[0] = WIKRT_OTAG_TEXT;
    phdr[2] = (w->charct << 16) | w->buffsz;
    phdr[3] = addr_buff;

    // (ops * (stack * (texts * e)))
    wikrt_assocl(cx); wikrt_wswap(cx); // swizzle texts to top

    // output written texts so far (in reverse chunk order)
    wikrt_val* const v = wikrt_pval(cx, cx->val);
    phdr[1] = (*v);
    (*v) = wikrt_tag_addr(WIKRT_O, addr_hdr);

    wikrt_wswap(cx); wikrt_assocr(cx); // return texts to bottom

    // clear the buffer
    w->buffsz = 0;
    w->charct = 0;

    return true;
}

static bool wikrt_writer_putchar(wikrt_cx* cx, wikrt_writer_state* w, uint32_t cp)
{
    _Static_assert((WIKRT_WRITE_BUFFSZ >= UTF8_MAX_CP_SIZE), "write buffer too small for safely processing text");
    w->charct += 1;
    w->buffsz += utf8_writecp_unsafe((w->buff + w->buffsz), cp);
    if(w->buffsz >= (WIKRT_WRITE_BUFFSZ - UTF8_MAX_CP_SIZE)) {
        return wikrt_writer_flush(cx, w);
    } else { return true; }
}

static wikrt_err wikrt_writer_fini(wikrt_cx* cx, wikrt_writer_state* w) 
{

    // context should be (ops * (stack * (texts * e))).
    bool const flushed = wikrt_writer_flush(cx, w);
    bool const empty_ops = (WIKRT_UNIT_INR == wikrt_pval(cx, cx->val)[0]);
    wikrt_drop(cx, NULL); // drop ops (even if not empty)
    bool const empty_stack = (0 == w->depth) && (WIKRT_UNIT == wikrt_pval(cx, cx->val)[0]);
    wikrt_drop(cx, NULL); // drop stack (even if not empty)

    // Assuming valid input (by the time we start writing), our only
    // source of error is 
    // reaching the memory limits of our context, either upon flush or
    // when processing quoted values. 
    bool const ok = flushed && empty_ops && empty_stack;
    if(!ok) { wikrt_drop(cx, NULL); return WIKRT_CXFULL; }

    wikrt_reverse_text_chunks(cx); // repair ordering of text
    return WIKRT_OK;
}

static bool wikrt_writer_pop_stack(wikrt_cx* cx, wikrt_writer_state* w)
{
    assert(WIKRT_UNIT_INR == wikrt_pval(cx, cx->val)[0]);
    if(0 == w->depth) { return false; } // empty stack
    
    bool const write_eob = wikrt_writer_putchar(cx, w, ']');
    bool const write_rel = !(w->relevant) || wikrt_writer_putchar(cx, w, ABC_REL);
    bool const write_aff = !(w->affine) || wikrt_writer_putchar(cx, w, ABC_AFF);

    bool const okEOB = write_eob && write_rel && write_aff;
    if(!okEOB) { return false; }

    // Context has (ops * (stack * (texts * e))).
    //   ops is empty list (we assert this above)
    //   stack is ((ss*ops')*stack') and 'ss' has substructural info.
    //   target is: (ss * (ops' * (stack' * (texts * e))).
    wikrt_err st = WIKRT_OK;
    st |= wikrt_drop(cx, NULL); // drop ops
    st |= wikrt_assocr(cx); // pop stack
    st |= wikrt_assocr(cx); // separate `ss` and `ops`
    assert(WIKRT_OK == st); // this operation should not fail (non-allocating, type safe by impl).

    // Access and update substructure for our parent.
    wikrt_val const ss_val = wikrt_pval(cx, cx->val)[0];
    assert(wikrt_i(ss_val));
    wikrt_drop(cx, NULL); // drop `ss`

    wikrt_int const ss = wikrt_v2i(ss_val);
    w->depth -= 1;
    w->relevant = (0 != (ss & WIKRT_SS_REL)) || (w->lazykf && w->relevant);
    w->affine = (0 != (ss & WIKRT_SS_AFF)) || (w->lazykf && w->affine);
    w->lazykf = (0 != (ss & WIKRT_SS_PEND)); // overloading use

    // Leave with context (ops' * (stack' * (texts * e))). 
    // Our next step will involve processing the next op.
    return true;
}




static bool wikrt_writer_step(wikrt_cx* cx, wikrt_writer_state* w) 
{
    // (ops * (stack * (texts * e))).
    wikrt_val const ops = wikrt_pval(cx, cx->val)[0];
    if(WIKRT_PL == wikrt_vtag(ops)) {
        assert(WIKRT_UNIT_INL != ops);
        wikrt_val* const pv = wikrt_pval(cx, ops);
        if(wikrt_i(*pv)) { 
            return wikrt_writer_putchar(cx, w, wikrt_v2i(*pv)); 
        } else {
            // should be OPTOK or OPVAL
            return false;
        }
    } else if(WIKRT_UNIT_INR == ops) {
        return wikrt_writer_pop_stack(cx, w);
    } else {
        fprintf(stderr, "%s: unrecognized operator in block\n", __FUNCTION__);
        abort();
    }

    // ALTERNATIVE: use wikrt_unwrap_sum and wikrt_assocr to get current `op` on top.
    //  maybe keep a w->status instead of returning booleans in each step.
}


/* convert a block on the stack to text. */
wikrt_err wikrt_block_to_text(wikrt_cx* cx)
{
    wikrt_writer_state w;
    wikrt_err const stInit = wikrt_writer_init(cx, &w);
    if(WIKRT_OK != stInit) {
        wikrt_drop(cx, NULL); 
        return stInit; 
    }
    while(wikrt_writer_step(cx, &w)) { /* NOP */ }
    return wikrt_writer_fini(cx, &w);
}

