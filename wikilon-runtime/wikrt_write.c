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
//   (1) the object we're currently writing (e.g. text, bignum, block)
//   (2) a continuation stack (objects we have yet to process)
//   (3) the text object we're actively constructing
//
// This is essentially the converse of how our parser works. By constructing
// relatively large buffers of text, we avoid frequent access to the text
// object. So we can have (object * (cont * (texts * e))) as our cx->val
// during our eager write efforts.
//
// A particular concern is blocks with WIKRT_OPVAL_LAZYKF, which require
// I introduce substructure of a contained value. prior to continuing the
// computation. If a quoted value contains an affine block, then the block
// it is quoted into also becomes affine. I'll need to track the attributes
// of each block, and handle them properly when pushing and popping blocks.
//
// For simplicity, I'm going to assume valid input after reading the OTAG_BLOCK
// header, then process every element in that block even if we run out of space
// to write our results. The only error after we get moving is WIKRT_CXFULL.
// No need to optimize the error case by short circuiting.
//

// A reasonable chunk size for our texts.
//  must be smaller than WIKRT_OTAG_TEXT chunks (max size 0xFFFF)
#define WIKRT_WRITE_BUFFSZ (40 * 1000)
typedef struct wikrt_writer_state 
{
    // in case of error
    bool       cxfull;
    
    // conditions for current block
    bool       lazykf;
    bool       relevant;
    bool       affine;

    wikrt_size depth;
    wikrt_size bytect;
    wikrt_size charct;
    uint8_t   *buff;
} wikrt_writer_state;

static inline wikrt_size wikrt_writer_buff_free(wikrt_writer_state const* w) 
{
    return (WIKRT_WRITE_BUFFSZ - (w->buffsz)); 
}

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

// I'd prefer to maybe use a table of `wikrt_op const*`. But for the
// general case, I might eventually have accelerators that contain
// specific blocks or texts. So I'm using function pointers for the
// writing of multiple elements.
static void writer_TAILCALL(wikrt_cx* cx, wikrt_writer_state* w);
static void writer_INLINE(wikrt_cx* cx, wikrt_writer_state* w);
static void writer_PROD_SWAP(wikrt_cx* cx, wikrt_writer_state* w);
static void writer_INTRO_UNIT(wikrt_cx* cx, wikrt_writer_state* w);
static void writer_SUM_SWAP(wikrt_cx* cx, wikrt_writer_state* w);
static void writer_INTRO_VOID(wikrt_cx* cx, wikrt_writer_state* w);

typedef void (*wikrt_writer)(wikrt_cx*, wikrt_writer_state*);
#define ACCEL(X) [ACCEL_##X] = writer_##X
static wikrt_writer wikrt_accel_writers[OP_COUNT] =
 { ACCEL(TAILCALL),  ACCEL(INLINE)
 , ACCEL(PROD_SWAP), ACCEL(INTRO_UNIT)
 , ACCEL(SUM_SWAP),  ACCEL(INTRO_VOID)
 };
_Static_assert((6 == WIKRT_ACCEL_COUNT), "missing accelerator writer?");
#undef ACCEL

static void wikrt_writer_flush(wikrt_cx* cx, wikrt_writer_state* w)
{
    _Static_assert((WIKRT_WRITE_BUFFSZ <= 0xFFFF), "write buffer too large to trivially flush");
    if(0 == w->buffsz) { return; }

    // sanity check
    assert((w->buffsz <= WIKRT_WRITE_BUFFSZ)
        && (w->charct <= w->buffsz)
        && (w->buffsz <= (UTF8_MAX_CP_SIZE * w->charct)));

    // (ops * (stack * (texts * e)))
    wikrt_sizeb const szBuff  = wikrt_cellbuff(w->buffsz);
    wikrt_sizeb const szHdr   = 2 * WIKRT_CELLSIZE;
    wikrt_sizeb const szAlloc = szHdr + szBuff;

    bool const flush_space_available = wikrt_mem_reserve(cx, szAlloc);
    bool const no_prior_errors = !(w->cxfull);

    if(flush_space_available && no_prior_errors) {
        // Complete the flush of data.

        wikrt_assocl(cx); wikrt_wswap(cx); // swizzle texts to top
        wikrt_val* const v = wikrt_pval(cx, cx->val);

        wikrt_addr const addr_buff = wikrt_alloc_r(cx, szBuff);
        memcpy(wikrt_paddr(cx, addr_buff), w->buff, w->buffsz);

        // hdr is (OTAG_TEXT, next, (size-chars, size-bytes), buffer).
        wikrt_addr const addr_hdr = wikrt_alloc_r(cx, szHdr);
        wikrt_val* const phdr = wikrt_paddr(cx, addr_hdr);
        phdr[0] = WIKRT_OTAG_TEXT;
        phdr[2] = (w->charct << 16) | w->buffsz;
        phdr[3] = addr_buff;

        // output written texts so far (in reverse chunk order)
        phdr[1] = (*v);
        (*v) = wikrt_tag_addr(WIKRT_O, addr_hdr);
        wikrt_wswap(cx); wikrt_assocr(cx); // return texts to bottom
       
    } else { 
        // Otherwise, we'll record that we have a full context.
        // This will prevent future flushes, too.
        w->cxfull = true; 
    }

    // regardless of success or failure, clear the buffer
    w->buffsz = 0;
    w->charct = 0;
}

static void wikrt_writer_putchar(wikrt_cx* cx, wikrt_writer_state* w, uint32_t cp)
{
    assert(wikrt_text_char(cp)); // write valid text.
    _Static_assert((WIKRT_WRITE_BUFFSZ >= UTF8_MAX_CP_SIZE), "write buffer too small for safely processing text");
    if(w->buffsz > (WIKRT_WRITE_BUFFSZ - UTF8_MAX_CP_SIZE)) { wikrt_writer_flush(cx, w); } 
    w->buffsz += utf8_writecp_unsafe((w->buff + w->buffsz), cp);
    w->charct += 1;
}

static void wikrt_write_op(wikrt_cx* cx, wikrt_writer_state* w, wikrt_op op) 
{
    assert((OP_INVAL < op) && (op < OP_COUNT));
    wikrt_abc const abc_opchar = wikrt_op2abc_table[op];
    if(0 != abc_opchar) { 
        wikrt_writer_putchar(cx, w, abc_opchar); 
    } else { 
        wikrt_writer writer = wikrt_accel_writers[op];
        assert(NULL != writer);
        (*writer)(cx, w);
    }
}

static void writer_TAILCALL(wikrt_cx* cx, wikrt_writer_state* w) 
{   // $c 
    // (though it's a true tail-call only at the end of a function)
    wikrt_write_op(cx, w, OP_APPLY);
    wikrt_write_op(cx, w, OP_PROD_ELIM1);
}
static void writer_INLINE(wikrt_cx* cx, wikrt_writer_state* w)
{
    // vr($c)
    wikrt_write_op(cx, w, OP_PROD_INTRO1);
    wikrt_write_op(cx, w, OP_PROD_ASSOCR);
    wikrt_write_op(cx, w, ACCEL_TAILCALL);
}
static void writer_PROD_SWAP(wikrt_cx* cx, wikrt_writer_state* w)
{
    // vrwlc
    wikrt_write_op(cx, w, OP_PROD_INTRO1);
    wikrt_write_op(cx, w, OP_PROD_ASSOCR);
    wikrt_write_op(cx, w, OP_PROD_W_SWAP);
    wikrt_write_op(cx, w, OP_PROD_ASSOCL);
    wikrt_write_op(cx, w, OP_PROD_ELIM1);
}
static void writer_INTRO_UNIT(wikrt_cx* cx, wikrt_writer_state* w)
{
    // v(vrwlc)
    wikrt_write_op(cx, w, OP_PROD_INTRO1);
    wikrt_write_op(cx, w, ACCEL_PROD_SWAP);
}
static void writer_SUM_SWAP(wikrt_cx* cx, wikrt_writer_state* w)
{
    // VRWLC
    wikrt_write_op(cx, w, OP_SUM_INTRO0);
    wikrt_write_op(cx, w, OP_SUM_ASSOCR);
    wikrt_write_op(cx, w, OP_SUM_W_SWAP);
    wikrt_write_op(cx, w, OP_SUM_ASSOCL);
    wikrt_write_op(cx, w, OP_SUM_ELIM0);
}
static void writer_INTRO_VOID(wikrt_cx* cx, wikrt_writer_state* w)
{
    // V(VRWLC)
    wikrt_write_op(cx, w, OP_SUM_INTRO0);
    wikrt_write_op(cx, w, ACCEL_SUM_SWAP);
}

static inline bool wikrt_writer_pop_stack(wikrt_cx* cx, wikrt_writer_state* w)
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

static inline bool wikrt_writer_step(wikrt_cx* cx, wikrt_writer_state* w) 
{
    // Reserve as much space as we might need.
    wikrt_size const worst_case_alloc = WIKRT_UNWRAP_SUM_RESERVE + WIKRT_WRAP_SUM_RESERVE + (4 * WIKRT_CELLSIZE);
    if(!wikrt_mem_reserve(cx, worst_case_alloc)) { return false; }

    // (ops * (stack * (texts * e))).
    wikrt_sum_tag lr;
    wikrt_err const stSum = wikrt_unwrap_sum(cx, &lr);
    assert(WIKRT_OK == stSum); // bad op type?

    if(WIKRT_INR == lr) {
        wikrt_wrap_sum(cx, lr); // end of ops
        return wikrt_writer_pop_stack(cx, w); 
    } 

    // ((op*ops') * (stack * (texts * e))).
    wikrt_err const stPair = wikrt_assocr(cx); 
    assert(WIKRT_OK == stPair); // this must be a pair.
       
    // (op * (ops' * (stack * (texts * e)))

    // Basic operations represented by small integers
    wikrt_val const op = wikrt_pval(cx, cx->val)[0];
    if(wikrt_i(op)) {
        wikrt_drop(cx, NULL);
        return wikrt_write_op(cx, w, wikrt_v2i(op));
    } 

    // otherwise our `op` should be OPVAL or OPTOK.
    assert(wikrt_o(cx, op));
    wikrt_val* const opref = wikrt_pval(cx, op);
    wikrt_val const otag = LOBYTE(*opref);
    if(WIKRT_OTAG_OPVAL == otag) {
        return wikrt_write_opval
        wikrt_val const val = opref[1];
        

    } else if(WIKRT_OTAG_OPTOK == otag) {
        wikrt_size const toksz = (*opref) >> 8;
        assert((0 < toksz) && (toksz < WIKRT_TOK_BUFFSZ));

        uint8_t tokbuff[WIKRT_TOK_BUFFSZ];
        memcpy(tokbuff, (1 + opref), toksz);
        wikrt_drop(cx, NULL); // drop optok
        
        uint8_t* s = tokbuff;
        uint8_t* const s_end = tokbuff + toksz;

        bool ok = wikrt_writer_putchar(cx, w, '{');
        do {
            uint32_t const cp = utf8_step_unsafe(&s);
            assert(wikrt_token_char(cp));
            ok = ok && wikrt_writer_putchar(cx, w, cp);
        } while(s_end != s);
        ok = ok && wikrt_writer_putchar(cx, w, '}');
        return ok;
    }



        do { ok = ok && wikrt_writer_putchar(cx, w, utf8_step_unsafe(&s)) && ok; } while(s_end != s);
        
            uint32_t cp = utf8_step_unsafe(cx, w, &s);
            wikrt_writer_putchar


        

        ok = 
                



        wikrt_drop(cx, 

        
    } else {
        fprintf(stderr, "%s: unrecognized operator type", __FUNCTION__);
        abort();
    }
    
        
        
        
        // Otherwise we should have an (optok _text_) or (opval _val_). 
        assert(wikrt_o(op0)); 
        wikrt_val const* const pobj = wikrt_pval(cx, op0);
        wikrt_val const otag = LOBYTE(*pobj);
        if(WIKRT_OTAG_OPVAL == otag) {
        } else if(WIKRT_OTAG_OPTOK == otag) {
        } else {
            fprintf(stderr, "%s: unrecognized 
        }
        if(WIKRT_OTAG_OPTOK == otag) {
        } else if(WIKRT_OTAG_OPVAL == 

            
        
        
        return wikrt_writer_step_op(cx, w);
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
    // Expecting (block * e).
    bool const okType = wikrt_p(cx->val)
                     && wikrt_blockval(cx, wikrt_pval(cx, cx->val)[0]);
    if(!okType) {
        wikrt_drop(cx, NULL);
        return WIKRT_TYPE_ERROR;
    }

    // Prepare (ops * (continuation * (texts * e))) context.
    if(!wikrt_mem_reserve(cx, (2 * WIKRT_CELLSIZE)) {
        wikrt_drop(cx, NULL);
        return WIKRT_CXFULL; 
    }

    wikrt_intro_r(cx, WIKRT_UNIT_INR); wikrt_wswap(cx); // initial texts (empty list)
    wikrt_intro_r(cx, WIKRT_UNIT); wikrt_wswap(cx);     // initial continuation (unit)

    // Prepare our buffer.
    uint8_t wbuff[WIKRT_WRITER_BUFFSZ];
    wikrt_writer_state w = { .buff = wbuff };

    // Process every operation. Assume a valid block structure. Only 
    // possible error is to run out of space during write. We'll not
    // optimize for the error case.
    do {
        

    } while(1);

    

    // context should be (ops * (stack * (texts * e))).
    // `ops` should be an empty list. 
    // `stack` should be an empty stack.
    // `texts` is in reverse chunk order
    wikrt_writer_flush(cx, w);
    assert(0 == w->depth);

    bool const empty_ops = (WIKRT_UNIT_INR == wikrt_pval(cx, cx->val)[0]);
    wikrt_drop(cx, NULL); // drop ops (even if not empty)
    bool const empty_stack = (0 == w->depth) && (WIKRT_UNIT == wikrt_pval(cx, cx->val)[0]);
    wikrt_drop(cx, NULL); // drop stack (even if not empty)


    if(WIKRT_OK != w->status) {
        wikrt_drop(cx, NULL);
        wikrt_drop(cx, NULL);
        wikrt_drop(cx, NULL);
        return;
    }


    // Assuming valid input (by the time we start writing), our only
    // source of error is 
    // reaching the memory limits of our context, either upon flush or
    // when processing quoted values. 
    bool const ok = flushed && empty_ops && empty_stack;
    if(!ok) { w->status = WIKRT_CXFULL; }
wikrt_drop(cx, NULL); return WIKRT_CXFULL; }

    wikrt_reverse_text_chunks(cx); // repair ordering of text
    return WIKRT_OK;


    // since I have a block, the only other error will be
    // WIKRT_CXFULL. I need to prepare our stack, then process
    // every operator.
    wikrt_writer_state w;
    w.
    


    if(!wikrt_p(cx->val) || !wikrt_blockval(cx, wikrt_pval(cx, cx-


    wikrt_writer_state w;
    wikrt_err const stInit = wikrt_writer_init(cx, &w);
    if(WIKRT_OK != stInit) {
        wikrt_drop(cx, NULL); 
        return stInit; 
    }
    while(wikrt_writer_step(cx, &w)) { /* NOP */ }
    return wikrt_writer_fini(cx, &w);
}

