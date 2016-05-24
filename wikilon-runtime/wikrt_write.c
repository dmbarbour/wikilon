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
// A particular concern is blocks with WIKRT_OPVAL_LAZYKF, which requires
// I promote substructure of contained values to the substructure of the
// current block of code. I'll track this with a continuation stack.
//
// I will assume a valid block after reading the OTAG_BLOCK header. The
// most likely source of error is a full context, which might happen 
// when expanding opvals.
//

// A reasonable chunk size for our texts.
//  must be smaller than WIKRT_OTAG_TEXT chunks (max size 0xFFFF)
#define WIKRT_WRITE_BUFFSZ (40 * 1000)
typedef struct wikrt_writer_state 
{
    // conditions for current block
    bool rel, aff, lazykf;
    wikrt_size depth;

    wikrt_size bytect;
    wikrt_size charct;
    uint8_t    buff[WIKRT_WRITE_BUFFSZ];
} wikrt_writer_state;

// For bitfield tracking substructure
#define WIKRT_SS_LAZYKF (1 << 7)

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
static void writer_TAILCALL(wikrt_writer_state* w);
static void writer_INLINE(wikrt_writer_state* w);
static void writer_PROD_SWAP(wikrt_writer_state* w);
static void writer_INTRO_UNIT(wikrt_writer_state* w);
static void writer_SUM_SWAP(wikrt_writer_state* w);
static void writer_INTRO_VOID(wikrt_writer_state* w);

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
    if(0 == w->buffsz) { return; } // quick exit

    // sanity check
    assert((w->buffsz <= WIKRT_WRITE_BUFFSZ)
        && (w->charct <= w->buffsz)
        && (w->buffsz <= (UTF8_MAX_CP_SIZE * w->charct)));

    // (ops * (stack * (texts * e)))
    wikrt_sizeb const szBuff  = wikrt_cellbuff(w->buffsz);
    wikrt_sizeb const szHdr   = 2 * WIKRT_CELLSIZE;
    wikrt_sizeb const szAlloc = szHdr + szBuff;

    if(wikrt_mem_reserve(cx, szAlloc)) { 
        wikrt_assocl(cx); wikrt_wswap(cx); // swizzle text outputs to top
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

// (ops * (stack * (text * e))) → (ops' * (stack' * (text * e)))
//   where `stack` is ((ss * ops') * stack')
//   and `ss` accumulates substructure
static inline void wikrt_writer_pop_stack(wikrt_cx* cx, wikrt_writer_state* w)
{
    assert((WIKRT_UNIT_INR == wikrt_pval(cx, cx->val)[0]) && (w->depth > 0));

    wikrt_writer_putchar(cx, w, ']');
    if(w->rel) { wikrt_writer_putchar(cx, w, ABC_REL); }
    if(w->aff) { wikrt_writer_putchar(cx, w, ABC_AFF); }

    // initially (emptyOps * (stack * (text * e))) where
    //   stack is ((ss*ops')*stack')
    wikrt_dropk(cx);
    wikrt_assocr(cx);
    wikrt_assocr(cx);

    // Access and update substructure for our parent.
    wikrt_val const ss_val = wikrt_pval(cx, cx->val)[0];
    assert(wikrt_i(ss_val));
    wikrt_dropk(cx); // drop `ss`

    wikrt_int const ss = wikrt_v2i(ss_val);
    w->rel = (0 != (ss & WIKRT_SS_REL)) || (w->lazykf && w->rel);
    w->aff = (0 != (ss & WIKRT_SS_AFF)) || (w->lazykf && w->aff);
    w->lazykf = (0 != (ss & WIKRT_SS_LAZYKF));
    w->depth -= 1;

    // exiting with context (ops' * (stack' * (texts * e))). 
    //  and 'ss' merged into wirkt_writer_state
}

// (block * e) → (ops * e)
static void wikrt_writer_open_block(wikrt_cx* cx) 
{
    // `block` is (OTAG_BLOCK, ops) pair.
    wikrt_val* const v = wikrt_pval(cx, cx->val);
    wikrt_val* const pblock = wikrt_pval(cx, (*v));
    (*v) = pblock[1]; // dropping the OTAG
}


#if 0
static inline bool wikrt_writer_step(wikrt_cx* cx, wikrt_writer_state* w) 
{
    // Reserve as much space as we might need.
    wikrt_size const worst_case_alloc = WIKRT_UNWRAP_SUM_RESERVE + WIKRT_WRAP_SUM_RESERVE + (4 * WIKRT_CELLSIZE);
    if(!wikrt_mem_reserve(cx, worst_case_alloc)) { return false; }

    // (ops * (stack * (texts * e))).
    wikrt_sum_tag lr;
    wikrt_unwrap_sum(cx, &lr);

    if(WIKRT_INR == lr) {
        wikrt_wrap_sum(cx, lr); // end of ops
        wikrt_writer_pop_stack(cx, w); 
        return true;
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

#endif


static inline bool wikrt_writer_small_step(wikrt_cx* cx, wikrt_writer_state* w)
{
    wikrt_sum_tag lr;
    wikrt_unwrap_sum(cx, &lr);
    if(WIKRT_UNIT_INR == lr) {
        if(0 == w->depth) { return false; }
        wikrt_wrap_sum(cx, lr);
        wikrt_writer_pop_stack(cx, w); 
        return true; 
    } else {
        assert(wikrt_p(cx->val));
        wikrt_val* const v = wikrt_pval(cx, cx->val);
        
    }
}

static inline bool wikrt_cx_has_block(wikrt_cx* cx) {
    return wikrt_p(cx->val) && wikrt_blockval(cx, *wikrt_pval(cx, cx->val));
}

static inline void wikrt_writer_state_init(wikrt_writer_state* w) 
{
    w->rel      = false;
    w->aff      = false;
    w->lazykf   = false;
    w->depth    = 0;
    w->bytect   = 0;
    w->charct   = 0;
}

/* convert a block on the stack to text. */
void wikrt_block_to_text(wikrt_cx* cx)
{
    // Expecting (block * e).
    if(!wikrt_cx_has_block(cx)) { wikrt_set_error(cx, WIKRT_ETYPE); return; }
    if(!wikrt_mem_reserve(cx, (2 * WIKRT_CELLSIZE))) { return; }

    wikrt_writer_open_block(cx);                        // block → ops
    wikrt_intro_r(cx, WIKRT_UNIT_INR); wikrt_wswap(cx); // initial texts (empty list)
    wikrt_intro_r(cx, WIKRT_UNIT); wikrt_wswap(cx);     // initial continuation (unit)

    wikrt_writer_state w;
    wikrt_writer_state_init(&w);

    // Write every operator. This may fail at some intermediate step,
    // but should succeed in general (i.e. there should be no type
    // errors, and context shouldn't grow much unless we add a lot
    // more accelerators).
    while(wikrt_writer_small_step(cx,&w)) { /* continue */ }

    if(WIKRT_OK != wikrt_error(cx)) { return; }

    wikrt_writer_flush(cx, &w);
    assert(0 == w->depth);

    bool const empty_ops = (WIKRT_UNIT_INR == wikrt_pval(cx, cx->val)[0]);
    wikrt_dropk(cx); // drop ops (even if not empty)
    bool const empty_stack = (0 == w->depth) && (WIKRT_UNIT == wikrt_pval(cx, cx->val)[0]);
    wikrt_dropk(cx); // drop stack (even if not empty)

    bool const ok_state = empty_ops && empty_stack;
    if(!ok_state) { wikrt_set_error(cx, WIKRT_ETYPE); return; }
    wikrt_reverse_text_chunks(cx); // repair order of text
}

