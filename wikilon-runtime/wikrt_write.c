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
// Our write environment must contain the following elements:
//
// (1) the value we're currently processing
// (2) any continuation, what we are to process next
// (3) the output, a final text object
//
// I can potentially use a stack of the form (val * (cont * (texts * e))).
// This matches how the parser works. Our continuation may need multiple
// forms based on our current value type. 
//
// A particular concern is blocks with WIKRT_OPVAL_LAZYKF, which requires
// I promote substructure of contained values to the substructure of the
// current block of code. I'll track this with a continuation stack.
//
// I will assume a valid block after reading the OTAG_BLOCK header. The
// most likely source of error is a full context, which might happen 
// when expanding opvals.

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

static inline void wikrt_writer_state_init(wikrt_writer_state* w) 
{
    w->rel      = false;
    w->aff      = false;
    w->lazykf   = false;
    w->depth    = 0;
    w->bytect   = 0;
    w->charct   = 0;
}


// Basic ABC (elements in ASCII range)
#define OP(X) [OP_##X] = ABC_##X
static uint8_t wikrt_op2abc_table[OP_COUNT] =
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

// I'd prefer to maybe use a table, but it won't generalize easily
// if the table contains constants of any sort. 
//
// OTOH, I could maybe create an operator for each 'constant' that
// I use within any accelerator. This would provide a simple basis
// for fast interpretation of new accelerators.
//
static void writer_TAILCALL(wikrt_cx*, wikrt_writer_state* w);
static void writer_INLINE(wikrt_cx*, wikrt_writer_state* w);
static void writer_PROD_SWAP(wikrt_cx*, wikrt_writer_state* w);
static void writer_INTRO_UNIT(wikrt_cx*, wikrt_writer_state* w);
static void writer_SUM_SWAP(wikrt_cx*, wikrt_writer_state* w);
static void writer_INTRO_VOID(wikrt_cx*, wikrt_writer_state* w);
static void writer_wrzw(wikrt_cx*, wikrt_writer_state* w);
static void writer_wzlw(wikrt_cx*, wikrt_writer_state* w);

typedef void (*wikrt_writer)(wikrt_cx*, wikrt_writer_state*);
#define ACCEL(X) [ACCEL_##X] = writer_##X
static wikrt_writer wikrt_accel_writers[OP_COUNT] =
 { ACCEL(TAILCALL),  ACCEL(INLINE)
 , ACCEL(PROD_SWAP), ACCEL(INTRO_UNIT)
 , ACCEL(SUM_SWAP),  ACCEL(INTRO_VOID)
 , ACCEL(wrzw),      ACCEL(wzlw)
 };
_Static_assert((8 == WIKRT_ACCEL_COUNT), "missing accelerator writer?");
#undef ACCEL

static void wikrt_writer_flush(wikrt_cx* cx, wikrt_writer_state* w)
{
    _Static_assert((WIKRT_WRITE_BUFFSZ <= 0xFFFF), "write buffer too large to trivially flush");
    if(0 == w->bytect) { return; } // quick exit

    // sanity check
    assert((w->bytect <= WIKRT_WRITE_BUFFSZ)
        && (w->charct <= w->bytect)
        && (w->bytect <= (UTF8_MAX_CP_SIZE * w->charct)));

    // (ops * (stack * (texts * e)))
    wikrt_sizeb const szBuff  = wikrt_cellbuff(w->bytect);
    wikrt_sizeb const szHdr   = 2 * WIKRT_CELLSIZE;
    wikrt_sizeb const szAlloc = szHdr + szBuff;

    if(wikrt_mem_reserve(cx, szAlloc)) { 
        wikrt_assocl(cx); wikrt_wswap(cx); // swizzle text outputs to top
        wikrt_val* const v = wikrt_pval(cx, cx->val);

        wikrt_addr const addr_buff = wikrt_alloc_r(cx, szBuff);
        memcpy(wikrt_paddr(cx, addr_buff), w->buff, w->bytect);

        // hdr is (OTAG_TEXT, next, (size-chars, size-bytes), buffer).
        wikrt_addr const addr_hdr = wikrt_alloc_r(cx, szHdr);
        wikrt_val* const phdr = wikrt_paddr(cx, addr_hdr);
        phdr[0] = WIKRT_OTAG_TEXT;
        phdr[2] = (w->charct << 16) | w->bytect;
        phdr[3] = addr_buff;

        // output written texts so far (in reverse chunk order)
        phdr[1] = (*v);
        (*v) = wikrt_tag_addr(WIKRT_O, addr_hdr);
        wikrt_wswap(cx); wikrt_assocr(cx); // return texts to bottom
    }

    // regardless of success or failure, clear the buffer
    w->bytect = 0;
    w->charct = 0;

}

static void wikrt_writer_putchar(wikrt_cx* cx, wikrt_writer_state* w, uint32_t cp)
{
    assert(wikrt_text_char(cp)); // write valid text.
    _Static_assert((WIKRT_WRITE_BUFFSZ >= UTF8_MAX_CP_SIZE), "write buffer too small for safely processing text");
    if(w->bytect > (WIKRT_WRITE_BUFFSZ - UTF8_MAX_CP_SIZE)) { wikrt_writer_flush(cx, w); } 
    w->bytect += utf8_writecp_unsafe((w->buff + w->bytect), cp);
    w->charct += 1;
}

static void wikrt_writer_putcstr(wikrt_cx* cx, wikrt_writer_state* w, char const* const buff)
{
    _Static_assert(sizeof(uint8_t) == sizeof(char), "unsafe cast between char and uint8_t");
    uint8_t const* s = (uint8_t const*) buff;
    do {
        uint32_t cp = utf8_step_unsafe(&s);
        if(0 == cp) { break; }
        wikrt_writer_putchar(cx, w, cp);
    } while(true);
}

static void wikrt_write_op(wikrt_cx* cx, wikrt_writer_state* w, wikrt_op op) 
{
    assert((OP_INVAL < op) && (op < OP_COUNT));
    uint8_t const abc_opchar = wikrt_op2abc_table[op];
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
static void writer_wrzw(wikrt_cx* cx, wikrt_writer_state* w)
{
    wikrt_write_op(cx, w, OP_PROD_W_SWAP);
    wikrt_write_op(cx, w, OP_PROD_ASSOCR);
    wikrt_write_op(cx, w, OP_PROD_Z_SWAP);
    wikrt_write_op(cx, w, OP_PROD_W_SWAP);
}
static void writer_wzlw(wikrt_cx* cx, wikrt_writer_state* w)
{
    wikrt_write_op(cx, w, OP_PROD_W_SWAP);
    wikrt_write_op(cx, w, OP_PROD_Z_SWAP);
    wikrt_write_op(cx, w, OP_PROD_ASSOCL);
    wikrt_write_op(cx, w, OP_PROD_W_SWAP);
}



// (block * e) → (ops * e), returning otag
static wikrt_otag wikrt_writer_open_block(wikrt_cx* cx) 
{
    // `block` is (OTAG_BLOCK ops) pair.
    _Static_assert(!WIKRT_NEED_FREE_ACTION, "free the 'block' tag");
    wikrt_val* const v = wikrt_pval(cx, cx->val);
    wikrt_val* const pblock = wikrt_pval(cx, (*v));
    wikrt_otag const otag = (*pblock);
    assert(WIKRT_OTAG_BLOCK == LOBYTE(otag));
    (*v) = pblock[1]; // dropping the OTAG
    return otag;
}

// (empty ops * (stack * (text * e))) → (ops' * (stack' * (text * e)))
//   where `stack` is ((ss * ops') * stack')
//   and `ss` accumulates substructure
static inline void wikrt_writer_pop_stack(wikrt_cx* cx, wikrt_writer_state* w)
{
    _Static_assert((']' == 93), "assuming ']' is 93");
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
    assert(wikrt_smallint(ss_val));
    wikrt_dropk(cx); // drop `ss`

    wikrt_int const ss = wikrt_v2i(ss_val);
    w->rel = (0 != (ss & WIKRT_SS_REL)) || (w->lazykf && w->rel);
    w->aff = (0 != (ss & WIKRT_SS_AFF)) || (w->lazykf && w->aff);
    w->lazykf = (0 != (ss & WIKRT_SS_LAZYKF));
    w->depth -= 1;

    // exiting with context (ops' * (stack' * (texts * e))). 
    //  and 'ss' merged into wirkt_writer_state
}

// ((block ops') * (ops * (stack * (texts * e)))) → 
//   (ops' * (stack' * (texts' * e)))
//  where stack' = ((ss*ops)*stack) and texts' includes the '['
static inline void wikrt_writer_push_stack(wikrt_cx* cx, wikrt_writer_state* w, bool block_lazykf) 
{
    _Static_assert(('[' == 91), "assuming '[' is 91");

    wikrt_otag const block_tag = wikrt_writer_open_block(cx); // (block ops') → ops'

    if(!wikrt_mem_reserve(cx, WIKRT_CELLSIZE)) { return; }
    wikrt_wswap(cx); wikrt_zswap(cx); // (ops * (stack * (block * (texts * e))))

    wikrt_int const ss = (w->rel ? WIKRT_SS_REL : 0) 
                       | (w->aff ? WIKRT_SS_AFF : 0) 
                       | (w->lazykf ? WIKRT_SS_LAZYKF : 0);

    wikrt_intro_r(cx, wikrt_i2v(ss));
    wikrt_assocl(cx); // ((ss*ops) * (stack * (ops' * ...))))
    wikrt_assocl(cx); // (((ss*ops)*stack) * (ops' * ...))) = (stack' * (ops' * ...))
    wikrt_wswap(cx);  // (ops' * (stack' * (texts * e)))

    w->rel = (0 != (WIKRT_BLOCK_RELEVANT & block_tag));
    w->aff = (0 != (WIKRT_BLOCK_AFFINE & block_tag));
    w->lazykf = block_lazykf;
    w->depth += 1;

    wikrt_writer_putchar(cx, w, '[');
}

static inline bool wikrt_is_text_chunk(wikrt_cx* cx, wikrt_val v) {
    return wikrt_o(v) && wikrt_otag_text(*wikrt_pval(cx, v));
}

// embed a text value if it's suitably formulated 
static bool wikrt_is_embeddable_text(wikrt_cx* cx, wikrt_val v) {
    while(WIKRT_UNIT_INR != v) {
        if(!wikrt_is_text_chunk(cx, v)) { return false; }
        v = wikrt_pval(cx, v)[1]; // next element
    }
    return true;
}

// given (int * (ops * (stack * (texts * e))))
//  write and drop the integer...
static void wikrt_write_int(wikrt_cx* cx, wikrt_writer_state* w) 
{
    // double check a few static assumptions
    _Static_assert((ABC_NEG == '-'), "unexpected negate op");
    _Static_assert((ABC_NUM == '#'), "unexpected number introduction op");
    _Static_assert((ABC_D1 == '1') && (ABC_D2 == '2') && (ABC_D3 == '3')
                && (ABC_D4 == '4') && (ABC_D5 == '5') && (ABC_D6 == '6')
                && (ABC_D7 == '7') && (ABC_D8 == '8') && (ABC_D9 == '9')
                && (ABC_D0 == '0'), "unexpected digit ops");


    size_t len = 0;
    wikrt_peek_istr(cx, NULL, &len);
    assert(0 != len);

    char buff[1 + len]; buff[len] = 0;
    wikrt_peek_istr(cx, buff, &len);
    wikrt_dropk(cx); // drop the integer

    wikrt_writer_putchar(cx, w, ABC_NUM);
    if('0' != buff[0]) { // except for zero (`#` not `#0`)
        // write the pseudo-literal integer. 
        bool const negate = ('-' == buff[0]);
        char const* s = buff + (negate ? 1 : 0);
        do { wikrt_writer_putchar(cx, w, *s++); } while(*s);
        if(negate) { wikrt_writer_putchar(cx, w, ABC_NEG); }
    } 
}

// given (textval * (ops * (stack * (output text * e)))
//  write and drop the text value. We also need to add 
//  the start, terminal, and any newline-indent escapes.
static void wikrt_write_text(wikrt_cx* cx, wikrt_writer_state* w)
{
    _Static_assert(('"' == 34) && ('\n' == 10) && (' ' == 32) && ('~' == 126)
                  ,"assumed codes for embedded text");

    // need (val * (cont * (output text * e))), so cont = (ops*stack)
    wikrt_wswap(cx); wikrt_zswap(cx); wikrt_assocl(cx); wikrt_wswap(cx); 

    // begin writing text
    wikrt_writer_putchar(cx, w, '"');

    // I'll need to process every character of the text in order
    // to escape all contained `\n` characters.
    do {
        size_t len = 8 * 1000;
        char buff[len];

        wikrt_read_text(cx, buff, &len, NULL);
        if(0 == len) { break; } // done reading

        _Static_assert((sizeof(uint8_t) == sizeof(char)), "unsafe cast between uint8_t and char");
        uint8_t const* const s = (uint8_t const*)buff;
        size_t offset = 0;
        do {
            uint32_t cp;
            offset += utf8_readcp_unsafe(s + offset, &cp);
            wikrt_writer_putchar(cx, w, cp); 
            if('\n' == cp) { wikrt_writer_putchar(cx, w, ' '); } // escape newline by following indentation
        } while(offset != len);

    } while(true);

    // terminate text
    wikrt_writer_putchar(cx, w, '\n'); 
    wikrt_writer_putchar(cx, w, '~'); 

    wikrt_elim_list_end(cx); // drop the text value
    wikrt_assocr(cx); // open cont back to (ops * (stack * ...))
}

// given (val * (ops * (stack * (texts * e))))
//  return (ops' * (stack' * (texts' * e)))
//
// This will write - or begin to write - one value. Some values will be
// divided into extra ops to be written later. Block values will write
// the opening character then grow the stack.
//
// A significant challenge here is writing of embedded texts. When I 
// encounter a WIKRT_OTAG_TEXT, it is possible that I won't want to
// write it as a text value (e.g. list doesn't terminate properly).
//
// Other potential issues: rendering arrays and binaries might benefit
// from injecting {&array} and {&binary} annotations. I'd need to 
// somehow track that I've injected them, though.
static void wikrt_write_val(wikrt_cx* cx, wikrt_writer_state* w, wikrt_otag opval_tag) 
{ tailcall: { switch(wikrt_type(cx)) {

    case WIKRT_TYPE_PROD: {
        // (a * b) pair → write b, write a, write `l` to pair them
        wikrt_intro_op(cx, OP_PROD_ASSOCL);
        wikrt_consd(cx);
        wikrt_assocr(cx);
        wikrt_wrap_otag(cx, opval_tag);
        wikrt_consd(cx);
        goto tailcall;
    } break;

    case WIKRT_TYPE_UNIT: {
        wikrt_dropk(cx);
        wikrt_write_op(cx, w, ACCEL_INTRO_UNIT);
        return;
    } break;

    case WIKRT_TYPE_INT: { 
        wikrt_write_int(cx, w); 
        return;
    } break;

    case WIKRT_TYPE_SUM: {

        // special handling to print obvious embeddable texts
        bool const render_text_as_list = (0 != (WIKRT_OPVAL_ASLIST & opval_tag));
        wikrt_val const v =  wikrt_pval(cx, cx->val)[0];
        if(!render_text_as_list && wikrt_is_text_chunk(cx, v)) {
            if(wikrt_is_embeddable_text(cx, wikrt_pval(cx,v)[1])) { 
                wikrt_write_text(cx, w); 
                return;
            } else {
                opval_tag |= WIKRT_OPVAL_ASLIST;
            }
        }

        // unwrap the value, but add code to re-wrap it.
        wikrt_sum_tag lr;
        wikrt_unwrap_sum(cx, &lr);
        wikrt_op const sumop = (WIKRT_INL == lr) ? OP_SUM_INTRO0 : ACCEL_INTRO_VOID;
        wikrt_intro_op(cx, sumop);
        wikrt_consd(cx);
        goto tailcall;

    } break;

    case WIKRT_TYPE_BLOCK: {
        // I'll need to print '[' and push our block to our stack.
        bool const lazykf = (0 != (WIKRT_OPVAL_LAZYKF & opval_tag));
        wikrt_writer_push_stack(cx, w, lazykf);
        return;
    } break;

    case WIKRT_TYPE_SEAL: {
        // print contained value then the sealer token (via optok)
        char tokbuff[WIKRT_TOK_BUFFSZ];
        wikrt_unwrap_seal(cx, tokbuff);
        wikrt_intro_optok(cx, tokbuff);
        wikrt_consd(cx);
        goto tailcall;

    } break;

    case WIKRT_TYPE_STOW: {
        // Stowage will print as a token. I will also need to capture
        // substructure if our value is LAZYKF.
        fprintf(stderr, "todo: write stowed values\n");
        wikrt_set_error(cx, WIKRT_IMPL);
        return;
    } break;

    case WIKRT_TYPE_PEND: {
        // I'll need to somehow print the lazy continuation.
        fprintf(stderr, "todo: write pending values\n");
        wikrt_set_error(cx, WIKRT_IMPL);
        return;
    } break;

    case WIKRT_TYPE_TRASH: {  // e.g. []kf{&trash} for linear value
        wikrt_val* const pobj = wikrt_pval(cx, *wikrt_pval(cx, cx->val));
        assert(wikrt_otag_trash(pobj[0]) && (WIKRT_UNIT_INR == pobj[1]));
        pobj[0] = ((~0xFF) & pobj[0]) | WIKRT_OTAG_BLOCK; // write as block
        wikrt_intro_optok(cx, "&trash"); // after the block
        wikrt_consd(cx); 
        goto tailcall;
    } break;

    case WIKRT_TYPE_UNDEF:
    default: {
        wikrt_set_error(cx, WIKRT_ETYPE); 
        return;
    }
}}}

static inline bool wikrt_writer_small_step(wikrt_cx* cx, wikrt_writer_state* w)
{
    wikrt_sum_tag lr;
    wikrt_unwrap_sum(cx, &lr);
    if(WIKRT_INR == lr) {
        // End of current block.
        if(0 == w->depth) { return false; }
        wikrt_wrap_sum(cx, lr);
        wikrt_writer_pop_stack(cx, w); 
        return true; 
    } 

    wikrt_assocr(cx); // (op * (ops * (cont * (text * e))))
    wikrt_val* const pv = wikrt_pval(cx, cx->val);
    wikrt_val const opv = *pv;

    if(wikrt_smallint(opv)) {

        // basic operator
        wikrt_dropk(cx);
        wikrt_write_op(cx, w, (wikrt_op) wikrt_v2i(opv));

    } else { 

        // Expecting OTAG_OPVAL or OTAG_OPTOK.
        assert(wikrt_o(opv));
        wikrt_val* const popv = wikrt_pval(cx, opv);
        wikrt_val const otag = popv[0];
        wikrt_val const otype = LOBYTE(otag);

        if(WIKRT_OTAG_OPVAL == otype) {

            _Static_assert(!WIKRT_NEED_FREE_ACTION, "must free opval wrapper");
            pv[0] = popv[1]; // drop opval wrapper

            // force parsed texts to write as texts (mostly for empty text)
            bool const emtext = (0 != (WIKRT_OPVAL_EMTEXT & otag));

            if(emtext) { wikrt_write_text(cx, w); }
            else { wikrt_write_val(cx, w, otag); }

        } else {
            // This should be a 'sealed' unit value, 
            // i.e. OTAG_SEAL or OTAG_SEAL_SM.
            char tokbuff[WIKRT_TOK_BUFFSZ];
            wikrt_unwrap_seal(cx, tokbuff);
            wikrt_elim_unit(cx);
            
            wikrt_writer_putchar(cx, w, '{');
            wikrt_writer_putcstr(cx, w, tokbuff);
            wikrt_writer_putchar(cx, w, '}');
        } 
    }
    return !wikrt_has_error(cx);
}

static inline bool wikrt_cx_has_block(wikrt_cx* cx) {
    return wikrt_p(cx->val) && wikrt_blockval(cx, *wikrt_pval(cx, cx->val));
}

/* convert a block on the stack to text. */
void wikrt_block_to_text(wikrt_cx* cx)
{
    // Expecting (block * e).
    if(!wikrt_mem_reserve(cx, (2 * WIKRT_CELLSIZE))) { return; }
    if(!wikrt_cx_has_block(cx)) { wikrt_set_error(cx, WIKRT_ETYPE); return; }

    // (block * e) → (ops * (unit * (text * e)))
    wikrt_intro_r(cx, WIKRT_UNIT_INR); wikrt_wswap(cx); // initial texts (empty list)
    wikrt_intro_r(cx, WIKRT_UNIT); wikrt_wswap(cx);     // initial continuation (unit)
    wikrt_writer_open_block(cx);                        // block → ops

    wikrt_writer_state w;
    wikrt_writer_state_init(&w);

    // Write every operator. This may fail at some intermediate step,
    // but should succeed in general (i.e. there should be no type
    // errors, and context shouldn't grow much unless we add a lot
    // more accelerators).
    while(wikrt_writer_small_step(cx,&w)) { /* continue */ }

    if(wikrt_has_error(cx)) { return; }
    assert(0 == w.depth);
    wikrt_writer_flush(cx, &w);
    wikrt_dropk(cx); // drop ops
    wikrt_dropk(cx); // drop cont
    wikrt_reverse_text_chunks(cx); // repair text order
}

