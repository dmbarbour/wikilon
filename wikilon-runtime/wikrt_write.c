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

// Thoughts: more compact representations? For example:
//
//   WIKRT_UNIT_INR, "\n~ (3 chars) where vvrwlcVVRWLC is 12
//
// I'm not sure it's worthwhile, though. Especially not once we
// include stream compression or eventual ABCD extensions. I'll just
// make sure extensions for tighter representations make the list.

// A reasonable chunk size for our texts.
//  must be smaller than WIKRT_OTAG_TEXT chunks (max size 0xFFFF)
#define WIKRT_WRITE_BUFFSZ (60 * 1000)
typedef struct wikrt_writer_state 
{
    // conditions for current block
    bool rel, aff, lazykf;
    wikrt_size depth;

    wikrt_size bytect;
    uint8_t    buff[WIKRT_WRITE_BUFFSZ];
} wikrt_writer_state;

// For bitfield tracking substructure
#define WIKRT_SS_LAZYKF (1 << 7)

static inline void wikrt_writer_state_init(wikrt_writer_state* w, wikrt_otag block_tag) 
{
    w->rel      = (0 != (WIKRT_BLOCK_RELEVANT & block_tag));
    w->aff      = (0 != (WIKRT_BLOCK_AFFINE & block_tag));
    w->lazykf   = false;
    w->depth    = 0;
    w->bytect   = 0;
}


// Basic ABC (elements in ASCII range)
#define OP(X) [OP_##X] = ABC_##X
static const uint8_t wikrt_op2abc_table[OP_COUNT] =
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
static void writer_INTRO_UNIT_LEFT(wikrt_cx*, wikrt_writer_state* w);
static void writer_SUM_SWAP(wikrt_cx*, wikrt_writer_state* w);
static void writer_INTRO_VOID_LEFT(wikrt_cx*, wikrt_writer_state* w);
static void writer_wrzw(wikrt_cx*, wikrt_writer_state* w);
static void writer_wzlw(wikrt_cx*, wikrt_writer_state* w);
static void writer_ANNO_TRACE(wikrt_cx*, wikrt_writer_state* w);
static void writer_ANNO_TRASH(wikrt_cx*, wikrt_writer_state* w);
static void writer_ANNO_LOAD(wikrt_cx*, wikrt_writer_state* w);
static void writer_ANNO_STOW(wikrt_cx*, wikrt_writer_state* w);
static void writer_ANNO_LAZY(wikrt_cx*, wikrt_writer_state* w);
static void writer_ANNO_FORK(wikrt_cx*, wikrt_writer_state* w);
static void writer_ANNO_JOIN(wikrt_cx*, wikrt_writer_state* w);
static void writer_ANNO_TEXT(wikrt_cx*, wikrt_writer_state* w);
static void writer_ANNO_BINARY(wikrt_cx*, wikrt_writer_state* w);

typedef void (*wikrt_writer)(wikrt_cx*, wikrt_writer_state*);
#define ACCEL(X) [ACCEL_##X] = writer_##X
static const wikrt_writer wikrt_accel_writers[OP_COUNT] =
 { ACCEL(TAILCALL),  ACCEL(INLINE)
 , ACCEL(PROD_SWAP), ACCEL(INTRO_UNIT_LEFT)
 , ACCEL(SUM_SWAP),  ACCEL(INTRO_VOID_LEFT)
 , ACCEL(wrzw),      ACCEL(wzlw)
 , ACCEL(ANNO_TRACE), ACCEL(ANNO_TRASH)
 , ACCEL(ANNO_LOAD), ACCEL(ANNO_STOW)
 , ACCEL(ANNO_LAZY), ACCEL(ANNO_FORK), ACCEL(ANNO_JOIN)
 , ACCEL(ANNO_TEXT), ACCEL(ANNO_BINARY) 
 };
_Static_assert((17 == WIKRT_ACCEL_COUNT), "missing accelerator writer?");
#undef ACCEL


static void wikrt_writer_flush(wikrt_cx* cx, wikrt_writer_state* w)
{
    if(0 == w->bytect) { return; } // quick exit

    wikrt_assocl(cx); wikrt_wswap(cx); // swizzle text outputs to top
    wikrt_cons_binary_chunk(cx, w->buff, w->bytect);
    wikrt_wswap(cx); wikrt_assocr(cx); // return texts to bottom

    // regardless of success or failure, clear the buffer
    w->bytect = 0;
}

static inline void wikrt_writer_putbyte(wikrt_cx* cx, wikrt_writer_state* w, uint8_t byte)
{
    if(w->bytect >= WIKRT_WRITE_BUFFSZ) { wikrt_writer_flush(cx, w); }
    w->buff[(w->bytect)++] = byte;
}

static inline void wikrt_writer_putcstr(wikrt_cx* cx, wikrt_writer_state* w, char const* const buff)
{
    _Static_assert(sizeof(uint8_t) == sizeof(char), "unsafe cast between char and uint8_t");
    uint8_t const* s = (uint8_t const*) buff;
    while(0 != (*s)) { wikrt_writer_putbyte(cx, w, *(s++)); }
}

static void wikrt_write_op(wikrt_cx* cx, wikrt_writer_state* w, wikrt_op op) 
{
    assert((OP_INVAL < op) && (op < OP_COUNT));
    uint8_t const abc_opchar = wikrt_op2abc_table[op];
    if(0 != abc_opchar) { 
        wikrt_writer_putbyte(cx, w, abc_opchar);
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
static void writer_INTRO_UNIT_LEFT(wikrt_cx* cx, wikrt_writer_state* w)
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
static void writer_INTRO_VOID_LEFT(wikrt_cx* cx, wikrt_writer_state* w)
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

static void writer_ANNO_TRACE(wikrt_cx* cx, wikrt_writer_state* w) { wikrt_writer_putcstr(cx, w, "{&trace}"); }   
static void writer_ANNO_TRASH(wikrt_cx* cx, wikrt_writer_state* w) { wikrt_writer_putcstr(cx, w, "{&trash}"); }
static void writer_ANNO_LOAD(wikrt_cx* cx, wikrt_writer_state* w)  { wikrt_writer_putcstr(cx, w, "{&load}"); }
static void writer_ANNO_STOW(wikrt_cx* cx, wikrt_writer_state* w)  { wikrt_writer_putcstr(cx, w, "{&stow}"); }
static void writer_ANNO_LAZY(wikrt_cx* cx, wikrt_writer_state* w)  { wikrt_writer_putcstr(cx, w, "{&lazy}"); }
static void writer_ANNO_FORK(wikrt_cx* cx, wikrt_writer_state* w)  { wikrt_writer_putcstr(cx, w, "{&fork}"); }
static void writer_ANNO_JOIN(wikrt_cx* cx, wikrt_writer_state* w)  { wikrt_writer_putcstr(cx, w, "{&join}"); }
static void writer_ANNO_TEXT(wikrt_cx* cx, wikrt_writer_state* w)   { wikrt_writer_putcstr(cx, w, "{&text}"); }
static void writer_ANNO_BINARY(wikrt_cx* cx, wikrt_writer_state* w) { wikrt_writer_putcstr(cx, w, "{&binary}"); }


// (empty ops * (stack * (text * e))) → (ops' * (stack' * (text * e)))
//   where `stack` is ((ss * ops') * stack')
//   and `ss` accumulates substructure
static inline void wikrt_writer_pop_stack(wikrt_cx* cx, wikrt_writer_state* w)
{
    _Static_assert((']' == 93), "assuming ']' is 93");
    assert((WIKRT_UNIT_INR == wikrt_pval(cx, cx->val)[0]) && (w->depth > 0));

    wikrt_writer_putbyte(cx, w, ']');

    // Write substructural attributes. 
    //  I might try to optimize cases like `kf$` or `kfvr$c` to
    //  suppress writing of irrelevant substructural attributes.
    if(w->rel) { wikrt_writer_putbyte(cx, w, ABC_REL); }
    if(w->aff) { wikrt_writer_putbyte(cx, w, ABC_AFF); }

    // initially (emptyOps * (stack * (text * e))) where
    //   stack is ((ss*ops')*stack')
    wikrt_elim_list_end(cx);
    wikrt_assocr(cx);
    wikrt_assocr(cx); // (ss * (ops' * (stack' * e)))

    // Access and update substructure for our parent.
    wikrt_val const ss_val = wikrt_pval(cx, cx->val)[0];
    assert(wikrt_smallint(ss_val));
    wikrt_drop(cx); // drop `ss`

    wikrt_int const ss = wikrt_v2i(ss_val);
    w->rel = (0 != (ss & WIKRT_SS_REL)) || (w->lazykf && w->rel);
    w->aff = (0 != (ss & WIKRT_SS_AFF)) || (w->lazykf && w->aff);
    w->lazykf = (0 != (ss & WIKRT_SS_LAZYKF));
    w->depth -= 1;

    // exiting with context (ops' * (stack' * (texts * e))). 
    // substructure `ss` is merged into wikrt_writer_state.
}

// ((block ops') * (ops * (stack * (texts * e)))) → 
//   (ops' * (stack' * (texts' * e)))
//  where stack' = ((ss*ops)*stack) and texts' includes the '['
static inline void wikrt_writer_push_stack(wikrt_cx* cx, wikrt_writer_state* w, bool block_lazykf) 
{
    _Static_assert(('[' == 91), "assuming '[' is 91");

    wikrt_otag const tag = wikrt_open_block_ops(cx); // (block ops') → ops'
    wikrt_wswap(cx); wikrt_zswap(cx); // (ops * (stack * (ops' * (texts * e))))

    // Write evaluation mode flags - `{&lazy}` or `{&fork}`.
    if(wikrt_otag_has_flags(tag, WIKRT_BLOCK_LAZY)) { 
        wikrt_intro_op(cx, ACCEL_ANNO_LAZY);    // `{&lazy}` after block 
        wikrt_cons(cx); 
    }
    if(wikrt_otag_has_flags(tag, WIKRT_BLOCK_FORK)) {
        wikrt_intro_op(cx, ACCEL_ANNO_FORK);    // `{&fork}` after block
        wikrt_cons(cx);
    }

    // Build the continuation stack. We'll return to it later.
    wikrt_int const ss = (w->rel ? WIKRT_SS_REL : 0) 
                       | (w->aff ? WIKRT_SS_AFF : 0) 
                       | (w->lazykf ? WIKRT_SS_LAZYKF : 0);

    wikrt_intro_smallval(cx, wikrt_i2v(ss));
    wikrt_assocl(cx); // ((ss*ops) * (stack * (ops' * ...))))
    wikrt_assocl(cx); // (((ss*ops)*stack) * (ops' * ...))) = (stack' * (ops' * ...))
    wikrt_wswap(cx);  // (ops' * (stack' * (texts * e)))

    w->rel = wikrt_otag_has_flags(tag, WIKRT_BLOCK_RELEVANT);
    w->aff = wikrt_otag_has_flags(tag, WIKRT_BLOCK_AFFINE);
    w->lazykf = block_lazykf;
    w->depth += 1;

    wikrt_writer_putbyte(cx, w, '[');
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
    wikrt_drop(cx); // drop the integer

    wikrt_writer_putbyte(cx, w, ABC_NUM);
    if('0' != buff[0]) { // except for zero (`#` not `#0`)
        // write the pseudo-literal integer. 
        bool const negate = ('-' == buff[0]);
        char const* s = buff + (negate ? 1 : 0);
        wikrt_writer_putcstr(cx, w, s);
        if(negate) { wikrt_writer_putbyte(cx, w, ABC_NEG); }
    } 
}

static inline bool wikrt_has_utf8(wikrt_cx* cx) {
    return wikrt_p(cx->val) 
        && wikrt_value_is_utf8(cx, *wikrt_pval(cx, cx->val));
}

static void wikrt_unwrap_utf8(wikrt_cx* cx) 
{
    wikrt_val* const v = wikrt_pval(cx, cx->val);
    bool const okType = wikrt_p(cx->val) &&  wikrt_value_is_utf8(cx, (*v));
    if(!okType) { wikrt_set_error(cx, WIKRT_ETYPE); return; }
    (*v) = wikrt_pobj(cx, (*v))[1];
}

// given (textval * (ops * (stack * (output text * e)))
//  write and drop the text value. We also need to add 
//  the start, terminal, and any newline-indent escapes.
static void wikrt_write_utf8(wikrt_cx* cx, wikrt_writer_state* w)
{
    _Static_assert(('"' == 34) && ('\n' == 10) && (' ' == 32) && ('~' == 126)
                  ,"assumed codes for embedded text");

    wikrt_accel_wzlw(cx); // (input text * ((ops*stack) * (output text * e)))
    wikrt_unwrap_utf8(cx); // require valid text

    // begin writing text
    wikrt_writer_putbyte(cx, w, '"');

    // process texts. I'll just assume valid utf8 behind the utf8 tag. But
    // I do need to inject all the newline escapes into the output string.
    // This requires reading everything.
    do {
        size_t len = 8 * 1000;
        uint8_t buff[len];
        wikrt_read_binary(cx, buff, &len);
        if(0 == len) { break; } // done reading
        for(size_t ix = 0; ix < len; ++ix) {
            uint8_t const b = buff[ix];
            wikrt_writer_putbyte(cx, w, b); 
            if('\n' == b) { wikrt_writer_putbyte(cx, w, ' '); } // escape newlines
        }
    } while(true);

    // terminate text
    wikrt_writer_putbyte(cx, w, '\n'); 
    wikrt_writer_putbyte(cx, w, '~'); 

    wikrt_elim_list_end(cx); // drop the text value
    wikrt_assocr(cx); // back to (ops * (stack * ...))
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
        wikrt_intro_op(cx, OP_PROD_ASSOCL); wikrt_consd(cx); // add `l` to ops
        wikrt_assocr(cx); // expand (a * b)
        wikrt_wrap_otag(cx, opval_tag); wikrt_consd(cx); // add (opval a) to ops
        goto tailcall; // process `b`
    } break;

    case WIKRT_TYPE_UNIT: {
        wikrt_elim_unit(cx);
        wikrt_write_op(cx, w, ACCEL_INTRO_UNIT_LEFT);
        return;
    } break;

    case WIKRT_TYPE_INT: { 
        wikrt_write_int(cx, w); 
        return;
    } break;

    case WIKRT_TYPE_SUM: {
        // utf8 text values are sums, but I'll embed them as texts.
        // To keep it simple, this is the only case for writing texts.
        if(wikrt_has_utf8(cx)) { wikrt_write_utf8(cx, w); return; }
        // TODO: consider how to preserve arrays or binaries.

        // For now, other sums will be written using the normal
        // construction mechanism, adding a `V` or `VVRWLC` after
        // the value. This isn't the most efficient rep for deep
        // sums. But it is straightforward.
        wikrt_sum_tag lr;
        wikrt_unwrap_sum(cx, &lr);
        wikrt_op const sumop = (WIKRT_INL == lr) ? OP_SUM_INTRO0 : ACCEL_INTRO_VOID_LEFT;
        wikrt_intro_op(cx, sumop); wikrt_consd(cx); // add sum constructor to ops
        goto tailcall; // process contained value
    } break;

    case WIKRT_TYPE_BLOCK: {
        // I'll need to print '[' and push our block to our stack.
        bool const lazykf = (0 != (WIKRT_OPVAL_LAZYKF & opval_tag));
        wikrt_writer_push_stack(cx, w, lazykf);
        return;
    } break;

    case WIKRT_TYPE_SEAL: {
        // print contained value then the sealer token.
        char tokbuff[WIKRT_TOK_BUFFSZ];
        wikrt_unwrap_seal(cx, tokbuff);
        wikrt_intro_optok(cx, tokbuff); wikrt_consd(cx);
        goto tailcall;
    } break;

    case WIKRT_TYPE_STOW: {
        // Stowage will print as a token. I will also need to capture
        // substructure if our value is LAZYKF.
        fprintf(stderr, "todo: write stowed values\n");
        wikrt_set_error(cx, WIKRT_IMPL);
        return;
    } break;

    case WIKRT_TYPE_FUTURE: {
        // treat quoted pending computations as linear.
        bool const lazykf = (0 != (WIKRT_OPVAL_LAZYKF & opval_tag));
        if(lazykf) { w->rel = true; w->aff = true; }

        // print a representation of the value, if feasible.
        wikrt_val* const pobj = wikrt_pval(cx, *wikrt_pval(cx, cx->val));
        if(wikrt_otag_pend(*pobj)) {

            // (pending (block * value)) → `value block {&lazy} $`
            wikrt_wswap(cx);
            wikrt_intro_op(cx, OP_APPLY);   wikrt_cons(cx); // add '$' to ops
            wikrt_intro_op(cx, ACCEL_ANNO_LAZY); wikrt_cons(cx); // add {&lazy} to ops
            wikrt_wswap(cx);

            wikrt_open_pending(cx); // extract the (block * value) pair.
            wikrt_assocr(cx); // (block * (value * (ops * ...)))
            wikrt_wrap_otag(cx, WIKRT_OTAG_OPVAL); wikrt_consd(cx); // add block to ops
    
            goto tailcall; // print the partially computed value
        } else {
            fprintf(stderr, "%s: unhandled pending value\n", __FUNCTION__);
            abort();
        }
    } break;

    case WIKRT_TYPE_TRASH: {  
        // Print as `val {&trash}`
        wikrt_intro_op(cx, ACCEL_ANNO_TRASH); wikrt_consd(cx); // {&trash}
        wikrt_val* const v = wikrt_pval(cx, cx->val);
        if(WIKRT_NORMAL_TRASH == (*v)) { 
            // print as zero value `#`
            (*v) = WIKRT_IZERO;
        } else {
            // print block with substructure (OTAG_TRASH → OTAG_BLOCK)
            wikrt_val* const pobj = wikrt_pobj(cx, (*v));
            assert(wikrt_o(*v) && wikrt_otag_trash(*pobj));
            (*pobj) = ((~0xFF) & *pobj) | WIKRT_OTAG_BLOCK;
        }
        goto tailcall; // print value
    } break;

    case WIKRT_TYPE_UNDEF:
    default: {
        if(!wikrt_has_error(cx)) {
            fprintf(stderr, "%s: unhandled value printer\n", __FUNCTION__);
            abort();
        }
    }
}}}

static inline bool wikrt_writer_small_step(wikrt_cx* cx, wikrt_writer_state* w)
{
    wikrt_sum_tag lr;
    wikrt_unwrap_sum(cx, &lr);
    if(WIKRT_INR == lr) {
        // End of current block.
        wikrt_wrap_sum(cx, lr); // simplify structural invariants
        if(0 == w->depth) { return false; }
        wikrt_writer_pop_stack(cx, w); 
        return true; 
    } 

    wikrt_assocr(cx); // (op * (ops * (cont * (text * e))))
    wikrt_val* const pv = wikrt_pval(cx, cx->val);
    wikrt_val const opv = *pv;

    if(wikrt_smallint(opv)) {

        // basic operator
        wikrt_drop(cx);
        wikrt_write_op(cx, w, (wikrt_op) wikrt_v2i(opv));

    } else { 

        // Expecting OTAG_OPVAL or a Sealed unit value.
        assert(wikrt_o(opv));
        wikrt_val* const popv = wikrt_pobj(cx, opv);
        if(wikrt_otag_opval(*popv)) {
            _Static_assert(!WIKRT_NEED_FREE_ACTION, "must free opval wrapper");
            pv[0] = popv[1]; // drop opval wrapper
            wikrt_write_val(cx, w, popv[0]); // 

            // Note: recycling the opval tag here could save about 50%
            // allocations for writing pairs and 33% for lists. So this is
            // a good location for a small free-list element.
        } else {
            // This should be a 'sealed' unit value, 
            // i.e. OTAG_SEAL or OTAG_SEAL_SM.
            char tokbuff[WIKRT_TOK_BUFFSZ];
            wikrt_unwrap_seal(cx, tokbuff);
            wikrt_elim_unit(cx);
            
            wikrt_writer_putbyte(cx, w, '{');
            wikrt_writer_putcstr(cx, w, tokbuff);
            wikrt_writer_putbyte(cx, w, '}');
        } 
    }
    return !wikrt_has_error(cx);
}

static inline bool wikrt_cx_has_block(wikrt_cx* cx) {
    return wikrt_p(cx->val) && wikrt_blockval(cx, *wikrt_pval(cx, cx->val));
}


wikrt_ss wikrt_block_to_text_ss(wikrt_cx* cx) 
{
    if(wikrt_has_error(cx)) { return 0; }

    // (block * e) → (ops * (unit * (text * e)))
    wikrt_intro_empty_list(cx); wikrt_wswap(cx); // initial output text (empty list)
    wikrt_intro_unit(cx); wikrt_wswap(cx); // initial continuation stack (unit)
    wikrt_otag const block_tag = wikrt_open_block_ops(cx); // block → ops

    wikrt_writer_state w;
    wikrt_writer_state_init(&w, block_tag);

    // Write every operator. This may fail at some intermediate step,
    // but should succeed in general (i.e. there should be no type
    // errors, and context shouldn't grow much unless we add a lot
    // more accelerators).
    while(wikrt_writer_small_step(cx,&w)) { /* continue */ }

    if(wikrt_has_error(cx)) { return 0; }

    assert(0 == w.depth);
    wikrt_writer_flush(cx, &w);
    wikrt_elim_list_end(cx); // drop empty toplevel ops
    wikrt_elim_unit(cx); // drop empty continuation stack
    wikrt_reverse_binary_chunks(cx);      // repair binary chunk order
    wikrt_wrap_otag(cx, WIKRT_OTAG_UTF8); // wrap the utf8 tag

    return (w.rel ? WIKRT_SS_REL : 0)
         | (w.aff ? WIKRT_SS_AFF : 0);
}

/* convert a block on the stack to text. */
void wikrt_block_to_text(wikrt_cx* cx)
{
    wikrt_block_to_text_ss(cx); // ignore toplevel substructure
}

