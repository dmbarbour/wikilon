// This file is just for `wikrt_text_to_block` and any variants.
// It involves a simplistic hand-written parser that recognizes
// tail-call opportunities.

#include <string.h>
#include <assert.h>
#include <stdio.h>
#include "wikrt.h"

typedef enum 
{ WIKRT_PARSE_OP  = 0
, WIKRT_PARSE_TXT = 1
, WIKRT_PARSE_TXT_LF = 2
, WIKRT_PARSE_TOK = 3
} wikrt_parse_type;

#define WIKRT_PARSE_BUFFSZ (WIKRT_CELLSIZE * 1024)
typedef struct {
    wikrt_parse_type type;        // special parser states
    wikrt_size       depth;       // stack size, hierarchical depth of `[`

    // For tokens and texts, use the intermediate buffer.
    wikrt_size bytect;
    uint8_t    buff[WIKRT_PARSE_BUFFSZ];
} wikrt_parse_state;

// How much to read in one step.
#define WIKRT_PARSE_READSZ (30 * 1000)

#define OP(X) [ABC_##X] = OP_##X
static wikrt_op const wikrt_abc2op_ascii_table[128] = 
{ OP(PROD_ASSOCL), OP(PROD_ASSOCR)
, OP(PROD_W_SWAP), OP(PROD_Z_SWAP)
, OP(PROD_INTRO1), OP(PROD_ELIM1)
, OP(SUM_ASSOCL), OP(SUM_ASSOCR)
, OP(SUM_W_SWAP), OP(SUM_Z_SWAP)
, OP(SUM_INTRO0), OP(SUM_ELIM0)
, OP(COPY), OP(DROP)
, OP(SP), OP(LF)
, OP(APPLY), OP(COMPOSE), OP(QUOTE), OP(REL), OP(AFF)
, OP(NUM)
, OP(D1), OP(D2), OP(D3), OP(D4), OP(D5)
, OP(D6), OP(D7), OP(D8), OP(D9), OP(D0)
, OP(ADD), OP(MUL), OP(NEG), OP(DIV), OP(GT)
, OP(CONDAP), OP(DISTRIB), OP(FACTOR), OP(MERGE), OP(ASSERT)
};
#undef OP

static inline wikrt_op wikrt_cp_to_op(uint32_t cp) {
    wikrt_op const result = (cp < 128) ? wikrt_abc2op_ascii_table[cp] : OP_INVAL;
    // fprintf(stderr, "%s: %d → %d\n", __FUNCTION__, cp, result);
    return result;
}

/* In addition to wikrt_parser_state, our context must hold some data.
 * In particular, it will hold:
 *
 *   (1) An object being constructed (text or block)
 *   (2) A stack of continuations to return to parsing
 *   (3) The text value that we're reading
 *
 * I can read big buffer chunks of text then process them, such that 
 * the above is in approximate order of access (for common sizes).
 * One option is to hold these in cx->val, e.g. as a triple:
 *
 *    (object * (stack * (text * e)))
 *
 */

static void wikrt_intro_parse(wikrt_cx* cx, wikrt_parse_state* p) 
{
    size_t const szAlloc = 2 * WIKRT_CELLSIZE;
    if(!wikrt_mem_reserve(cx, szAlloc)) { return; }
    wikrt_intro_unit(cx);       // stack
    wikrt_intro_empty_list(cx); // ops list
    p->type   = WIKRT_PARSE_OP; // expecting an operator
    p->depth  = 0;
    p->bytect = 0;
}

// Ops are initially constructed in a reverse-ordered list.
// I need to reverse this list ordering after all is done.
//   (reversed ops * e) → (ops * e)
static void wikrt_reverse_opslist(wikrt_cx* cx) 
{
    // Goal is to reverse the operations list.
    // The current implementation is non-allocating.
    wikrt_val hd = wikrt_pval(cx, cx->val)[0];
    wikrt_val tl = WIKRT_UNIT_INR;
    while(WIKRT_UNIT_INR != hd) 
    {
        assert(wikrt_pl(hd));
        // assuming cons list (cell per value), cf. wikrt_cons
        wikrt_val* const phd = wikrt_pval(cx, hd);
        wikrt_val const next_hd = phd[1];
        phd[1] = tl;
        tl = hd;
        hd = next_hd;
    }
    wikrt_pval(cx, cx->val)[0] = tl;
}

// Recognizing tailcalls is an essential optimization for
// large, loopy computations. At a bare minimum, I must
// recognize blocks ending with `$c]` in either the parser
// or the evaluator. I chose to do this in the parser. 
// Explicit simplification and optimization can do more.
static void wikrt_parser_accel_tailcall(wikrt_cx* cx)
{
    wikrt_val* const v = wikrt_pval(cx, cx->val);
    if(wikrt_pl(*v)) {
        wikrt_val* const x = wikrt_pval(cx, *v);
        if((WIKRT_I2V(OP_PROD_ELIM1) == x[0]) && wikrt_pl(x[1])) {
            wikrt_val* const y = wikrt_pval(cx, x[1]);
            if(WIKRT_I2V(OP_APPLY) == y[0]) {
                _Static_assert(!WIKRT_NEED_FREE_ACTION, "free cell when parsing tailcall");
                x[1] = y[1];
                x[0] = WIKRT_I2V(ACCEL_TAILCALL); // $c
            }
        }
    }
}

// Given (reversed ops * e), return (block * e).
//
// To simplify the evaluator and the tail-call optimization
// (without relying on a simplification pass), the parser will
// also recognize any `$c` tail-call opportunity and accelerate
// it. 
//  
static void wikrt_parser_finish_block(wikrt_cx* cx)
{
    if(wikrt_has_error(cx)) { return; }

    wikrt_parser_accel_tailcall(cx);
    wikrt_reverse_opslist(cx);  
    wikrt_wrap_otag(cx, WIKRT_OTAG_BLOCK);
}

static void wikrt_flush_parse_text(wikrt_cx* cx, wikrt_parse_state* p)
{
    if(0 == p->bytect) { return; } // nothing to flush
    wikrt_sizeb const szBuff  = wikrt_cellbuff(p->bytect);
    wikrt_sizeb const szAlloc = szBuff + (2 * WIKRT_CELLSIZE);
    if(wikrt_mem_reserve(cx,szAlloc) && wikrt_p(cx->val)) {
        // At the moment, we're building binary chunks.
        wikrt_addr const addr_buff = wikrt_alloc_r(cx, szBuff);
        memcpy(wikrt_paddr(cx, addr_buff), p->buff, p->bytect);

        // (OTAG_BINARY, next, (size-char, size-bytes), buffer)
        wikrt_addr const addr_hdr = wikrt_alloc_r(cx, (2 * WIKRT_CELLSIZE));
        wikrt_val* const phdr = wikrt_paddr(cx, addr_hdr);
        wikrt_val* const chunks = wikrt_pval(cx, cx->val);
        phdr[0] = WIKRT_OTAG_BINARY;
        phdr[1] = (*chunks);
        phdr[2] = p->bytect;
        phdr[3] = addr_buff;
        (*chunks) = wikrt_tag_addr(WIKRT_O, addr_hdr);
    }
    // clear buffer before continuing, even on flush failure
    p->bytect = 0;
}

static void wikrt_parser_write_char(wikrt_cx* cx, wikrt_parse_state* p, uint32_t cp) 
{
    _Static_assert((WIKRT_PARSE_BUFFSZ >= UTF8_MAX_CP_SIZE), "parse buffer too small to safely process text");
    if(p->bytect > (WIKRT_PARSE_BUFFSZ - UTF8_MAX_CP_SIZE)) { wikrt_flush_parse_text(cx, p); } 
    p->bytect += utf8_writecp_unsafe((p->buff + p->bytect), cp);
}

static void wikrt_step_parse_char(wikrt_cx* cx, wikrt_parse_state* p, uint32_t cp)
{ switch(p->type) {
    case WIKRT_PARSE_TOK: {
        _Static_assert(('}' == 125), "assuming '}' is 125"); 
        if('}' == cp) {

            p->buff[p->bytect] = 0; // so we have a C string.
            wikrt_intro_optok(cx, (char const*) p->buff);
            wikrt_cons(cx);
            p->type = WIKRT_PARSE_OP;

        } else { 
            _Static_assert((WIKRT_PARSE_BUFFSZ >= (UTF8_MAX_CP_SIZE + WIKRT_TOK_BUFFSZ)), 
                "assuming buffer sufficient for slightly oversized tokens");

            size_t const next_toksz = p->bytect + utf8_writecp_size(cp);
            if(wikrt_token_char(cp) && (next_toksz < WIKRT_TOK_BUFFSZ)) {
                utf8_writecp_unsafe((p->buff + p->bytect), cp);
                p->bytect = next_toksz;
            } else {
                wikrt_set_error(cx, WIKRT_ETYPE); // invalid token
            }
        }
    } break;
    case WIKRT_PARSE_TXT_LF: {
        _Static_assert((10 == '\n'), "assuming '\\n' is 10, LF");
        _Static_assert((32 == ' '), "assuming ' ' is 32");
        _Static_assert((126 == '~'), "assuming '~' is 126");

        // Prior LF must be followed by SP or ~
        if(' ' == cp) { // SP escapes prior LF
            wikrt_parser_write_char(cx, p, '\n');
            p->type = WIKRT_PARSE_TXT;
        } else if('~' == cp) { // ~ terminates text 
            wikrt_flush_parse_text(cx, p);
            wikrt_reverse_binary_chunks(cx);
            wikrt_wrap_otag(cx, WIKRT_OTAG_UTF8); // text is (utf8, binary).
            wikrt_wrap_otag(cx, WIKRT_OTAG_OPVAL); // embed text as opval
            wikrt_accel_wrzw(cx); // expand stack below text
            wikrt_cons(cx); // add text opval to ops
            p->type = WIKRT_PARSE_OP;
        } else { 
            wikrt_set_error(cx, WIKRT_ETYPE); 
        } 
    } break;
    case WIKRT_PARSE_TXT: {
        _Static_assert((10 == '\n'), "assuming '\\n' is 10, LF");

        // Within an embedded ABC text.
        if('\n' == cp) { p->type = WIKRT_PARSE_TXT_LF; }
        else if(wikrt_text_char(cp)) { wikrt_parser_write_char(cx, p, cp); }
        else { wikrt_set_error(cx, WIKRT_ETYPE); }

    } break;
    case WIKRT_PARSE_OP: {
        // forbid localization other than the ASCII subset of Unicode...
        _Static_assert(('[' == 91), "assuming '[' is 91");
        _Static_assert((']' == 93), "assuming ']' is 93");
        _Static_assert(('{' == 123), "assuming '{' is 123");
        _Static_assert(('"' == 34), "assuming '\"' is 34");


        if('[' == cp) {
            // Begin a new block. We have (ops * (stack * (text * e))).
            wikrt_assocl(cx); // ((ops*stack)*(text*e)) - add ops added to stack
            wikrt_intro_empty_list(cx); // new ops list

            p->depth += 1;
            p->type = WIKRT_PARSE_OP;

        } else if(']' == cp) {
            if(p->depth < 1) { wikrt_set_error(cx, WIKRT_ETYPE); return; }
           
            wikrt_parser_finish_block(cx); // reversed ops → block
            wikrt_wrap_otag(cx, WIKRT_OTAG_OPVAL); // (opval block ops))
            wikrt_accel_wrzw(cx); // expand stack below block opval
            wikrt_cons(cx);  // add block opval to parent's ops

            p->depth -= 1;
            p->type = WIKRT_PARSE_OP;

        } else if('{' == cp) {

            p->type = WIKRT_PARSE_TOK;
            p->bytect = 0;

        } else if('"' == cp) {
            wikrt_assocl(cx); // ((ops * stack) * (in-text * e))
            wikrt_intro_empty_list(cx); // initial text binary 
            p->type = WIKRT_PARSE_TXT;
            p->bytect = 0;

        } else {

            wikrt_intro_op(cx, wikrt_cp_to_op(cp));
            wikrt_cons(cx);

        } 
    } break;
    default: {
        fprintf(stderr, "%s: invalid parser state (%d)\n", __FUNCTION__, p->type);
        abort();
    } 
}}

/* Process a given buffer of text. 
 *
 * For the moment, my goal is to get this working correctly and keep it simple.
 * However, this will hurt performance in some cases - such as fast processing
 * of large texts. This assumes our text is valid utf8.
 */
static inline void wikrt_step_parse(wikrt_cx* cx, wikrt_parse_state* p, uint8_t const* s, size_t buffsz) 
{
    uint8_t const* const s_end = s + buffsz;
    while(s_end != s) { wikrt_step_parse_char(cx, p, utf8_step_unsafe(&s)); }
}

// (reversed ops * (unit * (emptyText * e))) → (block * e)
static void wikrt_fini_parse(wikrt_cx* cx, wikrt_parse_state* p) 
{
    bool const parseStateSeemsOk = (0 == p->depth) && (WIKRT_PARSE_OP == p->type) && !wikrt_has_error(cx);
    if(!parseStateSeemsOk) { wikrt_set_error(cx, WIKRT_ETYPE); return; }

    wikrt_parser_finish_block(cx); // reversed ops → block
    wikrt_wswap(cx);     // (unit * (block * (text * e)))
    wikrt_elim_unit(cx); // (block * (text * e))
    wikrt_wswap(cx);     // (text * (block * e))
    wikrt_elim_list_end(cx); // (block * e)
}

/* Construct a block value from a text. 
 *
 * The current implementation will construct a basic linked list. Long
 * term, I'll need to compact code more tightly for performance, and 
 * eventually eliminate code copying within tight loops. I may need to
 * move tight inner loops into a special non-copying space. 
 */
void wikrt_text_to_block(wikrt_cx* cx)
{
    _Static_assert(sizeof(char) == sizeof(uint8_t), "assuming safe cast between char* and uint8_t*");

    if(!wikrt_p(cx->val)) { wikrt_set_error(cx, WIKRT_ETYPE); return; }

    wikrt_parse_state p;
    wikrt_intro_parse(cx, &p);

    // read and process the text!
    size_t bytes_read;
    char buff[WIKRT_PARSE_READSZ];
    do { // cx has (ops * (stack * (text * e)))
        wikrt_assocl(cx); wikrt_wswap(cx); // swizzle text to top
        bytes_read = WIKRT_PARSE_READSZ;
        wikrt_read_text(cx, buff, &bytes_read, NULL); 
        wikrt_wswap(cx); wikrt_assocr(cx); // swizzle text to bottom
        wikrt_step_parse(cx, &p, (uint8_t const*) buff, bytes_read);
    } while(0 != bytes_read);

    wikrt_fini_parse(cx, &p);
}


// utility functions

void wikrt_intro_optok(wikrt_cx* cx, char const* tok) 
{
    // A 'token' operator is generally represented as a sealed unit
    // value. However, a special case is made for resource identifiers
    // or annotations with specific handlers (which reduce to a single
    // opcode).
    if('\'' == *tok) {
        _Static_assert((39 == '\''), "prefix on resource identifier in ASCII/UTF8");
        // stowage resource identifiers 
        wikrt_intro_sv(cx, tok);
        wikrt_wrap_otag(cx, WIKRT_OTAG_OPVAL);
    } else {
        wikrt_intro_unit(cx);
        wikrt_wrap_seal(cx, tok);
    }
}

void wikrt_intro_op(wikrt_cx* cx, wikrt_op op) 
{
    bool const validOp = ((OP_INVAL < op) && (op < OP_COUNT));
    if(!validOp) { wikrt_set_error(cx, WIKRT_ETYPE); return; }
    wikrt_intro_smallval(cx, wikrt_i2v(op));
}

// Incremental construction of large binary and text data.
void wikrt_reverse_binary_chunks(wikrt_cx* cx) 
{
    if(!wikrt_p(cx->val)) { wikrt_set_error(cx, WIKRT_ETYPE); }
    if(wikrt_has_error(cx)) { return; }

    // Given a sequence of binary chunks in reverse order
    //   each of form (header, next, size, buffer)
    // reverse the chunk ordering.

    wikrt_val hd = wikrt_pval(cx, cx->val)[0];
    wikrt_val binary = WIKRT_UNIT_INR;
    while(WIKRT_UNIT_INR != hd) {
        // expecting (binary, next, size, buffer) objects (strictly)
        assert(wikrt_value_is_compact_binary(cx, hd));
        wikrt_val* const phd = wikrt_pobj(cx, hd);
        wikrt_val const next = phd[1];
        phd[1] = binary;
        binary = hd;
        hd = next;
    }
    wikrt_pval(cx, cx->val)[0] = binary;
}


