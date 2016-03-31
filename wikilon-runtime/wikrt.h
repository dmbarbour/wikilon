/** This is the internal header for Wikilon runtime. 
 */
#pragma once

#include "wikilon-runtime.h"
#include "lmdb/lmdb.h"
#include "utf8.h"

#include <stdbool.h>
#include <stdint.h>
#include <stdlib.h>
#include <stddef.h>
#include <pthread.h>


/** Value references internal to a context. */
typedef uint32_t wikrt_val;
#define WIKRT_VAL_MAX UINT32_MAX

/** Corresponding signed integer type. */
typedef int32_t wikrt_int;
#define WIKRT_INT_MAX INT32_MAX

/** size within a context; documents a number of bytes */
typedef wikrt_val wikrt_size;
#define WIKRT_SIZE_MAX WIKRT_VAL_MAX

/** size buffered to one cell (i.e. 8 bytes for 32-bit context) */
typedef wikrt_size wikrt_sizeb;

/** address within a context; documents offset from origin. */
typedef wikrt_val wikrt_addr;

/** tag uses lowest bits of a value */
typedef wikrt_val wikrt_tag;

/** intermediate structure between context and env */
typedef struct wikrt_cxm wikrt_cxm;

/* LMDB-layer Database. */
typedef struct wikrt_db wikrt_db;

/** @brief Non-standard, proposed ABCD extensions and accelerators.
 *
 * This serves as a proving ground for proposed ABCD operators. These
 * ops are not yet standardized, not accepted by the runtime APIs, and
 * their opcode designations may change. Some may never become standards.
 *
 * Candidates for ABCD are driven by effective acceleration and streaming
 * bytecode compression. So we'll probably see a lot of stack manipulations
 * here, and eventually list-processing. Maybe a simple dictionary model or
 * a table processing model.
 */
typedef enum wikrt_opcode_ext
{ ABCD_INLINE       = 105 // 'i' - vr$c
, ABCD_TAILCALL     = 101 // 'e' - $c
, ABCD_SWAP         = 115 // 's' - vrwlc
, ABCD_SUM_SWAP     =  83 // 'S' - VRWLC
, ABCD_UNIT         = 117 // 'u' - vvrwlc (or `vs`)
, ABCD_INL          =  85 // 'U' - VVRWLC (or `VS`) 
} wikrt_opcode_ext;


// misc. constants and static functions
#define WIKRT_LNBUFF(SZ,LN) ((((SZ)+((LN)-1))/(LN))*(LN))
#define WIKRT_LNBUFF_POW2(SZ,LN) (((SZ) + ((LN) - 1)) & ~((LN) - 1))
#define WIKRT_CELLSIZE (2 * sizeof(wikrt_val))
#define WIKRT_CELLBUFF(sz) WIKRT_LNBUFF_POW2(sz, WIKRT_CELLSIZE)
#define WIKRT_PAGESIZE (1 << 17)
#define WIKRT_PAGEBUFF(sz) WIKRT_LNBUFF_POW2(sz, WIKRT_PAGESIZE)

// for lockfile, LMDB file
#define WIKRT_FILE_MODE (mode_t)(S_IRUSR | S_IWUSR | S_IRGRP | S_IWGRP)
#define WIKRT_DIR_MODE (mode_t)(WIKRT_FILE_MODE | S_IXUSR | S_IXGRP)

/** wikrt_val bits
 *
 * low bits xy0: small integers
 * low bits 001: tagged object
 * low bits 011: pointer to pair
 * low bits 101: pointer to pair in left
 * low bits 111: pointer to pair in right
 *
 * Unit represented as pair at address zero:
 *   unit          = 3
 *   unit in left  = 5
 *   unit in right = 7
 *
 * If we pump up to 64-bit words, I'd introduce tags specific to small
 * binaries, small texts, large numbers, and perhaps short array lists.
 *
 * For many tagged objects, we must use upper bits for extra data.
 *
 *   large integers: top ? bits for integer data? 20-30 bits?
 *     so maybe 1 bit for tag, 1 bit for sign?
 *   reference counts: no longer a separate object...
 */
#define WIKRT_O             1
#define WIKRT_P             3
#define WIKRT_PL            5
#define WIKRT_PR            7

// address zero 
#define WIKRT_VOID      WIKRT_O
#define WIKRT_UNIT      WIKRT_P
#define WIKRT_UNIT_INL  WIKRT_PL
#define WIKRT_UNIT_INR  WIKRT_PR

#define WIKRT_MASK_TAG      (WIKRT_CELLSIZE - 1)
#define WIKRT_MASK_ADDR     (~WIKRT_MASK_TAG)

static inline wikrt_addr wikrt_vaddr(wikrt_val v) { return (v & WIKRT_MASK_ADDR); }
static inline wikrt_tag  wikrt_vtag(wikrt_val v)  { return (v & WIKRT_MASK_TAG);  }
static inline wikrt_val  wikrt_tag_addr(wikrt_tag t, wikrt_addr a) { return (t | a); }
static inline bool wikrt_p(wikrt_val v) { return (WIKRT_P == wikrt_vtag(v)) && (0 != wikrt_vaddr(v)); } // pair, not unit
static inline bool wikrt_o(wikrt_val v) { return (WIKRT_O == wikrt_vtag(v)) && (0 != wikrt_vaddr(v)); } // object, not void

/** @brief small integers
 * 
 * Small integers range roughly plus or minus one billion. I imagine
 * this is enough for many common use cases, though perhaps not for
 * floating point or rational computations.
 */
#define WIKRT_SMALLINT_MAX  ((1 << 30) - 1)
#define WIKRT_SMALLINT_MIN  (- WIKRT_SMALLINT_MAX)
#define WIKRT_I2V(I) ((wikrt_val)(I << 1))
#define WIKRT_V2I(V) (((wikrt_int)V) >> 1)
static inline wikrt_val wikrt_i2v(wikrt_int i) { return WIKRT_I2V(i); }
static inline wikrt_int wikrt_v2i(wikrt_val v) { return WIKRT_V2I(v); }
static inline bool wikrt_i(wikrt_val v) { return (0 == (v & 1)); }

/** The zero integer value. */
#define WIKRT_IZERO WIKRT_I2V(0) 

/** @brief tagged objects 
 *
 * Currently, I just use the low byte of the tag word to indicate its
 * general type, and the next few bytes for for flags or data.
 * I'm unlikely to ever need more than a few dozen tags, so this
 * should be sufficient almost indefinitely.
 *
 * Tagged objects will never be used for basic products, mostly to
 * keep the logic simpler. At the moment, much code assumes that if
 * we aren't a tagged object, we're a basic pair structure.
 *
 * WIKRT_OTAG_DEEPSUM
 *
 *   For deep sums, the upper 24 bits are all data bits indicating sums
 *   of depth one to twelve: `10` for `in left` and `11` for `in right`.
 *   The second word in our sum is the value, which may reference another
 *   deep sum. Deep sums aren't necessarily packed as much as possible,
 *   but should be heuristically tight.
 *
 * WIKRT_OTAG_BIGINT
 *
 *   The upper 24 bits contain size and sign. Size is a number of 30-bit
 *   'digits' in the range 0..999999999 (a compact binary coded decimal).
 *   Sign requires one bit, so size is limited to 2^23-1 of these digits.
 *   (This corresponds to 75 million decimal digits.) Size is at least two
 *   words. The encoding is little-endian.
 *
 * WIKRT_OTAG_BLOCK  (block-header, list-of-ops)
 *
 *   This refers to a trivial block representation: a list of opcodes and
 *   quoted values. This representation is useful as an 'expanded' form 
 *   for various simplifications. The list of opcodes is represented in a
 *   manner that is transparently copyable and droppable.
 *
 *   WIKRT_OTAG_OPVAL 
 *
 *   This tag is used for the quotation operation, and also for partial
 *   evaluations. In the latter case, I'll include a tag bit to suppress
 *   substructural attribute checks for the contained values. This helps
 *   with lazy checking of substructural properties.
 *
 *   WIKRT_OTAG_OPTOK
 *
 *   This tag is used to carry unrecognized tokens. The tag bits include
 *   only a length for the token. This token is not NUL-terminated.
 *
 * WIKRT_OTAG_SEAL   (size, value, sealer)
 *
 *   Just a copy of the sealer token together with the value. The data 
 *   bits will indicate the size in bytes of the sealer token.
 *
 *   WIKRT_OTAG_SEAL_SM  (sealer, value)
 *
 *     An optimized representation for small discretionary seals, i.e.
 *     such as {:map}. Small sealers must start with ':' and have no
 *     more than four bytes. The sealer bytes are encoded in the tag's
 *     data bits and require no additional space. Developers should be
 *     encouraged to leverage this feature for performance.
 *   
 * WIKRT_OTAG_ARRAY 
 *
 *   An array has type `μL.((a*L)+b)` for some element type `a` and terminal
 *   type `b`. It's a compact representation of this type, using offsets into
 *   a region instead of a lazy linked list. The representation is
 *
 *      (hdr, size, buffer, next).
 *          `buffer` is wikrt_addr of first item
 *          `next` should be (_+b) or continue the list
 *          `size` is a number of items.
 *          `hdr` includes a bit for logical reversal   
 *
 *   This representation enables chunked-arrays, i.e. lists of array-like
 *   blocks that are still more efficient than conventional linked lists.
 *
 * WIKRT_OTAG_BINARY
 *  
 *   A binary is a specialized array type, elements restricted to the range
 *   of small numbers in 0..255. The basic array structure is the same.
 * 
 * WIKRT_OTAG_TEXT
 *
 *   A text is a specialized compact list type, using a utf-8 encoding and
 *   limited segment sizes. In particular, each segment may be no more than
 *   2^16 bytes. Our 'size' field is divided into a byte count and codepoint
 *   count, each ranging 1..2^16. Texts larger than 2^16 bytes are divided
 *   into multiple chunks. The chunked representation limits how much scanning
 *   is necessary to index or split a text, providing a simplistic index.
 *
 *   The size field is divided such that the size in bytes is the lower 16 
 *   bits. Note that we'll probably use the same limits even if we upgrade
 *   to 64-bit pointers (the overhead is marginal in either case).
 *
 * WIKRT_OTAG_STOWAGE
 *
 */

#define WIKRT_OTAG_BIGINT   78   /* N */
#define WIKRT_OTAG_DEEPSUM  83   /* S */
#define WIKRT_OTAG_BLOCK    91   /* [ */
#define WIKRT_OTAG_OPVAL    39   /* ' */
#define WIKRT_OTAG_OPTOK   123   /* { */
#define WIKRT_OTAG_SEAL     36   /* $ */
#define WIKRT_OTAG_SEAL_SM  58   /* : */
#define WIKRT_OTAG_ARRAY    86   /* V */
#define WIKRT_OTAG_BINARY   56   /* 8 */
#define WIKRT_OTAG_TEXT     34   /* " */
//#define WIKRT_OTAG_STOWAGE  64   /* @ */
#define LOBYTE(V) ((V) & 0xFF)

#define WIKRT_DEEPSUMR      3 /* bits 11 */
#define WIKRT_DEEPSUML      2 /* bits 10 */

#define WIKRT_BIGINT_DIGIT          1000000000
#define WIKRT_BIGINT_MAX_DIGITS  ((1 << 23) - 1)

// array, binary, text header
//   one bit for logical reversals
//   considering: free bytes to reduce fragmentation
#define WIKRT_ARRAY_REVERSE  (1 << 8)

// block header bits
#define WIKRT_BLOCK_RELEVANT (1 << 8)
#define WIKRT_BLOCK_AFFINE   (1 << 9)
#define WIKRT_BLOCK_PARALLEL (1 << 10)

// lazy substructure testing for quoted values
#define WIKRT_OPVAL_LAZYKF (1 << 8)

static inline bool wikrt_otag_bigint(wikrt_val v) { return (WIKRT_OTAG_BIGINT == LOBYTE(v)); }
static inline bool wikrt_otag_deepsum(wikrt_val v) { return (WIKRT_OTAG_DEEPSUM == LOBYTE(v)); }
static inline bool wikrt_otag_block(wikrt_val v) { return (WIKRT_OTAG_BLOCK == LOBYTE(v)); }
static inline bool wikrt_otag_seal(wikrt_val v) { return (WIKRT_OTAG_SEAL == LOBYTE(v)); }
static inline bool wikrt_otag_seal_sm(wikrt_val v) { return (WIKRT_OTAG_SEAL_SM == LOBYTE(v)); }
//static inline bool wikrt_otag_array(wikrt_val v) { return (WIKRT_OTAG_ARRAY == LOBYTE(v)); }
//static inline bool wikrt_otag_stowage(wikrt_val v) { return (WIKRT_OTAG_STOWAGE == LOBYTE(v)); }

static inline void wikrt_capture_block_ss(wikrt_val otag, wikrt_ss* ss)
{
    if(NULL != ss) { 
        if(WIKRT_BLOCK_RELEVANT & otag) { (*ss) |= WIKRT_SS_REL; }
        if(WIKRT_BLOCK_AFFINE & otag)   { (*ss) |= WIKRT_SS_AFF; }
    }
}
static inline bool wikrt_opval_hides_ss(wikrt_val otag) { return (0 == (WIKRT_OPVAL_LAZYKF & otag)); }

static inline wikrt_val wikrt_mkotag_bigint(bool positive, wikrt_size nDigits) {
    wikrt_val const tag_data = (nDigits << 1) | (positive ? 0 : 1);
    return (tag_data << 8) | WIKRT_OTAG_BIGINT;
}

/* Internal API calls. */
wikrt_err wikrt_copy_m(wikrt_cx*, wikrt_ss*, wikrt_cx*); 

// wikrt_vsize_ssp: return space required to deep-copy a value. 
//   Uses scratch space as a stack. May be bypassed if we can
//   easily determine that we have sufficient size.
wikrt_size wikrt_vsize_ssp(wikrt_cx* cx, wikrt_val v);
#define WIKRT_ALLOW_SIZE_BYPASS 0

// wikrt_vss_ssp: compute substructural attributes of a value.
//   Uses scratch space as a stack. This is only used when
//   dropping values for which we need the substructure.
wikrt_ss wikrt_vss_ssp(wikrt_cx* cx, wikrt_val v);

// wikrt_copy_r: assumes sufficient reserve space!
void wikrt_copy_r(wikrt_cx* lcx, wikrt_val lval, wikrt_ss* ss, wikrt_cx* rcx, wikrt_val* rval);

void wikrt_drop_v(wikrt_cx* cx, wikrt_val v, wikrt_ss*);

wikrt_err wikrt_wswap_v(wikrt_cx*, wikrt_val*);

wikrt_err wikrt_sum_wswap_v(wikrt_cx*, wikrt_val*);
wikrt_err wikrt_sum_zswap_v(wikrt_cx*, wikrt_val*);
wikrt_err wikrt_sum_assocl_v(wikrt_cx*, wikrt_val*);
wikrt_err wikrt_sum_assocr_v(wikrt_cx*, wikrt_val*);
wikrt_err wikrt_sum_swap_v(wikrt_cx*, wikrt_val*);

wikrt_err wikrt_wrap_sum_v(wikrt_cx*, bool inRight, wikrt_val*);
wikrt_err wikrt_unwrap_sum_v(wikrt_cx*, bool* inRight, wikrt_val*);
wikrt_err wikrt_expand_sum_v(wikrt_cx*, wikrt_val*); 
wikrt_err wikrt_wrap_seal_v(wikrt_cx*, char const* tok, wikrt_val*);
wikrt_err wikrt_unwrap_seal_v(wikrt_cx*, char* tokbuff, wikrt_val*);

wikrt_err wikrt_alloc_i32_v(wikrt_cx*, wikrt_val*, int32_t);
wikrt_err wikrt_alloc_i64_v(wikrt_cx*, wikrt_val*, int64_t);
wikrt_err wikrt_peek_i32_v(wikrt_cx*, wikrt_val const, int32_t*);
wikrt_err wikrt_peek_i64_v(wikrt_cx*, wikrt_val const, int64_t*);
wikrt_err wikrt_alloc_istr_v(wikrt_cx*, wikrt_val*, char const*, size_t strlen);
wikrt_err wikrt_peek_istr_v(wikrt_cx*, wikrt_val const, char* buff, size_t* strlen); 

wikrt_err wikrt_alloc_binary_v(wikrt_cx*, wikrt_val*, uint8_t const*, size_t);
wikrt_err wikrt_alloc_text_v(wikrt_cx*, wikrt_val*, char const*, size_t);
wikrt_err wikrt_read_binary_v(wikrt_cx*, wikrt_val*, uint8_t*, size_t*);
wikrt_err wikrt_read_text_v(wikrt_cx*, wikrt_val*, char* buff, size_t* bytes, size_t* chars);

wikrt_err wikrt_quote_v(wikrt_cx*, wikrt_val*);
wikrt_err wikrt_compose_v(wikrt_cx*, wikrt_val ab, wikrt_val bc, wikrt_val* out);
wikrt_err wikrt_block_attrib_v(wikrt_cx*, wikrt_val*, wikrt_val attribs);

wikrt_err wikrt_int_add_v(wikrt_cx*, wikrt_val a, wikrt_val b, wikrt_val* r);
wikrt_err wikrt_int_neg_v(wikrt_cx*, wikrt_val*);
wikrt_err wikrt_int_mul_v(wikrt_cx*, wikrt_val a, wikrt_val b, wikrt_val* r);
wikrt_err wikrt_int_div_v(wikrt_cx*, wikrt_val dividend, wikrt_val divisor, 
    wikrt_val* quotient, wikrt_val* remainder);
wikrt_err wikrt_int_cmp_v(wikrt_cx*, wikrt_val a, wikrt_ord*, wikrt_val b);

// return number of valid bytes and chars, up to given limits. Return
// 'true' only if all bytes were read or we stopped on a NUL terminal.
// Here 'szchars' may be NULL.
bool wikrt_valid_text_len(char const* s, size_t* szbytes, size_t* szchars);
bool wikrt_valid_key_len(char const* s, size_t* szBytes);

#if 0
wikrt_err wikrt_wswap_v(wikrt_cx*, wikrt_val*);
wikrt_err wikrt_zswap_v(wikrt_cx*, wikrt_val*);
wikrt_err wikrt_assocl_v(wikrt_cx*, wikrt_val*);
wikrt_err wikrt_assocr_v(wikrt_cx*, wikrt_val*);
wikrt_err wikrt_swap_v(wikrt_cx*, wikrt_val*);
wikrt_err wikrt_sum_wswap_v(wikrt_cx*, wikrt_val*);
wikrt_err wikrt_sum_zswap_v(wikrt_cx*, wikrt_val*);
wikrt_err wikrt_sum_distrib_v(wikrt_cx*, wikrt_val*);
wikrt_err wikrt_sum_factor_v(wikrt_cx*, wikrt_val*);
#endif

/** @brief Stowage address is 64-bit address. 
 *
 * The lowest four bits of the address are reserved for type flags
 * and specializations. But currently we only use `00kf` where k=1
 * iff relevant and f=1 iff affine.
 *
 * Addresses are allocated monotonically, and are never reused. In
 * theory, this means we might run out of addresses. In practice,
 * this is a non-issue: it would take tens of thousands of years
 * at least, writing as fast as we can.
 * 
 * Old stowage is incrementally GC'd while new data is written. And
 * while addresses are not reused, we will try to collapse common
 * structures into the same address.
 */ 
typedef uint64_t stowaddr;


bool wikrt_db_init(wikrt_db**, char const*, uint32_t dbMaxMB);
void wikrt_db_destroy(wikrt_db*);
void wikrt_db_flush(wikrt_db*);

struct wikrt_env { 
    wikrt_db           *db;
    wikrt_cx           *cxlist;     // linked list of context roots
    size_t              cxsize;     // size for each new context
    pthread_mutex_t     mutex;      // shared mutex for environment
};

static inline void wikrt_env_lock(wikrt_env* e) {
    pthread_mutex_lock(&(e->mutex)); }
static inline void wikrt_env_unlock(wikrt_env* e) {
    pthread_mutex_unlock(&(e->mutex)); }

void wikrt_add_cx_to_env(wikrt_cx* cx);
void wikrt_remove_cx_from_env(wikrt_cx* cx);

/** wikrt_cx internal state.
 *
 * I've decided to try a bump-pointer allocation with a semi-space
 * collection process. I'll also allocate downwards, i.e. loading
 * my heap in reverse order, since this reduces computation for
 * allocations (avoiding 'size' in most computations).
 */ 
struct wikrt_cx {
    // doubly-linked list of contexts in environment.
    wikrt_cx           *cxnext;
    wikrt_cx           *cxprev;
    wikrt_env          *env;

    // Memory
    void*               mem;        // active memory
    wikrt_addr          alloc;      // allocate towards zero
    wikrt_size          size;       // size of memory

    wikrt_val           val;    // primary value
    wikrt_val           txn;    // transaction data

    // semispace garbage collection.
    void*               ssp;    // for GC, scratch
    wikrt_size          compaction_size;    // memory after compaction
    wikrt_size          compaction_count;   // count of compactions

    // Other... maybe move error tracking here?
};
// just for sanity checks
#define WIKRT_CX_ROOT_CT 2

static inline wikrt_val* wikrt_paddr(wikrt_cx* cx, wikrt_addr addr) {
    return (wikrt_val*)(addr + ((char*)(cx->mem))); 
}
static inline wikrt_val* wikrt_pval(wikrt_cx* cx, wikrt_val v) {
    return wikrt_paddr(cx, wikrt_vaddr(v));
}

/* NOTE: Because I'm using a moving GC, I need to be careful about
 * how I represent and process allocations. Any allocation I wish 
 * to preserve must be represented in the root set. When copying an
 * array or similar, I'll need to be sure that all the contained 
 * values are copied upon moving them.
 *
 * Despite being a semi-space collector, I still use linear values
 * and mutation in place where feasible. So locality shouldn't be
 * a huge problem for carefully designed
 */

// copy from mem to ssp. swap ssp to mem.
void wikrt_mem_compact(wikrt_cx*);
static inline bool wikrt_mem_available(wikrt_cx* cx, wikrt_sizeb sz) { return (sz < cx->alloc); }
static inline bool wikrt_mem_reserve(wikrt_cx* cx, wikrt_sizeb sz) 
{
    if(wikrt_mem_available(cx,sz)) { return true; }
    wikrt_mem_compact(cx);
    return wikrt_mem_available(cx,sz);
} 

// Allocate a given amount of space, assuming sufficient space is reserved.
// This will not risk compacting and moving data. OTOH, if there isn't enough
// space we'll have a very severe bug.
static inline wikrt_addr wikrt_alloc_r(wikrt_cx* cx, wikrt_sizeb sz) 
{
    cx->alloc -= sz; 
    return cx->alloc; 
}

// introduce a cell from a known reserved space
static inline void wikrt_intro_r(wikrt_cx* cx, wikrt_val v) 
{
    wikrt_addr const a = wikrt_alloc_r(cx, WIKRT_CELLSIZE);
    wikrt_val* const pa = wikrt_paddr(cx, a);
    pa[0] = v;
    pa[1] = cx->val;
    cx->val = wikrt_tag_addr(WIKRT_P, a);
}

// allocate with potential memory compaction to make space
bool wikrt_alloc_c(wikrt_cx* cx, wikrt_size sz, wikrt_addr* addr);

// Other thoughts: It could be useful to keep free-lists regardless,
// and allocate from them only after the bump-pointer arena is full
// or when a valid sized element is at the head of the list. 

/* Recognize values represented entirely in the reference. */
static inline bool wikrt_copy_shallow(wikrt_val const v) {
    return (wikrt_i(v) || (0 == wikrt_vaddr(v)));
}

/* Test whether a valid utf-8 codepoint is okay for a token. */
static inline bool wikrt_token_char(uint32_t c) {
    bool const bInvalidChar =
        ('{' == c) || ('}' == c) ||
        isControlChar(c) || isReplacementChar(c);
    return !bInvalidChar;
}

/* Test whether a valid utf-8 codepoint is okay for a text. */
static inline bool wikrt_text_char(uint32_t c) {
    bool const bInvalidChar =
        (isControlChar(c) && (c != 10)) || 
        isReplacementChar(c);
    return !bInvalidChar;
}

