/** This is the internal header for Wikilon runtime. 
 */
#pragma once

#include "wikilon-runtime.h"
#include "utf8.h"

#include <stdbool.h>
#include <stdint.h>
#include <stdlib.h>
#include <stddef.h>
#include <limits.h>
#include <pthread.h>

_Static_assert((INTPTR_MIN == INT64_MIN) && 
               (INTPTR_MAX == INT64_MAX) &&
               (UINTPTR_MAX == UINT64_MAX)
              ,"Wikilon runtime expects a 64-bit system");
_Static_assert((sizeof(uintptr_t) == 8), "Expecting eight byte pointers.");
_Static_assert((sizeof(uint8_t) == 1), "Expecting one byte octets.");
_Static_assert((sizeof(uint8_t) == sizeof(char)), "Expecting safe cast between char and uint8_t");

/** Value references internal to a context. */
typedef uintptr_t wikrt_val;
#define WIKRT_VAL_MAX UINTPTR_MAX


/** Corresponding signed integer type. */
typedef intptr_t wikrt_int;
#define WIKRT_INT_MAX INTPTR_MAX

/** size within a context; documents a number of bytes */
typedef wikrt_val wikrt_size;
#define WIKRT_SIZE_MAX WIKRT_VAL_MAX

/** size buffered to one two-word cell. 16 bytes for a 64-bit context. */
typedef wikrt_size wikrt_sizeb;

/** address within a context; documents offset from origin. */
typedef wikrt_val wikrt_addr;

/** tag uses lowest bits of a value */
typedef wikrt_val wikrt_tag;

/** otag is first value in a WIKRT_O object */
typedef wikrt_val wikrt_otag;

/** intermediate structure between context and env */
typedef struct wikrt_cxm wikrt_cxm;

/* LMDB-layer Database. */
typedef struct wikrt_db wikrt_db;

/** @brief Internal opcodes. 
 *
 * Internal opcodes include ABC's primitive 42 opcodes in addition to
 * accelerators. Internal opcodes are encoded for adjacency in jump
 * tables rather than for convenient textual representation.
 */
typedef enum wikrt_op
{ OP_INVAL = 0

// Block: Primitive ABC (42 ops, codes 1..42)
, OP_SP, OP_LF
, OP_PROD_ASSOCL, OP_PROD_ASSOCR
, OP_PROD_W_SWAP, OP_PROD_Z_SWAP
, OP_PROD_INTRO1, OP_PROD_ELIM1
, OP_SUM_ASSOCL, OP_SUM_ASSOCR
, OP_SUM_W_SWAP, OP_SUM_Z_SWAP
, OP_SUM_INTRO0, OP_SUM_ELIM0
, OP_COPY, OP_DROP
, OP_APPLY, OP_COMPOSE, OP_QUOTE, OP_REL, OP_AFF
, OP_NUM
, OP_D1, OP_D2, OP_D3, OP_D4, OP_D5
, OP_D6, OP_D7, OP_D8, OP_D9, OP_D0
, OP_ADD, OP_MUL, OP_NEG, OP_DIV, OP_GT
, OP_CONDAP, OP_DISTRIB, OP_FACTOR, OP_MERGE, OP_ASSERT

// Block: Accelerators
, ACCEL_TAILCALL    // $c
, ACCEL_INLINE      // vr$c
, ACCEL_PROD_SWAP   // vrwlc
, ACCEL_INTRO_UNIT_LEFT  // vvrwlc
, ACCEL_SUM_SWAP         // VRWLC
, ACCEL_INTRO_VOID_LEFT  // VVRWLC

// deeper data plumbing
, ACCEL_wrzw  // (a * ((b * c) * d)) → (a * (b * (c * d)))
, ACCEL_wzlw  // (a * (b * (c * d))) → (a * ((b * c) * d))

// common data plumbing
//, ACCEL_rw // ((a * b) * c) → (b * (a * c))
//, ACCEL_wl // (b * (a * c)) → ((a * b) * c)

// potential future accelerators?
//  stack-level manipulations
//  fixpoint functions (plus future support for non-copying loopy code)


// Misc.
, OP_COUNT  // how many ops are defined?
} wikrt_op;
#define WIKRT_ACCEL_START ACCEL_TAILCALL
#define WIKRT_ACCEL_COUNT (OP_COUNT - WIKRT_ACCEL_START)

typedef enum wikrt_ss
{ WIKRT_SS_NORM = 0
, WIKRT_SS_REL  = 1<<0
, WIKRT_SS_AFF  = 1<<1
, WIKRT_SS_PEND = 1<<2
} wikrt_ss;

static inline bool wikrt_ss_copyable(wikrt_ss ss)  { return (0 == (ss & (WIKRT_SS_AFF | WIKRT_SS_PEND))); }
static inline bool wikrt_ss_droppable(wikrt_ss ss) { return (0 == (ss & (WIKRT_SS_REL | WIKRT_SS_PEND))); }


// misc. constants and static functions
#define WIKRT_LNBUFF(SZ,LN) ((((SZ)+((LN)-1))/(LN))*(LN))
#define WIKRT_LNBUFF_POW2(SZ,LN) (((SZ) + ((LN) - 1)) & ~((LN) - 1))
#define WIKRT_CELLSIZE (2 * sizeof(wikrt_val))
#define WIKRT_CELLBUFF(sz) WIKRT_LNBUFF_POW2(sz, WIKRT_CELLSIZE)

static inline wikrt_sizeb wikrt_cellbuff(wikrt_size n) { return WIKRT_CELLBUFF(n); }

// for lockfile, LMDB file
#define WIKRT_FILE_MODE (mode_t)(S_IRUSR | S_IWUSR | S_IRGRP | S_IWGRP)
#define WIKRT_DIR_MODE (mode_t)(WIKRT_FILE_MODE | S_IXUSR | S_IXGRP)

#define WIKRT_EDIV0 WIKRT_ETYPE


/** wikrt_val bits (64-bit runtime edition)
 *
 * Goals: eighteen-digit small integers, eliminate checking of
 * NULL pointers, and favor absolute pointer values. A pair in
 * left or right should be encoded in tag bits. Use of the zero
 * address should probably cause a segfault, so we quickly learn
 * of any uninitialized data.
 *
 * It would be convenient if we can simply look at, say, the low
 * few bits and discriminate immediately with a case statement
 * for deep copies, size computations, etc..
 *
 * Candidate 'Minimal Bitrep':
 *
 * prefix bit 0: pointers
 *  00 tagged objects
 *  01 pair value
 *  10 pair in left
 *  11 pair in right
 * prefix bit 1: small constants
 *  00 small integers
 *  01 constant
 *  10 constant in left
 *  11 constant in right
 *
 * The prefix bit could be on right or left. I chose left (value 4).
 * The small constants currently include only unit, encoded as zero.
 *
 * This candidate does not attempt to optimize sums beyond what is
 * essential for compact lists and booleans. The set of constants
 * could be large, however, e.g. small texts up to a few bytes. But
 * anything that might be affine or relevant should be in WIKRT_O.
 *
 */
#define WIKRT_O     0
#define WIKRT_P     1
#define WIKRT_PL    2
#define WIKRT_PR    3
#define WIKRT_I     4
#define WIKRT_U     5
#define WIKRT_UL    6
#define WIKRT_UR    7

// for static assertions
#define WIKRT_USING_MINIMAL_BITREP 1

// WIKRT_I, WIKRT_U, WIKRT_UL, WIKRT_UR are 'shallow copy'.
#define wikrt_copy_shallow(V) (4 & (V)) 

// Small constants: 
// 
// Unit will be encoded as the constant zero.


#define WIKRT_UNIT        WIKRT_U
#define WIKRT_UNIT_INL    WIKRT_UL
#define WIKRT_UNIT_INR    WIKRT_UR

#define WIKRT_REF_MASK_TAG    (7)     
#define WIKRT_REF_MASK_ADDR   (~WIKRT_REF_MASK_TAG)
#define wikrt_vaddr(V) ((V) & WIKRT_REF_MASK_ADDR)
#define wikrt_vtag(V) ((V) & WIKRT_REF_MASK_TAG)
#define wikrt_tag_addr(TAG,ADDR) ((TAG) | (ADDR))

// I'll probably want to deprecate these
static inline bool wikrt_p(wikrt_val v) { return (WIKRT_P == wikrt_vtag(v)); }
static inline bool wikrt_pl(wikrt_val v) { return (WIKRT_PL == wikrt_vtag(v)); }
static inline bool wikrt_pr(wikrt_val v) { return (WIKRT_PR == wikrt_vtag(v)); }
static inline bool wikrt_o(wikrt_val v) { return (WIKRT_O == wikrt_vtag(v)); }

/** @brief small integers (64 bit Wikilon Runtime)
 *
 * Small integers are indicated by low bits `100` and guarantee eighteen good
 * decimal digits. Wikilon runtime probably won't take the effort to support
 * larger integers any time soon.
 *
 */
#define WIKRT_SMALLINT_MAX  (999999999999999999)
#define WIKRT_SMALLINT_MIN  (- WIKRT_SMALLINT_MAX)
#define WIKRT_I2V(I) (WIKRT_I | (((wikrt_val)I) << 3))
#define WIKRT_V2I(V) (((wikrt_int)V) >> 3)
static inline wikrt_val wikrt_i2v(wikrt_int i) { return WIKRT_I2V(i); }
static inline wikrt_int wikrt_v2i(wikrt_val v) { return WIKRT_V2I(v); }
static inline bool wikrt_smallint(wikrt_val v) { return (WIKRT_I == wikrt_vtag(v)); }


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
 * WIKRT_OTAG_BIGINT (disabled for 64-bit)
 *   Disabled: arbitrary width integers are cool, but I probably need to
 *   use a library to get them working correctly, quickly. This is more
 *   than I want to do at the moment. Anyhow, it shouldn't be difficult
 *   to find a good representation for big numbers.
 *
 * WIKRT_OTAG_BLOCK  (block-header, list-of-ops)
 *
 *   This refers to a trivial block representation: a list of opcodes, 
 *   tokens, and captured values. Opcodes are represented by small 
 *   integers and include accelerators. Quoted values are also used
 *   for embedded blocks or texts. Tag bits in the block header indicate
 *   affine and relevance substructural attributes.
 *
 *   WIKRT_OTAG_OPVAL 
 *
 *   This tag is used for embedded blocks and texts, for fast quotation
 *   of values, and for partial evaluation efforts. One tag bit enables
 *   the substructure of the value to be promoted to the containing block
 *   so we don't need to compute it immediately upon quoting a value. 
 *
 *   WIKRT_OTAG_SEAL(_SM)
 * 
 *   Tokens not recognized as specialized operators will be represented 
 *   using sealed 'unit' values. The cost of doing so is marginal (one
 *   extra word per token). And this means there is no extra allocation
 *   for applying sealer tokens.
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
 *      (hdr, next, size, buffer).
 *          `buffer` is wikrt_addr of first item
 *          `size` is a number of items.
 *          `next` should be (_+b) or continue the list
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
 * WIKRT_OTAG_UTF8
 *
 *   (utf8, binary). Wraps a binary list terminating in unit to treat it as
 *   an embedded utf8 text. The binary must be known to be valid utf8 text.
 *
 * WIKRT_OTAG_STOWAGE (planned)
 *
 *    I must track references to stowed resources. Note: I might want to
 *    couple stowage with value sealers.
 *
 * WIKRT_OTAG_ERROR (potential)
 *
 *    It might be useful to wrap a value after a type error occurs, perhaps
 *    include flag bits suggesting the nature of this type error and which 
 *    type was expected instead. This might be understood as a specialized
 *    value sealer.
 *
 * WIKRT_OTAG_TRASH
 *
 *    A placeholder for a value that has been marked as garbage, allowing
 *    its memory to be recycled. Preserves substructural properties. Any
 *    attempt to access or observe the value results in a type error, but
 *    it may be deleted or copied like the original value.
 *
 * WIKRT_OTAG_PEND
 *
 *    Just a tagged (block * value) pair, representing an incomplete
 *    or latent evaluation. Parallel values may receive special attention
 *    later, but I'll probably use a different OTAG.
 *
 * I'd like to eventually explore logical copies that work with the idea of
 * linear or affine values, but I haven't good ideas for this yet - use of
 * sum types might work.
 */

//#define WIKRT_OTAG_BIGINT   78   /* N */

#define WIKRT_OTAG_DEEPSUM  43   /* + */
#define WIKRT_OTAG_BLOCK    91   /* [ */
#define WIKRT_OTAG_OPVAL    39   /* ' */
#define WIKRT_OTAG_SEAL     123  /* { */
#define WIKRT_OTAG_SEAL_SM  58   /* : */
#define WIKRT_OTAG_ARRAY    86   /* V */
#define WIKRT_OTAG_BINARY   56   /* 8 */
#define WIKRT_OTAG_UTF8     34   /* " */
//#define WIKRT_OTAG_STOWAGE  64   /* @ */
#define WIKRT_OTAG_TRASH    95   /* _ */
#define WIKRT_OTAG_PEND     126  /* ~ */
#define LOBYTE(V) ((V) & 0xFF)

#define WIKRT_DEEPSUMR      3 /* bits 11 */
#define WIKRT_DEEPSUML      2 /* bits 10 */

// array, binary, text header
//   one bit for logical reversals
//   considering: free bytes to reduce fragmentation
#define WIKRT_ARRAY_REVERSE  (1 << 8)

// block header bits
#define WIKRT_BLOCK_RELEVANT (1 << 8)  // forbid drop
#define WIKRT_BLOCK_AFFINE   (1 << 9)  // forbid copy
#define WIKRT_BLOCK_LAZY     (1 << 10) // when applied, create a pending computation

// block inherits substructural attributes from contained value
// this is used for quoted values within a block.
#define WIKRT_OPVAL_LAZYKF      (1 << 8)  

// Currently, support for large integers is disabled. This enables
// simple assertions
#define WIKRT_HAS_BIGINT 0

// render text as a basic list of numbers, to avoid O(N^2) rendering issues
#define WIKRT_OPVAL_ASLIST      (1 << 9)  

static inline bool wikrt_otag_deepsum(wikrt_otag v) { return (WIKRT_OTAG_DEEPSUM == LOBYTE(v)); }
static inline bool wikrt_otag_block(wikrt_otag v) { return (WIKRT_OTAG_BLOCK == LOBYTE(v)); }
static inline bool wikrt_otag_opval(wikrt_otag v) { return (WIKRT_OTAG_OPVAL == LOBYTE(v)); }
static inline bool wikrt_otag_seal(wikrt_otag v) { return (WIKRT_OTAG_SEAL == LOBYTE(v)); }
static inline bool wikrt_otag_seal_sm(wikrt_otag v) { return (WIKRT_OTAG_SEAL_SM == LOBYTE(v)); }
static inline bool wikrt_otag_binary(wikrt_otag v) { return (WIKRT_OTAG_BINARY == LOBYTE(v)); }
static inline bool wikrt_otag_array(wikrt_otag v) { return (WIKRT_OTAG_ARRAY == LOBYTE(v)); }
static inline bool wikrt_otag_utf8(wikrt_otag v) { return (WIKRT_OTAG_UTF8 == LOBYTE(v)); }
static inline bool wikrt_otag_trash(wikrt_otag v) { return (WIKRT_OTAG_TRASH == LOBYTE(v)); }
static inline bool wikrt_otag_pend(wikrt_otag v) { return (WIKRT_OTAG_PEND == LOBYTE(v)); }
//static inline bool wikrt_otag_stowage(wikrt_val v) { return (WIKRT_OTAG_STOWAGE == LOBYTE(v)); }

static inline void wikrt_capture_block_ss(wikrt_val otag, wikrt_ss* ss)
{
    if(NULL != ss) { 
        if(WIKRT_BLOCK_RELEVANT & otag) { (*ss) |= WIKRT_SS_REL; }
        if(WIKRT_BLOCK_AFFINE & otag)   { (*ss) |= WIKRT_SS_AFF; }
    }
}
static inline bool wikrt_opval_hides_ss(wikrt_val otag) { return (0 == (WIKRT_OPVAL_LAZYKF & otag)); }

/* Internal API calls. */
void wikrt_copy_m(wikrt_cx*, wikrt_ss*, bool moving_copy, wikrt_cx*); 

// wikrt_vsize: return allocation required to deep-copy a value. 
//   Use a given stack space for recursive structure tasks.
wikrt_size wikrt_vsize(wikrt_cx* cx, wikrt_val* stack, wikrt_val v);
#define WIKRT_ALLOW_SIZE_BYPASS 0

void wikrt_drop_sv(wikrt_cx* cx, wikrt_val* stack, wikrt_val v, wikrt_ss* ss);
void wikrt_copy_r(wikrt_cx* lcx, wikrt_val lval, wikrt_ss* ss, bool moving_copy, wikrt_cx* rcx, wikrt_val* rval);
//  moving_copy: true if we're actually moving the value, i.e. the origin is dropped.

#if 0
void wikrt_quote_v(wikrt_cx*, wikrt_val*);
void wikrt_compose_v(wikrt_cx*, wikrt_val ab, wikrt_val bc, wikrt_val* out);
void wikrt_block_attrib_v(wikrt_cx*, wikrt_val*, wikrt_val attribs);
#endif

// for text to/from block, in wikrt_parse.c
void wikrt_intro_optok(wikrt_cx* cx, char const*); // e → (optok * e)
void wikrt_intro_op(wikrt_cx* cx, wikrt_op op); // e → (op * e)

// return number of valid bytes and chars, up to given limits. Return
// 'true' only if all bytes were read or we stopped on a NUL terminal.
// Here 'szchars' may be NULL.
bool wikrt_valid_text_len(char const* s, size_t* szbytes);
bool wikrt_valid_key_len(char const* s, size_t* szBytes);

// given length, determine if we have a valid token
bool wikrt_valid_token_l(char const* s, size_t len);

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
    uint64_t            cxcount;    // how many contexts created?
    pthread_mutex_t     mutex;      // shared mutex for environment
};

static inline void wikrt_env_lock(wikrt_env* e) {
    pthread_mutex_lock(&(e->mutex)); }
static inline void wikrt_env_unlock(wikrt_env* e) {
    pthread_mutex_unlock(&(e->mutex)); }

void wikrt_add_cx_to_env(wikrt_cx* cx);
void wikrt_remove_cx_from_env(wikrt_cx* cx);

/** trace buffer */
typedef struct trace_buf { 
    char*       buf;
    size_t      size;
    size_t      reader;
    size_t      writer;
} trace_buf;

/** wikrt_cx internal state.
 *
 * I've decided to favor a bump-pointer allocation with a semi-space
 * collection process. I'll probably need special handling for stowed
 * values (I may try a bloom filter). 
 *
 * I'm contemplating additional use of free lists to optimize a few
 * heuristic cases, reducing allocations for sum type data plumbing
 * and perhaps for list processing with arrays. However, this must be
 * weighed against the additional use of conditional expressions.
 */ 
struct wikrt_cx {
    // unique context identifier within env
    uint64_t            cxid;       

    // doubly-linked list of contexts in environment.
    wikrt_cx           *cxnext;
    wikrt_cx           *cxprev;
    wikrt_env          *env;

    wikrt_ecode         ecode;      // WIKRT_OK or first error

    // Memory
    void*               ssp;        // scratch space (and GC)
    void*               mem;        // active memory

    // registers, root data
    wikrt_val           val;        // primary value 
    wikrt_val           pc;         // program counter (eval)
    wikrt_val           cc;         // continuation stack (eval)
    wikrt_val           txn;        // transaction data

    wikrt_addr          alloc;      // allocate towards zero
    wikrt_size          size;       // maximum size of memory
    wikrt_size          skip;       // volume of reserved memory (favor recycling)

    // semispace and garbage collection.
    wikrt_size          compaction_size;  // memory after compaction
    uint64_t            compaction_count; // count of compactions
    uint64_t            bytes_compacted;  // bytes copied during compaction
    uint64_t            bytes_collected;  // bytes allocated then collected

    // trace output for printf style debugging & warnings
    trace_buf           tb;

    // other:
    //   child contexts for parallelism
    //   track futures and stowage
};

// just for sanity checks
#define WIKRT_CX_REGISTER_CT 4
#define WIKRT_REG_TXN_INIT WIKRT_UNIT
#define WIKRT_REG_PC_INIT WIKRT_UNIT
#define WIKRT_REG_CC_INIT WIKRT_UNIT
#define WIKRT_REG_VAL_INIT WIKRT_UNIT
#define WIKRT_REG_WL_INIT WIKRT_UNIT_INR
#define WIKRT_FREE_LIST_CT 0 /* memory freelist count (currently none) */
#define WIKRT_NEED_FREE_ACTION 0 /* for static assertions */
#define WIKRT_HAS_SHARED_REFCT_OBJECTS 0 /* for static assertions */

/* Idea: I could try to track a free list, perhaps a single element, for
 * specific cases, e.g. the deep-sum tag (for VRWLCZ ops) and for opening
 * and closing the UTF8 tag or OPVAL tags when writing. Doing so could
 * save a fair portion of allocations in some special cases.
 *
 * However, so far the savings have been marginal at best. So I'm going to
 * omit this for now (as an unnecessary complication).
 */

/* For large contexts (e.g. 20MB+) I will want to try to use less than
 * the maximum available space. Doing so improves memory locality and 
 * reduce virtual memory pressure, though the virtual memory space is 
 * still reserved. It's very simple to allocate an empty 'skip' space.
 *
 * If a context is within WIKRT_MEM_FACTOR of being full, this will do 
 * nothing. I further increase free memory to a given page boundary.
 */
#define WIKRT_MEM_FACTOR 5 /* free space for some factor of current use (if possible) */
#define WIKRT_MEM_PAGEMB 2 /* free space in chunks of so many megabytes (if possible) */

// Consider:
//  a debug list for warnings (e.g. via {&warn} or {&error})
//  a space for non-copying loopy code - shared memory that
//    is more accessible than stowage?
//  

// A strategy for 'splitting' an array is to simply share references
// within a cell. This is safe only if we allocate the extra space to
// split them for real in the future.
#define WIKRT_ALLOW_OVERCOMMIT_BUFFER_SHARING 0

#define wikrt_has_error(cx) (cx->ecode)

#define wikrt_paddr(CX,A) ((wikrt_val*)A)
#define wikrt_pval(CX,V)  wikrt_paddr(CX, wikrt_vaddr(V))

// take advantage of WIKRT_O == 0
_Static_assert((0 == WIKRT_O), "assuming WIKRT_O has no bit flags");
#define wikrt_vaddr_obj(V) (V)
#define wikrt_pobj(CX,V)  wikrt_paddr((CX),wikrt_vaddr_obj(V))


/* NOTE: Because I'm using a moving GC, I need to be careful about
 * how I represent and process allocations. Any allocation I wish 
 * to preserve must be represented in the root set. When copying an
 * array or similar, I'll need to be sure that all the contained 
 * values are copied upon moving them.
 */

static inline bool wikrt_mem_available(wikrt_cx* cx, wikrt_sizeb sz) { return (sz < cx->alloc); }
bool wikrt_mem_gc_then_reserve(wikrt_cx* cx, wikrt_sizeb sz);
static inline bool wikrt_mem_reserve(wikrt_cx* cx, wikrt_sizeb sz) { 
    return wikrt_mem_available(cx, sz) ? true : wikrt_mem_gc_then_reserve(cx, sz); 
}

// Allocate a given amount of space, assuming sufficient space is reserved.
// This will not risk compacting and moving data. OTOH, if there isn't enough
// space we'll have a very severe bug.
static inline wikrt_addr wikrt_alloc_r(wikrt_cx* cx, wikrt_sizeb sz) 
{
    cx->alloc -= sz; 
    return (cx->alloc + ((wikrt_addr) cx->mem)); // Now using absolute addressing!
}

// How much space have we allocated with data since last compaction?
//  Note that some of this memory might be dead if GC'd.
static inline wikrt_size wikrt_mem_in_use(wikrt_cx* cx) { 
    return ((cx->size - cx->alloc) - cx->skip); 
}

static inline wikrt_val wikrt_alloc_cellval_r(wikrt_cx* cx, wikrt_tag tag, wikrt_val fst, wikrt_val snd) 
{
    wikrt_addr const addr = wikrt_alloc_r(cx, WIKRT_CELLSIZE);
    wikrt_val* const pa = wikrt_paddr(cx, addr);
    pa[0] = fst;
    pa[1] = snd;
    return wikrt_tag_addr(tag, addr);
}

static inline void wikrt_intro_r(wikrt_cx* cx, wikrt_val v) {
    cx->val = wikrt_alloc_cellval_r(cx, WIKRT_P, v, cx->val); 
}
static inline void wikrt_intro_smallval(wikrt_cx* cx, wikrt_val v) 
{
    if(!wikrt_mem_reserve(cx, WIKRT_CELLSIZE)) { return; }
    wikrt_intro_r(cx, v);
}
static inline void wikrt_wrap_otag(wikrt_cx* cx, wikrt_otag otag) {
    if(!wikrt_p(cx->val)) { wikrt_set_error(cx, WIKRT_ETYPE); return; }
    if(!wikrt_mem_reserve(cx, WIKRT_CELLSIZE)) { return; }
    wikrt_val* const v = wikrt_pval(cx, cx->val);
    (*v) = wikrt_alloc_cellval_r(cx, WIKRT_O, otag, (*v));
}




static inline void wikrt_drop_v(wikrt_cx* cx, wikrt_val v, wikrt_ss* ss) {
    wikrt_drop_sv(cx, (wikrt_val*)(cx->ssp), v, ss); }

static inline void wikrt_pval_swap(wikrt_val* a, wikrt_val* b) {
    wikrt_val const tmp = (*b);
    (*b) = (*a);
    (*a) = tmp;
} 


// Other thoughts: It could be useful to keep free-lists regardless,
// and allocate from them only after the bump-pointer arena is full
// or when a valid sized element is at the head of the list. 

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

static inline bool wikrt_integer(wikrt_cx* cx, wikrt_val v) {
    // big integers disabled
    return wikrt_smallint(v); 
}
static inline bool wikrt_blockval(wikrt_cx* cx, wikrt_val v) {
    return (wikrt_o(v) && wikrt_otag_block(*wikrt_pobj(cx, v))); 
}

static inline bool wikrt_value_is_utf8(wikrt_cx* cx, wikrt_val v) {
    return (wikrt_o(v) && wikrt_otag_utf8(*(wikrt_pobj(cx,v)))); 
}

static inline bool wikrt_value_is_compact_binary(wikrt_cx* cx, wikrt_val v) {
    return (wikrt_o(v) && wikrt_otag_binary(*(wikrt_pobj(cx,v)))); 
}

// (block * e) → (ops * e), returning block tag (e.g. with substructure)
wikrt_otag wikrt_open_block_ops(wikrt_cx* cx);

// (pending (block * value)) * e → (block * value) * e
void wikrt_open_pending(wikrt_cx* cx);

static inline bool wikrt_trashval(wikrt_cx* cx, wikrt_val v) {
    return wikrt_o(v) && wikrt_otag_trash(*wikrt_pobj(cx, v));
}

// utility for incremental construction of large binaries and texts.
void wikrt_reverse_binary_chunks(wikrt_cx* cx);

static inline bool wikrt_cx_has_txn(wikrt_cx* cx) { 
    return (WIKRT_REG_TXN_INIT == cx->txn); 
}
void wikrt_drop_txn(wikrt_cx*);

// e → (empty * e)
static inline void wikrt_intro_empty_list(wikrt_cx* cx)
{   
    wikrt_intro_smallval(cx, WIKRT_UNIT_INR); 
}

// (a * (as * e)) → (a:as * e); same as `lV`
static inline void wikrt_cons(wikrt_cx* cx)
{
    wikrt_assocl(cx);
    wikrt_wrap_sum(cx, WIKRT_INL);
}

// (a * e) → (list of a * e)
static inline void wikrt_wrap_list_singleton(wikrt_cx* cx) 
{
    wikrt_intro_empty_list(cx);
    wikrt_wswap(cx);
    wikrt_cons(cx);
}

// (x * (y * (xs * e)) → (y * (x:xs * e))
static inline void wikrt_consd(wikrt_cx* cx) 
{
    wikrt_zswap(cx);
    wikrt_cons(cx);
    wikrt_wswap(cx);
}

static inline void wikrt_elim_sum(wikrt_cx* cx, wikrt_sum_tag const lr_expected) 
{
    wikrt_sum_tag lr;
    wikrt_unwrap_sum(cx, &lr);
    if(lr_expected != lr) { wikrt_set_error(cx, WIKRT_ETYPE); }
}

// drop a list terminal (while validating its type)
static inline void wikrt_elim_list_end(wikrt_cx* cx)
{
    wikrt_elim_sum(cx, WIKRT_INR);
    wikrt_elim_unit(cx);
}

// (v*e) → ((otag v) * e). Wrap a value with an otag. 
void wikrt_wrap_otag(wikrt_cx* cx, wikrt_otag otag);


