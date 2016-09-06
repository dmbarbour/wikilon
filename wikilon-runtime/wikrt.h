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

/** Value references internal to a context. */
typedef uint32_t wikrt_val;
#define WIKRT_VAL_MAX UINT32_MAX

/** Corresponding signed integer type. */
typedef int32_t wikrt_int;
#define WIKRT_INT_MAX INT32_MAX

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

// Block: Accelerators. 
// Note: This must be an append-only once stowage is working.
// But until then I'm free to tweak things around a fair bit.
, ACCEL_TAILCALL    // $c
, ACCEL_INLINE      // vr$c
, ACCEL_PROD_SWAP   // vrwlc
, ACCEL_SUM_SWAP    // VRWLC
, ACCEL_INTRO_UNIT_LEFT  // vvrwlc
, ACCEL_INTRO_VOID_LEFT  // VVRWLC
, ACCEL_wrzw  // (a * ((b * c) * d)) → (a * (b * (c * d)))
, ACCEL_wzlw  // (a * (b * (c * d))) → (a * ((b * c) * d))
, ACCEL_ANNO_TRACE  // {&trace}
, ACCEL_ANNO_TRASH  // {&trash}
, ACCEL_ANNO_LOAD   // {&load}
, ACCEL_ANNO_STOW   // {&stow}
, ACCEL_ANNO_LAZY   // {&lazy}
, ACCEL_ANNO_FORK   // {&fork}
, ACCEL_ANNO_JOIN   // {&join}
, ACCEL_ANNO_TEXT   // {&text}
, ACCEL_ANNO_BINARY // {&binary}
//, ACCEL_ANNO_SIMPLIFY  // {&simplify}
//, ACCEL_ANNO_OPTIMIZE  // {&optimize}
//, ACCEL_rw // ((a * b) * c) → (b * (a * c))
//, ACCEL_wl // (b * (a * c)) → ((a * b) * c)
//, ACCEL_rzl // ((a * b) * (c * d)) → ((a * c) * (b * d))

// potential future accelerators?
//  increment and decrement by 1.
//  stack-level manipulations
//  fixpoint functions (a constant overhead variant)
//  loop functions (repeat, foreach, etc.)
//  collections processing (list access, cut, update)


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

#define WIKRT_REF_MASK_TAG    (7)     
#define WIKRT_REF_MASK_ADDR   (~WIKRT_REF_MASK_TAG)
#define wikrt_vaddr(V) ((V) & WIKRT_REF_MASK_ADDR)
#define wikrt_vtag(V) ((V) & WIKRT_REF_MASK_TAG)
#define wikrt_tag_addr(TAG,ADDR) ((TAG) | (ADDR))

// for static assertions
#define WIKRT_USING_MINIMAL_BITREP 1

// WIKRT_I, WIKRT_U, WIKRT_UL, WIKRT_UR are 'shallow copy'.
#define wikrt_copy_shallow(V) (4 & (V)) 

// Small constants: 
//
// The most important small constant is the 'unit' value, which enables
// the unit-left and unit-right (booleans, empty list, etc.) to also
// be encoded in a single word.
// 
// Trash that can be copied or dropped as a normal value, normal trash,
// is also encoded as a small constant to maximize memory recovery.
//
// For performance reasons, small constants must not directly encode 
// pairs nor anything with substructural types. Encoding pairs wrapped
// with sums, e.g. short lists, is okay. Small constants might encode
// small binary or textual data (for records, tables, etc.).


#define WIKRT_UNIT        WIKRT_U
#define WIKRT_UNIT_INL    WIKRT_UL
#define WIKRT_UNIT_INR    WIKRT_UR

#define WIKRT_SMALL_CONST(N) (WIKRT_U | (N << 3))
#define WIKRT_NORMAL_TRASH   WIKRT_SMALL_CONST(1)


// I'll probably want to deprecate these
static inline bool wikrt_p(wikrt_val v) { return (WIKRT_P == wikrt_vtag(v)); }
static inline bool wikrt_pl(wikrt_val v) { return (WIKRT_PL == wikrt_vtag(v)); }
static inline bool wikrt_pr(wikrt_val v) { return (WIKRT_PR == wikrt_vtag(v)); }
static inline bool wikrt_o(wikrt_val v) { return (WIKRT_O == wikrt_vtag(v)); }

/** @brief small integers (32 bit Wikilon Runtime)
 *
 * Small integers are, at the moment, indicated by low bits `100`.
 * So I'm limited to about 2^28 or ~256 million. 
 */
#define WIKRT_SMALLINT_MAX  ((1<<28)-1)
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
 *   affine and relevance substructural attributes, and optionally some
 *   annotations for use (e.g. lazy/parallel).
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
 *          `next` continues the list (or terminates it)
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
 *   (utf8, binary). Wrap a utf8 binary terminating in unit. This is the
 *   default for embedded texts, and is necessary to serialize text as 
 *   embedded. 
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
 *    A placeholder value, generally produced by the {&trash} or {&trace}
 *    annotations. The original value was marked as garbage, its memory
 *    recycled, leaving only this opaque hole. This form of trash has 
 *    substructural attributes, otherwise I favor WIKRT_NORMAL_TRASH.
 *
 * WIKRT_OTAG_PEND
 *
 *    A pending value, e.g. from a latent or parallel computation. Pending
 *    values are currently represented by `(pending (block * value))` as
 *    lazy values in general. I might specialize in the future, or just use
 *    blocks with special tokens.
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
#define WIKRT_BLOCK_LAZY     (1 << 16) // call by need result
#define WIKRT_BLOCK_FORK     (1 << 17) // parallel evaluation

// safe attributes: commutative, idempotent, and compositional.
//  right now, this is just our substructural attributes
#define WIKRT_SAFE_BLOCK_ATTRIBS (WIKRT_BLOCK_RELEVANT | WIKRT_BLOCK_AFFINE)

static inline bool wikrt_otag_has_flags(wikrt_otag val, wikrt_otag flags) { return (flags == (val & flags)); }

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
        (*ss) |= (wikrt_otag_has_flags(otag, WIKRT_BLOCK_RELEVANT) ? WIKRT_SS_REL : 0)
              |  (wikrt_otag_has_flags(otag, WIKRT_BLOCK_AFFINE)   ? WIKRT_SS_AFF : 0);   
    }
}
static inline bool wikrt_opval_hides_ss(wikrt_val otag) { 
    return !wikrt_otag_has_flags(otag, WIKRT_OPVAL_LAZYKF);
}
static inline wikrt_otag wikrt_ss_to_block_flags(wikrt_ss ss) 
{
    return (wikrt_ss_copyable(ss)  ? 0 : WIKRT_BLOCK_AFFINE)
         | (wikrt_ss_droppable(ss) ? 0 : WIKRT_BLOCK_RELEVANT);
}


/* Internal API calls. */
void wikrt_copy_m(wikrt_cx*, wikrt_ss*, bool moving_copy, wikrt_cx*); 

// wikrt_vsize: return allocation required to deep-copy a value. 
//   Use a given stack space for recursive structure tasks.
//
// TODO: I'll need to tweak this to receive a pair of stacks, one to count
// shared context-local objects once and only once, the other to count and
// scan the normal objects.
wikrt_size wikrt_vsize(wikrt_cx* cx, wikrt_val* stack, wikrt_val v);
#define WIKRT_ALLOW_SIZE_BYPASS 0

void wikrt_drop_sv(wikrt_cx* cx, wikrt_val* stack, wikrt_val v, wikrt_ss* ss);
void wikrt_drop_v(wikrt_cx* cx, wikrt_val v, wikrt_ss* ss);

void wikrt_copy_r(wikrt_cx* lcx, wikrt_val lval, wikrt_ss* ss, bool moving_copy, wikrt_cx* rcx, wikrt_val* rval);
//  moving_copy: true if we're actually moving the value, i.e. the origin is dropped.

// as block_to_text, but also return toplevel substructure.
wikrt_ss wikrt_block_to_text_ss(wikrt_cx* cx);


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

void wikrt_add_cx_to_env(wikrt_cx* cx, wikrt_env*);
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
 * The main task of a context is managing memory. A context is represented
 * primarily by a contiguous block of memory with relative addressing. 
 *
 *     [(context)(arena1).....(arena2).....]
 *
 * Memory is divided into two primary arenas. We'll be using a compacting
 * collector, so allocation is less than 50% efficient. However, this is
 * mostly an issue for active memory, not for stowage or big data. 
 *
 * I am contemplating a separate divide for 'mature' objects, growing from
 * the top of one arena. The motivation is to reduce frequency of compaction
 * for objects that have already survived many compactions, likely only for
 * shared objects.
 *
 * Active memory may also leverage free-list GC. This enables migrant worker
 * threads to perform more useful work between compactions, which is handled
 * by the API thread. However, precision isn't critical in this case.
 */ 
struct wikrt_cx {
    // doubly-linked list of contexts in environment.
    wikrt_env          *env;
    wikrt_cx           *cxnext;
    wikrt_cx           *cxprev;

    // context-level locks
    pthread_rwlock_t    gclock;     // prevent compacting GC
    pthread_mutex_t     cxlock;     // shared wikrt_cx stuff

    // context status
    wikrt_ecode         ecode;      // WIKRT_OK or first error

    // memory management
    wikrt_size          size;       // Size of one arena. 
    wikrt_addr          arena1;     // Start of first arena  (sizeof(wikrt_cx) + buffer)
    wikrt_addr          arena2;     // Start of second arena (arena1+size)
    wikrt_addr          last;       // First invalid address (arena2+size)

    wikrt_addr          m_mem;      // start of mature space (arena2 or last)
    wikrt_addr          m_alloc;    // bottom of mature space (allocate downwards)
    
    wikrt_addr          ssp;        // scratch space location (opposite mem)
    wikrt_addr          mem;        // active memory arena (arena1 or arena2).
    wikrt_addr          alloc;      // active memory allocation cursor.
    wikrt_addr          cap;        // soft cap for next compacting GC.

    // registers, rooted data
    wikrt_val           val;        // primary value 
    wikrt_val           pc;         // program counter (eval)
    wikrt_val           cc;         // continuation stack (eval)
    wikrt_val           txn;        // transaction data


    // memory statistics
    wikrt_size          compaction_size;    // memory after compaction
    uint64_t            compaction_count;   // count of compactions
    uint64_t            compaction_mmct;    // count mature memory compactions 
    uint64_t            bytes_compacted;    // bytes copied during compaction
    uint64_t            bytes_collected;    // bytes allocated then collected

    // secondary output buffer 
    trace_buf           tb;

    // controlling evaluation effort
    wikrt_effort_model  effort_model;
    uint32_t            effort_value;
    uint64_t            blocks_evaluated;

    // other:
    //   child contexts for parallelism
    //   track futures and stowage
};

/* Notes on shared memory: 
 *
 * Context local shared memory is a bit finicky. I need to be careful that
 * every reference to my mature memory space also has a a value in my
 * unshared space, or I might break stacks when computing sizes of things.
 * Fortunately, I'd probably want to do that anyway, to support offset and
 * range within the shared binary.
 */


// just for sanity checks
#define WIKRT_CX_REGISTER_CT 4
#define WIKRT_REG_TXN_INIT WIKRT_NORMAL_TRASH
#define WIKRT_REG_PC_INIT WIKRT_NORMAL_TRASH
#define WIKRT_REG_CC_INIT WIKRT_NORMAL_TRASH
#define WIKRT_REG_VAL_INIT WIKRT_UNIT
#define WIKRT_FREE_LIST_CT 0 /* memory freelist count (currently none) */
#define WIKRT_NEED_FREE_ACTION 0 /* for static assertions */
#define WIKRT_HAS_SHARED_REFCT_OBJECTS 0 /* for static assertions */

// For large memory contexts, reduce VM pressure and improve
// locality by using a smaller fraction of the memory arena 
#define WIKRT_MEM_FACTOR        7 /* preserve some factor of current use */
#define WIKRT_MEM_FACTOR_PRIOR  4 /* preserve some factor of prior use  */
#define WIKRT_MEM_PAGEMB        2 /* preserve blocks of so many megabytes */

// Default effort model. I've decided megabytes allocated is a good
// default model because it's robust to changes in context size, and
// is a decent metric of the 'size' of a computation. A few hundred
// megabytes effort is non-trivial. 
#define WIKRT_DEFAULT_EFFORT_MODEL WIKRT_EFFORT_MEGABYTES
#define WIKRT_DEFAULT_EFFORT_VALUE 1000

// estimate of effort up until moment of call. 
uint64_t wikrt_effort_snapshot(wikrt_cx*);

// TODO: Develop a 'shared object' space at the wikrt_env layer.
// I.e. so I can work with large programs, binaries, texts without
// deep copying or creating extra GC memory pressure.

// A strategy for 'splitting' an array is to simply share references
// within a cell. This is safe only if we allocate the extra space to
// split them for real in the future.
#define WIKRT_ALLOW_OVERCOMMIT_BUFFER_SHARING 0

#define wikrt_has_error(cx) (cx->ecode)

#define wikrt_paddr(CX,A) ((wikrt_val*)(A + (char*)(CX)))
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

static inline bool wikrt_mem_available(wikrt_cx* cx, wikrt_sizeb sz) 
{ 
    return ((cx->cap - cx->alloc) >= sz);
}
bool wikrt_mem_gc_then_reserve(wikrt_cx* cx, wikrt_sizeb sz);
static inline bool wikrt_mem_reserve(wikrt_cx* cx, wikrt_sizeb sz) 
{ 
    return wikrt_mem_available(cx, sz) ? true 
         : wikrt_mem_gc_then_reserve(cx, sz); 
}

// Allocate a given amount of space, assuming sufficient space is reserved.
// This will not risk compacting and moving data. OTOH, if there isn't enough
// space we'll have a very severe bug.
static inline wikrt_addr wikrt_alloc_r(wikrt_cx* cx, wikrt_sizeb sz) 
{
    wikrt_addr const r = cx->alloc;
    cx->alloc += sz;
    return r;
}

static inline wikrt_size wikrt_mature_volume(wikrt_cx const* cx) { 
    return (cx->m_mem - cx->m_alloc); 
}
static inline wikrt_size wikrt_active_volume(wikrt_cx const* cx) { 
    return (cx->alloc - cx->mem); 
}

// How much memory is in use? 
static inline wikrt_size wikrt_memory_volume(wikrt_cx const* cx) { 
    return wikrt_active_volume(cx) + wikrt_mature_volume(cx);
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

void wikrt_intro_id_block(wikrt_cx* cx);

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

void wikrt_erase_trashval(wikrt_cx* cx, wikrt_ss ss); // (v*e)→(trash*e)
static inline bool wikrt_trashval(wikrt_cx* cx, wikrt_val v) {
    return (WIKRT_NORMAL_TRASH == v)
        || (wikrt_o(v) && wikrt_otag_trash(*wikrt_pobj(cx, v)));
}

// utility for incremental construction of large binaries and texts.
void wikrt_cons_binary_chunk(wikrt_cx* cx, uint8_t const* buff, size_t buffSize);
void wikrt_reverse_binary_chunks(wikrt_cx* cx);

static inline bool wikrt_cx_has_txn(wikrt_cx* cx) { 
    return (WIKRT_REG_TXN_INIT != cx->txn); 
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

void wikrt_block_lazy(wikrt_cx*);
void wikrt_block_fork(wikrt_cx*);

