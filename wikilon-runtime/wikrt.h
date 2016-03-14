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

/** size within a context; documents a number of bytes */
typedef wikrt_val wikrt_size;

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

// misc. constants and static functions
#define WIKRT_LNBUFF(SZ,LN) (((SZ+(LN-1))/LN)*LN)
#define WIKRT_LNBUFF_POW2(SZ,LN) ((SZ + (LN - 1)) & ~(LN - 1))
#define WIKRT_CELLSIZE (2 * sizeof(wikrt_val))
#define WIKRT_CELLBUFF(sz) WIKRT_LNBUFF_POW2(sz, WIKRT_CELLSIZE)
#define WIKRT_PAGESIZE (1 << 14)
#define WIKRT_PAGEBUFF(sz) WIKRT_LNBUFF_POW2(sz, WIKRT_PAGESIZE)

// root set management
#define WIKRT_ROOTSET_SIZE 31 // max number of root values

// free list management
#define WIKRT_FLCT_QF 16 // quick-fit lists (sep by cell size)
#define WIKRT_FLCT_FF 10 // first-fit lists (exponential)
#define WIKRT_FLCT (WIKRT_FLCT_QF + WIKRT_FLCT_FF)
#define WIKRT_QFSIZE (WIKRT_FLCT_QF * WIKRT_CELLSIZE)
#define WIKRT_FFMAX  (WIKRT_QFSIZE * (1 << (WIKRT_FLCT_FF - 1)))
#define WIKRT_QFCLASS(sz) ((sz - 1) / WIKRT_CELLSIZE)
#define WIKRT_FREE_THRESH (1 << 21)

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

#define WIKRT_MASK_TAG      7
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
static inline wikrt_val wikrt_i2v(int32_t n) { return (wikrt_val)(n << 1); }
static inline int32_t wikrt_v2i(wikrt_val v) { return (((int32_t)v) >> 1); }
static inline bool wikrt_i(wikrt_val v) { return (0 == (v & 1)); }

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
 * WIKRT_OTAG_BINARY
 * WIKRT_OTAG_TEXT
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
//#define WIKRT_OTAG_ARRAY    86   /* V */
//#define WIKRT_OTAG_BINARY   56   /* 8 */
//#define WIKRT_OTAG_TEXT     34   /* " */
//#define WIKRT_OTAG_STOWAGE  64   /* @ */
#define LOBYTE(V) ((V) & 0xFF)

#define WIKRT_DEEPSUMR      3 /* bits 11 */
#define WIKRT_DEEPSUML      2 /* bits 10 */

#define WIKRT_BIGINT_DIGIT          1000000000
#define WIKRT_BIGINT_MAX_DIGITS  ((1 << 23) - 1)

#define WIKRT_MEDINT_D1MAX  36028796

// block header bits
#define WIKRT_BLOCK_RELEVANT (1 << 8)
#define WIKRT_BLOCK_AFFINE   (1 << 9)
#define WIKRT_BLOCK_PARALLEL (1 << 10)
#define WIKRT_BLOCK_LAZY     (1 << 11)

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
    return  (((nDigits << 1) | (positive ? 0 : 1)) << 8) | WIKRT_OTAG_BIGINT;
}

/* Internal API calls. */
wikrt_err wikrt_copy_m(wikrt_cx*, wikrt_ss*, wikrt_cx*); 
wikrt_err wikrt_copy_v(wikrt_cx* lcx, wikrt_val lval, wikrt_ss* ss, wikrt_cx* rcx, wikrt_val* rval);
void wikrt_drop_v(wikrt_cx* cx, wikrt_val v, wikrt_ss*);

wikrt_err wikrt_wswap_v(wikrt_cx*, wikrt_val);

wikrt_err wikrt_sum_wswap_v(wikrt_cx*, wikrt_val*);
wikrt_err wikrt_sum_zswap_v(wikrt_cx*, wikrt_val*);
wikrt_err wikrt_sum_assocl_v(wikrt_cx*, wikrt_val*);
wikrt_err wikrt_sum_assocr_v(wikrt_cx*, wikrt_val*);
wikrt_err wikrt_sum_swap_v(wikrt_cx*, wikrt_val*);

wikrt_err wikrt_wrap_sum_v(wikrt_cx*, bool inRight, wikrt_val*);
wikrt_err wikrt_unwrap_sum_v(wikrt_cx*, bool* inRight, wikrt_val*);
wikrt_err wikrt_wrap_seal_v(wikrt_cx*, char const* tok, wikrt_val*);
wikrt_err wikrt_unwrap_seal_v(wikrt_cx*, char* tokbuff, wikrt_val*);

wikrt_err wikrt_alloc_i32_v(wikrt_cx*, wikrt_val*, int32_t);
wikrt_err wikrt_alloc_i64_v(wikrt_cx*, wikrt_val*, int64_t);
wikrt_err wikrt_peek_i32_v(wikrt_cx*, wikrt_val const, int32_t*);
wikrt_err wikrt_peek_i64_v(wikrt_cx*, wikrt_val const, int64_t*);
wikrt_err wikrt_alloc_istr_v(wikrt_cx*, wikrt_val*, char const*, size_t strlen);
wikrt_err wikrt_peek_istr_v(wikrt_cx*, wikrt_val const, char* buff, size_t* strlen); 


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

/** Internal variants of API calls. 
 *
 * This is due to the layer of indirection to handle an external rootlist and
 * compacting GC. The internal calls do not require root values for input or output.
 */
wikrt_err _wikrt_alloc_binary(wikrt_cx*, wikrt_val*, uint8_t const*, size_t);
wikrt_err _wikrt_read_binary(wikrt_cx*, size_t buffsz, size_t* bytesRead, uint8_t* buffer, wikrt_val* binary);
wikrt_err _wikrt_alloc_text(wikrt_cx*, wikrt_val*, char const*, size_t);
wikrt_err _wikrt_read_text(wikrt_cx*, size_t buffsz, size_t* bytesRead, size_t* charsRead, char* buffer, wikrt_val* text);
wikrt_err _wikrt_alloc_block(wikrt_cx*, wikrt_val*, char const*, size_t, wikrt_abc_opts);

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
    wikrt_cxm          *cxmlist;     // linked list of context roots
    pthread_mutex_t     mutex;       // shared mutex for environment
    uint32_t            cxm_created; // stat: wikrt_cx_create() count.
};

static inline void wikrt_env_lock(wikrt_env* e) {
    pthread_mutex_lock(&(e->mutex)); }
static inline void wikrt_env_unlock(wikrt_env* e) {
    pthread_mutex_unlock(&(e->mutex)); }

/** wikrt size class index, should be in 0..(WIKRT_FLCT-1) */
typedef int wikrt_sc;

/** A circular free list of (size, next) pairs. 
 * 
 * Note that the head element of a circular singly-linked free list is
 * also the last one allocated (since we cannot update its prior ref).
 * It's closer in nature to a tail pointer.
 */
typedef wikrt_addr wikrt_flst;

/** @brief Size-segregated free lists.
 *
 * Each of these 'free lists' is a *circular* free-list of (size, next)
 * pairs, or empty (address 0). The circular structure is intended to
 * simplify splicing of lists while reducing header memory overhead. 
 *
 * Todo: minimize fragmentation between contexts.
 *
 * For now, I'll move forward with the current implementation.
 */
typedef struct wikrt_fl {
    wikrt_size free_bytes;
    wikrt_size frag_count;
    wikrt_flst size_class[WIKRT_FLCT];
} wikrt_fl;

// basic strategies without fallback resources
bool wikrt_fl_alloc(void* mem, wikrt_fl*, wikrt_sizeb, wikrt_addr*);
void wikrt_fl_free(void* mem, wikrt_fl*, wikrt_sizeb, wikrt_addr); // 
void wikrt_fl_coalesce(void* mem, wikrt_fl*); // combines adjacent free blocks
void wikrt_fl_merge(void* mem, wikrt_fl* src, wikrt_fl* dst); // moves free blocks from src to dst

/** Shared state for multi-threaded contexts. */ 
struct wikrt_cxm {
    // doubly-linked list of contexts for env. 
    wikrt_cxm          *next;
    wikrt_cxm          *prev;

    // for now, keeping a list of associated contexts
    wikrt_cx           *cxlist;

    // lock for shared state within context
    pthread_mutex_t     mutex;

    // shared environment for multiple contexts.
    wikrt_env          *env;

    // primary context memory
    wikrt_size          size;
    void               *memory;

    // root free-list, shared between threads 
    wikrt_fl            fl;
};

static inline void wikrt_cxm_lock(wikrt_cxm* cxm) {
    pthread_mutex_lock(&(cxm->mutex)); }
static inline void wikrt_cxm_unlock(wikrt_cxm* cxm) {
    pthread_mutex_unlock(&(cxm->mutex)); }

/* The 'wikrt_cx' is effectively the thread-local storage for
 * wikilon runtime computations. It's assumed this is used from
 * only one thread.
 *
 * Todo: 
 *   latent destruction of items
 *   
 */
struct wikrt_cx {
    wikrt_cx           *next;       // sibling context
    wikrt_cx           *prev;       // sibling context
    wikrt_cxm          *cxm;        // shared memory structures

    wikrt_val           txn;        // context's transaction
    wikrt_val           val;        // context's held value

    void               *memory;     // main memory
    wikrt_fl            fl;         // local free space

    // statistics and metrics, supports effort quotas
    uint64_t            ct_bytes_freed; // bytes freed
    uint64_t            ct_bytes_alloc; // bytes allocated

    // fragments returned to commons (by wikrt_free) since defrag
    uint64_t            fragmentation;
};

static inline wikrt_val* wikrt_pval(wikrt_cx* cx, wikrt_addr addr) {
    return (wikrt_val*)(addr + ((char*)(cx->memory))); 
}

bool wikrt_alloc(wikrt_cx*, wikrt_size, wikrt_addr*);
void wikrt_free(wikrt_cx*, wikrt_size, wikrt_addr);
void wikrt_release_mem(wikrt_cx*);
bool wikrt_realloc(wikrt_cx*, wikrt_size, wikrt_addr*, wikrt_size);
bool wikrt_prealloc(wikrt_cx*, wikrt_size);
static inline bool wikrt_prealloc_cell(wikrt_cx* cx) {
    if(cx->fl.frag_count > 0) { return true; }
    return wikrt_prealloc(cx, WIKRT_CELLSIZE); 
}

// Allocate a cell value tagged with WIKRT_O, WIKRT_P, WIKRT_PL, or WIKRT_PR
// note that 'dst' is only modified on success. Some code depends on this.
static inline bool wikrt_alloc_cellval(wikrt_cx* cx, wikrt_val* dst, 
    wikrt_tag tag, wikrt_val v0, wikrt_val v1) 
{
    wikrt_addr addr;
    if(!wikrt_alloc(cx, WIKRT_CELLSIZE, &addr)) { 
        return false; 
    }
    (*dst) = wikrt_tag_addr(tag, addr);
    wikrt_val* const pv = wikrt_pval(cx, addr);
    pv[0] = v0;
    pv[1] = v1;
    return true;
}

// Allocate a double cell tagged WIKRT_O. 
// note that 'dst' is only modified on success. Some code depends on this.
static inline bool wikrt_alloc_dcellval(wikrt_cx* cx, wikrt_val* dst,
    wikrt_val v0, wikrt_val v1, wikrt_val v2, wikrt_val v3)
{
    wikrt_addr addr;
    if(!wikrt_alloc(cx, (2 * WIKRT_CELLSIZE), &addr)) { 
        return false; 
    }
    (*dst) = wikrt_tag_addr(WIKRT_O, addr);
    wikrt_val* const pv = wikrt_pval(cx, addr);
    pv[0] = v0;
    pv[1] = v1;
    pv[2] = v2;
    pv[3] = v3;
    return true;
}

/* An allocator for integers up to 3 big digits (~90 bits). */
wikrt_err wikrt_alloc_medint(wikrt_cx*, wikrt_val*, bool positive, uint32_t d0, uint32_t d1, uint32_t d2);
wikrt_err wikrt_peek_medint(wikrt_cx*, wikrt_val, bool* positive, uint32_t* d0, uint32_t* d1, uint32_t* d2);

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

