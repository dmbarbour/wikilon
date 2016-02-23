/** This is the internal header for Wikilon runtime. 
 */
#pragma once

#include <stdbool.h>
#include <stdint.h>
#include <stdlib.h>
#include <stddef.h>
#include <pthread.h>

#include "lmdb/lmdb.h"
#include "utf8.h"
#include "wikilon-runtime.h"

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
#define WIKRT_THREADSZ (WIKRT_PAGESIZE << 7) 

// free list management
#define WIKRT_FLCT_QF 16 // quick-fit lists (sep by cell size)
#define WIKRT_FLCT_FF 10 // first-fit lists (exponential)
#define WIKRT_FLCT (WIKRT_FLCT_QF + WIKRT_FLCT_FF)
#define WIKRT_QFSIZE (WIKRT_FLCT_QF * WIKRT_CELLSIZE)
#define WIKRT_FFMAX  (WIKRT_QFSIZE * (1 << (WIKRT_FLCT_FF - 1)))
#define WIKRT_QFCLASS(sz) ((sz - 1) / WIKRT_CELLSIZE)

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

#define WIKRT_MASK_TAG      7
#define WIKRT_MASK_ADDR     (~WIKRT_MASK_TAG)

static inline wikrt_addr wikrt_vaddr(wikrt_val v) { return (v & WIKRT_MASK_ADDR); }
static inline wikrt_tag  wikrt_vtag(wikrt_val v)  { return (v & WIKRT_MASK_TAG);  }
static inline wikrt_val  wikrt_tag_addr(wikrt_tag t, wikrt_addr a) { return (t | a); }

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
 * Currently, I just use the low byte of each tag to indicate its
 * general type, and the upper 24 bits are used for flags or data.
 * I'm unlikely to ever need more than a few dozen tags, so this
 * should be sufficient going forward.
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
 * WIKRT_OTAG_BLOCK
 *
 *   The block is a representation of Awelon Bytecode, but optimized for
 *   fast interpretation.
 *
 *   Note: I may need an intermediate representation for blocks for fast
 *   simplifications, optimizations, and similar features. I may also want
 *   a compact representation for composition of blocks.
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
 *   Arrays are compact representations of lists or list-like structures,
 *   i.e. of shape `μL.((a*L)+b)`. Rather than requiring a full two-word
 *   cell per item, an array will generally make do with just one word 
 *   per item (or less for binaries and texts). With accelerators, these
 *   arrays shall also enable indexed access, logical split and join, 
 *   logical reversals, etc..
 *
 *   Implementation of arrays is low priority at this time, but high
 *   priority long term.
 *
 * WIKRT_OTAG_STOWAGE
 *   Fully stowed values use a 64-bit reference to LMDB storage, plus a 
 *   few linked-list references for ephemeron GC purposes. Latent stowage
 *   is also necessary (no address assigned yet). And we'll need reference
 *   counting for stowed values.
 *
 *   I might need to use multiple tags for different stowage.
 */

#define WIKRT_OTAG_BIGINT   73  
#define WIKRT_OTAG_DEEPSUM  83 
#define WIKRT_OTAG_BLOCK    66
#define WIKRT_OTAG_SEAL     84
#define WIKRT_OTAG_SEAL_SM  58
#define WIKRT_OTAG_ARRAY    65
#define WIKRT_OTAG_STOWAGE  88
#define LOBYTE(V) ((V) & 0xFF)

#define WIKRT_DEEPSUMR      3 /* bits 11 */
#define WIKRT_DEEPSUML      2 /* bits 10 */

#define WIKRT_BIGINT_DIGIT          1000000000
#define WIKRT_BIGINT_MAX_DIGITS  ((1 << 23) - 1)

static inline bool wikrt_otag_bigint(wikrt_val v) { return (WIKRT_OTAG_BIGINT == LOBYTE(v)); }
static inline bool wikrt_otag_deepsum(wikrt_val v) { return (WIKRT_OTAG_DEEPSUM == LOBYTE(v)); }
static inline bool wikrt_otag_block(wikrt_val v) { return (WIKRT_OTAG_BLOCK == LOBYTE(v)); }
static inline bool wikrt_otag_seal(wikrt_val v) { return (WIKRT_OTAG_SEAL == LOBYTE(v)); }
static inline bool wikrt_otag_seal_sm(wikrt_val v) { return (WIKRT_OTAG_SEAL_SM == LOBYTE(v)); }
static inline bool wikrt_otag_array(wikrt_val v) { return (WIKRT_OTAG_ARRAY == LOBYTE(v)); }
static inline bool wikrt_otag_stowage(wikrt_val v) { return (WIKRT_OTAG_STOWAGE == LOBYTE(v)); }

static inline wikrt_val wikrt_mkotag_bigint(bool sign, wikrt_size nDigits) {
    return  (((nDigits << 1) | (sign ? 1 : 0)) << 8) | WIKRT_OTAG_BIGINT;
}

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
    wikrt_cxm          *cxmlist;  // linked list of context roots
    pthread_mutex_t     mutex;    // shared mutex for environment
    wikrt_db           *db;
};

static inline void wikrt_env_lock(wikrt_env* e) {
    pthread_mutex_lock(&(e->mutex)); }
static inline void wikrt_env_unlock(wikrt_env* e) {
    pthread_mutex_unlock(&(e->mutex)); }


/** wikrt size class index, should be in 0..(WIKRT_FLCT-1) */
typedef int wikrt_sc;

/** @brief A singular free-list supporting fast addend. */
typedef struct wikrt_flst {
    wikrt_addr head; // top of stack of free-list; empty if zero
    wikrt_addr tail; // for fast addend; invalid if head is empty
} wikrt_flst;

/** @brief Size-segregated free lists.
 *
 * Use of multiple free-lists for different sizes is a known strategy
 * that has proven effective. Each list is used as a stack, i.e. the
 * last element free'd is the first allocated. Coalescing is not done
 * except by explicit call.
 */
typedef struct wikrt_fl {
    wikrt_size free_bytes;
    wikrt_size frag_count;
    wikrt_flst size_class[WIKRT_FLCT];
} wikrt_fl;

// basic strategies without fallback resources
bool wikrt_fl_alloc(void* mem, wikrt_fl*, wikrt_sizeb, wikrt_addr*);
bool wikrt_fl_grow_inplace(void* mem, wikrt_sizeb memsz, wikrt_fl*, wikrt_sizeb sz0, wikrt_addr, wikrt_sizeb szf);
void wikrt_fl_free(void* mem, wikrt_fl*, wikrt_sizeb, wikrt_addr);
void wikrt_fl_coalesce(void* mem, wikrt_fl*);
void wikrt_fl_merge(void* mem, wikrt_fl* src, wikrt_fl* dst); // invalidates src


/** Shared state for multi-threaded contexts. */ 
struct wikrt_cxm {
    // doubly-linked list of contexts for env. 
    wikrt_cxm          *next;
    wikrt_cxm          *prev;

    // for now, keeping a list of associated contexts
    wikrt_cx           *cxlist;

    // shared environment for multiple contexts.
    wikrt_env          *env;

    // lock for shared state within context
    pthread_mutex_t     mutex;

    // primary context memory
    wikrt_sizeb         size;
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
 */
struct wikrt_cx {
    wikrt_cx           *next; // sibling context
    wikrt_cx           *prev; // sibling context
    wikrt_cxm          *cxm;  // shared memory structures

    void               *memory; // main memory
    wikrt_fl            fl;     // local free space

    // statistics or metrics
    uint64_t            ct_bytes_freed; // bytes freed
    uint64_t            ct_bytes_alloc; // bytes allocated
};

static inline wikrt_val* wikrt_pval(wikrt_cx* cx, wikrt_addr addr) {
    return (wikrt_val*)(addr + ((char*)(cx->memory)));
}

bool wikrt_alloc(wikrt_cx*, wikrt_size, wikrt_addr*);
void wikrt_free(wikrt_cx*, wikrt_size, wikrt_addr);
bool wikrt_realloc(wikrt_cx*, wikrt_size, wikrt_addr*, wikrt_size);

// Allocate a cell value tagged with WIKRT_O, WIKRT_P, WIKRT_PL, or WIKRT_PR
static inline bool wikrt_alloc_cellval(wikrt_cx* cx, wikrt_val* dst, 
    wikrt_tag tag, wikrt_val v0, wikrt_val v1) 
{
    wikrt_addr addr;
    if(!wikrt_alloc(cx, WIKRT_CELLSIZE, &addr)) { 
        (*dst) = WIKRT_VOID; 
        return false; 
    }
    (*dst) = wikrt_tag_addr(tag, addr);
    wikrt_val* const pv = wikrt_pval(cx, addr);
    pv[0] = v0;
    pv[1] = v1;
    return true;
}

/* Recognize values represented entirely in the reference. */
static inline bool wikrt_copy_shallow(wikrt_val const v) {
    return (wikrt_i(v) || (0 == wikrt_vaddr(v)));
}

wikrt_err wikrt_alloc_seal_len(wikrt_cx* cx, wikrt_val* sv, 
    char const* s, size_t len, wikrt_val v);

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

