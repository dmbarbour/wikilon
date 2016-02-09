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

/** size within a context; documents a positive number of bytes */
typedef wikrt_val wikrt_size;

/** address within a context; documents offset from origin. */
typedef wikrt_val wikrt_addr;

/** tag uses lowest bits of a value */
typedef wikrt_val wikrt_tag;

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
#define WIKRT_TAG_OBJECT    1
#define WIKRT_TAG_PROD      3
#define WIKRT_TAG_PROD_INL  5
#define WIKRT_TAG_PROD_INR  7

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
static inline wikrt_val wikrt_i2v_small(int32_t n) { return (wikrt_val)(n << 1); }
static inline int32_t wikrt_v2i_small(wikrt_val v) { return (((int32_t)v) >> 1); }
static inline bool wikrt_is_smallint(wikrt_val v) { return (0 == (v & 1)); }

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

struct wikrt_env { 
    wikrt_cx           *cxhd;  // linked list of contexts
    pthread_mutex_t     mutex; // shared mutex for environment

    // stowage and key-value persistence
    bool                db_enable;
    int                 db_lockfile;  
    MDB_env            *db_env;       
    MDB_dbi             db_memory; // address → value
    MDB_dbi             db_caddrs; // hash → [address]
    MDB_dbi             db_keyval; // key → address or data
    MDB_dbi             db_refcts; // address → number
    MDB_dbi             db_refct0; // address → unit
    stowaddr            db_last_gc; 
    stowaddr            db_last_alloc;
    // todo: HMAC key(s) for stowed data (persistent via db).

    // todo: the LMDB writer state and locks
    // todo: worker threads and task queues
    // todo: ephemeral stowage to control GC
    // transactions: maybe attempt to combine concurrent transactions
   

    // question: can we combine writes for concurrent transactions?
    //  I would effectively need to track writes, ensure transactions
    //  are serialized  
};

bool wikrt_db_init(wikrt_env*, char const*, uint32_t dbMaxMB);
void wikrt_db_destroy(wikrt_env*);

void wikrt_env_lock(wikrt_env*);
void wikrt_env_unlock(wikrt_env*);

struct wikrt_cx { 
    // environment's list of contexts (necessary for global
    // sweeps, e.g. stowage or transaction conflict).
    wikrt_cx           *next;
    wikrt_cx           *prev;

    // a context knows its parent environment
    wikrt_env          *env;

    // primary memory is mutable flat array of some size
    void               *memory;
    uint32_t            sizeMB; 

    // internal context mutex?
    //   may need for multi-threaded allocator if not lockless
    //pthread_mutex_t     mutex;

    // most other data will be represented within cx_memory.
    // But I may need to develop a proper 'header' region.

    // todo: for stowage, we must track:
    //  stowed addresses to prevent GC
    //  values pending stowage

    // do I want a per-context mutex?
};

static inline wikrt_val* wikrt_pval(wikrt_cx* cx, wikrt_addr addr) {
    return (wikrt_val*)(((char*)cx->memory)+addr);
}

/* size-segregated free lists... */
#define WIKRT_FLCT_QF 16 // quick-fit lists 
#define WIKRT_FLCT_FF 10 // first-fit lists (exponential)
#define WIKRT_FLCT (WIKRT_FLCT_QF + WIKRT_FLCT_FF)

/** wikrt size class index, should be in 0..(WIKRT_FLCT-1) */
typedef int wikrt_sc;

/** @brief Memory allocation 'free list'.
 *
 * Currently just using size-segregated free lists. Most allocations
 * for Wikilon runtime will be two or four words. But larger data
 * becomes common with support for arrays and binaries.
 *
 * The caller must also provide sizes when deleting objects. No size
 * headers are used, at least not implicitly. This enables splitting
 * of arrays, for example. 
 *
 * Free lists use (size, addr) pairs, with 0 for the final address.
 * No tag bits are used at this layer, and sizes are in bytes.
 */
typedef struct wikrt_fl {
    wikrt_size free_bytes;
    wikrt_size frag_count;
    wikrt_addr size_class[WIKRT_FLCT];
    // todo: heuristics for coalesce decisions
    wikrt_size frag_count_df; // frag count after last coalesce
} wikrt_fl;

bool wikrt_alloc(wikrt_cx*, wikrt_fl*, wikrt_addr*, wikrt_size);
void wikrt_free(wikrt_cx*, wikrt_fl*, wikrt_addr, wikrt_size);
void wikrt_coalesce(wikrt_cx*, wikrt_fl*);
bool wikrt_coalesce_maybe(wikrt_cx*, wikrt_fl*, wikrt_size); // heuristic

/** @brief Header for cx->memory
 *
 * At the moment, this mostly consists of a 'free list'. When I go
 * multi-threaded, I may also need a shared free list between the
 * threads.
 *
 * Other things context is likely to include:
 *
 * - a list of available worker thread contexts (free list, etc.)
 * - a list of tasks awaiting parallel computations
 * - a list indexing stowage references to guard against GC
 */
typedef struct wikrt_cx_hdr {
    wikrt_fl flmain; // 
} wikrt_cx_hdr;

static inline wikrt_cx_hdr* wikrt_cxh(wikrt_cx* cx) {
    return ((wikrt_cx_hdr*)(cx->memory));
}

static inline wikrt_fl* wikrt_flmain(wikrt_cx* cx) { 
    return &(wikrt_cxh(cx)->flmain);
}

// To enable thread-local allocations and minimize synchronization, I will
// use a separate free list for each separate thread. This requires most
// allocating functions to include a thread-local variant.
wikrt_err wikrt_alloc_text_fl(wikrt_cx*, wikrt_fl*, wikrt_val*, char const*);
wikrt_err wikrt_alloc_block_fl(wikrt_cx*, wikrt_fl*, wikrt_val*, char const*, wikrt_abc_opts);
wikrt_err wikrt_alloc_binary_fl(wikrt_cx*, wikrt_fl*, wikrt_val*, uint8_t const*, size_t);
wikrt_err wikrt_alloc_i32_fl(wikrt_cx*, wikrt_fl*, wikrt_val*, int32_t);
wikrt_err wikrt_alloc_i64_fl(wikrt_cx*, wikrt_fl*, wikrt_val*, int64_t);
wikrt_err wikrt_alloc_prod_fl(wikrt_cx*, wikrt_fl*, wikrt_val* p, wikrt_val fst, wikrt_val snd);
wikrt_err wikrt_split_prod_fl(wikrt_cx*, wikrt_fl*, wikrt_val p, wikrt_val* fst, wikrt_val* snd);
wikrt_err wikrt_alloc_sum_fl(wikrt_cx*, wikrt_fl*, wikrt_val* c, bool inRight, wikrt_val);
wikrt_err wikrt_split_sum_fl(wikrt_cx*, wikrt_fl*, wikrt_val c, bool* inRight, wikrt_val*);
wikrt_err wikrt_alloc_seal_fl(wikrt_cx*, wikrt_fl*, wikrt_val* sv, char const* s, wikrt_val v); 
wikrt_err wikrt_cons_fl(wikrt_cx*, wikrt_fl*, wikrt_val* result, wikrt_val elem, wikrt_val list);

wikrt_err wikrt_copy_fl(wikrt_cx*, wikrt_fl*, wikrt_val* copy, wikrt_val const src, bool bCopyAff);
wikrt_err wikrt_drop_fl(wikrt_cx*, wikrt_fl*, wikrt_val, bool bDropRel);
wikrt_err wikrt_stow_fl(wikrt_cx*, wikrt_fl*, wikrt_val* out, wikrt_val);

// misc. constants and static functions
#define WIKRT_PAGESIZE 4096
#define WIKRT_LNBUFF(SZ,LN) (((SZ+(LN-1))/LN)*LN)
#define WIKRT_LNBUFF_POW2(SZ,LN) ((SZ + (LN - 1)) & ~(LN - 1))
#define WIKRT_PAGEBUFF(sz) WIKRT_LNBUFF_POW2(sz, WIKRT_PAGESIZE)
#define WIKRT_CELLSIZE (2 * sizeof(wikrt_val))
#define WIKRT_CELLBUFF(sz) WIKRT_LNBUFF_POW2(sz, WIKRT_CELLSIZE)
#define WIKRT_QFSIZE (WIKRT_FLCT_QF * WIKRT_CELLSIZE)
#define WIKRT_FFMAX  (WIKRT_QFSIZE * (1 << (WIKRT_FLCT_FF - 1)))
#define WIKRT_QFCLASS(sz) ((sz - 1) / WIKRT_CELLSIZE)

// for lockfile, LMDB file
#define WIKRT_FILE_MODE (mode_t)(S_IRUSR | S_IWUSR | S_IRGRP | S_IWGRP)
#define WIKRT_DIR_MODE (mode_t)(WIKRT_FILE_MODE | S_IXUSR | S_IXGRP)

