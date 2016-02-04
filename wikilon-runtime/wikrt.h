/** This is the internal header for Wikilon runtime. 
 */
#pragma once

#include <stdbool.h>
#include <stdint.h>
#include <stdlib.h>
#include <stddef.h>
#include <pthread.h>

#include "lmdb/lmdb.h"
#include "wikilon-runtime.h"

/** size within a context; documents a positive number of bytes */
typedef wikrt_val wikrt_size;

/** address within a context; documents offset from origin. */
typedef wikrt_val wikrt_addr;

/** strip tag bits from our wikrt_val.
 *
 * Note that a zero address refers to the unit value, and should 
 * not be dereferenced (it points into our header arena). Also, 
 * small integers (low bit zero) should not be converted to addresses. 
 */
static inline wikrt_addr wikrt_vaddr(wikrt_val v) { return (v & ~0x7); }

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

// Small integers have range roughly + or - 1 billion. A bit larger
// than this is alright. I ensure closure of with negation, so the 
// valid range is (1<<30 - 1) or 1073741823 or its negation. Outside
// of this range, we'll use a separate 'bignum' representation.
#define WIKRT_SMALLINT_MAX  1073741823
#define WIKRT_SMALLINT_MIN -1073741823

// 




// stowage: I'll probably want to use zstd or similar compression
// for large values.


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
    // pthread_mutex_t     mutex;

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
#define WIKRT_FLCT_FF 6 // first-fit lists (exponential)
#define WIKRT_FLCT (WIKRT_FLCT_QF + WIKRT_FLCT_FF)

/** wikrt size class index, should be in 0..(WIKRT_FLCT-1) */
typedef int wikrt_sc;

/** @brief Memory allocation 'free list'.
 *
 * Currently just using size-segregated free lists. Most allocations
 * for Wikilon runtime will be one or two 'cells'. But larger data
 * becomes common with support for arrays and binaries.
 *
 * The caller must also provide sizes when deleting objects. No size
 * headers are used, at least not implicitly. This enables splitting
 * of arrays, for example. 
 *
 * Free lists use (size, addr) pairs, with 0 for the final address.
 * No tag bits are used, and sizes are in bytes.
 */
typedef struct wikrt_freelist {
    wikrt_size free_bytes;
    wikrt_size frag_count;
    wikrt_addr size_class[WIKRT_FLCT];
} wikrt_freelist;

bool wikrt_alloc(wikrt_cx*, wikrt_freelist*, wikrt_addr*, wikrt_size);
void wikrt_free(wikrt_cx*, wikrt_freelist*, wikrt_addr, wikrt_size);

/** Combine free fragments from a free-list, as much as possible. */
//void wikrt_coalesce(wikrt_cx*, wikrt_freelist*);

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
    wikrt_freelist flmain; // 
} wikrt_cx_hdr;

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

