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

// conventional max/min macros for static values
#define WIKRT_MAX(a,b) (((a) > (b)) ? (a) : (b))
#define WIKRT_MIN(a,b) (((a) < (b)) ? (a) : (b))

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
 */

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

void wikrt_env_lock(wikrt_env*);
void wikrt_env_unlock(wikrt_env*);

// Context currently uses a separate pointer for context memory.
// -  I'd prefer to avoid outwards pointers from context memory.
//
struct wikrt_cx { 
    // for environment's list of contexts
    wikrt_cx           *next;
    wikrt_cx           *prev;

    // a context knows its environment
    wikrt_env          *env;

    // primary memory is flat array of words
    wikrt_val          *memory;
    uint32_t            sizeMB; 

    // most other data will be represented within cx_memory.
    // But I may need to develop a proper 'header' region.

    // todo: for stowage, we must track:
    //  stowed addresses to prevent GC
    //  values pending stowage

    // do I want a per-context mutex?
};

void wikrt_cx_resetmem(wikrt_cx*); 

/** @brief Header for cx->memory
 *
 * The most important object in the header is the free list for 
 * memory allocations. I'll probably want to upgrade to a buddy
 * system or a sized and sorted free list, later. For now, it's
 * a 'simplest thing' solution.
 *
 * Additionally, we'll need to track ephemeral references to
 * stowed data. 
 */
typedef struct wikrt_memory_hdr
{ wikrt_val freelist; // linked list of (sizeInBytes, nextAddress) pairs
} wikrt_memory_hdr;

// for lockfile, LMDB file
#define WIKRT_FILE_MODE (mode_t)(S_IRUSR | S_IWUSR | S_IRGRP | S_IWGRP)
#define WIKRT_DIR_MODE (mode_t)(WIKRT_FILE_MODE | S_IXUSR | S_IXGRP)

