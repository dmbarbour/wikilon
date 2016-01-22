/** This is the internal header for Wikilon runtime. 
 */
#pragma once

#include <stdbool.h>
#include <stdint.h>
#include <pthread.h>

#include "lmdb/lmdb.h"
#include "wikilon-runtime.h"

/** wikrt_val bits
 *
 * low bits xy0: small integers
 * low bits 001: tagged object
 * low bits 011: pointer to pair
 * low bits 101: pointer to pair in left
 * low bits 111: pointer to pair in right
 *
 * Unit represented as pair at 0 address:
 *   unit          = 3
 *   unit in left  = 5
 *   unit in right = 7
 */

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
    pthread_mutex_t     mutex;   // shared mutex for environment

    // stowage and key-value persistence
    
    bool                db_enable;
    int                 db_lockfile;
    MDB_env            *db_env;
    MDB_dbi             db_memory;
    MDB_dbi             db_caddrs;
    MDB_dbi             db_keyval;
    MDB_dbi             db_refcts;
    MDB_dbi             db_refct0;
    stowaddr            db_last_gc;
    stowaddr            db_last_alloc;

    // todo: ephemeral stowage addresses to prevent GC
    // question: can we combine writes for concurrent transactions?
    //  I would effectively need to track writes, ensure transactions
    //  are serialized  
};

// thoughts: should wikrt_cx simply use the mmap'd space?
//  I'd prefer to avoid pointers within the memory, for now.
struct wikrt_cx { 
    // for environment's list of contexts
    wikrt_cx           *next;
    wikrt_cx           *prev;

    // a context knows its environment
    wikrt_env          *env;

    // our primary memory
    wikrt_val          *memory;
    uint32_t            sizeMB; 

    // most other data will be represented within cx_memory.
    // But I may need to develop a proper 'header' region.

    // todo: for stowage, we must track:
    //  stowed addresses to prevent GC
    //  values pending stowage

    // do I want a per-context mutex?
};


#define WIKRT_FILE_MODE (mode_t)(S_IRUSR | S_IWUSR | S_IRGRP | S_IWGRP)

