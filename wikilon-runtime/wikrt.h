/** This is the internal header for Wikilon runtime. 
 */
#pragma once

#include <stdio.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdlib.h>
#include <stddef.h>
#include <string.h>
#include <fcntl.h>
#include <unistd.h>
#include <sys/file.h>
#include <sys/stat.h>
#include <sys/types.h>

//#include <time.h>
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
    wikrt_cx           *e_cxhead;  // linked list of contexts
    pthread_mutex_t     e_mutex;   // shared mutex for environment

    // shutdown indicators
    bool                e_shutdown;


    // stowage and key-value persistence
    
    bool                e_db_enable;
    int                 e_db_lockfile;
    MDB_env            *e_db_env;
    MDB_dbi             e_db_memory;
    MDB_dbi             e_db_caddrs;
    MDB_dbi             e_db_keyval;
    MDB_dbi             e_db_refcts;
    MDB_dbi             e_db_refct0;
    stowaddr            e_db_last_gc;
    stowaddr            e_db_last_alloc;

    // todo: ephemeral stowage addresses to prevent GC
    // question: can we combine writes for concurrent transactions?
    //  I would effectively need to track writes, ensure transactions
    //  are serialized  
};

struct wikrt_cx { 
    // for environment's list of contexts
    wikrt_cx           *cx_next;
    wikrt_cx           *cx_prev;

    // a context knows its environment
    wikrt_env          *cx_env;

    // our primary memory
    wikrt_val          *cx_memory;  
    uint32_t            cx_sizeMB; 

    // most other data will be represented within cx_memory.
    // But I may need to develop a proper 'header' region.

    // todo: for stowage, we must track:
    //  stowed addresses to prevent GC
    //  values pending stowage

    // do I want a per-context mutex?
};


#define WIKRT_FILE_MODE (mode_t)(S_IRUSR | S_IWUSR | S_IRGRP | S_IWGRP)

