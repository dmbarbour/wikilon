
#pragma once
#ifndef WIKRT_H

#include <stdint.h>
#include <stdbool.h>
#include <limits.h>
#include <errno.h>
#include <pthread.h>
#include <semaphore.h>
#include <lmdb.h>
#include "wikrt.h"

/** NOTES
 * 
 * Bits: We'll use native pointers internally. This performs well, and is
 * necessary instead of offsets for wikrt_cx_freeze.
 * 
 * Dictionary Names: valid Awelon words up to so many bytes are accepted.
 * Anything else is aliased via secure hash. This should be invisible to 
 * our API clients. 
 *
 * Timing Attacks: Secure hashes must resist timing attacks. Expose only
 * first 60 bits or so to timing, compare rest using constant-time method
 * when searching the database.
 *
 * Copy on Write: I can introduce wikrt_cx_freeze action to the API such that
 * subsequent copies of a frozen context are logical, shallow, copy-on-write
 * in nature. I'll try this as an experimental API. (Now added.) Writeable
 *
 * Write-Exec: Can I just make it so contexts are fully read-write-exec?
 * I might lose the ability to work on SE-linux, but that isn't a huge 
 * problem for me. Look into mprotect(2) and personality(2).
 *
 * GC of Secure Hash Resources: I'll need some environment-level counting
 * bloom filters. For now, I could probably just use a fixed size filter.
 *
 * Multi-Process Utilities: use shm_open to create and manage the ephemeron
 * table. Ephemerons will be tracked via simple counting hashtable. We'll
 * need to assign a unique ID to each runtime database for this to work.
 */

// Using native sized words.
typedef uintptr_t wikrt_v;

// Aliases for internal documentation.
typedef wikrt_v  wikrt_o;           // tagged object header
typedef wikrt_v  wikrt_n;           // small natural number
typedef intptr_t wikrt_i;           // small integer number
typedef wikrt_v  wikrt_z;           // arbitrary size value
typedef wikrt_v  wikrt_a;           // location in memory
#define WIKRT_V_MAX  UINTPTR_MAX
#define WIKRT_Z_MAX  WIKRT_V_MAX

#define WIKRT_LNBUFF(SZ,LN) ((((SZ)+((LN)-1))/(LN))*(LN))
#define WIKRT_LNBUFF_POW2(SZ,LN) (((SZ) + ((LN) - 1)) & ~((LN) - 1))
#define WIKRT_CELLSIZE (2 * sizeof(wikrt_v))
#define WIKRT_CELLBUFF(sz) WIKRT_LNBUFF_POW2(sz, WIKRT_CELLSIZE)
#define WIKRT_FILE_MODE (mode_t)(S_IRUSR | S_IWUSR | S_IRGRP | S_IWGRP)
#define WIKRT_DIR_MODE (mode_t)(WIKRT_FILE_MODE | S_IXUSR | S_IXGRP)

#define WIKRT_WORKER_STACK_MIN (64 * 1024)
#define WIKRT_WORKER_STACK_SIZE WIKRT_LNBUFF(WIKRT_WORKER_STACK_MIN, PTHREAD_STACK_MIN)

static inline wikrt_z wikrt_cellbuff(wikrt_z n) { return WIKRT_CELLBUFF(n); }

/** Bit Representations
 *
 *      b00     small constants, actions, tags
 *      b01     tagged objects or actions (header ..data..)
 *      b10     composition cell (B, A) => [B A]
 *      b11     constructor cell (H, T) => [H T :]
 *      `b` bit is 1 for blocks or values words, 0 for inline actions
 *
 * Small Constants (2 bits + b00)
 *
 *      00      extended
 *      10      naturals        
 *      x1      integers
 *
 *   Naturals have a range up to 2^27-1 on a 32-bit system or 2^59-1
 *   on a 64-bit system before the 'big' tagged object encoding must
 *   be used. Integers use the same range plus or minus so we can 
 *   trivially convert between small naturals and small integers.
 *
 * Extended Small Constants (3 bits + 00b00)
 *
 *      000     built-in primitives, accelerators, etc.
 *      (small rationals, decimals, labels, texts, etc.)
 *
 *   The number of built-ins will be relatively small.
 *
 *   Chances are I'll not get far on a 32-bit system. But I'm assuming
 *   64-bit systems will be the common option, so losing 8 bits for the
 *   value type can cover a reasonably large set of values.
 *
 */
#define WIKRT_SMV   0
#define WIKRT_OBJ   1
#define WIKRT_COMP  2
#define WIKRT_CONS  3

#define WIKRT_REF_MASK_CBIT     2
#define WIKRT_REF_MASK_TYPE     3
#define WIKRT_REF_MASK_IBIT     4
#define WIKRT_REF_MASK_ADDR     (~((wikrt_v)7))

#define WIKRT_SMALL_INT_OP    8  /* _1000; int behavior */
#define WIKRT_SMALL_INT_VAL  12  /* _1100; int value */
#define WIKRT_SMALL_NAT_OP   16  /* 10000; nat behavior */
#define WIKRT_SMALL_NAT_VAL  20  /* 10100; nat value */

#define WIKRT_SMALLNAT_MAX (WIKRT_V_MAX >> 5)
#define WIKRT_SMALLINT_MAX WIKRT_SMALLNAT_MAX
#define WIKRT_SMALLINT_MIN (- WIKRT_SMALLINT_MAX)

// bit-level utility functions
static inline wikrt_v wikrt_vtag(wikrt_v v) { return (WIKRT_REF_MASK_TYPE & v); }
static inline wikrt_a wikrt_v2a(wikrt_v v) { return (WIKRT_REF_MASK_ADDR & v); }
static inline bool wikrt_action(wikrt_v v) { return !(WIKRT_REF_MASK_IBIT & v); }
static inline bool wikrt_value(wikrt_v v) { return !wikrt_action(v); }

static inline bool wikrt_val_in_ref(wikrt_v v) { return !wikrt_vtag(v); }
static inline bool wikrt_is_basic_op(wikrt_v v) { return !(v & 0xFF); }
static inline wikrt_v* wikrt_a2p(wikrt_a a) { return (wikrt_v*)a; }
static inline wikrt_v* wikrt_v2p(wikrt_v v) { return wikrt_a2p(wikrt_v2a(v)); }
static inline bool wikrt_is_ptr(wikrt_v v) { return !wikrt_val_in_ref(v); }

static inline bool wikrt_is_small_nat_op(wikrt_v v)  { return (WIKRT_SMALL_NAT_OP  == (v & 0x1F)); }
static inline bool wikrt_is_small_nat_val(wikrt_v v) { return (WIKRT_SMALL_NAT_VAL == (v & 0x1F)); }
static inline bool wikrt_is_small_int_op(wikrt_v v)  { return (WIKRT_SMALL_INT_OP  == (v & 0x0F)); }
static inline bool wikrt_is_small_int_val(wikrt_v v) { return (WIKRT_SMALL_INT_VAL == (v & 0x0F)); }

static inline wikrt_n wikrt_from_small_nat(wikrt_v v) { return (((wikrt_n)v) >> 5); }
static inline wikrt_v wikrt_to_small_nat_val(wikrt_n n) { return (((wikrt_v)(n << 5))|WIKRT_SMALL_NAT_VAL); }
static inline wikrt_v wikrt_to_small_nat_op(wikrt_n n) { return (((wikrt_v)(n << 5))|WIKRT_SMALL_NAT_OP); }

static inline wikrt_i wikrt_from_small_int(wikrt_v v) { return (((wikrt_i)v) >> 4); }
static inline wikrt_v wikrt_to_small_int_val(wikrt_i i) { return (((wikrt_v)(i << 4))|WIKRT_SMALL_INT_VAL); }
static inline wikrt_v wikrt_to_small_int_op(wikrt_i i) { return (((wikrt_v)(i << 4))|WIKRT_SMALL_INT_OP); }

/** Secure Random Data
 *
 * Wikilon Runtime doesn't need entropy for much. But one place it
 * does need some random data is allocation of the shared memory
 * ephemeron tables.
 */
void wikrt_get_entropy(size_t amt, uint8_t* out);

/** Tagged Objects
 *
 * The bytes of a tagged object will give a basic type, and common
 * metadata: substructure (nc, nd); unique vs sharable. And so on.
 *
 * Some object types we need:
 *
 * - blocks
 *   - simple array of actions, ending in a return action
 *   - may later reference a JIT version, too. 
 *   - fixpoint blocks? maybe by header bit, representing `[[F]z]`.  
 * - big natural numbers
 *   - array of 30-bit words (each in 0..999999999), little-endian
 * - arrays; a compact list representation
 *   - binary, text, values
 *   - copy on write if shared
 *   - logical reversal of array fragments
 *   - array slices (slice array offset count)
 *   - array logical append (append array array size)
 *   - potential buffering (add/remove at one edge?)
 * - annotations or words
 *   - might intern all such references
 *   - slots for link value, word, tree node structure
 *   - match unseal to seal efficiently
 *   - words might be updated via transactions
 *   - words can track arity or have a "linked" variant
 *   - debugger and profiling options for words
 * - error values?
 *
 * Logical append of arrays enables some array fragments to be writable
 * while others are copy-on-write. 
 *
 * Object types will simply be encoded in the first byte of the 
 * tagged object. 
 */
typedef enum wikrt_otype
{ WIKRT_OTYPE_ID = 0    // wrap a value, likely to add (nc) or (nd) attributes
, WIKRT_OTYPE_STREAM    // a stream object, referenced from context toplevel
, WIKRT_OTYPE_WS        // write set used for generational GC
, WIKRT_OTYPE_TASK      // a fragment of code under evaluation
, WIKRT_OTYPE_BLOCK   
, WIKRT_OTYPE_BIGNAT
, WIKRT_OTYPE_WORD      // interned, and includes annotations
, WIKRT_OTYPE_ERROR     // mark value as erroneous
, WIKRT_OTYPE_ARRAY     // compact list of arbitrary values
, WIKRT_OTYPE_BINARY 
, WIKRT_OTYPE_UTF8      // wraps a binary  
, WIKRT_OTYPE_SLICE     // array slice with offset, size 
, WIKRT_OTYPE_APPEND    // array join
} wikrt_otype;

/** Basic Array Structure
 *
 * A sized, contiguous list of values. 
 *
 * Potentially a unique reference to support in-place update.
 */
typedef struct wikrt_array {
    wikrt_o otype_array;    // WIKRT_ARRAY
    wikrt_z size;           // element count
    wikrt_v data[];     
} wikrt_array;

/** Built-ins (Primitives, Accelerators, Annotations)
 *
 * Awelon relies on accelerators as a primary performance technique,
 * both for functions and data. The "built ins" are just the set of
 * basic or accelerated fixed-form functions.
 *
 * Accelerators cannot be referenced directly by user code, instead
 * being accessed indirectly by matching. Define function "w" to the
 * program "(a2) [] b a" and you'll use OP_w. This method is fragile
 * in general, but that can be mitigated with wikrt_write_prelude
 * and de-facto standardization.
 * 
 * Annotations are included in this list, excepting debug gates or
 * value sealers that use the annotation symbol.
 * 
 * New built-ins can be added at any location in the enumeration. 
 * But OP_NOP should be the zero value to ensure a freshly zeroed
 * memory has no behavior.
 */
typedef enum wikrt_op 
{ OP_NOP = 0    // empty program (identity behavior)
, OP_a          // apply; [B][A]a == A[B]
, OP_b          // bind;  [B][A]b == [[B]A]
, OP_c          // copy;  [A]c == [A][A]
, OP_d          // drop;  [A]d ==

// Arity Annotations
, OP_ANNO_a2    // [B][A](a2) == [B][A]
, OP_ANNO_a3    // [C][B][A](a3) == [C][B][A]
, OP_ANNO_a4    // ...
, OP_ANNO_a5
, OP_ANNO_a6
, OP_ANNO_a7
, OP_ANNO_a8
, OP_ANNO_a9

// Substructural Types
, OP_ANNO_nc    // (nc) no-copy, forbid value copy
, OP_ANNO_nd    // (nd) no-drop, forbid value drop

// Active Debugging (Preliminary)
, OP_ANNO_error // (error) marks a value
, OP_ANNO_trace // (trace) writes debug output

// Performance Annotations
, OP_ANNO_par   // (par) evaluates block in parallel
, OP_ANNO_eval  // (eval) evaluate before continuing
, OP_ANNO_memo  // (memo) memoize computation of block
, OP_ANNO_stow  // [large value](stow) => [$secureHash]
                // [small value](stow) => [small value]
, OP_ANNO_trash // (trash) replace block with error value
    // todo: annotations for link, optimize, compile

// Extensions for Compiled code
, OP_EXT_RETURN // represents end of block
 , OP_EXT_RETURN_ad // tail call via `... a d]`
 , OP_EXT_RETURN_i  // tail call via `... i]`
, OP_EXT_RPUSH // push data to return stack
, OP_EXT_RPOP // /pop data from return stack

// Annotations to control Optimization, Compilation?

// Simple Accelerators
 // future: permutations of data plumbing. Common loops.
 // Note: I should probably guide this via actual usage.
, OP_w          // swap;   [B][A]w == [A][B]; w = (a2) [] b a
, OP_rot        // [C][B][A]rot == [A][C][B]; rot = (a3) [] b b a
, OP_i          // inline; [A]i == A; i = [] w a d
, OP_z          // fixpoint Z combinator; [X][F]z == [X][[F]z]F
                // z = [[(a3) c i] b (=z) [c] a b w i](a3) c i

// Conditional Behaviors
, OP_true       // [B][A]true i     == A;    true = [a d]
, OP_false      // [B][A]false i    == B;    false = [d i] (= 0)
, OP_L          // [B][A][[V]L] i   == [V]B; L = (a3) w d w i
, OP_R          // [B][A][[V]R] i   == [V]A; R = (a3) w b a d
, OP_ANNO_bool  // (bool) type assertion  [F] or [T]
, OP_ANNO_opt   // (opt) type assertion   [F] or [[V]R]
, OP_ANNO_sum   // (sum) type assertion   [[V]L] or [[V]R]
, OP_ANNO_cond  // (cond) type assertion  (sum or boolean)
, OP_if         // if = rot (cond) i

// Natural Number Arithmetic
//  I need at least add, multiply, difference, and division.
//  Diff and div should be lossless. Like 7 11 diff might be 0 4
//  to record the latter was larger, and division has remainder.
//  An accelerated GCD might also be nice.
//
//  These operations are only accelerated if our 0 and S are
//  defined appropriately, along with the specific operations.
//
//  Conveniently, I don't need any divide-by-zero errors at the
//  API layer. That would become a "divide-by-zero"(error) and
//  freeze the relevant portion of the evaluation.
, OP_ANNO_nat   // (nat) type assertion
, OP_S          // essentially `[(nat)R]b`
, OP_nat_add    
, OP_nat_mul
, OP_nat_diff
, OP_nat_divmod

// Integer Arithmetic
//  add, mul, div, sub, abs, neg

// List and Array Operations
} wikrt_op;

/** The Ephemeron Table
 *
 * The purpose of the ephemeron table is to prevent GC from touching
 * resources that are referenced from a context. This is represented
 * by a fixed size counting bloom filter, accepting a small risk of
 * false positives that prevent GC of resources not in use. 
 *
 * Compared to a conventional counting bloom filter, we expect a lot
 * of repeat values yet relatively few unique values to be reserved.
 * So this table is optimized by having relatively few entries with
 * large max counts. In the worst case, we just end up designating
 * popular values as unforgettable when we reach the maximum count.
 *
 * The size chosen below should be adequate well beyond practical work
 * loads. It's a little over a half megabyte of shared memory. We'll
 * deallocate the shared memory via refct when we destroy the context,
 * but any crashed process will prevent this deallocation.
 */
#define WIKRT_EPH_TBL_SIZE (1<<18)
typedef struct wikrt_eph {
    uint16_t        table[WIKRT_EPH_TBL_SIZE];
    pthread_mutex_t mutex; // must be 'robust'
    uint32_t        refct; // process count to support unlink
    uint32_t        wikrt_api_ver;  
} wikrt_eph;

/** The Database
 * 
 * We mostly use the secure hashes as keys. Lookups for hashes use
 * the form `hash (60 bits) → hash (300 bits) | data` and using a
 * constant-time comparison for the latter 300 bits to resist timing
 * attacks. The hashes are still encoded in base64url form, and the
 * full hash is used (which is moderate overhead, but acceptable).
 *
 * Reference counts must track which counts have been processed and
 * which are latent to support lazy reference counting and loading
 * of resources top-down rather than bottom-up. They'll also be
 * formatted using simple text. 
 *
 * The 'roots' table is just arbitrary data and updates to it must
 * be manually counted.
 *
 * Additionally, we'll track an ephemeron table using shared memory.
 */ 
typedef struct wikrt_db {
    // LMDB layer resources
    MDB_env            *mdb;
    MDB_dbi             roots;  // name → binary data
    MDB_dbi             memory; // hash → binary data
    MDB_dbi             refcts; // hash → reference counts and deltas
    MDB_dbi             refupd; // partial hashes for pending deltas
    // to add: persistent memo caches

    // Ephemeron Resources
    char                ephid[32]; // identifier for shm_open, shm_unlink
    wikrt_eph          *eph;       // a shared memory map
} wikrt_db;

void wikrt_db_close(wikrt_env*);

/** The Environment
 *
 * This models the physical machine resources shared by contexts,
 * including the persistence layer and virtual 'CPUs' in the form
 * of worker threads.
 * 
 * All contexts are tracked. I use two circular linked lists, with
 * one being the list with 'work available' so worker threads don't
 * need to repeatedly scan passive contexts when searching for work.
 * The other is essentially a list of passive or single-threaded
 * contexts. A context may also abort work, forcing it back to the
 * 
 * I might need an additional, separate thread to manage GC of the
 * database. Or I could keep some heuristics and perform this from
 * whichever threads are working with the database at the time.
 *
 * Worker threads will cycle through available contexts, those marked
 * as having work available.
 */
struct wikrt_env {
    // every context is exclusively in one list
    wikrt_cx        *cxs;   // single-threaded or passive contexts
    wikrt_cx        *cxw;   // contexts with obvious work available
    pthread_mutex_t mutex;  // mutex for environment manipulations

    // database and shared memory ephemeron table
    wikrt_db        *db;

    // workers thread pool and work signaling
    uint32_t        workers_alloc;  // for increasing thread count
    uint32_t        workers_max;    // for reducing thread count
    pthread_cond_t  work_available; // work in cxw or if max<alloc
    pthread_cond_t  workers_halted; // for safe shutdown
};
// for static assertions, if we break this assumption remove the def
#define WIKRT_ENV_HAS_TWO_CONTEXT_LISTS 1

void* wikrt_worker_behavior(void* e); 
void wikrt_halt_threads(wikrt_env* e);

/** Write Set for Generational GC
 *
 * Generational GC requires tracking references from the old generation
 * to the young generation. I intend to track this at the field level so
 * we can work with parts of a large array. My current plan is to use a
 * hashtable indexing small 'pages' of fields to a bitfield. 
 *
 * Assuming the table is half filled, this has worst case 12.5% overhead
 * on a 32-bit system or 6.25% overhead on a 64-bit system. The normal 
 * case is considerably better because we expect most old objects to be
 * unmodified. All of these tables can be cleared every time we perform
 * a full context GC, so this also is recoverable space.
 */
typedef struct wikrt_wsd {
    wikrt_n     page;
    wikrt_n     bits;
} wikrt_wsd;
typedef struct wikrt_ws {
    wikrt_o     otype_ws;   // == WIKRT_OTYPE_WS, no compact data
    wikrt_v     next;       // for cheap merges upon promotion
    wikrt_z     size;       // buffer size
    wikrt_z     fill;       // element count
    wikrt_wsd   data[];     // data, adjacent to the header.
} wikrt_ws;

/** Work Pools, Effort Quotas, and Background Parallelism
 *
 */

/** Multi-Threading and Garbage Collection
 * 
 * Threads operate within a context, evaluating parallel tasks. To minimize
 * synchronization, each thread has its own 'nursery' - a volume of memory
 * for lightweight allocations that is independently garbage collected. An
 * important invariant is that a thread never references another's nursery.
 *
 * Communication between threads requires memory be 'promoted' from nursery
 * to shared context, much like promoting to a higher generation during GC.
 * The latency of waiting on GC can be mitigated by heuristics to aggressively
 * promote memory when available shared work dwindles.
 *
 * In some cases, one task may end up waiting on results from another, not
 * just for evaluation to complete but also stable via promotion. Each task
 * must record its readiness status such that we can casually test whether
 * the result is available. 
 *
 * In any case, all this means each thread needs to track:
 *
 *   - memory and local generations
 *   - write log for generational GC
 *   - parallel tasks - pending, waiting
 * 
 * I'm also interested in support for fine-grained control of parallelism
 * within a context, but it isn't a critical feature at this time. For now,
 * each thread will simply track a local effort quota.
 *
 * Effort tracking: I'd like to generally preallocate the effort for a few
 * GC cycles. We can likely estimate based on a previous cycle time, and
 * prepay for a few GC cycles. The 'cycle' in question might be a survivor
 * GC cycle rather than the youngest generation.
 */
typedef struct wikrt_thread { 
    // Context for Shared Memory and Large Allocations
    wikrt_cx* cx;

    // Thread Exclusive Memory
    wikrt_a start;  // first reserved address
    wikrt_a end;    // last reserved address
    wikrt_a gen;    // end of survivor generation
    wikrt_a stop;   // allocation cap (for GC, reserves space for marking)
    wikrt_a alloc;  // current allocator

    // Write Set for Generational GC.
    wikrt_ws* writes;

    // Tasks to Perform.
    wikrt_v ready;      // tasks we can work on now
    wikrt_v pending;    // tasks recently allocated
    wikrt_v waiting;    // tasks awaiting promotion

    // Local Memory Statistics
    uint64_t gc_bytes_processed;
    uint64_t gc_bytes_collected;

    // effort tracking
    uint64_t time_last;  // wikrt_thread_time at last allocation
    uint32_t effort;     // pre-allocated effort for this cycle
} wikrt_thread;

/** current timestamp in microseconds */
uint64_t wikrt_thread_time(); // microseconds
    
/** Streams.
 * 
 * A context hosts a set of binary streams. A stream is identified by
 * a simple integer (wikrt_s). A set of streams essentially forms the
 * root set for a context.
 *
 * I'll probably need to add a bunch of fields to track reader state,
 * e.g. for partial reads of a large word, or pending repetitions of
 * `:]` at the end of a list. It might also be useful to track content
 * that cannot be further evaluated.
 *
 */
typedef struct wikrt_stream {
    wikrt_o otype_stream;   // stream type, always linear
    wikrt_s stream_id;      // external ID of stream

    // data and evaluation tasks?
    wikrt_v data;
} wikrt_stream;

/** A context.
 *
 * A context is represented by a contiguous volume of memory, and has
 * a corresponding dictionary. Memory is filled via 'streams', which
 * represent externally accessible binary data. 
 *
 * Additionally, each context tracks words loaded from the dictionary.
 * This supports the transaction modela nd allows for compilation of 
 * words, and partial GC as the context fills.
 *
 * A context is associated with a dictionary in persistent storage.
 * If a dictionary name is an invalid word or is larger than its
 * secure hash, we'll rewrite it to the secure hash of the name.
 *
 * Worker threads will operate in a context until either no work is
 * available or until interrupted via workers_halt. Each thread has 
 * its own wikrt_thread, with shared allocations from cx->memory
 * synchronized via cx->mutex. The main thread is preserved so it
 * can be used across many API calls.
 */
struct wikrt_cx {
    wikrt_env      *env;
    
    // note: following fields are protected by env->mutex
    // to support worker threads, etc.
    wikrt_cx       *cxn;                // circular list of contexts
    wikrt_cx       *cxp;
    bool            in_env_worklist;    // in env->cxw (as opposed to cxs)
    uint32_t        worker_count;       // count of workers in this thread
    bool            workers_halt;       // request active workers to halt
    pthread_cond_t  workers_done;       // signal when (0 == worker_count)

    // mutex for content within context
    pthread_mutex_t mutex;              // to protect local allocator

    // to support frozen contexts
    wikrt_n         refct;              // references as a frozen context 
    wikrt_cx*       proto;              // a frozen prototype context 
    bool            frozen;             // whether this context is frozen
    
    // parallel computations in shared memory
    uint32_t        effort;             // available compute effort
    size_t          size;               // initial allocation
    wikrt_thread    memory;             // shared context memory

    // Dictionary Data
    size_t          dict_name_len;      // 0..WIKRT_HASH_SIZE
    uint8_t         dict_name[WIKRT_HASH_SIZE + 4]; // unique name of dictionary (NUL terminated) 
    uint8_t         dict_ver[WIKRT_HASH_SIZE + 4];  // an import/export hash val (NUL terminated)
    wikrt_v         words_table;        // intern words read in memory
    wikrt_v         writes_list;        // writes since last commit

    // Stream Roots
    wikrt_s         trace;              // stream ID for (trace)
    wikrt_v         streams;            // for O(1) lookup
    wikrt_thread    main;               // resources for API main thread

    // todo:
    // Stowage tracking? Or would that be a task, too?
};

// a sufficient minimum size that we won't have too many problems
#define WIKRT_CX_MIN_SIZE (1<<14)

// default effort is about 100ms labor
#define WIKRT_CX_DEFAULT_EFFORT (100 * 1000)

wikrt_z wikrt_gc_bitfield_size(wikrt_z alloc_space);
wikrt_z wikrt_compute_alloc_space(wikrt_z space_total); // include GC reserve space

void wikrt_add_cx(wikrt_cx** plist, wikrt_cx* cx);
void wikrt_rem_cx(wikrt_cx** plist, wikrt_cx* cx);
void wikrt_cx_signal_work_available(wikrt_cx*);
void wikrt_cx_interrupt_work(wikrt_cx*);
void wikrt_cx_set_dict_name(wikrt_cx* cx, char const* const dict_name);
bool wikrt_cx_has_work(wikrt_cx*);


#define WIKRT_H
#endif

