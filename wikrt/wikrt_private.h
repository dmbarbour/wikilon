
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
#include "wikrt_eph.h"

/** NOTES
 * 
 * Pointers: We'll use native pointers internally. This performs well, and 
 * is necessary for wikrt_cx_freeze to support references into the parent
 * context.
 * 
 * Dictionary Names: valid Awelon words up to so many bytes are accepted.
 * Anything else is aliased via secure hash. This should be invisible to 
 * our API clients. 
 *
 * Timing Attacks: Secure hashes must resist timing attacks. Expose only
 * first 60 bits or so to timing. This might be achieved by performing a
 * partial key search using comparisons, and comparing the rest via scan.
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
typedef struct wikrt_thread wikrt_thread;
#define WIKRT_V_MAX  UINTPTR_MAX
#define WIKRT_Z_MAX  WIKRT_V_MAX

#define WIKRT_LNBUFF(SZ,LN) ((((SZ)+((LN)-1))/(LN))*(LN))
#define WIKRT_LNBUFF_POW2(SZ,LN) (((SZ) + ((LN) - 1)) & ~((LN) - 1))
#define WIKRT_CELLSIZE (2 * sizeof(wikrt_v))
#define WIKRT_CELLBUFF(sz) WIKRT_LNBUFF_POW2(sz, WIKRT_CELLSIZE)
#define WIKRT_QUADSIZE (2 * WIKRT_CELLSIZE)
#define WIKRT_FILE_MODE (mode_t)(S_IRUSR | S_IWUSR | S_IRGRP | S_IWGRP)
#define WIKRT_DIR_MODE (mode_t)(WIKRT_FILE_MODE | S_IXUSR | S_IXGRP)

// worker threads don't need a lot of stack space
#define WIKRT_WORKER_STACK_MIN (64 * 1024)
#define WIKRT_WORKER_STACK_SIZE WIKRT_LNBUFF(WIKRT_WORKER_STACK_MIN, PTHREAD_STACK_MIN)

static inline wikrt_z wikrt_cellbuff(wikrt_z n) { return WIKRT_CELLBUFF(n); }

/** Bit Representations
 *
 *      v00     small constants, actions, tags
 *      v01     tagged objects or actions (header ..data..)
 *      v10     composition cell (B, A) => [B A]
 *      v11     constructor cell (H, T) => [H T :]
 *      `v` bit is 1 for blocks or value words, 0 for inline actions
 *
 *    Composition can be used for concatenation, and if the `v` bit
 *    is not set we essentially get `B A` without the wrapping block.
 *
 * Common Small Constants (2 bits + b00)
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
 *      001     single raw binary bytes in stream
 *      (small decimals, labels, texts)
 *
 *   I'm assuming 64-bit systems will be the common option. Decimals 
 *   with an 8 bit exponent (base 10) and 48-bit mantissa would cover
 *   a lot of practical values. On a 32-bit system, extended small
 *   constants won't cover nearly as much but are still useful.
 *
 * Note: one reason for constants to be arranged the way they are is
 * to ensure a zero-filled memory corresponds to a sequence of OP_NOP
 * inaction.
 *
 * Aside: Awelon and Wikilon are unlikely to support floating point
 * data, excepting very careful acceleration of linear algebras. CPUs
 * may vary in how much internal precision they provide, etc..
 */
#define WIKRT_SMALL 0
#define WIKRT_OBJ   1
#define WIKRT_COMP  2
#define WIKRT_CONS  3
#define WIKRT_VAL   4

#define WIKRT_SMV   (WIKRT_VAL | WIKRT_SMALL)
#define WIKRT_VOBJ  (WIKRT_VAL | WIKRT_OBJ)

#define WIKRT_REF_MASK_TYPE     3
#define WIKRT_REF_MASK_ADDR     (~((wikrt_v)7))

#define WIKRT_SMALL_INT_OP    8  /* _1000; int behavior */
#define WIKRT_SMALL_INT_VAL  (WIKRT_SMALL_INT_OP | WIKRT_VAL)
#define WIKRT_SMALL_NAT_OP   16  /* 10000; nat behavior */
#define WIKRT_SMALL_NAT_VAL  (WIKRT_SMALL_NAT_OP | WIKRT_VAL)

#define WIKRT_SMALLNAT_MAX (WIKRT_V_MAX >> 5)
#define WIKRT_SMALLINT_MAX WIKRT_SMALLNAT_MAX
#define WIKRT_SMALLINT_MIN (- WIKRT_SMALLINT_MAX)

// bit-level utility functions
static inline wikrt_v wikrt_vtag(wikrt_v v) { return (WIKRT_REF_MASK_TYPE & v); }
static inline wikrt_a wikrt_v2a(wikrt_v v) { return (WIKRT_REF_MASK_ADDR & v); }
static inline bool wikrt_action(wikrt_v v) { return (0 == (WIKRT_VAL & v)); }
static inline bool wikrt_value(wikrt_v v) { return !wikrt_action(v); }

static inline bool wikrt_is_obj(wikrt_v v) { return (WIKRT_OBJ == wikrt_vtag(v)); }
static inline bool wikrt_is_small(wikrt_v v) { return (WIKRT_SMALL == wikrt_vtag(v)); }
static inline bool wikrt_is_comp(wikrt_v v) { return (WIKRT_COMP == wikrt_vtag(v)); }
static inline bool wikrt_is_cons(wikrt_v v) { return (WIKRT_CONS == wikrt_vtag(v)); }

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

/** Random Data
 *
 * Wikilon Runtime doesn't need entropy for much. But one place it
 * does need some random data is allocation of the shared memory
 * ephemeron table.
 */
void wikrt_get_entropy(size_t amt, uint8_t* out);

/** Tagged Objects
 *
 * Wikilon runtime shouldn't need more than 32 common object types,
 * especially if I consolidate common pairs and quadruples. Beyond
 * the structural type, each object tracks a little substructure.
 *
 *   - shared:  reference shared, must copy on write
 *   - no-copy: the (nc) annotation has been applied
 *   - no-drop: the (nd) annotation has been applied
 * 
 * Objects are unique by default, enabling in-place update. But
 * in-place update is only possible with specific accelerators,
 * such as fast, indexed update of a list (via an array).
 */
typedef enum wikrt_otype
{ WIKRT_OTYPE_PAIR = 0  // (header, value) pairs
, WIKRT_OTYPE_QUAD      // (header, value, value, value) quadruples
    // ... more as needed

    // data has a size field
, WIKRT_OTYPE_BLOCK     // flat sequence of code
, WIKRT_OTYPE_ARRAY     // compact list value
, WIKRT_OTYPE_BINARY    // array of byte values

    // Special Cases
, WIKRT_OTYPE_TASK      // a fragment of code under evaluation
, WIKRT_OTYPE_RTB_NODE  // register table binary tree node
, WIKRT_OTYPE_WORD      // interned, and includes annotations
} wikrt_otype;

#define WIKRT_O_TYPE  (0x1f)
#define WIKRT_O_SHARE (1<<5)
#define WIKRT_O_NC    (1<<6)
#define WIKRT_O_ND    (1<<7)
#define WIKRT_O_DATA_OFF  8
#define WIKRT_O_DATA_MAX (WIKRT_Z_MAX >> WIKRT_O_DATA_OFF)
#define WIKRT_O_DATA_BIT(N) (1 << (N + WIKRT_O_DATA_OFF))

static inline wikrt_o     wikrt_new_obj_hdr(wikrt_otype o, wikrt_n d) { return (o | (d << WIKRT_O_DATA_OFF)); }
static inline wikrt_otype wikrt_o_type(wikrt_v v) { return (WIKRT_O_TYPE & *(wikrt_v2p(v))); }
static inline wikrt_n     wikrt_o_data(wikrt_o o) { return (o >> WIKRT_O_DATA_OFF); }

/** PTYPE - used in WIKRT_O_DATA field for WIKRT_OTYPE_PAIR 
 *
 * This allows interpretation of a representation. Binaries could
 * represent objects, texts, or large natural numbers. We can also
 * represent a logically reversed array, list, or binary.
 */
typedef enum wikrt_ptype 
{ WIKRT_PTYPE_BINARY_RAW    // wrap a binary object
, WIKRT_PTYPE_UTF8_TEXT     // wrap a binary as text
, WIKRT_PTYPE_BIGNUM        // wrap a binary as bignum
, WIKRT_PTYPE_ARRAY_REVERSE // reverse array or binary
, WIKRT_PTYPE_ERROR         // [value](error)
// maybe a fixpoint block wrapper?
} wikrt_ptype;

static inline wikrt_o wikrt_new_ptype_hdr(wikrt_ptype p) { 
    return wikrt_new_obj_hdr(WIKRT_OTYPE_PAIR, p); } 

/** QTYPE - used in WIKRT_O_DATA field for WIKRT_OTYPE_QUAD 
 *
 * This gives us three fields after the header.
 */
typedef enum wikrt_qtype
{ WIKRT_QTYPE_ARRAY_APPEND  // size, left, right. (Size 0 is a blank.)
, WIKRT_QTYPE_ARRAY_SLICE   // offset, size, array.
, WIKRT_QTYPE_QUALIFIER     // e.g. [foo], @y, unused
// maybe a JIT block wrapper?
} wikrt_qtype;

static inline wikrt_o wikrt_new_qtype_hdr(wikrt_qtype q) {
    return wikrt_new_obj_hdr(WIKRT_OTYPE_QUAD, q); }

/** Arrays
 *
 * Arrays are simple (header, val, val, val, ...) with size in header.
 * Arrays model lists, but with less overhead and pointer chasing and
 * nicer asymptotic performance for indexed operations. Arrays require
 * accelerated operations for most benefits.
 *
 * Lists and arrays in Awelon are heterogeneous normally, since there
 * is no implicit constraint to support homogeneous folds.
 */
typedef struct wikrt_array { wikrt_o o; wikrt_v d[]; } wikrt_array;
static inline wikrt_z wikrt_array_size(wikrt_z elems) {
    return wikrt_cellbuff(sizeof(wikrt_v) * (1 + elems));
}

/** Blocks
 *
 * A block of code is a simple sequence of operations. However, we must
 * consider interaction with performance optimizations and profiling.
 * 
 * Profiling: For heap profiling, we might track syntactic origin for
 * every block. This might be achieved by adding invisible annotations
 * or comments to the block, representing parse origin of that block.
 *
 * Optimization: while a block should contain evaluated code, we could
 * also track arity and compute the link-optimized code (with partial
 * evaluation and logical inlining). JIT code acts similarly to this
 * link optimized code, and would frequently use the same field.
 *
 * My current inclination is to inject code for profiling, and to wrap
 * code for link optimization and JIT. Meanwhile, the basic sequence 
 * of ops can reuse the array structure.
 */
typedef wikrt_array wikrt_block;

/** Binary Data - (header, bytes) with header data = size. */
typedef struct wikrt_binary { wikrt_o o; uint8_t b[]; } wikrt_binary;
static inline wikrt_z wikrt_binary_size(wikrt_z bytes) {
    return wikrt_cellbuff(sizeof(wikrt_o) + bytes);
}
wikrt_v wikrt_alloc_binary(wikrt_thread* t, uint8_t const*, wikrt_z);

/** Big Numbers
 *
 * Awelon primarily supports natural numbers, and works with them
 * always in base 10. Under the hood, we'll use a variation on the
 * binary coded decimals: a big number is represented by sequence
 * of 32-bit words, each ranging 0..999999999, little-endian (low
 * words first). This representation may be used at parse time for
 * large number words.
 *
 * Big integers, decimals, or rationals will be modeled explicitly
 * above big natural numbers, whereas small integers and useful
 * decimals (on a 64-bit system) can be modeled via small values.
 */

/** Words
 *
 * Excepting numbers, words are interned and kept in a hashtable.
 * Each word will need a bunch of metadata.
 *
 *     the word itself, maybe namespace qualifiers
 *     block having word's link-optimized definition
 *      - potentially wrapped by JIT-compiled definition
 *     computed link arity (e.g. 0-1 for value words)
 *     track update to word definition since commit
 *      - track dependencies? maybe via persistent index?
 *     breakpoint state
 *
 * Almost any access to our word table will be synchronized, but
 * ideally the words themselves may be used with minimal synch.
 * 
 * Since words are mostly present as a cache, it's okay to GC words
 * that aren't used and reload them from dictionary as needed. To
 * track caching, we might want to model a `recently used` field in
 * each word, clearing it when we perform full GC then setting it
 * when first link a word after GC. This would simplify decisions
 * to GC data that isn't used much (including stowage resources).
 *
 * The exception is written definitions, which cannot be collected
 * until we commit. So we must auto-mark words whose definitions 
 * have yet to be written. 
 */

/** Tasks
 *
 * A 'task' describes a computation in progress and provides a 
 * barrier against accessing results for parallel evaluations. 
 * Tasks have some registers for ongoing computation and slots
 * for managing task queues.
 *
 * In some cases, a task will wait upon another. Ideally, we
 * can directly track these waits such that the client task
 * may continue immediately. However, this doesn't work well
 * with the current GC model.
 * 
 * Relevant attributes: complete, waiting, stability
 *
 * The last attribute regards memory stability in context of
 * parallelism and GC. I must track when a task cannot be 
 * accessed because it is under evaluation or its result
 * might reference another thread's nursery arena.
 *
 * We might also have attributes to specify evaluation mode.
 */
typedef struct wikrt_task {
    wikrt_o o;      // WIKRT_OTYPE_TASK + bitfield
    wikrt_v next;   // next task (linked list, 0 terminated)
    wikrt_v wait;   // waiting on referenced task, if any

    // evaluation registers (uniquely referenced)
    wikrt_v lhs;    // data stack, left hand side of cursor
    wikrt_v rhs;    // call stack, right hand side of cursor
    wikrt_n amt;    // arity available in lhs
} wikrt_task;

#define WIKRT_TASK_UNSTABLE WIKRT_O_DATA_BIT(0)
#define WIKRT_TASK_COMPLETE WIKRT_O_DATA_BIT(1)
#define WIKRT_OTYPE_TASK_COMPLETE (WIKRT_OTYPE_TASK | WIKRT_TASK_COMPLETE)

static inline bool wikrt_is_task(wikrt_v t) { 
    return wikrt_is_obj(t) && (WIKRT_OTYPE_TASK == wikrt_o_type(t)); 
}

/** Built-in Operations (Primitives, Accelerators, Annotations)
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
 * Annotations are included in this list, excepting sealers and other
 * annotations that have a partially user-defined symbol. Unrecognized
 * annotations will be dropped upon parsing.
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

/** The Database
 * 
 * Other than named roots, Wikilon uses secure hashes to reference
 * binary values. Thus most keys are 60 byte secure hashes encoded
 * in base64url. This has non-trivial storage and lookup overheads.
 * But it simplifies several problems related to structure sharing
 * and stability of identifiers for import/export. 
 *
 * For security reasons, lookups with a secure hash (db->memory etc.)
 * will limit exposure for timing attacks to at most 60 bits. 
 *
 * Lazy reference counting GC is used for secure hash resources. The
 * laziness is achieved by separating stable reference counts from
 * pending updates. Objects with zero references are tracked via the
 * shared memory wikrt_eph ephemeron table.
 *
 * The 'roots' table is just arbitrary data and updates to it must
 * be manually reference counted as part of a transactional update.
 * Roots are not themselves reference counted.
 */ 
typedef struct wikrt_db {
    // LMDB layer resources
    MDB_env            *mdb;
    MDB_dbi             roots;  // name → binary data
    MDB_dbi             memory; // hash → binary data
    MDB_dbi             refcts; // hash → reference counts and pending deltas
    MDB_dbi             refupd; // list of partial hashes with pending deltas
} wikrt_db;

void wikrt_db_close(wikrt_env*);

/** The Environment
 *
 * This models the physical machine resources shared by contexts,
 * primarily a persistence layer (database) and virtual CPUs via
 * worker threads.
 * 
 * Contexts may invite one worker at a time via a queue mechanism.
 * If more than one worker is needed, a worker may invite another.
 * It's generally safe to invite a worker, even when no work is
 * available. At most, we'll waste a little bit of time waking
 * a worker thread.
 * 
 * I might need an additional, separate thread to manage GC of the
 * database. But I'd prefer to perform GC incrementally with normal
 * updates. 
 */
struct wikrt_env {
    pthread_mutex_t mutex;  // for thread safety, cond vars
    wikrt_z         cxct;   // count of associated contexts

    // database and shared memory ephemeron table
    wikrt_db        *db;    // persistent storage
    wikrt_eph       *eph;   // ephemeral reference tracking

    // worker thread management
    uint32_t        workers_alloc;  // for increasing thread count
    uint32_t        workers_max;    // for reducing thread count
    pthread_cond_t  work_available; // work in cxw or if max<alloc
    pthread_cond_t  workers_halted; // for safe shutdown
    wikrt_cx        *wq;            // circular worker queue
};

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
 *
 * A write set is represented within a WIKRT_OTYPE_BINARY object, and
 * may further be wrapped as a composable list of write sets. 
 */
typedef struct wikrt_wsd {
    wikrt_n     page;
    wikrt_n     bits;
} wikrt_wsd;
typedef struct wikrt_ws {
    wikrt_z     size;       // buffer size
    wikrt_z     fill;       // element count
    wikrt_wsd   data[];     // data, adjacent to the header.
} wikrt_ws;

/** Multi-Threading and Garbage Collection
 * 
 * Threads will operate in a context, using a small region of dedicated
 * memory - the 'nursery' - for most allocations and independent GC. 
 *
 * A constraint is that no other thread may directly reference a nursery.
 * Full context GC will need to interrupt existing threads. Communication
 * between threads requires promoting the object out of the nursery. This
 * promotion can be modeled conservatively by promoting the entire nursery.
 *
 * Each thread thus tracks a local 'nursery', and the thread structure is
 * also used for any full-context GC.
 *
 * Each thread shall have an effort quota (pre-allocated) and a 
 *
 * Each thread will have a local effort quota, which it preallocates. If a
 * thread terminates and sufficient quota remains it may return some, but
 * it won't subtract again from the parent upon termination.
 *
 * Worker threads will generally operate in a context until they hit the
 * quota or run out of local memory, unless signaled via cx->workers_halt`.
 * Whenever a worker runs out of memory, it will rotate to another context
 * (potentially back to the same one), and may attempt full GC on entering
 * the context if necessary (only if it is the first thread). The main
 * thread may act a lot like a worker thread during evaluation tasks, but
 * with some extra tactics for prioritizing work.
 *
 * Note: worker threads will only operate on one task at a time, performing
 * as many allocated '(par)' tasks as feasible and promoting anything else
 * to the context. There are a few motivations for this. A task is not
 * likely to share memory with other tasks, so promotion avoids unnecessary
 * copying of a prior task's memory. Processing new tasks in order of their
 * construction will minimize waits within a thread. 
 *
 * I haven't decided on any particular order of evaluation for par tasks.
 *
 * Effort tracking: I'd like to generally preallocate the effort for a few
 * GC cycles. We can likely estimate based on a previous cycle time, and
 * prepay for a few GC cycles. The 'cycle' in question might be a survivor
 * GC cycle rather than the youngest generation.
 */
typedef struct wikrt_thread { 
    wikrt_cx* cx;   // for large allocations, coordination

    // Memory Management (no free lists)
    wikrt_a start;  // first reserved address
    wikrt_a end;    // last reserved address
    wikrt_a elder;  // end of prior young generation
    wikrt_a young;  // end of young generation (alloc start)
    wikrt_a stop;   // allocation cap (for GC, reserves space for marking)
    wikrt_a alloc;  // current allocator

    // Write Set for Generational GC.
    wikrt_v write_set;

    // Tasks to Perform.
    wikrt_v ready;      // tasks we can work on now
    wikrt_v waiting;    // tasks waiting on others

    // Debug Logs - thread local; moved to cx->memory when stable
    wikrt_v trace;      // (trace) messages 
    wikrt_v prof;       // stack profile

    // Local Memory Statistics
    uint64_t gc_bytes_processed;
    uint64_t gc_bytes_collected;

    // effort tracking
    uint64_t time_last;  // wikrt_thread_time at last allocation
    uint32_t effort;     // pre-allocated effort for this cycle

    // Idea: I could add an mdb_txn here via mdb_txn_reset and
    //  mdb_txn_renew to reduce malloc/free overheads within a
    //  thread.
} wikrt_thread;

/** current timestamp in microseconds */
uint64_t wikrt_thread_time(); // microseconds
void wikrt_thread_poll_waiting(wikrt_thread*);
void wikrt_thread_move_ready_r(wikrt_thread*);

/** Registers Table
 * 
 * Registers are recorded in a simple linear collision hash table, a pair
 * for IDs and values that permits about a 2/3 fill. This ensures a O(1)
 * cost for register operations, though degenerate cases are possible.
 *
 * At the moment, the registers table grows monotonically, based on maximum
 * number in use during a context's history.  
 */
typedef struct wikrt_rtb {
    wikrt_v ids;
    wikrt_v data;
    wikrt_z fill;
    wikrt_z size;
} wikrt_rtb;

// rtb_prealloc assumes api_enter. 
// reg_set assumes rtb_prealloc for target register
bool wikrt_rtb_resize(wikrt_cx*, wikrt_z);
bool wikrt_rtb_prealloc(wikrt_cx* cx, wikrt_z amt);
void wikrt_reg_set(wikrt_cx*, wikrt_r, wikrt_v);
wikrt_v wikrt_reg_get(wikrt_cx const*, wikrt_r);

// addend to an existing value in register
void wikrt_reg_write(wikrt_cx*, wikrt_r, wikrt_v);
#define WIKRT_REG_WRITE_PREALLOC WIKRT_CELLSIZE

/** A context.
 *
 * A context is represented by a contiguous volume of memory, and has
 * a corresponding dictionary. A context has a set of binary registers
 * for external access, only manipulated via the main API thread.
 *
 * Additionally, each context tracks words loaded from the dictionary.
 * This supports the transaction model and allows for compilation of 
 * words, and partial GC as the context fills.
 *
 * A context is associated with a dictionary in persistent storage.
 * If a dictionary name is an invalid word or is larger than its
 * secure hash, we'll rewrite it to the secure hash of the name.
 *
 * For parallelism, a context may be placed in a work queue. For MT
 * safety involving exclusive control of a context, it is permitted
 * to lock cx->mutex while holding cx->env->mutex.
 *
 * A challenge: I need full GC of the context by worker threads for
 * background parallelism, and I also need stable memory for API ops.
 * So the question is how to prevent full context GC from background
 * threads while the main API is active. My current idea is to just
 * accept the parallelism hit - use cx->mutex to lock memory for API
 * operations that need it, but try to keep these critical sections
 * small. (Since worker threads require very little synchronization,
 * modulo large allocations, this should work well.) When the main 
 * thread performs long running evaluation, it may act as an extra,
 * local worker thread.
 */
struct wikrt_cx {
    wikrt_env      *env;

    // protected by cx->env->mutex
    wikrt_cx       *wqn, *wqp;          // used for worker queue
    uint32_t        worker_count;       // count of workers in context
    pthread_cond_t  workers_done;       // signal when (0 == worker_count)

    // mutex for content within context
    pthread_mutex_t mutex;              // to protect local allocator
    bool            interrupt;          // signal workers to abandon context

    // to support frozen contexts
    wikrt_n         refct;              // references as a frozen context 
    wikrt_cx*       proto;              // a frozen prototype context 
    bool            frozen;             // whether this context is frozen

    // Debug Flags
    bool            trace_enable;       // (trace) messages
    bool            prof_enable;        // stack profiling
    
    // parallel computations in shared memory
    size_t          size;               // initial allocation
    wikrt_thread    memory;             // shared context memory

    // Dictionary Data
    size_t          dict_name_len;      // 0..WIKRT_HASH_SIZE
    uint8_t         dict_name[WIKRT_HASH_SIZE + 4]; // unique name of dictionary (NUL terminated) 
    uint8_t         dict_ver[WIKRT_HASH_SIZE + 4];  // an import/export hash val (NUL terminated)
    wikrt_v         words;              // words table in context memory

    // Registers Table
    wikrt_rtb       rtb;

    // todo:
    // Stowage tracking: need to know all stowage roots
    // Dictionary indexing?
};

// default effort is about 100ms labor
#define WIKRT_CX_DEFAULT_EFFORT (100 * 1000)

wikrt_z wikrt_gc_bitfield_size(wikrt_z alloc_space);
wikrt_z wikrt_compute_alloc_space(wikrt_z space_total); // include GC reserve space

void wikrt_add_cx(wikrt_cx** plist, wikrt_cx* cx);
void wikrt_rem_cx(wikrt_cx** plist, wikrt_cx* cx);
void wikrt_cx_signal_work(wikrt_cx*);       // invites a worker thread
bool wikrt_cx_work_available(wikrt_cx*);    // assumes mutex held
size_t wikrt_word_len(uint8_t const* src, size_t maxlen);

// test availability of thread-local memory 
static inline bool wikrt_thread_mem_available(wikrt_thread const* t, wikrt_z amt)
{
    return ((t->stop - t->alloc) >= amt);
}
bool wikrt_thread_mem_gc_then_reserve(wikrt_thread* t, wikrt_z amt);

// attempt to reserve some thread-local memory
static inline bool wikrt_thread_mem_reserve(wikrt_thread* t, wikrt_z amt)
{
    return wikrt_thread_mem_available(t, amt) ? true 
         : wikrt_thread_mem_gc_then_reserve(t, amt);
}

// Allocate from thread memory. Assumes `amt` is buffered to wikrt_cellbuff,
// and that our thread has sufficient space. 
static inline wikrt_a wikrt_thread_alloc(wikrt_thread* t, wikrt_z amt)
{
    wikrt_a const r = t->alloc;
    t->alloc += amt;
    return r;
}

static inline wikrt_a wikrt_thread_alloc_cell(wikrt_thread* t, wikrt_v fst, wikrt_v snd) 
{
    wikrt_a const a = wikrt_thread_alloc(t, WIKRT_CELLSIZE);
    wikrt_v* const pv = wikrt_a2p(a);
    pv[0] = fst;
    pv[1] = snd;
    return a;
}
static inline wikrt_a wikrt_thread_alloc_quad(wikrt_thread* t, wikrt_v v0, wikrt_v v1, wikrt_v v2, wikrt_v v3)
{
    wikrt_a const a = wikrt_thread_alloc(t, WIKRT_QUADSIZE);
    wikrt_v* const pv = wikrt_a2p(a);
    pv[0] = v0;
    pv[1] = v1;
    pv[2] = v2;
    pv[3] = v3;
    return a;
}


/** API Entry, Exit, and Memory Management
 *
 * Operations on main memory and registers must lock the context to
 * prevent background GC. Long-running computations from the API 
 * should operate as a special worker thread.
 */
static inline void wikrt_api_enter(wikrt_cx* cx) {
    // not much to do here.
    pthread_mutex_lock(&(cx->mutex));
}
void wikrt_api_exit(wikrt_cx*);
void wikrt_api_gc(wikrt_cx*, wikrt_z amt);
void wikrt_api_interrupt(wikrt_cx*);    
static inline bool wikrt_api_mem_prealloc(wikrt_cx* cx, wikrt_z amt) {
    if(wikrt_thread_mem_available(&(cx->memory),amt)) { return true; }
    wikrt_api_gc(cx, amt);
    return wikrt_thread_mem_available(&(cx->memory), amt);
}
static inline wikrt_a wikrt_api_alloc(wikrt_cx* cx, wikrt_z amt) {
    return wikrt_thread_alloc(&(cx->memory), amt);
}
static inline bool wikrt_api_prealloc(wikrt_cx* cx, wikrt_z nReg, wikrt_z nBytes) {
    return wikrt_rtb_prealloc(cx, nReg) && wikrt_api_mem_prealloc(cx, nBytes);
}

#define WIKRT_H
#endif

