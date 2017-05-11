
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
 * GC Roots: To simplify GC, it's easiest if we can support some ad-hoc
 * external roots for our worker threads rather than tying everything to
 * a fixed set of user-facing registers. A "write set" can serve this
 * role, I think.
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
typedef wikrt_v  wikrt_n;           // small natural number
typedef intptr_t wikrt_i;           // small integer number
typedef wikrt_v  wikrt_z;           // arbitrary size value
typedef wikrt_v  wikrt_a;           // location in memory
typedef struct wikrt_heap wikrt_heap;
typedef struct wikrt_thread wikrt_thread;
typedef struct wikrt_ws wikrt_ws;
typedef struct wikrt_task wikrt_task;
typedef enum wikrt_otype wikrt_otype;
typedef enum wikrt_ptype wikrt_ptype;
typedef enum wikrt_qtype wikrt_qtype;
typedef enum wikrt_op wikrt_op;
#define WIKRT_V_MAX  UINTPTR_MAX
#define WIKRT_Z_MAX  WIKRT_V_MAX

#define WIKRT_LNBUFF(SZ,LN) ((((SZ)+((LN)-1))/(LN))*(LN))
#define WIKRT_LNBUFF_POW2(SZ,LN) (((SZ) + ((LN) - 1)) & ~((LN) - 1))
#define WIKRT_CELLSIZE (2 * sizeof(wikrt_v))
#define WIKRT_CELLBUFF(sz) WIKRT_LNBUFF_POW2(sz, WIKRT_CELLSIZE)
#define WIKRT_QUADSIZE (2 * WIKRT_CELLSIZE)
#define WIKRT_FILE_MODE (mode_t)(S_IRUSR | S_IWUSR | S_IRGRP | S_IWGRP)
#define WIKRT_DIR_MODE (mode_t)(WIKRT_FILE_MODE | S_IXUSR | S_IXGRP)

// Worker threads don't need much stack space, certainly not the
// ~2MB default. I'll use tight worker threads.
#define WIKRT_WORKER_STACK_MIN (64 * 1024)
#define WIKRT_WORKER_STACK_SIZE WIKRT_LNBUFF(WIKRT_WORKER_STACK_MIN, PTHREAD_STACK_MIN)

// To avoid sharing memory between worker threads at the hardware
// layer or abstraction layers, I align heap allocations to pages. 
#define WIKRT_THREAD_HEAP_PAGE 4096


static inline wikrt_z wikrt_cellbuff(wikrt_z n) { return WIKRT_CELLBUFF(n); }

/** Bit Representations
 *
 * Pointer Bits
 *
 *      (data)v0    small constant in field
 *      (addr)sv1   ref to object in memory
 *
 *   Small constants generally use a few more bits for type, but the
 *   data is entirely local to the reference. For memory objects, the
 *   `s` bit indicates sharing, that the object might be accessed via
 *   more than one reference. In both cases, the `b` bit indicates a
 *   value may be treated as a block for bind and data plumbing.
 *
 * Objects (header, data ...)
 * 
 *   Objects reference allocated memory. If the first element is an
 *   object header (a special kind of small constant) then we will
 *   use the header to understand the object size and structure. Any
 *   other value is simply treated as a cell pair, a composition in
 *   most contexts (but might mean 'list cons' in some contexts.)
 *
 *   A goal with this representation is to keep it simple for GC. 
 *   Also, avoiding type information in the pointer should simplify
 *   the conditional behaviors working with objects.
 *
 * Small Constants (a few bits + v0)
 *
 *        1     small signed integers
 *       10     small natural numbers
 *      000     extended small constants
 *      100     object type headers (v=0)
 *
 *   Small integers have the same positive range as natural numbers,
 *   and may go negative as far as they go positive. The bulk of our
 *   small constants are thus designated for naturals or integers. 
 *  
 *   Limiting object headers to five bits gives me a few more bits in
 *   the header for primary object header type.
 *   
 * Extended Small Constants  (three bits + 000v0)
 *
 *   Currently just using the remainder of the byte for type, and the
 *   rest of the field for contained data. 
 *
 *      000     operators (suffix 0, so value 0 = NOP)
 *      001     raw bytes (for serialization, parsing)
 *        potentials...
 *              decimals  (for efficient math)
 *              labels    (for deep sums and records)
 *
 *   Small constants cannot be extended much beyond operators and raw
 *   bytes on a 32-bit system, but on a 64-bit system it seems feasible
 *   to shove a variety of useful types into a single field.
 *
 * Awelon doesn't have great support for floating point numbers at this
 * time, but could represent them as boxed objects. Ideally in context
 * of larger binary structures.
 */

#define WIRKT_SMALL     0
#define WIKRT_OBJ       1
#define WIKRT_VAL       2
#define WIKRT_SHARED    4
#define WIKRT_INT_OP    4
#define WIKRT_NAT_OP    8
#define WIKRT_OHEAD    16

#define WIKRT_SMV  (WIKRT_VAL | WIKRT_SMALL)
#define WIKRT_VOBJ (WIKRT_VAL | WIKRT_OBJ)
#define WIKRT_INT_VAL (WIKRT_INT_OP | WIKRT_VAL)
#define WIKRT_NAT_VAL (WIKRT_NAT_OP | WIKRT_VAL)

#define WIKRT_INT_SHIFT 3
#define WIKRT_NAT_SHIFT 4
#define WIKRT_EXT_TYPE_SHIFT 5
#define WIKRT_EXT_DATA_SHIFT 8

#define WIKRT_SMALL_NAT_MAX (WIKRT_Z_MAX >> WIKRT_INT_SHIFT)
#define WIKRT_SMALL_INT_MAX ((wikrt_i)(WIKRT_SMALL_NAT_MAX))
#define WIKRT_SMALL_INT_MIN (-(WIKRT_SMALL_INT_MAX))
  // this loses one small integer from the representation.
  // OTOH, it simplifies negation, absolute values, etc..

#define WIKRT_RAW_BYTE_TYPE 0x20
#define WIKRT_RAW_BYTE(N) ((((wikrt_v)N) << WIKRT_EXT_DATA_SHIFT) | WIKRT_RAW_BYTE_TYPE)

// bit-level utility functions
static inline bool wikrt_match_f(wikrt_v req, wikrt_v rej, wikrt_v val) { return (req == ((req|rej) & val)); }
static inline bool wikrt_is_small(wikrt_v v) { return wikrt_match_f(0, WIKRT_OBJ, v); }
static inline bool wikrt_is_object(wikrt_v v) { return !wikrt_is_small(v); }
static inline bool wikrt_is_shared(wikrt_v v) { return wikrt_match_f((WIKRT_OBJ | WIKRT_SHARED), 0, v); }
static inline bool wikrt_is_ohead(wikrt_v v) { return (0x10 == (0x1F & v)); }
static inline bool wikrt_is_primop(wikrt_v v) { return (0 == (0xFF & v)); }

static inline bool wikrt_is_small_nat_val(wikrt_v v) { return (WIKRT_NAT_VAL == (0xF & v)); }
static inline wikrt_n wikrt_val_to_nat(wikrt_v v) { return (((wikrt_n)v) >> WIKRT_NAT_SHIFT); }
static inline wikrt_v wikrt_nat_to_val(wikrt_n n) { return ((((wikrt_v)n) << WIKRT_NAT_SHIFT) | WIKRT_NAT_VAL); }

static inline bool wikrt_is_small_int_val(wikrt_v v) { return (WIKRT_INT_VAL == (0x7 & v)); }
static inline wikrt_i wikrt_val_to_int(wikrt_v v) { return (((wikrt_i)v) >> WIKRT_INT_SHIFT); }
static inline wikrt_v wikrt_int_to_val(wikrt_i i) { return ((((wikrt_v)i) << WIKRT_INT_SHIFT) | WIKRT_INT_VAL); }

static inline bool wikrt_is_action(wikrt_v v) { return wikrt_match_f(0, WIKRT_VAL, v); }
static inline bool wikrt_is_value(wikrt_v v) { return !wikrt_is_action(v); }
static inline wikrt_v wikrt_act_to_val(wikrt_v v) { return (v | WIKRT_VAL); }
static inline wikrt_v wikrt_val_to_act(wikrt_v v) { return (v & ~((wikrt_v)WIKRT_VAL)); }

static inline bool wikrt_is_raw_byte(wikrt_v v) { return (0x20 == (0xFF & v)); }
static inline wikrt_v wikrt_raw_byte_to_val(uint8_t n) { return WIKRT_RAW_BYTE(n); }
static inline uint8_t wikrt_val_to_raw_byte(wikrt_v v) { return (uint8_t)(v >> 8); }

#define WIKRT_OBJ_ADDR_MASK (~((wikrt_v)7))

static inline wikrt_a  wikrt_v2a(wikrt_v v) { return (WIKRT_OBJ_ADDR_MASK & v); }
static inline wikrt_v* wikrt_a2p(wikrt_a a) { return (wikrt_v*)a; }
static inline wikrt_v* wikrt_v2p(wikrt_v v) { return wikrt_a2p(wikrt_v2a(v)); }

/** Tagged Objects
 *
 * Wikilon runtime shouldn't need more than 32 common object types,
 * especially if I consolidate common pairs and quadruples. Beyond
 * the structural type, each object tracks a little substructure.
 *
 *   - no-copy: the (nc) annotation has been applied
 *   - no-drop: the (nd) annotation has been applied
 * 
 * Objects are unique by default, enabling in-place update. But
 * in-place update is only possible with specific accelerators,
 * such as fast, indexed update of a list (via an array).
 */
enum wikrt_otype
{ WIKRT_OTYPE_PAIR = 0  // (header, value) pairs
, WIKRT_OTYPE_QUAD      // (header, value, value, value) quadruples
    // ... more as needed

    // data has a size field
, WIKRT_OTYPE_BLOCK     // flat sequence of code
, WIKRT_OTYPE_ARRAY     // compact list value
, WIKRT_OTYPE_BINARY    // array of byte values

    // Special Cases
, WIKRT_OTYPE_TASK      // a fragment of code under evaluation
, WIKRT_OTYPE_WORD      // interned, and includes annotations
};

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
 *
 * A text wraps a binary value. In this case, the binary value does
 * not include any extra spaces to escape newlines.
 */
enum wikrt_ptype 
{ WIKRT_PTYPE_BINARY_RAW    // wrap a binary object
, WIKRT_PTYPE_TEXT          // wrap a binary as text
, WIKRT_PTYPE_BIGNUM        // wrap a binary as bignum
, WIKRT_PTYPE_ARRAY_REVERSE // reverse array or binary
, WIKRT_PTYPE_ERROR         // [value](error)
// maybe a fixpoint block wrapper?

// special cases for incremental serialization in wikrt_read
, WIKRT_PTYPE_TEXT_RAW      // for serializing text
, WIKRT_PTYPE_TEXT_RAW_LF   // serialization of text with LF escape
};

static inline wikrt_o wikrt_new_ptype_hdr(wikrt_ptype p) { 
    return wikrt_new_obj_hdr(WIKRT_OTYPE_PAIR, p); } 

/** QTYPE - used in WIKRT_O_DATA field for WIKRT_OTYPE_QUAD 
 *
 * This gives us three fields after the header.
 */
enum wikrt_qtype
{ WIKRT_QTYPE_ARRAY_APPEND  // size, left, right. (Size 0 is a blank.)
, WIKRT_QTYPE_ARRAY_SLICE   // offset, size, array.
, WIKRT_QTYPE_QUALIFIER     // e.g. [foo], @y, unused
// maybe a JIT block wrapper?
};

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
struct wikrt_task {
    wikrt_o o;      // WIKRT_OTYPE_TASK + bitfield
    wikrt_v next;   // next task (linked list, 0 terminated)
    wikrt_v wait;   // waiting on referenced task, if any

    // evaluation registers (uniquely referenced)
    wikrt_v lhs;    // data stack, left hand side of cursor
    wikrt_v rhs;    // call stack, right hand side of cursor
    wikrt_n amt;    // arity available in lhs
};

#define WIKRT_TASK_UNSTABLE WIKRT_O_DATA_BIT(0)
#define WIKRT_TASK_COMPLETE WIKRT_O_DATA_BIT(1)
#define WIKRT_OTYPE_TASK_COMPLETE (WIKRT_OTYPE_TASK | WIKRT_TASK_COMPLETE)

static inline bool wikrt_is_task(wikrt_v t) { 
    return wikrt_is_obj(t) && (WIKRT_OTYPE_TASK == wikrt_o_type(t)); 
}

/** Built-in Operations (Primitives, Accelerators, Annotations)
 *
 * Awelon relies on accelerators as a primary performance technique,
 * both for functions and data.
 *
 * Accelerators are referenced from user code by defining specific
 * words in a specific manner. These definitions are validated by
 * the runtime before acceleration is applied. This is a fragile 
 * approach, but a simple one.
 * 
 * Annotations are included in this list, excepting sealers and other
 * annotations that have a partially user-defined symbol. Unrecognized
 * annotations will be dropped upon parsing.
 */
enum wikrt_op 
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

// Type and Representation Annotations
, OP_ANNO_nat   // (nat) type assertion   42 == [41 S]
, OP_ANNO_int   // (int) type assertion   [Nat Nat int] - [0 42 int] is -42
, OP_ANNO_dec   // (dec) type assertion   [Int Int dec] - [3141 -3 dec] is 3.141
, OP_ANNO_text  // (text) type assertion
, OP_ANNO_binary // (binary) type assertion
, OP_ANNO_array // (array) type assertion
, OP_ANNO_bool  // (bool) type assertion  [F] or [T]
, OP_ANNO_opt   // (opt) type assertion   [F] or [[V]R]
, OP_ANNO_sum   // (sum) type assertion   [[V]L] or [[V]R]
, OP_ANNO_cond  // (cond) type assertion  (sum or boolean)

// Simple Accelerators
 // future: permutations of data plumbing. Common loops.
 // Note: I should probably guide this via actual usage.
, OP_w          // swap;   [B][A]w == [A][B]; w = (a2) [] b a
, OP_i          // inline; [A]i == A; i = [] w a d
, OP_z          // fixpoint Z combinator; [X][F]z == [X][[F]z]F
                // z = [[(a3) c i] b (~z) [c] a b w i](a3) c i
, OP_if         // if = (a3) [] b b a (cond) i

// TODO:
// Arithmetic
// List and Array Processing

, OP_int        


// Special Extensions for Compiled code
, OP_EXT_RETURN // represents end of block
 , OP_EXT_RETURN_ad // tail call via `... a d]`
 , OP_EXT_RETURN_i  // tail call via `... i]`
, OP_EXT_RPUSH // push data to return stack
, OP_EXT_RPOP // /pop data from return stack

// auto-define op count
, WIKRT_OP_COUNT
};

static inline wikrt_op wikrt_opval_to_op(wikrt_v v) { return (wikrt_op)(v >> 8); }
static inline wikrt_v  wikrt_op_to_opval(wikrt_op op) { return (((wikrt_v)op) << 8); }

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
 * For parallelism, I've struggled between precise signaling of the
 * workers versus simple reasoning about system state. At this time,
 * I err in favor of simplicity. Workers scan all contexts for work.
 * Overhead cost is proportional to inactive contexts. But overhead
 * should be acceptable for my intended use case.
 */
struct wikrt_env {
    pthread_mutex_t mutex;  // for thread safety, cond vars
    wikrt_cx       *cxs;    // circular list of env contexts

    // worker threads management
    uint32_t        workers_alloc;  // for increasing thread count
    uint32_t        workers_max;    // for reducing thread count
    pthread_cond_t  work_available; // work in cxw or if max<alloc
    pthread_cond_t  workers_halted; // for safe shutdown

    // database and shared memory ephemeron table
    wikrt_db        *db;    // persistent storage
    wikrt_eph       *eph;   // ephemeral reference tracking
};

void wikrt_halt_threads(wikrt_env* e); // stop worker threads

/** Write Set for Thread GC
 * 
 * The write set tracks fields written from a thread into external
 * contexts. This serves as a 'root set' for thread GC, and may
 * contain fields within active tasks in addition to writes via
 * accelerated functions into arrays or objects.
 *
 * The write set is optimized in favor of writes to fields that are
 * near each other in memory, e.g. writes to a segment of an array
 * or within an object or task.
 */
typedef struct wikrt_ws {
    wikrt_z     size;       // slots maximum (wikrt_wsd fields)
    wikrt_z     fill;       // slots filled  (loaded fields)
    wikrt_v     data;       // binary array
} wikrt_ws;

void wikrt_ws_add(wikrt_ws*, wikrt_v*); // add a field
void wikrt_ws_rem(wikrt_ws*, wikrt_v*); // remove a field
void wikrt_ws_clr(wikrt_ws*); // clear all fields (preserve allocation)
bool wikrt_ws_prealloc(wikrt_thread*, wikrt_ws*, wikrt_z nFields); // grow if needed
wikrt_v* wikrt_ws_iter(wikrt_ws*, wikrt_v*); // start and end at NULL, random order

/** Heap
 * 
 * Each thread operates in its own "heap" within a context. Allocation
 * in a heap uses a bump-pointer mechanism.
 *
 * Additionally, to support GC, I provide two bitfields - two bits for
 * marking objects, one for tracking whether an address is an object or 
 * a cell during the mark effort.
 *
 * Ideally, we could   
 */
struct wikrt_heap {
    wikrt_a start;  // first reserved address
    wikrt_a end;    // last reserved address
    wikrt_a stop;   // allocation limit and start of mark bits
    wikrt_a alloc;  // current allocator
};
wikrt_heap wikrt_heap_init(wikrt_a, size_t);
static inline bool wikrt_mem_avail(wikrt_heap const* h, wikrt_z amt)
{
    return ((h->stop - h->alloc) >= amt);
}

static inline wikrt_a wikrt_mem_alloc(wikrt_heap* h, wikrt_z amt)
{
    wikrt_a const a = h->alloc;
    h->alloc += amt;
    return a;
}

static inline wikrt_a wikrt_alloc_cell(wikrt_heap* h, wikrt_v fst, wikrt_v snd) 
{
    wikrt_a const a = wikrt_mem_alloc(h, WIKRT_CELLSIZE);
    wikrt_v* const pv = wikrt_a2p(a);
    pv[0] = fst;
    pv[1] = snd;
    return a;
}

static inline wikrt_a wikrt_alloc_quad(wikrt_heap* h, wikrt_v v0, wikrt_v v1, wikrt_v v2, wikrt_v v3)
{
    wikrt_a const a = wikrt_mem_alloc(h, WIKRT_QUADSIZE);
    wikrt_v* const pv = wikrt_a2p(a);
    pv[0] = v0;
    pv[1] = v1;
    pv[2] = v2;
    pv[3] = v3;
    return a;
}

static inline wikrt_v wikrt_alloc_comp(wikrt_heap* h, wikrt_v hd, wikrt_v tl) 
{
    return WIKRT_COMP | wikrt_alloc_cell(h, hd, tl); 
}

/** Thread Data
 *
 * A thread has its own heap (generally sliced out of the context heap), and
 * also tracks some debug outputs and a set of tasks to perform. A write-set
 * tracks external roots for a thread, such that a thread may be GC'd without
 * external references.
 */
struct wikrt_thread { 
    wikrt_cx* cx;   // for large allocations, coordination

    // Memory Management (no free lists)
    wikrt_heap mem;

    // To support GC, we must track thread roots.
    wikrt_ws ws; 

    // Debug Logs. Reverse order. To be promoted to context. 
    wikrt_v trace;      // (trace) messages 
    wikrt_v prof;       // stack profile

    // Tasks to Perform.
    wikrt_v ready;      // tasks we can work on now
    wikrt_v waiting;    // tasks waiting on others
    wikrt_v done;       // tasks awaiting promotion

    // Some Memory Statistics
    uint64_t gc_bytes_processed;
    uint64_t gc_bytes_collected;
};

void wikrt_thread_gc(wikrt_thread*);
static inline bool wikrt_thread_mem_prealloc(wikrt_thread* t, wikrt_z amt)
{ 
    if(wikrt_mem_avail(&(t->mem), amt)) { return true; }
    wikrt_thread_gc(t);
    return wikrt_mem_avail(&(t->mem), amt);
}
static inline wikrt_a wikrt_thread_alloc(wikrt_thread* t, wikrt_z amt) {
    return wikrt_mem_alloc(&(t->mem), amt); }

void wikrt_thread_poll_waiting(wikrt_thread*);
uint64_t wikrt_thread_time(); // timestamp in microseconds

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

bool wikrt_rtb_prealloc(wikrt_cx*, wikrt_z amt);
void wikrt_reg_set(wikrt_cx*, wikrt_r, wikrt_v);
wikrt_v wikrt_reg_get(wikrt_cx const*, wikrt_r);

#define WIKRT_REG_ADDEND_PREALLOC WIKRT_CELLSIZE
void wikrt_reg_addend(wikrt_cx*, wikrt_r, wikrt_v);

/** Context structure.
 *
 * A context is represented within a contiguous volume of memory. It
 * has a dictionary and a transaction for updates. Worker threads may
 * enter a context to attempt to perform some work.
 *
 * Contexts have different GC behavior from threads. A thread can be
 * GC'd independently, but a context requires coordinating with worker
 * threads and special handling of ephemeron tables.
 */
struct wikrt_cx {
    wikrt_cx       *cxn, *cxp;          // list contexts in environment
    wikrt_env      *env;                // the environment

    // mutex for content within context
    pthread_mutex_t mutex;              // to protect local allocator
    pthread_cond_t  workers_done;       // signal when (0 == worker_count)
    uint32_t        worker_count;       // count of workers in context
    bool            interrupt;          // signal workers to abandon context

    // to support frozen contexts
    wikrt_n         refct;              // references as a frozen context 
    wikrt_cx*       proto;              // a frozen prototype context 
    bool            frozen;             // whether this context is frozen

    // Quotas
    size_t          size;               // initial allocation
    wikrt_heap      memory;             // shared context memory
    uint32_t        effort;             // quota (CPU microseconds)

    // GC Metrics
    uint64_t        gc_bytes_collected; // memory recovered
    uint64_t        gc_bytes_processed; // bytes touched by GC

    // Registers Table
    wikrt_rtb       rtb;

    // Debug Flags and Logs
    bool            trace_enable;       // enable (trace) messages
    bool            prof_enable;        // enable stack profiling
    wikrt_v         trace;              // log of trace messages
    wikrt_v         prof;               // log of stack profiles

    // Dictionary Data
    size_t          dict_name_len;      // 0..WIKRT_HASH_SIZE
    uint8_t         dict_name[WIKRT_HASH_SIZE + 4]; // unique name of dictionary (NUL terminated) 
    uint8_t         dict_ver[WIKRT_HASH_SIZE + 4];  // an import/export hash val (NUL terminated)
    wikrt_v         words;              // words table in context memory

    // Parallel Tasks
    wikrt_v         ready;        
    wikrt_v         waiting;      


    // todo:
    // Stowage tracking: need to know all stowage roots
    // Dictionary indexing?
    // Memoization.
};

// default effort is about 200ms labor
#define WIKRT_CX_DEFAULT_EFFORT (200 * 1000)

wikrt_z wikrt_gc_bitfield_size(wikrt_z alloc_space);
wikrt_z wikrt_compute_alloc_space(wikrt_z space_total); // include GC reserve space
void wikrt_cx_signal_work(wikrt_cx*);  // invite a worker thread
size_t wikrt_word_len(uint8_t const* src, size_t maxlen);

/** API Entry, Exit, and Memory Management
 *
 * At the moment, API entry simply locks the context, and API exit will
 * unlock the context. But this might need to change if we later change
 * how worker threads interact with the context.
 */
static inline void wikrt_api_enter(wikrt_cx* cx) { pthread_mutex_lock(&(cx->mutex)); }
static inline void wikrt_api_exit(wikrt_cx* cx) { pthread_mutex_unlock(&(cx->mutex)); }
void wikrt_api_gc(wikrt_cx*);
void wikrt_api_interrupt(wikrt_cx*);    
static inline bool wikrt_api_mem_prealloc(wikrt_cx* cx, wikrt_z amt) 
{
    if(wikrt_mem_avail(&(cx->memory), amt)) { return true; }
    wikrt_api_gc(cx);
    return wikrt_mem_avail(&(cx->memory), amt);
}
static inline wikrt_a wikrt_api_alloc(wikrt_cx* cx, wikrt_z amt) {
    return wikrt_mem_alloc(&(cx->memory), amt); }
static inline bool wikrt_api_prealloc(wikrt_cx* cx, wikrt_z nReg, wikrt_z nBytes) 
{
    return wikrt_rtb_prealloc(cx, nReg) 
        && wikrt_api_mem_prealloc(cx, nBytes);
}

#define WIKRT_H
#endif

