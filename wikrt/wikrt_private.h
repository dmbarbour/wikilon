
#pragma once
#ifndef WIKRT_H

#include "wikrt.h"
#include <stdint.h>

/** NOTES
 * 
 * Bits: We'll use native pointers internally, to simplify copy-on-write
 * reuse when we freeze and copy a context and to avoid offset overheads.
 * 
 * Dictionary Names: valid Awelon words up to so many bytes are accepted.
 * Anything else is rewritten via secure hash. Outside of low level 
 * import/export or timing, this should be invisible to our API clients. 
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
 * Multi-Process Utilities: use shm_open to manage the ephemeron table.
 * I might need a large random number to name the resource, or perhaps
 * hash the canonical directory name. 
 */

// Using native sized words.
typedef uintptr_t wikrt_val;

// Aliases for internal documentation.
typedef wikrt_val wikrt_size;       // arbitrary size value
typedef wikrt_size wikrt_sizeb;     // size aligned to cell (2*sizeof(wikrt_val))
typedef wikrt_val  wikrt_addr;      // location in memory
typedef wikrt_val wikrt_ohdr;       // tagged object headers
typedef struct wikrt_db wikrt_db;   // we'll get back to this later
#define WIKRT_VAL_MAX   UINTPTR_MAX
#define WIKRT_SIZE_MAX  WIKRT_VAL_MAX

#define WIKRT_LNBUFF(SZ,LN) ((((SZ)+((LN)-1))/(LN))*(LN))
#define WIKRT_LNBUFF_POW2(SZ,LN) (((SZ) + ((LN) - 1)) & ~((LN) - 1))
#define WIKRT_CELLSIZE (2 * sizeof(wikrt_val))
#define WIKRT_CELLBUFF(sz) WIKRT_LNBUFF_POW2(sz, WIKRT_CELLSIZE)
#define WIKRT_FILE_MODE (mode_t)(S_IRUSR | S_IWUSR | S_IRGRP | S_IWGRP)
#define WIKRT_DIR_MODE (mode_t)(WIKRT_FILE_MODE | S_IXUSR | S_IXGRP)


static inline wikrt_sizeb wikrt_cellbuff(wikrt_size n) { return WIKRT_CELLBUFF(n); }

/** Bit Representations
 *
 *      i00     small constants, actions, tags
 *      i01     tagged objects or actions (header ..data..)
 *      i10     composition cell (B, A) => [B A]
 *      i11     constructor cell (H, T) => [H T :]
 * 
 * The `i` bit is 0 when inlined, 1 for blocks and value words. Any
 * value may thus be inlined by tweaking a reference bit. This helps
 * with further composition.
 */
#define WIKRT_VALREF            0
#define WIKRT_OBJECT            1
#define WIKRT_BIND_CELL         2
#define WIKRT_CONS_CELL         3

#define WIKRT_REF_MASK_CBIT     2
#define WIKRT_REF_MASK_TYPE     3
#define WIKRT_REF_MASK_IBIT     4
#define WIKRT_REF_MASK_ADDR     (~((wikrt_val)7))

static inline wikrt_val wikrt_vtag(wikrt_val v) { return (WIKRT_REF_MASK_TYPE & v); }
static inline wikrt_addr wikrt_vaddr(wikrt_val v) { return (WIKRT_REF_MASK_ADDR & v); }
static inline bool wikrt_action(wikrt_val v) { return !(WIKRT_REF_MASK_IBIT & v); }
static inline bool wikrt_valref(wikrt_val v) { return !wikrt_vtag(v); }

static inline wikrt_val* wikrt_addr_to_ptr(wikrt_addr a) { return (wikrt_val*)a; }
static inline wikrt_val* wikrt_val_to_ptr(wikrt_val v) { return wikrt_addr_to_ptr(wikrt_addr(v)); }
static inline bool wikrt_is_ptr(wikrt_val v) { return (WIKRT_REF_MASK_TYPE & v); }





/** Small Constants
 * 
 * Need a few more bits to discriminate type.
 *
 *      00      extended
 *      10      naturals
 *      x1      integers
 *
 * This ensures integers cover the same range as naturals, and that
 * within this constraint our naturals cover the maximum range, and
 * that the zero word can still represent a NOP action.
 *
 * Our extended small constants further discriminate type.
 *
 *      000     built-in functions (primitives, accelerators, annotations)
 *      ...     (todo: specify more as needed)
 *
 * It is feasible to support a few more number types like decimals
 * and rational numbers. Short labels and texts are also feasible
 * assuming a 64-bit system.
 *
 * While working with complex encodings is somewhat expensive, the
 * hope is that we'll make up for it in reduced allocation and GC
 * overheads. And interpretation of built-ins will gradually be
 * replaced by JIT code.
 */

/** Large Memory Objects
 *
 * We reference a memory object using a pointer tagged WIKRT_OBJECT. 
 *
 * Headers always have low bits `00` like small constants to simplify
 * the garbage collection. I'll want to track more.
 *
 * - substructure (nc) and (nd), two bits
 * - uniqueness of reference for in-place updates
 * - a write barrier "updated" bit for generational GC
 * - whether the value is copyable (evaluated, deeply immutable)
 *
 * The latter three elements could be squeezed into two bits because
 * they are not independent. So we've dedicated six bits per header.
 * Beyond that, it would be convenient for GC if these objects have
 * a simple and uniform size computation from a header, using a table
 * of allocation sizes.
 *
 * GC seems to be a big issue here. When I run a mark-compact algorithm,
 * I need to know which cells are moved when I compact, which requires
 * knowing the pointer tag from which an object is referenced. The mark
 * phase must perhaps include a third bit per item to indicate HOW it
 * was marked (cell vs. large memory object). 
 * 

, but it isn't necessary if I use a
 * threaded compacting GC (so I can track pointer types).
 */



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
, OP_ANNO_error  // (error) marks a value
, OP_ANNO_trace  // (trace) writes debug output

// Annotations to control Optimization, Compilation?

// Data Stowage
, OP_ANNO_stow  // [large value](stow) => [$secureHash]
                // [small value](stow) => [small value]

// Simple Accelerators
 // future: permutations of data plumbing. Common loops.
, OP_w          // swap;   [B][A]w == [A][B]; w = (a2) [] b a
, OP_rot        // [C][B][A]rot == [A][C][B]; rot = (a3) [] b b a
, OP_i          // inline; [A]i == A; i = [] w a d
, OP_z          // fixpoint Z combinator; [X][F]z == [X][[F]z]F
                // z = [[(a3) c i] b (=z) [c] a b w i](a3) c i


// Conditional Behaviors
, OP_T          // [B][A]T    == A;    T = a d
, OP_F          // [B][A]F    == B;    F = d i
, OP_L          // [B][A][V]L == [V]B; L = (a3) w d w i
, OP_R          // [B][A][V]R == [V]A; R = (a3) w b a d
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
, OP_nat_div

// List and Array Operations
// Integer Arithmetic
}


#define WIKRT_H
#endif

