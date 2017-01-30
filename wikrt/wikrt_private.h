
#pragma once
#ifndef WIKRT_H

#include "wikrt.h"
#include <stdint.h>

/** NOTES AND IDEAS
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
typedef wikrt_size wikrt_size_b;    // size aligned to cell (2*sizeof(wikrt_val))
typedef wikrt_val wikrt_ohdr;       // tagged object headers
typedef struct wikrt_db wikrt_db;   // we'll get back to this later

/* Bit Representations
 *
 *      i00     small constants, actions, tags
 *      i01     tagged objects (type in header)
 *      i10     composition cell (B, A) => [B A]
 *      i11     constructor cell (H, T) => [H T :]
 *
 * In each case, the `i` bit is `inline`, except for tags. Tags use
 * `00` low bits to simplify GC, but reinterpret everything else.
 *
 * Primitive operations and accelerators will generally have at least
 * one representation as inline constants, i.e. so `[a]` and `a` can
 * be represented in a single word. I might translate them to tagged
 * objects during evaluation (for uniformity).
 */


#define WIKRT_H
#endif

