
#pragma once
#ifndef WIKRT_H
/** NOTES AND IDEAS
 * 
 * Dictionary Names: valid Awelon words up to 63 bytes are accepted as is.
 * Anything else is rewritten via secure hash to 64 bytes of base64url.
 * Outside of lower level import/export or timing, this should be invisible
 * to our API clients. 
 *
 * Timing Attacks: Secure hash resources are not properly protected by access
 * control. Instead, we assume they are secure capabilities. Unfortunately, 
 * lookup via a tree-based database like LMDB is subject to timing attacks: a
 * resource ID can be discovered by guessing at bytes and timing how long it
 * takes to return a "no such item" response.
 *
 * A robust solution is to apply a secure hash on the resource ID to produce
 * the key for storage and lookup. This has high overhead, unfortunately.
 *
 * A viable lightweight option is to use only the first 64 or 96 bits for
 * our tree based lookup, then scan the rest using a deterministic time
 * hash comparison. This would limit timing attacks to discovering only
 * those initial few bits. 
 *
 * Copy on Write: I can introduce wikrt_cx_freeze action to the API such that
 * subsequent copies of a frozen context are logical, shallow, copy-on-write
 * in nature. I'll try this as an experimental API. (Now added.)
 *
 * Write-Exec: For JIT, I'll need a space for machine code. Ideally, I can 
 * just use the same context space as for everything else, and relocatable
 * code that I can GC normally. This suggests I need read-write-exec permit
 * on all pages at least for every context. It seems I can do this via 
 * mprotect(2) or personality(2) (the latter with READ_IMPLIES_EXEC).
 * 
 * I'm perfectly okay with making Wikilon just run on Linux. I can leverage
 * linux-specific features in this context.
 *
 * Volatile Resources: use a counting Bloom filter at the wikrt_env layer
 * to track ephemeral resources. 
 */


#define WIKRT_H
#endif

