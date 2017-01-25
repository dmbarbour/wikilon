
#pragma once
#ifndef WIKRT_H
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
 * in nature. I'll try this as an experimental API. (Now added.)
 *
 * Write-Exec: Can I just make it so contexts are fully read-write-exec?
 * I might lose the ability to work on SE-linux, but that isn't a huge 
 * problem for me. Look into mprotect(2) and personality(2).
 *
 * GC of Secure Hash Resources: I'll need some environment-level counting
 * bloom filters. For now, I could probably just use a fixed size filter.
 *
 * Multi-Process Utilities:
 */




#define WIKRT_H
#endif

