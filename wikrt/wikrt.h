
#pragma once
#ifndef WIKRT_H
/** NOTES AND IDEAS
 * 
 * Dictionary Names: valid Awelon words up to 63 bytes are accepted as is.
 * Anything else is rewritten via secure hash to 64 bytes of base64url.
 * Outside of lower level import/export or timing, this should be invisible
 * to our API clients.
 *
 * Copy on Write: I can introduce wikrt_cx_freeze action to the API such that
 * subsequent copies of a frozen context are logical, shallow, copy-on-write
 * in nature. I'll try this as an experimental API. (Now added.)
 */


/** Freeze a context for copy-on-write. (Experimental)
 *
 * Once a context is frozen, the only valid operations on it are 
 * copy (with the frozen context as the source) and destroy. Each
 * copy will be logical, performing a simple incref. When destroyed,
 * the context may no longer be copied, but will remain in memory
 * until all clients are also destroyed.
 *
 * This only improves performance for use cases where a context
 * is copied many times AND has a lot of useful cached resources.

can potentially improve performance for use cases where a
 * context will be copied many times.


 * Normally wikrt_cx_copy will perform a full, deep copy of context
 * resources. But, for some use cases (like templated computation),
 * this can be inefficient. An alternative is copy-on-write behavior,
 * which requires freezing the source before sharing it among multiple
 * copies.
 *
 * When a frozen context is provided as `src` to wikrt_cx_copy, we
 * will logically incref the source instead. Upon wikrt_cx_destroy,
 * the context remains in memory until no remaining clients require
 * it. Updates to a frozen context, including reset, will fail.
 */


 *
 * Context Sharing: ideally, copy of a context would be logical, e.g. via
 * copy-on-write. This requires that *both* copies of our original source
 * are 

I would like to support lightweight sharing of partial
 * results and computations. The wikrt_cx_copy() option is a decent start
 * here, but doesn't permit easy sharing and reuse of data. What I need 
 * instead is a sort of copy-on-write option. 
 */

#define WIKRT_H
#endif

