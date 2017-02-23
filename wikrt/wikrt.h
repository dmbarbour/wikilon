/** Wikilon Runtime
 *
 * This is a runtime for Awelon language code, implemented for Wikilon,
 * a programmable wiki-inspired development environment. Besides running 
 * code, Wikilon runtime manages persistent codebases. Applications in
 * Awelon and Wikilon are leverage RESTful patterns.
 *
 * The Wikilon Runtime API focuses on binary processing. Structured
 * representations and indices may be used under the hood, especially 
 * during evaluation. But the API presents all input and output as 
 * binary data.
 *
 * This runtime is currently implemented only for Linux systems.
 *
 * USAGE
 *
 * - Create and configure the environment, persistence, parallelism, etc..
 * - Create a context bound to a named dictionary for computation, transaction.
 * - Maintain a named codebase with basic key-value updates
 * - Evaluate code
 *
 * Most operations in this API also use `errno` for additional details
 * about the cause of error, adapting common error names. 
 * 
 *  (c) 2015-2017 David Barbour
 *  LICENSE: BSD 3-clause <https://opensource.org/licenses/BSD-3-Clause>
 */
#pragma once
#ifndef WIKILON_RUNTIME_H

#include <stdint.h>
#include <stdbool.h>
#include <stddef.h>

/** Opaque structure for overall Wikilon environment.
 * 
 * A wikrt_env may host multiple named, persistent dictionaries. A
 * named dictionary may be shared by many computation contexts.
 * The environment configures many resources that must be shared by
 * computation contexts, such as persistent storage and parallel
 * processing. 
 *
 * Eventually, some form of networking, for resources or distributed
 * KPN computations, may also be supported. But at the moment, Wikilon
 * runtime is limited to a single process.
 */
typedef struct wikrt_env wikrt_env;

/** Opaque structure representing a context for computation.
 *
 * A context is associated with a volume of memory and a dictionary. 
 * Given a context, we can evaluate code and search or transactionally 
 * update the dictionary. 
 *
 * A context is single-threaded at this API, but supports multi-tasking
 * via multiple streams, and background parallel evaluations via `(par)`
 * annotations. No thread-local storage is used, so you're free to use
 * the context from multiple threads - just exclusively, one at a time.
 *
 * Wikilon uses `errno` to return extra error information when returning
 * `false` in success/fail cases.
 */
typedef struct wikrt_cx wikrt_cx;

/** Support a simple consistency check for dynamic library.
 *
 * Compare WIKRT_API_VER to wikrt_api_ver(). If they aren't the same,
 * then your app was compiled against a different interface than the
 * linked object implements. This is a sensible sanity check.
 */
uint32_t wikrt_api_ver();
#define WIKRT_API_VER 20170103

/** Accelerated Prelude
 *
 * Awelon is a simple language with a very small set of primitives. To
 * achieve performance, a runtime uses accelerators and annotations. To 
 * communicate the available annotations and accelerators, this runtime
 * provides a 'prelude' - a small dictionary in the Awelon import/export
 * format that may be used to seed other dictionaries or perused by an
 * interested human. 
 *
 * Definitions and documentation included. Ideally, the prelude should
 * provide enough information to a human to get started with Awelon or
 * even reimplement it. This is presented as a simple C string, compiled
 * into the runtime. This should be no larger than a few dozen kilobytes.
 */
char const* wikrt_prelude();


/** Create a Wikilon environment. 
 *
 * After creation, the environment should be further configured with
 * wikrt_db_open and other parameters. Creation may return NULL if
 * allocation fails.
 */
wikrt_env* wikrt_env_create();

/** Release environment resources. 
 *
 * Perform only after destroying contexts. 
 */
void wikrt_env_destroy(wikrt_env*);

/** Construct a context for computation. 
 *
 * A named dictionary is part of the context. This dictionary is used
 * when linking definitions or persistent updates. A web service can
 * provide some access control at this named dictionary level. If the
 * dictionary argument is NULL or empty, a fresh volatile dictionary 
 * is used and commits will fail. 
 *
 * This operation may fail and return NULL, most likely because the
 * context cannot be allocated at the requested size. It is highly
 * recommended that a context be at least a few megabytes, and never
 * more than physical memory (favor stowage over disk swap space).
 *
 * The client of this API is responsible for access control to the
 * named dictionary.
 */
wikrt_cx* wikrt_cx_create(wikrt_env*, char const* dict, size_t);

/** Clear a context for lightweight reuse. */
void wikrt_cx_reset(wikrt_cx*, char const* dict);

/** Copy one context into another. 
 *
 * Copying a context is the primary means of resizing a context that
 * is small or oversized. Additionally, it may prove useful for some 
 * snapshots, forks, or templated computations. Copy may fail (and
 * return false) if the destination is too small (ENOMEM) or was 
 * created for a different environment (EINVAL).
 *
 * Notes: The destination context is implicitly reset by the copy.
 */
bool wikrt_cx_copy(wikrt_cx* src, wikrt_cx* dst);

/** Freeze a context for fast logical copy.
 * 
 * A frozen context may be copied or destroyed. Copies of a frozen
 * context are logical, referencing frozen context memory instead
 * of replicating it. This makes copies cheap, with copy-on-write
 * performance properties. Destruction of the frozen context will
 * be deferred until all references are reset or destroyed.
 */
void wikrt_cx_freeze(wikrt_cx*);

/** Destroy a context, recover memory. */
void wikrt_cx_destroy(wikrt_cx*);

/** A context knows its parent environment. */
wikrt_env* wikrt_cx_env(wikrt_cx*);

/** Binary IO Model
 *
 * Wikilon uses binaries as the primary input and output structure at
 * the API layer - used for updating or accessing the dictionary and
 * also for constructing programs and evaluation. Under the hood, a
 * program uses a more sensible representation during evaluation. But
 * Wikilon still presents it as a binary at this API.
 *
 * Streams are identified by small natural numbers. Streams have two
 * primary states: they're either empty or they have some data. You
 * can simply addend data to any stream except the NULL (0) stream, 
 * which remains always empty. Clients can use streams similarly to
 * named registers. 
 *
 * Writes may fail, likely with ENOMEM if the context is full. Reads
 * "fail" if the returned byte count does not match the request, and
 * may fail with ENODATA after exhausting the stream, or with ENOMEM
 * when converting from an under-the-hood representation (which may
 * require a stack).
 *
 * It's up to the client to track stream identifiers in use. This API
 * does not provide iteration over active stream IDs.
 */
typedef uint64_t wikrt_s;
bool wikrt_write(wikrt_cx*, wikrt_s, uint8_t const*, size_t);
size_t wikrt_read(wikrt_cx*, wikrt_s, uint8_t*, size_t);
bool wikrt_is_empty(wikrt_cx*, wikrt_s); // test if stream is empty
void wikrt_clear(wikrt_cx*, wikrt_s); // stream is empty after clear

/** Stream Composition
 *
 * Move or copy the source stream, addending the destination stream.
 * This could be useful for snapshots and similar. The move variant
 * will implicitly clear the source, but may have reduced resource
 * overheads. That said, copies are logical and reasonably cheap.
 */
bool wikrt_move(wikrt_cx*, wikrt_s src, wikrt_s dst);
bool wikrt_copy(wikrt_cx*, wikrt_s src, wikrt_s dst);

/** Codebase Access and Update
 *
 * An Awelon codebase is effectively a key-value database, albeit with
 * characteristics that permit treating an entire codebase as a value.
 * The normal use case is to operate on Awelon words and definitions.
 * But symbols of form "@dict" refer to a hierarchical dictionary via
 * secure hash. Symbol "@" similarly refers to codebase as a whole.
 *
 * Data saved through this API are not immediately persistent. Instead,
 * one must further 'commit' the update. However, updated definitions
 * will immediately influence further evaluation.
 *
 * These operations may fail and return false for a variety of reasons
 * like bad key (EKEYREJECTED) or an undefined value (ENOENT) or badly
 * formed data (EBADMSG), or lack of context memory (ENOMEM). Loads 
 * also fail if the stream is not initially empty (EADDRINUSE).
 */
bool wikrt_load_def(wikrt_cx*, wikrt_s, char const* k);
bool wikrt_save_def(wikrt_cx*, wikrt_s, char const* k);

/** Secure Hash Resources
 *
 * Awelon leverages secure hashes to uniquely and globally identify
 * binary values. This concept is useful for network distributions,
 * large value stowage (a virtual memory alternative), versioning 
 * and sharing of entire codebases, and reference to large binary
 * data from Awelon source code (via %secureHash).
 *
 * Awelon uses a 360 bit BLAKE2b secure hash encoded as 60 characters
 * in base64url. A hash in this form can be computed via wikrt_hash.
 *
 * Wikilon runtime can save and load binary resources by secure hash.
 * Saving a stream returns a secure hash if it succeeds. Loading will
 * retrieve the resource only if it is known. However, resources that
 * are not referenced from the persistent dictionary may be garbage
 * collected. The wikrt_test_rsc will check to see whether a 'load' 
 * would work modulo context memory limits to help scan for missing
 * resources.
 *
 * Failure behavior matches load_def, save_def operations.
 *
 * Note: Secure hashes are bearer tokens, authorizing access to binary
 * data. Systems should resist timing attacks to discover known hashes.
 * Wikilon will expose only the initial 60 bits of each hash to timing,
 * but clients must take similar care to guard resource IDs.
 */
#define WIKRT_HASH_SIZE 60
void wikrt_hash(char* h, uint8_t const*, size_t);
bool wikrt_load_rsc(wikrt_cx*, wikrt_s, char const* h);
bool wikrt_save_rsc(wikrt_cx*, wikrt_s, char* h); 

/** Transactional Persistence
 * 
 * Wikilon runtime uses a simple, optimistic concurrency model with
 * transactional update. Reads operate on a snapshot of the database,
 * and reads are recorded (perhaps conservatively). Writes are local
 * to the context until wikrt_commit. Optimistic concurrency is not
 * ideal for all use cases, but more conservative models are readily
 * constructed at higher layers.
 *
 * On success we return true and update the underlying database, but 
 * the read log is preserved until the context is destroyed or reset.
 * On failure, we return false and set errno appropriately - conflict
 * is reported as ESTALE, but running out of space or memory is quite
 * possible, and EROFS is returned if writes are blocked in general.
 *
 * Durability is optional, and waits for a successful commit to flush
 * updates to disk before returning.
 */
bool wikrt_commit(wikrt_cx*, bool durable);

/** Scan Code for Parse Errors
 * 
 * Awelon language is syntactically very simple. A linear scan can 
 * reliably determine where and whether a parse error will occur.
 * Given program text, the followng function returns a pass/fail
 * boolean and enough data points to quickly diagnose most errors.
 *
 * This is probably sufficient for most use cases, especially since
 * we expect projectional editing techniques so Awelon code is oft
 * generated by algorithm.
 */
typedef struct wikrt_parse_data { 
    size_t accepted; // valid program prefix
    size_t parsed;   // accepted modulo balance
    size_t balance;  // count of unmatched '['
    size_t scanned;  // where is error noticed?
} wikrt_parse_data;
bool wikrt_parse_code(uint8_t const*, size_t, wikrt_parse_data*);

/** Program Evaluation
 * 
 * Evaluation of Awelon code rewrites a program to an equivalent that
 * is closer to normal form, like `(6 * 7)` can be rewritten to `42`
 * in arithmetic. Wikilon interprets the binary stream as containing
 * a program to evaluate. This function performs a deep, in-place 
 * evaluation on the program and values represented within it.
 * 
 * Evaluation may halt on resource limits, failing with ETIMEDOUT or
 * ENOMEM. See wikrt_set_effort to control timeouts. But we'll have a
 * valid partial evaluation even if we halt on resource limits. Other
 * errors, such as divide by zero or type errors, are represented in
 * the evaluated code via `(error)` annotations.
 *
 * Caution: Evaluation of partial programs is possible, but you must
 * be relatively careful to avoid truncating a word, and it's best
 * to use wikrt_eval_cmd to extract partial results.
 */
bool wikrt_eval(wikrt_cx*, wikrt_s);

/** Data Stack Evaluation
 *
 * A common application pattern involves interaction with an external 
 * agent that reads data computed by a program, makes decisions, then
 * extends the program by injecting new data and operations. This fits
 * REPLs, monadic effects models, even some GUI apps. In Awelon, these
 * observations will generally be performed on the "stack", meaning
 * the rightmost data elements of a program.
 *
 * To support this pattern, Wikilon provides evaluator that moves top 
 * stack elements from the remainder of the program, enabling the stack
 * to be evaluated, copied, and viewed independently. This evaluation
 * is minimal to move the data, so resulting blocks are not evaluated.
 *
 * On failure, nothing is moved, but partial evaluation may modify the
 * source. This fails when the destination is not empty (EADDRINUSE) or
 * if there is insufficient data (ENODATA). It may also fail because
 * of evaluation resource limits (ENOMEM, ETIMEDOUT).
 */
bool wikrt_eval_data(wikrt_cx*, wikrt_s src, uint32_t amt, wikrt_s dst); 

/** Command Stream Processing
 *
 * Awelon is amenable to processing a stream of commands. A command
 * is a subprogram that cannot be further rewritten by addending the
 * right hand side of a program, for example of form `[args] word` 
 * where the word has arity two. Commands provide a natural boundary
 * for streaming computation, allowing output before a program is
 * fully represented. 
 *
 *      [proc] commandA => commandB [proc']
 *
 * Wikilon supports command stream processing by evaluating just far
 * enough to recognize the command output then moves the commands to
 * a destination stream to be read or evaluated further. Effectively,
 * this focuses the evaluator on the left hand side of our program
 * whereas wikrt_eval_data attends the right hand side. 
 *
 * This operation returns successfully if it moves any command data,
 * but subsequent operations may evaluate further and move more data.
 * If no command data is available, this returns false with ENODATA.
 * Otherwise it may fail due to evaluation limits (ENOMEM, ETIMEDOUT). 
 */
bool wikrt_eval_cmd(wikrt_cx*, wikrt_s src, wikrt_s dst);

/** Binary Data Input
 * 
 * A binary in Awelon is represented as a list of natural numbers 0..255.
 *
 *      [127 [0 [1 [32 ~ :] :] :] :](binary)
 *
 * This is a naive and terribly inefficient representation, of course,
 * taking roughly eight times as much space as raw binary data. Awelon
 * doesn't have syntactic sugar for binaries, though it may reference
 * external binary objects as secure hash resource - `%secureHash`.
 *
 * However, an implementation of Awelon should support an accelerated
 * representation for binary data under the hood, using arrays of bytes.
 * This allows efficient operations on binary resources and IO. Wikilon
 * uses the `(binary)` annotation on a list to indicate it should be
 * encoded using an array of bytes under the hood.
 * 
 * The operation wikrt_reify_binary logically rewrites the binary data
 * within a stream to the binary value format (an ~8x expansion), but
 * actually uses a compact representation under the hood. Inputting a
 * large text might be represented by reifying a binary then applying
 * another accelerated function (like a utf8-to-text conversion). 
 */
bool wikrt_reify_binary(wikrt_cx*, wikrt_s);

/** Evaluation of Streaming Binary Data Output
 *
 * Awelon represents binary data as a simple list of byte values, that
 * is natural numbers in the range 0 to 255. However, lists in Awelon
 * may represent unbounded streams via lazy evaluation. The goal here
 * is to extract binary data from such a list, moving it to the output
 * stream in small steps as needed.
 *
 * The input stream must consist of a single Awelon binary list value.
 * This can be achieved via wikrt_eval_data, assuming binary data type.
 * Data is incrementally evaluated, extracted, and moved, up to the 
 * requested amount, translating from the Awelon binary value to the
 * stream-level binary for efficient output.
 *
 * The return value is how many bytes were actually moved. If this is
 * less than the request, errno will contain some extra information 
 * about why we stopped: there is no source binary (ENOENT) or the
 * type seems wrong (EDOM) or the binary is empty (ENODATA), or due
 * to evaluation resource limits (ENOMEM, ETIMEDOUT).
 */
size_t wikrt_eval_binary(wikrt_cx*, wikrt_s src, size_t amt, wikrt_s dst);

/** Parallel Evaluations
 * 
 * Background parallelism is possible when partial evaluation functions
 * (eval_data, eval_cmd, eval_binary) use `(par)` to initiate parallel
 * computation but the result is not immediately necessary. Background
 * evaluations will continue until completed or collected or they hit
 * a quota limit. They may force a wait (limited by quota) if you try
 * to serialize data.
 *
 * The function wikrt_eval_par behaves as wikrt_eval, except it returns
 * successfully only when all parallel computations complete. It will
 * perform work in the current thread as needed, so even if parallelism
 * is not enabled we can evaluate scheduled parallel tasks.
 *
 * Note: You can use wikrt_env_threadpool to enable parallelism.
 */
bool wikrt_eval_par(wikrt_cx*);

/** Effort Quota
 * 
 * To control infinite loops, a context has a finite effort quota. Like
 * memory, this quota is shared by many computations within the context.
 * Unlike memory, it's trivial to allocate more effort and continue. 
 *
 * Setting effort to zero will immediately halt labor within a thread,
 * while setting a non-zero value may continue background parallelism.
 * Effort is shared by worker threads, so a parallel computation may
 * burn through the effort more quickly.
 *
 * The effort is currently measured in CPU microseconds.
 */
void wikrt_set_effort(wikrt_cx*, uint32_t cpu_usec);

// TODO: evaluator options - rewrite optimizations, localization, etc..
//   options: shallow evaluation (don't evaluate blocks)
//            localization (rewrite hierarchical words shared by parent)
//            rewrite optimizations (provided by dictionary?)
//            data plumbing optimizations
//   for now let's just focus on getting it working
// TODO: debugger outputs - snapshots, trace logs, profiling, etc.

/** @brief Force a full garbage collection of context.
 *
 * This generally isn't necessary, but it's useful if you need more
 * precise memory and fragmentation stats or want to ensure stable
 * performance.
 */
void wikrt_cx_gc(wikrt_cx*);

/** Overview of a context's memory usage. */
typedef struct wikrt_mem_stats { 
    uint64_t    gc_bytes_processed; // total GC effort 
    uint64_t    gc_bytes_collected; // useful GC effort
    size_t      context_size;       // context allocation size
} wikrt_mem_stats;

/** Memory Usage Metrics */
void wikrt_cx_mem_stats(wikrt_cx* cx, wikrt_mem_stats* s);

// maybe get some effort stats, too?
//  CPU cycles, etc.


/** Debugging 
 *
 * Awelon is amenable to rich forms of debugging that involve keeping
 * snapshots, visualizing and animating program state. But I have yet
 * to develop a suitable API.
 *
 * For the short term, you can use (error) to mark erroneous values 
 * that will remain in the output but not be evaluated further such
 * as "to do: fix foo!"(error). Or you can use (trace) for printf
 * style debugging, printing each value as a comment to the trace
 * log configured below.
 *
 * For `[V](trace)` in source, we'll produce `[V](a2)d` in the log,
 * where `(a2)d` effectively marks the value as a comment with no
 * semantic behavior. The only way to "use" this log is to have a
 * human or another agent read it and extract information. The trace
 * annotation is removed from source at the same time.
 */
void wikrt_debug_trace(wikrt_cx*, wikrt_s); 

// Debugging, Profiling, Etc..
// 
// - set up tracing for words or (@gate) annotations
// - breakpoints and eventually animations on the same
// - statistics or profiling on words or gates
// 
// What should the profiling API look like?
//
// - enable profiling, if not by default
// - a write-profile option in general
// - a clear-profile option, potentially 
//

/** Enable Parallel Evaluation
 *
 * A context is single-threaded at this API, but worker threads can act
 * as virtual CPUs to perform parallel computations in the background.
 * Parallelism is guided by (par) annotations and accelerated functions.
 *
 * Within a context, parallel tasks are lightweight, and a worker thread
 * can easily operate on hundreds of tasks. But the the worker itself 
 * will allocate a little space and perform its own GC where possible,
 * so you should provide at least half a megabyte per thread beyond what
 * the single-threaded evaluation requires.
 *
 * Adjusting worker counts may be asynchronous, with the actual count
 * lagging the configured value.
 */
void wikrt_env_threadpool(wikrt_env*, uint32_t worker_count);

// TODO:
// a thread pool supports only local, CPU-layer parallelism
// low level parallelism: configure GPGPU or OpenCL cloud services
// high level parallelism: configure cloud/mesh networks, KPN acceleration

/** Filesystem-local Persistence
 *
 * The specified location is used for codebase management, stowage, 
 * and cached computations. The current implementation uses LMDB for
 * storage as one large file, and shared memory to track secure hash
 * resources held by contexts. The same database may be opened from
 * many environments and processes from a shared Linux kernel.
 *
 * Note: While the runtime attempts to be robust, a crashed process 
 * will not release ephemeral stowage references from shared memory.
 * This hinders GC of old secure hash resources until system reboot.
 */
bool wikrt_db_open(wikrt_env*, char const* dirPath, size_t dbMaxSize);

/** Flush pending writes. Ensures durability of prior transactions. */
void wikrt_db_sync(wikrt_env*);

/** Force GC of secure hash resources (stowage, binaries, etc.). 
 *
 * Resources referenced by a context or from a persistent dictionary
 * are protected. Anything else will normally be garbage collected
 * incrementally, but you can force a full GC with this function 
 * which may take some time to return.
 *
 * Note: GC is not fully precise. References held by contexts are
 * recorded using counting bloom filters or a similar technique.
 */
void wikrt_db_gc(wikrt_env*);

/** Compacting copy and backup of a database. */
bool wikrt_db_copy(wikrt_env*, char const* copyPath);

#define WIKILON_RUNTIME_H
#endif

