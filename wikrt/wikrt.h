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
#define WIKRT_API_VER 20170314

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
 * dictionary argument is empty (or NULL), a fresh volatile dictionary 
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

/** IO with Binary Registers
 *
 * Binaries are the primary input and output structure at the API 
 * layer - used for updating or accessing the dictionary and also
 * for constructing programs for evaluation. We use an efficient
 * representation during evaluation, but present any result as a
 * binary.
 *
 * At this API, multiple binaries may be managed using registers.
 * By default, every register is empty. Writes addend the right 
 * while reads process the left, allowing for simple streaming
 * models. A binary can be copied or moved to another register.
 *
 * Writes can fail with ENOMEM. Reads can fail with ENOMEM or 
 * ENODATA if the returned size is less than the requested size.
 */
typedef uint64_t wikrt_r;
bool wikrt_write(wikrt_cx*, wikrt_r, uint8_t const*, size_t);
size_t wikrt_read(wikrt_cx*, wikrt_r, uint8_t*, size_t);
bool wikrt_is_empty(wikrt_cx*, wikrt_r); // test if stream is empty
void wikrt_clear(wikrt_cx*, wikrt_r); // stream is empty after clear
bool wikrt_move(wikrt_cx*, wikrt_r src, wikrt_r dst);
bool wikrt_copy(wikrt_cx*, wikrt_r src, wikrt_r dst);

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
 * like bad key (EKEYREJECTED) or badly formed data (EBADMSG), or lack
 * of context memory (ENOMEM). Loads fail if the target register is not
 * initially empty (EADDRINUSE).
 *
 * There is no failure for 'undefined' symbols. Every symbol has a 
 * default definition: words default as defined to themselves, and
 * the empty string is used for empty dictionaries. 
 * 
 * Note: Updating a definition is atomic but semantically awkward in
 * context of background parallel computation. It is recommended you
 * wait until wikrt_eval_parallel succeed before saving, unless the
 * update is monotonic (previously undefined).
 */
bool wikrt_load_def(wikrt_cx*, wikrt_r, char const* k);
bool wikrt_save_def(wikrt_cx*, wikrt_r, char const* k);

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
bool wikrt_load_rsc(wikrt_cx*, wikrt_r, char const* h);
bool wikrt_save_rsc(wikrt_cx*, wikrt_r, char* h); 

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

/** Fast Scan for Parse Errors
 * 
 * Awelon language is syntactically simple, and a quick scan can easily
 * determine where and whether a parse error will occur. The parse_check
 * function provides this service, returning a pass/fail boolean and an
 * optional data output to indicate where the first error occurs. This
 * function performs no allocations.
 * 
 * Note: This matches behavior of wikrt_parse which assumes streaming,
 * so objects at end of input are only accepted after a clear separator
 * (SP or LF). This simplifies streaming and concatenative composition.
 */
typedef struct wikrt_parse_data { 
    size_t accepted; // valid program prefix
    size_t parsed;   // accepted modulo balance
    size_t balance;  // count of unmatched '['
    size_t scanned;  // where is error noticed?
} wikrt_parse_data;
bool wikrt_parse_check(uint8_t const*, size_t, wikrt_parse_data*);

/** Streaming Parse of Code
 * 
 * Parse code in source, moving it to destination such that whatever
 * remains in source was not parsed. Parsing will normalize whitespace  
 * but otherwise doesn't modify the code. Parse succeeds if everything
 * from the input is moved, otherwise it fails, returning false and
 * setting errno to EAGAIN, EBADMSG, or ENOMEM.  
 *
 * This is a stream parser: it assumes we may write more to source.
 * Thus, when source ends in `foo` we don't immediately accept that
 * because further writes may extend source to `foobar`. Likewise, 
 * `[code]` to `[code]@dict`. The EAGAIN error is used when writing
 * more to source might cause the parse to succeed. Adding a final
 * SP or LF at the end of input may be necessary.
 */
bool wikrt_parse(wikrt_cx*, wikrt_r src, wikrt_r dst);

/** Program Evaluation
 * 
 * Evaluation of Awelon code rewrites a program to an equivalent that
 * is closer to normal form, like `(6 * 7)` can be rewritten to `42`
 * in arithmetic. Wikilon interprets the binary stream as containing
 * a program to evaluate. This function performs a deep, in-place 
 * evaluation on the program and values represented within it.
 *
 * Evaluation assumes valid input, and will simply fail with EBADMSG
 * if the input doesn't fully parse (see wikrt_parse). Evaluation 
 * may also fail due to resource limits (ETIMEDOUT or ENOMEM). Use
 * of wikrt_set_effort can help control timeouts. 
 */
bool wikrt_eval(wikrt_cx*, wikrt_r);

/** Interactive Data Stack Evaluation
 *
 * A common application pattern involves interaction with an external 
 * agent that reads data computed by a program, makes decisions, then
 * extends the program by injecting new data and operations. This fits
 * REPLs, monadic effects models, even some GUI apps. In Awelon, these
 * observations will generally be performed on the "stack", referring
 * to the rightmost data elements of a program.
 *
 * To support this pattern, Wikilon provides this partial evaluator to
 * move these stack elements. This enables agents to isolate, observe,
 * and manipulate just parts of a stack or data structure, preserving 
 * background parallel computation for unobserved program fragments.
 * Evaluation will also be minimal, focused on a data goal.
 *
 * On failure, nothing is moved, but partial evaluation may modify the
 * source. This fails when the destination is not empty (EADDRINUSE) or
 * if there are insufficient data elements (ENODATA), or due to failure
 * of evaluation (see wikrt_eval).
 */
bool wikrt_eval_data(wikrt_cx*, wikrt_r src, uint32_t amt, wikrt_r dst); 

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
 * This operation returns successfully if it moves any command, but
 * does not guarantee it returns all the commands. That is, further
 * calls to wikrt_eval_stream may move more from src to dst. If no
 * further output commands can be computed, we'll return false with
 * EAGAIN, indicating more must be written to source. Otherwise, we
 * may fail at the evaluation layer (see wikrt_eval).
 */
bool wikrt_eval_stream(wikrt_cx*, wikrt_r src, wikrt_r dst);

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
 * uses a compact chunked array representation under the hood. Further
 * processing of the binary can use normal Awelon layer functions.
 */
bool wikrt_reify_binary(wikrt_cx*, wikrt_r);

/** Evaluation of Streaming Binary Data Output
 *
 * Awelon represents binary data as a simple list of byte values, that
 * is natural numbers in the range 0 to 255. However, lists in Awelon
 * may represent unbounded streams via lazy evaluation. The goal here
 * is to extract binary data from such a list, moving it to the output
 * stream, incrementally as needed.
 *
 * The input stream must consist of a single Awelon binary list value.
 * This can be achieved via wikrt_eval_data, assuming binary data type.
 * Data is incrementally evaluated, extracted, and moved, up to the 
 * requested amount, translating from the Awelon binary value to the
 * stream-level binary for efficient output.
 *
 * The return value is how many bytes were actually moved. If this is
 * less than requested, we'll also set errno appropriately: ENOENT if
 * the source is not a value, EDOM if the value is not binary, ENODATA
 * on the empty list. Evaluation errors also apply (see wikrt_eval).
 */
size_t wikrt_eval_binary(wikrt_cx*, wikrt_r src, size_t, wikrt_r dst);

/** Parallel Evaluations
 * 
 * Background parallelism is possible when partial evaluation functions
 * (eval of data, stream, or binary) use `(par)` to initiate parallel
 * computation but the result is unnecessary for partial evaluation to
 * return successfully. These computations continue until they complete,
 * prove unnecessary (via garbage collection), or quotas are exhausted.
 *
 * Use of wikrt_eval_parallel will wait for all background computations
 * finish, one way or the other. This returns 'true' if evaluation halts
 * successfully, otherwise returns false with ENOMEM or ETIMEDOUT. There
 * is no risk of a parse error here, given `(par)` tasks can only be
 * constructed after a successful parse.
 */
bool wikrt_eval_parallel(wikrt_cx*);

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
//   options: localization (rewrite hierarchical words shared by parent)
//            rewrite optimizations (provided by dictionary?)
//            data plumbing optimizations
//   better to make visible optimizations explicit via annotations
//   except localization, which should probably be the default
//   for now let's just focus on getting it working

/** @brief Force a full garbage collection of context.
 *
 * This generally isn't necessary, but it's useful if you need more
 * precise memory profiles. This temporarily interrupts any background
 * parallel computations, so it should be applied with caution.
 */
void wikrt_cx_gc(wikrt_cx*);

/** Trace Log Debugging
 *
 * Log-based debugging, like printing to stderr, is conventional and
 * convenient. Awelon supports this via `(trace)` annotations. Code 
 * of form `[M](trace)` rewrites to `[M]` and if tracing is enabled
 * it also addends subprogram `M` to the trace log.
 * 
 * The trace log must be moved to a client register for reading or
 * further processing, to simplify interactions with any background
 * threads. Disabling will clear the log. Defaults to disabled.
 */
void wikrt_debug_trace(wikrt_cx*, bool enable);
bool wikrt_debug_trace_move(wikrt_cx*, wikrt_r);

/** Breakpoint Debugging
 *
 * Breakpoints may be set or unset for specific words. When set, that
 * word will behave as if undefined during normal evaluation. The only
 * exception is wikrt_debug_eval_step, which may bypass a breakpoint.
 * 
 * Stepping an evaluation will forcibly link a word, albeit within the
 * limits of lazy linking. Some targets like WIKRT_DEBUG_STEP_LEFTMOST
 * will focus on the program spatially instead of targeting a word.
 *
 * Awelon's rewrite semantics work nicely with breakpoint debugging,
 * enabling animation of evaluation 'frames'. While it costs memory,
 * this can readily subsume tracing and provide better context. This
 * can similarly support time-travel debugging.
 *
 * Note: Breakpoints and parallelism together can be awkward. It is
 * recommended we wait for wikrt_eval_parallel to succeed between
 * breakpoint debug operations, as we should for wikrt_save_def.
 */
bool wikrt_debug_breakpoint(wikrt_cx*, char const* word, bool set);
bool wikrt_debug_eval_step(wikrt_cx*, wikrt_r, char const* target);
#define WIKRT_DEBUG_STEP_LEFTMOST  "(l)"
#define WIKRT_DEBUG_STEP_RIGHTMOST "(r)"
#define WIKRT_DEBUG_STEP_UNIVERSAL "(*)"

/** Stack Profiling
 * 
 * Like conventional language runtimes, Wikilon uses a call-return
 * stack to efficiently represent the current continuation. Probing
 * this stack periodically offers an imprecise statistical profile
 * of where our program resources are expended. This helps debug
 * performance problems.
 * 
 * The profile is recorded as a log of triples, one per line, with
 * a current call stack, elapsed microseconds, and bytes allocated.
 *
 *     /foo/bar/baz 400 1000
 *     /blub/glub   100 8000
 *     /foo/bar/qux 300 1200
 *
 * This record is imprecise, but enough entries can still provide a
 * useful estimate of where evaluation efforts are expended.
 *
 * The profile log must be moved to a client register for reading or
 * further processing, to simplify interactions with any background
 * threads. Disabling will clear the log. Defaults to disabled.
 */
void wikrt_prof_stack(wikrt_cx*, bool enable);
bool wikrt_prof_stack_move(wikrt_cx*, wikrt_r);

/** Overview of a context memory usage. */
typedef struct wikrt_mem_stats { 
    uint64_t    gc_bytes_processed; // total GC effort 
    uint64_t    gc_bytes_collected; // useful GC effort
    size_t      elder_gen_size;     // total elder heap
    size_t      young_gen_size;     // total young heap
    size_t      heap_available;     // free alloc space
    size_t      context_size;       // total context size
} wikrt_mem_stats;

/** Heap Profiling
 *
 * Heap profiling describes long-lived objects in memory. This can be
 * useful for debugging memory usage issues. Wikilon has support for
 * a low precision but reasonably efficient heap profile, associating
 * a word of with each closure based on the syntactic origin of the 
 * outermost block.
 * 
 * Calling wikrt_prof_heap will scan the stable portion of the heap,
 * excluding recent allocations so as to minimize interference with
 * background parallelism. (Call wikrt_cx_gc for up-to-date profile.) 
 * The profile is recorded as lines of `word number number` triples:
 *
 *      foo 1234 5678
 *      bar 9876 5432
 *      ...
 *
 * The two numbers represent byte costs associated with the word, in
 * the old and young GC generations to simplify analysis of dataflow.
 * Besides the profile, which is written to a register, we can also
 * output statistics to situate the profile or detect GC thrashing.
 *
 * Note: this does not profile the copy-on-write frozen context when
 * using wikrt_cx_copy after wikrt_cx_freeze.
 */ 
bool wikrt_prof_heap(wikrt_cx*, wikrt_r, wikrt_mem_stats*);

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

