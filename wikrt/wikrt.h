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
 * Most operations in this API also use an `errno` like technique to return 
 * additional details about a cause of error, adapting standard error codes.
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
 * The context API is not multi-thread safe. Any parallelism must be
 * achieved via `(par)` annotations or accelerated parallel functions.
 * 
 *
 * From this API, a context should be used from one thread at a time.
 * It doesn't use thread-local storage, so moving between threads is
 * safe. But parallelism should be achieved via `(par)` annotations 
 * or parallel accelerated functions (cf. wikrt_prelude).
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
 * Each context may be bound to a persistent, named dictionary. This
 * dictionary becomes the source of definitions and a default target
 * for transactionally commited updates. An exception is an empty or
 * NULL dictionary name, which specifies a new ephemeral dictionary.
 * Access control is left to the client of this API.
 *
 * This operation returns NULL if the context cannot be allocated at
 * the requested size, which must be at least WIKRT_CX_MINSIZE. 
 */
wikrt_cx* wikrt_cx_create(wikrt_env*, char const* dict, size_t);
#define WIKRT_CX_MINSIZE (1000 * 1000)

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
 * Binaries are the primary input and output representation at this 
 * API layer, and also leveraged for persistence. During evaluation
 * we'll use an efficient intermediate representation, but on read
 * the program must be serialized back to a binary.
 * 
 * For multi-tasking, debugging, profiling, and partial evaluation in
 * a context, multiple registers are used. Registers are named using
 * simple integers, and default to the empty binary. Allocation and
 * management of registers is left to the client.
 *
 * Writes may fail atomically with ENOMEM. If a write fails, nothing
 * was written. Reads may return fewer bytes than requested and set
 * errno to ENODATA (register is empty) or ENOMEM (if serialization
 * hits memory limits).
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

// TODO: 
//   I need functions to search the codebase, e.g. to find all words
//   given a prefix or suffix, full text search, reverse word lookup,
//   etc.. It might also be useful to obtain static type information.
//
//   Support for auto-completion would be convenient, especially if
//   it were type sensitive.

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
 * but clients should take similar care to guard resource IDs.
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
 * This is a stream parser: it assumes we may write more to source
 * then parse further. To fully parse source generally requires it
 * terminate in an SP or LF. Code ending in `foo` would not fully
 * parse because further writes may extend it to `foobar`. Likewise,
 * `[code]` may extend to `[code]@dict`.
 *
 * The EAGAIN error is used to indicate that parse might succeed if
 * more code (perhaps an SP or LF separator) is written to source.
 */
bool wikrt_parse(wikrt_cx*, wikrt_r src, wikrt_r dst);

/** Program Evaluation
 * 
 * Evaluation of Awelon code rewrites a program to an equivalent that
 * is closer to normal form, like `(6 * 7)` can be rewritten to `42`
 * in arithmetic. Wikilon interprets a binary register as containing
 * a program to evaluate. This function performs the full, in-place 
 * evaluation on the program and values represented within it. (This
 * is also the only API function that evaluates final blocks.)
 *
 * Evaluation assumes valid input, and will simply fail with EBADMSG
 * if the input doesn't fully parse (see wikrt_parse). Evaluation 
 * may also fail due to resource quotas (ETIMEDOUT or ENOMEM). Use
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
 * external binary objects as secure hash resources - `%secureHash`.
 *
 * However, a decent implementation of Awelon should support a compact
 * representation for binary data under the hood, using arrays of bytes.
 * This allows efficient operations on binary resources and IO. Wikilon
 * applies the `(binary)` annotation to a list to indicate it should be
 * encoded using large binary array fragments under the hood.
 * 
 * An operation wikrt_reify_binary logically rewrites the binary data
 * within a register to a binary value. Under the hood, an efficient,
 * compact representation is favored. However, if you read the binary,
 * it will serialize as the naive list expansion described above.
 *
 * You can translate back via wikrt_eval_binary.
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
 * The source register must consist of a single Awelon binary value. Use
 * wikrt_eval_data to isolate the value, if necessary. The binary data
 * will be incrementally evaluated and extracted as needed, no more than
 * the requested amount. If the compact binary representation is already
 * used at source, this should be quite efficient.
 *
 * The return value is how many bytes were successfully moved. If this 
 * is less than requested, we'll set errno appropriately: EDOM if source
 * is not a binary value, ENODATA if value is an empty list, or an error 
 * during evaluation (see wikrt_eval). Thus, if you're reading a binary 
 * of unknown size, a zero return with ENODATA is a success condition.
 */
size_t wikrt_eval_binary(wikrt_cx*, wikrt_r src, size_t amt, wikrt_r dst);

/** Parallel Evaluations
 * 
 * Background parallelism is possible when partial evaluation functions
 * (eval data, stream, or binary) initiate parallel computation but do
 * not require the result to return successfully. These computations 
 * continue in the background until they complete, or effort quota is
 * exhausted, or prove unnecessary during garbage collection.
 *
 * Use of wikrt_eval_parallel will wait for all background computations
 * finish (by quota or termination), and will perform work as needed.
 */
bool wikrt_eval_parallel(wikrt_cx*);

/** Effort Quota
 * 
 * To control infinite loops, a context has a finite effort quota. Like
 * memory, this quota is shared by many computations within the context.
 * Unlike memory, it's trivial to allocate more effort and continue. 
 *
 * Effort isn't precise, but it roughly corresponds to CPU microseconds.
 * This is distinct from wall-clock microseconds. With parallelism, the
 * allocated effort may be consumed quickly by several threads.
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
 * further processing. Disabling clears the log. Defaults disabled.
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
 * Note: Manipulating breakpoints with background parallelism is not
 * deterministic. Consider wikrt_eval_parallel to wait for threads.
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
 * elapsed microseconds, bytes allocated, and current call stack. 
 *
 *     400 1000 /foo/bar/baz
 *     100 8000 /blub/glub
 *     300 1200 /foo/bar/qux
 *
 * Each record is independent, with the time elapsed and bytes
 * allocated being reset to zero within the thread upon printing
 * to the log. The top level program is indicated by `/`. Inline
 * optimizations may eliminate some intermediate words from the
 * stack. But, despite lack of precision, this is sufficient for
 * estimating use of computation resources, especially in loops.
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
 * Heap profiling associates closures in memory with an origin word
 * and estimates a total size. This could be useful to debug memory 
 * issues. 
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
 * The two numbers represent byte costs associated with the word for
 * two GC generations. This simplifies analysis of dataflow. We can
 * also output statistics to situate the profile or detect GC thrashing.
 *
 * Note: excludes data from frozen copy-on-write origin context
 */ 
bool wikrt_prof_heap(wikrt_cx*, wikrt_r, wikrt_mem_stats*);

// memoization, allow disable for debugging?

/** Enable Parallel Evaluation
 *
 * A context is single-threaded at this API, but OS-level threads act
 * as virtual CPUs to perform parallel computations in the background.
 * Parallelism is guided mostly by `[computation](par)` annotations.
 * A single OS thread might handle hundreds of tasks, but the load is
 * readily divided among available threads.
 * 
 * A thread will allocate a local workspace upon entering a context,
 * so to leverage parallelism you must ensure sufficient space. Half
 * a megabyte is reasonable per OS thread in the context.
 */
void wikrt_env_threadpool(wikrt_env*, uint32_t worker_count);

// Thoughts: It might be convenient to have `wikrt_eval_parallel` work
// in a multi-threaded manner, adding workers to a specific context.
// (Not relevant for my intended use case, however.)

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

