/** Wikilon Runtime
 *
 * BACKGROUND
 *
 * An Awelon codebase serves as both codebase and database. Applications
 * are modeled in terms of continuous observation and maintenance of the
 * codebase by multiple software and human agents, using patterns such as
 * publish-subscribe or a variant of tuple spaces. Imperative code can be
 * represented as a work order for an external software agent. But most
 * data is stored in the codebase directly, views computed incrementally
 * like a spreadsheet.
 *
 * Awelon syntax and semantics is very simple, reminiscent of Forth. But
 * rich structure can be presented through editable views. For example,
 * `[38 100 ratio]` might be presented to a user as `38/100`, and editing
 * this to `40/100` would update the source to `[40 100 ratio]`. Editable
 * views can support comments, variables, keywords, conventional loops,
 * list comprehensions, infix notations, and namespaces. Views may expand
 * beyond plain text, to support graphical user interfaces or hypermedia.
 * 
 * We might think of views as distinct languages built upon Awelon, albeit
 * with a restriction that every language have a robust decompiler.
 *
 * Importantly, Awelon evaluates via simple, confluent rewriting to Awelon
 * code. Lazy linking and arity annotations ensure human-meaningful words 
 * such as `ratio` may be present in evaluated results. Hence, carefully
 * designed views may be directly evaluated and results presented in the
 * same form. Code becomes more directly accessible to users than it in
 * more conventional application models.
 *
 * Wikilon is a wiki-inspired interface to Awelon. Access through a web
 * service simplifies a lot of challenges surrounding editable views,
 * multi-agent maintenance, indexing, and continuous observation. That
 * said, Wikilon does support import/export of file resources, and it is
 * feasible to develop a Filesystem in Userspace (FUSE) adapter to view
 * and edit code through more conventional tools.
 *
 * Wikilon runtime aims to support core performance requirements for 
 * Wikilon. These include efficient evaluation, incremental computing, 
 * common update patterns, debugging, and resource control.
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
 * the context from multiple threads - just exclusively one at a time.
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
 * dictionary argument is NULL or empty, a temporary dictionary is
 * used and all commits will fail. 
 *
 * This operation may fail and return NULL, most likely because the
 * context cannot be allocated at the requested size. It is highly
 * recommended that a context be at least a few megabytes, and no
 * more than physical memory (favor stowage over swap).
 */
wikrt_cx* wikrt_cx_create(wikrt_env*, char const* dict, size_t);

/** Clear a context for lightweight reuse. */
void wikrt_cx_reset(wikrt_cx*, char const* dict);

/** Copy one context into another. 
 *
 * Copying a context is the primary means of resizing a context that
 * is too small or too large. Additionally, it may prove useful for 
 * snapshots, forks, or templated computations. Copy may fail (and
 * return false) if the destination is too small (ENOMEM) or was 
 * created for a different environment (EINVAL).
 *
 * Note: The destination context is implicitly reset before copy. 
 */
bool wikrt_cx_copy(wikrt_cx* src, wikrt_cx* dst);

/** Freeze a context for copy on write
 * 
 * Usage is freeze copy* destroy, with the frozen context as the source
 * in the copy. No other actions after freeze are valid. In return, the
 * frozen context supports lightweight logical copies. This offers some
 * advantage in cases where we might copy many times.
 */
void wikrt_cx_freeze(wikrt_cx*);

/** Destroy a context, recover memory. */
void wikrt_cx_destroy(wikrt_cx*);

/** A context knows its parent environment. */
wikrt_env* wikrt_cx_env(wikrt_cx*);

/** Binary Streams
 *
 * Wikilon uses binaries as the primary input and output structure at
 * the API layer - used for updating or accessing the dictionary and
 * also for constructing programs for evaluation.
 *
 * Streams are given stable identity via small integers. Logically, a
 * stream starts empty and you may write to any stream. Reading data
 * from the stream until empty or clearing it will recover associated
 * memory resources. The null stream (value zero) is a special case
 * and may not be read or written (failing with EINVAL).
 *
 * We might think of streams as registers or variables of a context. 
 *
 * Writes may fail with ENOMEM if the context is close to full.
 */
typedef uint32_t wikrt_s;
bool wikrt_write(wikrt_cx*, wikrt_s, uint8_t const*, size_t);
size_t wikrt_read(wikrt_cx*, wikrt_s, uint8_t*, size_t);
bool wikrt_is_empty(wikrt_cx*, wikrt_s);
void wikrt_clear(wikrt_cx*, wikrt_s);

/** Stream Composition
 *
 * Move or copy the source stream, addending the destination stream.
 * This could be useful for snapshots and similar. The move variant
 * will implicitly clear the source, but may have reduced resource
 * overheads. (That said, copies are logical and reasonably cheap.)
 */
bool wikrt_move(wikrt_cx*, wikrt_s src, wikrt_s dst);
bool wikrt_copy(wikrt_cx*, wikrt_s src, wikrt_s dst);

/** Stream Iteration
 *
 * This function enumerates non-empty streams, returning the next
 * non-empty stream after the specified stream, or returning zero if
 * there are no further non-empty streams. The initial use case is to 
 * clear streams modulo a whitelist after wikrt_cx_copy. But this is
 * potentially useful for debugging, saving a session, etc..
 */
wikrt_s wikrt_stream_iter(wikrt_cx*, wikrt_s);

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
 * formed data (EBADMSG), or context memory (ENOMEM). Load also fails 
 * if the stream is not empty (EADDRINUSE).
 */
bool wikrt_load_def(wikrt_cx*, wikrt_s, char const* k);
bool wikrt_save_def(wikrt_cx*, wikrt_s, char const* k); 

/** Secure Hash Resources
 *
 * Awelon leverages secure hashes to identify binary data resources. 
 * This concept is used for codebase structure sharing, large value
 * stowage (an alternative to virtual memory), and to access external
 * binary data from Awelon code.
 * 
 * Awelon's hash of choice is 360 bit BLAKE2b encoded as 60 characters
 * in base64url. Developers can access this via wikrt_hash or operating
 * on a stream.
 *
 * Wikilon runtime can save and load binary resources by secure hash.
 * Saving a stream returns a secure hash if it succeeds. Loading will
 * retrieve the resource if it is known. Wikilon may garbage collect 
 * resources that don't have a reference from codebase or context.
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
 * Durability is optional, and causes commit to also sync updates
 * to disk and await acknowledgements from the OS.
 */
bool wikrt_commit(wikrt_cx*, bool durable);

/** Scan Code for Parse Errors
 * 
 * Awelon language is syntactically very simple. A quick scan can 
 * reliably determine where and whether a parse error will occur.
 * Given program text, the followng function returns a pass/fail
 * boolean and some data points to help diagnose the error.
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

/** Full Program Evaluation
 * 
 * Awelon evaluation involves rewriting a program to an equivalent
 * program that is closer to normal form, much like `6 * 7` can be
 * rewritten to `42` in arithmetic. Wikilon interprets the binary
 * stream as a program and rewrites it as Awelon code. Under the
 * hood a more suitable representation is used, but this API will
 * present programs as binaries.
 * 
 * Evaluation may halt on resource limits, failing with ETIMEDOUT or
 * ENOMEM. See wikrt_set_effort to control timeouts. But we'll have a
 * valid partial evaluation even if we halt on resource limits. Other
 * errors, such as divide by zero or type errors, are represented in
 * the evaluated code via `(error)` annotations.
 *
 * Caution: Evaluation of partial programs is possible, but you must
 * be careful to avoid splitting words like from `foobar` to `foo`.
 */
bool wikrt_eval(wikrt_cx*, wikrt_s);

/** Command Stream Processing
 *
 * Awelon is amenable to processing a stream of commands. A command
 * is a subprogram that cannot be further rewritten by addending the
 * right hand side of a program, for example of form `[args] word` 
 * where the word has arity two. Commands provide a natural boundary
 * for incremental computation, allowing output before evaluation is
 * fully completed.
 *
 *      [proc] commandA => commandB [proc']
 * 
 * Wikilon supports command stream processing by evaluating just far
 * enough to recognize the command outputs then moving commands to
 * addend a separate stream to be read or evaluated further. For the
 * `[args] word` example, `[args]` would generally still be lazy.
 *
 * This operation returns successfully if it moves any command data,
 * and subsequent operations may move more command data. If there is
 * no command data available, this returns false with ENODATA. Also,
 * this can fail to evaluation resource limits (ENOMEM, ETIMEDOUT). 
 */
bool wikrt_eval_cmd(wikrt_cx*, wikrt_s src, wikrt_s dst);

/** Data Stack Evaluation
 *
 * A common application pattern involves an external agent that reads
 * data from the program, makes decisions, then extends the program by
 * injecting new data and commands. This pattern fits REPLs, monadic
 * effects models, and even some GUI apps. In context of Awelon, the
 * observations will generally be performed on the "stack", meaning
 * the rightmost data elements of a program.
 *
 * Wikilon supports this pattern by providing a function to separate
 * a few of those data elements from a source program to be further
 * evaluated, copied, observed, or returned by wikrt_move to source
 * if necessary. This function partially evaluates code, just enough
 * to move the data as a given count of blocks and value words. 
 *
 * On failure, nothing is moved but partial evaluation may modify the
 * source. This fails if the destination is not empty (EADDRINUSE) or
 * if there is insufficient data (ENODATA). It may also fail because
 * of evaluation resource limits (ENOMEM, ETIMEDOUT).
 */
bool wikrt_eval_data(wikrt_cx*, wikrt_s src, uint32_t amt, wikrt_s dst); 

/** Binary Data Input
 * 
 * A binary in Awelon is represented as a list of natural numbers 0..255.
 *
 *      [12 [0 [1 [32 ~ :]:]:]:](binary)
 *
 * This is a naive and inefficient representation, without any support
 * for syntactic sugar. Though, there is special support to reference
 * external binary structures via secure hash resource as `%secureHash`.
 *
 * Any reasonable implementation of Awelon will support an accelerated
 * representation for binary data under the hood, using arrays of bytes.
 * Wikilon is no exception. Wikilon recognizes the `(binary)` annotation
 * to indicate that a list structure should be formatted as binary data.
 *
 * At this API layer, Wikilon can logically convert a binary stream to 
 * the Awelon list of byte values as described above, but without the
 * inefficient intermediate structure of a naive Awelon representation.
 * Use of wikrt_move can make the data available to other computations.
 * 
 * Further conversions, as from binary to texts or vice versa, are left
 * to basic accelerated functions, e.g. an accelerated `utf8-to-text`.
 */
bool wikrt_stream_to_binary(wikrt_cx*, wikrt_s);

/** Streaming Binary Data Output
 *
 * Awelon represents binary data as a simple list of byte values, that
 * is natural numbers in the range 0 to 255. However, lists in Awelon
 * may represent unbounded streams via lazy evaluation. The idea here
 * is to extract binary data from a source program, moving available
 * bytes into an output stream and performing evaluation as needed.
 *
 * The source must contain a single datum (you can use wikrt_eval_data
 * to separate it from another program). Data extracted is appended to
 * the destination stream and removed from the source.
 *
 * In each step, we can specify a maximum number of bytes to extract,
 * and we'll return how many were successfully extracted. If these 
 * values differ, we have a failure. Failure may occur if there is no
 * source program (ENOENT) or if the program doesn't evaluate to the
 * binary list (EDOM) or if all available data has been extracted 
 * (ENODATA), or due to resource limits (ENOMEM, ETIMEDOUT).
 */
size_t wikrt_extract_binary(wikrt_cx*, wikrt_s src, size_t amt, wikrt_s dst);


/** Effort Quota
 * 
 * To control infinite loops, each context has a finite effort quota
 * for evaluations. When exhausted, evaluation fails with ETIMEDOUT.
 * However, the rewritten program will have a valid state after this
 * failure. You can easily reset the effort quota and continue.
 * 
 * Effort is specified in CPU microseconds, but is not very accurate.
 * Parallel computations may exhaust the effort quota very quickly.
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
 * precise memory and fragmentation stats. 
 */
void wikrt_cx_gc(wikrt_cx*);

/** Overview of a context's memory usage. */
typedef struct wikrt_mem_stats { 
    uint64_t    gc_bytes_processed; // ~ total GC effort 
    uint64_t    gc_bytes_collected; // ~ useful GC effort
    size_t      memory_last_gc;     // memory after last GC
    size_t      memory_in_use;      // memory usage currently
    size_t      memory_maximum;     // maximum memory usage
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

/** Parallel Evaluation
 *
 * A context is single-threaded at this API, but worker threads from
 * the environment layer may join in to help compute a result based
 * on (par) annotations or accelerators.
 *
 * You may configure a thread pool size, which will be shared by all
 * contexts. You should not configure this to more than the number of
 * CPUs on the current machine.
 */
void wikrt_env_threadpool(wikrt_env*, uint32_t pool_size);

// TODO:
// low level parallelism: configure GPGPU or OpenCL cloud service
// high level parallelism: configure cloud distribution for KPNs

/** Accelerated Prelude
 *
 * AO is a simple language with a tiny core of primitives. To achieve
 * performance, an AO runtime is expected to *accelerate* a useful set
 * of functions and data types, replacing the naive implementation by
 * optimized forms under the hood. For example, unary natural numbers 
 * are represented by machine words, and natural number arithmetic is
 * implemented using fast CPU arithmetic.
 *
 * This runtime will expose its accelerators by simply writing out a
 * suitable dictionary to the specified stream. This is less complete
 * than a standard library, but might be used as a seed for a larger
 * dictionary.
 */
bool wikrt_write_prelude(wikrt_cx*, wikrt_s);

/** Filesystem-local Persistence
 *
 * Multiple processes may share a database, albeit limited to the
 * same OS kernel because shared memory mutexes and file locks are
 * used. This supports command line utilities and background tasks.
 *
 * If a process crashes or is killed, it may be necessary to halt
 * all processes sharing a database to ensure locks are reset.
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

