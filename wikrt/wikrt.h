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
 * A context is single-threaded, at least with respect to this API. A
 * context may be used from multiple threads over its lifespan, but must
 * be used from at most one at a time. Internal parallelism is configured
 * at the environment layer (see wikrt_set_threadpool).
 *
 * In addition to the context, Wikilon runtime uses 'errno' to provide
 * extra information about API-layer errors. 
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
 * snapshots or templated computation. Copy may fail (and return false)
 * if the destination is too small or belongs to the wrong environment.
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
 * Wikilon uses binaries as the primary input and output structure.
 * We can write or load a binary, read or save one, or evaluate the
 * binary by interpreting it as Awelon code and rewriting it.
 *
 * Streams are given stable identity via small integer. Logically,
 * every stream starts empty and you can just start writing to any
 * stream (no need to allocate). The null stream (value 0) is the
 * special case: all writes to it fail.
 *
 * On read, size information is both input and output - buffer size
 * on input, bytes read on output.
 */
typedef uint32_t wikrt_s;
bool wikrt_write(wikrt_cx*, wikrt_s, uint8_t const*, size_t);
bool wikrt_read(wikrt_cx*, wikrt_s, uint8_t*, size_t*);
void wikrt_clear(wikrt_cx*, wikrt_s);

/** Stream Composition
 *
 * Move or copy the source stream to addend the destination stream.
 * This could be useful for snapshots and similar. The move variant
 * will implicitly clear the source, but avoids the extra resource
 * burdens of copying data.
 */
bool wikrt_concat_move(wikrt_cx*, wikrt_s src, wikrt_s dst);
bool wikrt_concat_copy(wikrt_cx*, wikrt_s src, wikrt_s dst);

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
 * retrieve the resource if it is known, overwriting the target stream. 
 * Wikilon may garbage collect resources that don't have a reference
 * from codebase or context.
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
 * formed data (EBADMSG), or context memory (ENOMEM).
 */
bool wikrt_load_def(wikrt_cx*, wikrt_s, char const* k);
bool wikrt_save_def(wikrt_cx*, wikrt_s, char const* k); 

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
 */
bool wikrt_commit(wikrt_cx*);

/** Transaction Durability
 *
 * Transactions are atomic and serializable by default, but are not
 * guaranteed to be durable. If you set wikrt_durable on a context,
 * the current transaction will wait for data to be acknowledged as
 * synchronized to disk before committing successfully. Otherwise,
 * consider periodic wikrt_sync to push recent updates.
 *
 * Durability is ultimately limited by the underlying filesystem and
 * hardware. Truly robust data requires distributed replication, but
 * that may increase synchronization costs.
 */
void wikrt_durable(wikrt_cx*);

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

/** Evaluation
 * 
 * Awelon evaluation involves rewriting a program to an equivalent
 * program that is closer to normal form, much like `6 * 7` can be
 * rewritten as `42`. In Wikilon, we interpret the current stream 
 * as a program and evaluate as much as possible. We succeed if we
 * reach a normal form, where further evaluation will not cause 
 * further rewrites.
 *
 * Of course, not all programs have normal forms (because infinite
 * loops) and even when they do we cannot always reach them (because
 * time or space limits). Evaluation may fail, returning false and
 * setting errno to ENOMEM or ETIMEDOUT. See wikrt_set_effort below
 * to control the timeout. 
 *
 * Most other errors are represented within the code itself. Modulo
 * imbalanced blocks, code that fails to parse will not be rewritten.
 */
bool wikrt_eval(wikrt_cx*, wikrt_s);

/** Stream Processing
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
 * enough to recognize the command outputs then moving them to another
 * stream. Hence, we split the stream between `commandB` and `[proc']`.
 * In case of `[args] word` the args would be moved, not evaluated. 
 * And the resulting process would not be moved or evaluated, instead
 * remaining available for processing as we addend the source stream.
 *
 * This returns 'true' when nothing more can be moved.
 */
bool wikrt_eval_cmd(wikrt_cx*, wikrt_s src, wikrt_s cmd_dst);

/** Effort Quota
 * 
 * To control infinite loops, each context has a finite effort quota
 * for evaluations. When exhausted, evaluation fails with ETIMEDOUT.
 * Effort is specified in CPU microseconds, though is not accurate.
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
 * At the moment, I only support log-based debugging via (@trace). Any
 * argument to (@trace) will simply be written to the stream specified
 * here, which defaults to the zero stream. This can serve a role like
 * printf-style debugging in C.
 *
 * Awelon is amenable to richer forms of debugging that involve keeping
 * snapshots, visualizing and animating  program state. But I need to
 * think about debugging APIs to make this work nicely, perhaps model
 * the configuration within Awelon code.
 */
void wikrt_debug_trace(wikrt_cx*, wikrt_s); // set (@trace) debug output

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
bool wikrt_write_accel(wikrt_cx*, wikrt_s);

/** Configure filesystem-local database and cache (using LMDB). */
bool wikrt_db_open(wikrt_env*, char const* dirPath, size_t dbMaxSize);

/** Flush pending writes. Ensures durability of prior transactions. */
void wikrt_db_sync(wikrt_env*);

/** Force GC of secure hash resources (stowage, binaries, etc.). */
void wikrt_db_gc(wikrt_env*);

/** Compacting copy and backup of a database. */
bool wikrt_db_clone(wikrt_env*, char const* copyPath);

#define WIKILON_RUNTIME_H
#endif

