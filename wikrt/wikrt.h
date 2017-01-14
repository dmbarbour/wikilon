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
 * Create and configure wikrt_env persistence environment. Create a 
 * wikrt_cx context for evaluation.
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
 * After creation, the environment should be further configured. At
 * the very least, one should configure the persistence layer with
 * wikrt_db_open. Creation may return NULL if allocation fails.
 */
wikrt_env* wikrt_env_create();

/** Release environment resources. 
 *
 * Perform only after destroying contexts. 
 */
void wikrt_env_destroy(wikrt_env*);

/** Create a context for computation. 
 *
 * If the given dict name is NULL, we use a volatile dictionary
 * and update commits will fail. Otherwise, we bind a persistent
 * dictionary with the given name.
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
 * The destination is reset prior to copy.
 */
bool wikrt_cx_copy(wikrt_cx* src, wikrt_cx* dst);

/** Freeze a context for copy on write. (Experimental!)
 * 
 * A frozen context may be used only as the source of copy or destroyed.
 * Instead of a deep copy, frozen context resources are reference counted
 * and shared with each copy. Destruction of this frozen context forbids
 * further direct copies, but resources remain in memory until the last
 * copy is reset or destroyed.
 * 
 * If the frozen context contains compiled, cached functions relevant to
 * operations in the destination, this may offer significant performance
 * benefits similarly to copy-on-write shared library objects. In theory.
 * I'll need to test whether this helps in practice.
 */
void wikrt_cx_freeze(wikrt_cx*);

/** Destroy a context, recover memory. */
void wikrt_cx_destroy(wikrt_cx*);

/** A context knows its parent environment. */
wikrt_env* wikrt_cx_env(wikrt_cx*);

/** Codebase Access and Update
 * 
 * Once a context is loaded with a named dictionary, clients can get or
 * set definitions for words like "fibonacci" or hierarchical structure
 * such as "@math". For import and export, a context dictionary can be
 * accessed or updated as a singular secure hash resource via NULL key.
 *
 * On failure, we'll return false and set errno appropriately.
 *
 * - EKEYREJECTED - invalid key string (set or get)
 * - ENOMEM - not enough context memory (set or get)
 * - ENOENT - requested resource undefined (get)
 * - ENOBUFS - not enough space in buffer (get)
 * - EBADMSG - data does not parse correctly (set)
 * 
 * Otherwise we'll return true and get or set data. On get, we copy data
 * into the provided buffer (buffer size is provided on input, data size
 * on output; if NULL buffer will report only the data size). On set, we
 * record our update in the local context, but we must later commit the
 * update to make it last.
 */
bool wikrt_set_def(wikrt_cx*, char const* key, uint8_t const*, size_t);
bool wikrt_get_def(wikrt_cx*, char const* key, uint8_t*, size_t*);

/** Transactional Persistence
 * 
 * Wikilon runtime supports an optimistic concurrency model. A context
 * has an implicit transaction, with a record of reads and writes. On
 * commit, we validate there are no conflicts from a concurrent update
 * at the granularity of symbols read and written. If no conflicts are
 * detected, we preserve the updates and return true. Conflict detection
 * may be conservative, with rare false positives.
 *
 * A transaction may fail for mundane reasons like context memory limits
 * (ENOMEM), but the main is conflicting concurrent update (ESTALE). On
 * failure, we return false and set errno appropriately. 
 *
 * Durability is not guaranteed unless 'durable' is specified on commit.
 * Updates from a non-durable transaction may be lost if the process is
 * halted suddenly, for example by signal or power failure. But see 
 * wikrt_sync, too.
 *
 * If optimistic concurrency is not suitable for your use case, it's up
 * to you to provide external controls. For example, single threaded 
 * writes from a queue will avoid all read-write conflicts.
 *
 * Note: Wikilon runtime also ensures snapshot consistency for reads.
 * A write-only or read-only transaction won't fail due to conflict.
 */
bool wikrt_commit(wikrt_cx*, bool durable);

// ideas:
//  merge concurrent updates (inverse transaction?)
//  declare read-only transaction (no need for read log, no commit)
//  wikrt_sync

/** Resource Provision and Access
 *
 * Resources are immutable binaries referenced by secure hash. Awelon
 * uses resources for representation of dictionaries, for large data
 * stowage (a precise alternative to virtual memory), and for access
 * to binary data from the programming language.
 * 
 * Resources are garbage collected unless held by context or reference.
 * Adding a resource to a context will prevent GC until that context is
 * destroyed or reset.
 *
 * The hash of choice is 360 bit BLAKE2b encoded as 60 characters in
 * base64url. Developers can access this via wikrt_hash or as optional
 * output from wikrt_add_rsc. I assume a buffer of WIKRT_HASH_SIZE.
 *
 * Note: resource storage may be shared among independent computations,
 * with the secure hash acting as a bearer token authorizing access. So
 * it is important for wikrt_get_rsc to resist timing attacks. Wikilon
 * ensures no more than the first 60 bits are exposed to timing attack.
 */

#define WIKRT_HASH_SIZE 60
void wikrt_hash(char* h, uint8_t const*, size_t);
bool wikrt_add_rsc(wikrt_cx*, char* h, uint8_t const*, size_t);
bool wikrt_get_rsc(wikrt_cx*, char const* h, uint8_t*, size_t*);

/** Computation 
 *
 * Since I don't want the complexities of
 * One of the most important things we'll do is evaluate a program in
 * context of 
 */

// I might want easy access to computed binary, text, and number data,
// and maybe just to the AST of Awelon code. But for now, my focus is
// upon basic computations, Awelon code as whole input and output.

  ////////////////////
 // MEMORY CONTROL //
////////////////////

// gc, mem stats, etc.

/** @brief Force a full garbage collection of context.
 *
 * This generally isn't necessary, but it's useful if you need more
 * precise memory and fragmentation stats. 
 */
void wikrt_cx_gc(wikrt_cx*);

/** Overview of a context's memory usage.
 *
 */
typedef struct wikrt_mem_stats { 
    uint64_t  gc_cycle_count;     // how many GC cycles?
    uint64_t  gc_bytes_processed; // ~ total GC effort 
    uint64_t  gc_bytes_collected; // ~ useful GC effort
    size_t    memory_lastgc;      // memory in use just after prior GC
    size_t    memory_current;     // memory currently in use
    size_t    memory_nextgc;      // soft maximum (next GC threshold)
    size_t    memory_maximum;     // hard maximum (CXFULL error)
} wikrt_mem_stats;

/** @brief Diagnostic peek at context memory usage.
 *
 * This isn't really sufficient for interesting profiles. But it can
 * at least help diagnose a thrashing computation.
 */
void wikrt_peek_mem_stats(wikrt_cx* cx, wikrt_mem_stats* s);

  ///////////////
 // DEBUGGING //
///////////////
// Debugging, Profiling, Etc..
// 
// - set up tracing for words or (@gate) annotations
// - breakpoints and eventually animations on the same
// - statistics or profiling on words or gates

/** @brief Enable tracing for flexible debugging.
 *
 * Wikilon runtime recognizes a `{&trace}` annotation, which supports
 * lightweight printf/stderr style debugging. This is appropriate more
 * for tracing, TODOs, etc. than for errors (cf. wikrt_trace_write).
 * By default, trace is disabled (i.e. size zero buffer). The buffer may
 * be resized when empty (cf. wikrt_trace_read).
 *
 * For most use cases, a small buffer will be sufficient for debugging.
 * Stable, complete code shouldn't be very noisy.
 */
bool wikrt_trace_enable(wikrt_cx*, size_t trace_buffer_size);

/** @brief Equivalent to invoking the {&trace} annotation.
 *
 *    {&trace} :: ∀v,e. (v * e) → ((trashed v) * e)
 *
 * Tracing will serialize arbitrary values to a special trace buffer.
 * Values that would overflow this buffer are simply dropped. 
 *
 * Arbitrary values may be traced. It's important that developers favor
 * trace messages that are render easily in the development environment,
 * and preferably aren't too large. But Wikilon runtime doesn't enforce 
 * any opinions on the structure of trace messages.
 *
 * As an annotation, tracing is a logical identity. For efficiency,
 * it trashes the argument to avoid need for implicit copies. See 
 * wikrt_trash and the {&trash} annotation.
 */
void wikrt_trace_write(wikrt_cx*);

/** @brief Iterate and process trace messages.
 *
 * Each call returns a C string pointer to the next traced message in
 * the buffer, then returns NULL when the buffer is empty. Each string
 * should be considered invalid upon reading the next message. The 
 * buffer will accept new messages after being fully emptied.
 *
 * Trace messages contain ABC that would simply regenerate the traced
 * value. In most cases, this should just be a plain text.
 *
 * Expected use cases: Streaming output - empty the buffer after each
 * call to wikrt_step_eval. Aggregated output - read the buffer only  
 * after the computation completes. 
 *
 * Trace messages are preserved by wikrt_cx_reset and wikrt_set_error.
 */
char const* wikrt_trace_read(wikrt_cx*);

  ///////////////////////
 // PEFORMANCE TUNING //
///////////////////////
/** @brief Configure environment level thread pool.
 *
 * Parallel computation is performed by a pool of worker threads at
 * the environment level. These threads only operate on contexts
 * that are actively being evaluated from the API-layer, and only
 * where parallelism is explicitly annotated.
 *
 * The default pool size is zero. Increasing the thread pool will
 * perform immediate allocation of the worker threads. Shrinking it
 * is asynchronous, but eventually extra workers will be dropped.
 */
void wikrt_env_threadpool(wikrt_env*, uint32_t pool_size);

  /////////////////////////////////////
 // DATABASE AND PERSISTENCE ENGINE //
/////////////////////////////////////

/** @brief Associate environment with persistence resources.
 * 
 * At the moment, this may only be set once, prior to context creation.
 * An LMDB database is created in the designated directory. Multiple
 * processes should not share the database. A simple `flock()` is applied
 * to resist accidental reuse.
 */
bool wikrt_db_open(wikrt_env*, char const* dirPath, uint32_t dbMaxMB);

/** @brief Flush pending writes to disk. 
 *
 * Wikilon runtime database does not guarantee 'durability' of writes
 * by default. Use of wikrt_db_sync will push pending writes to disk.
 */
void wikrt_db_sync(wikrt_env*);

/** @brief Force garbage collection on stowage and persistence layer.
 *
 * Wikilon runtime does not frequently garbage collect the database
 * layer, and may favor incremental or imprecise collection in most
 * cases. Explicitly requesting GC, however, will force deep, precise
 * analysis. 
 */
void wikrt_db_gc(wikrt_env*);

/** @brief Deep copy an environment's database.
 *
 * This is useful for backup, and potentially for compaction if a 
 * database has many empty pages. Returns false if it fails for any
 * reason, and `errno` may be set appropriately. Note that this does
 * not imply GC, and you probably want to force GC before copy.
 */
bool wikrt_db_clone(wikrt_env*, char const* copyPath);

#define WIKILON_RUNTIME_H
#endif
