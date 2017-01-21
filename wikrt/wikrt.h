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
 * dictionary argument is NULL, a temporary dictionary specific to 
 * the context is used.
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

/** Codebase Access and Update
 *
 * Wikilon runtime provides a simple set/get access to the codebase as
 * a key-value database. Naming conventions are based on standard import
 * and export representation, so most keys are Awelon words but symbols
 * of the form "@math" can refer to hierarchical substructure. 
 *
 * The codebase as a whole be referenced using the NULL key. Dictionaries
 * are represented by secure hash reference to the export resource, and
 * words are defined using valid Awelon code.
 * 
 * On get, the size_t* field should point to buffer size and we'll update
 * it to the data size. If the data buffer is NULL, we'll just return the 
 * size. On set, the NULL data means reset to the default.
 *
 * Failure may occur for various reasons. A bad key (EKEYREJECTED),
 * an undefined (ENOENT), context or database out of memory (ENOMEM,
 * ENOSPC), limited buffer space (ENOBUFS), etc.
 */
bool wikrt_get_def(wikrt_cx*, char const* key, uint8_t*, size_t*);
bool wikrt_set_def(wikrt_cx*, char const* key, uint8_t const*, size_t);

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
 * possible.
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
void wikrt_sync(wikrt_env*);

/** Resource Provision and Access
 *
 * Awelon references binary data by secure hash. This idea is widely
 * used for large data stowage (an alternative to virtual memory), 
 * for lightweight sharing of dictionary import/export structure, and
 * for simple reference to binary data from within the language.
 *
 * The hash of choice is 360 bit BLAKE2b encoded as 60 characters in
 * base64url. Developers can access this via wikrt_hash or as optional
 * output from wikrt_add_rsc, assuming a buffer of WIKRT_HASH_SIZE.
 * 
 * Note: Resource storage may be shared among independent computations,
 * with the secure hash acting as a bearer token authorizing access. So
 * it is important for wikrt_get_rsc to resist timing attacks. Wikilon
 * ensures no more than the first 60 bits are exposed to timing attack.
 */
#define WIKRT_HASH_SIZE 60
void wikrt_hash(char* h, uint8_t const*, size_t);
bool wikrt_add_rsc(wikrt_cx*, char* h, uint8_t const*, size_t);
bool wikrt_get_rsc(wikrt_cx*, char const* h, uint8_t*, size_t*);

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
    size_t balance;  // block balance (unmatched '[')
    size_t scanned;  // where is error noticed?
} wikrt_parse_data;
bool wikrt_parse_code(uint8_t const*, size_t, wikrt_parse_data*);

/** Program Input, Eval, Output 
 *
 * Awelon is amenable to command stream processing. We can output
 * an evaluated stream of `[args]action` commands where the action
 * has arity greater than the number of arguments. Similarly, we can
 * input commands on the input stream, incrementally, representing
 * incoming data and other requests.
 *
 *      [proc] commandA => commandB* [proc']
 * 
 * Command stream processing is convenient for integrating free monadic
 * or process network effects models. We can incrementally handle output
 * requests and incrementally add input commands or update messages. So
 * Wikilon runtime supports this style. Programs that don't fit this 
 * pattern simply won't produce useful output before end of input.
 */

/** Addend Program
 * 
 * A context initially has an empty program, but we may addend multiple
 * subprograms to this. Each write must be independently valid, having
 * complete texts and blocks and so on. 
 */
bool wikrt_write(wikrt_cx*, uint8_t const*, size_t);

/** Evaluate Program
 *
 * Computation in Awelon involves confluent rewriting of a program to
 * an equivalent program. A well formed program will converge, but is
 * not guaranteed to do so in a timely manner. Evaluation may fail to
 * complete due to resource constraints: context memory (ENOMEM) or 
 * effort quota (ETIMEDOUT). 
 *
 * Note: Recognized type errors are recorded with (error) annotations
 * in the evaluated program, and are thus visible in program output.
 * Undefined or badly defined words simply won't be linked.
 *
 * evaluated program. Undefined or badly defined words simply won't be
 * linked. So outside resource constraints, evaluation doesn't fail...
 * it simply returns a program with errors made explicit and obvious.
 */
void wikrt_eval(wikrt_cx*);

/** Read Program
 *
 * Reads will destructively extract valid subprograms into the given
 * data buffer. Concatenation of multiple small reads is equivalent 
 * to a single larger read. 
 */
bool wikrt_read(wikrt_cx*, uint8_t*, size_t*);

/** Evaluate or Read Next Command
 *
 * A "command" is a subprogram of positive arity. Evaluation and reads
 * at the granularity of individual commands are convenient for stream
 * processing. They permit lower latency feedback than full evaluation,
 * and better alignment for effects models based on matching command
 * words or similar.
 *
 * Read and evaluate are separated to help handle some corner cases (like
 * a recognized command with a divergent argument). But one must usually
 * evaluate some to recognize a command.
 */
bool wikrt_eval_cmd(wikrt_cx*);
bool wikrt_read_cmd(wikrt_cx*, uint8_t*, size_t*);

/** Clear Program. Equivalent to reading until empty. */
void wikrt_clear(wikrt_cx*);


/** Select Program Stream
 *
 * A context can work with multiple named program streams, albeit one
 * at a time by switching between them. All write/eval/read operations
 * are applied to the current program. (But note that definitions apply 
 * default stream is aliased from both the empty string and NULL. 
 *
 * This feature supports both multi-tasking and debug configuration.
 */
void wikrt_current_program(wikrt_cx*, char const*);

/** Effort Quota
 * 
 * To control infinite loops, each context has an effort quota. If
 * this quota is exhausted, evaluation will fail with ETIMEDOUT. 
 * Effort is measured in terms of CPU microseconds, but is only
 * checked infrequently so is not very precise.
 *
 * The default quota is 100ms.
 */
void wikrt_set_effort(wikrt_cx*, uint32_t);

// thoughts: I keep wondering whether effort in terms of rewrites
// would be a better choice. But it makes sense to keep efforts
// in terms of something externally meaningful, like CPU time. 
//
// Debug snapshots shouldn't be based on time quotas anyway.


// TODO: configure evaluation options, optimization flags

// TODO: debug outputs


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
