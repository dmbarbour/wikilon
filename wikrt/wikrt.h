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

/** Release environment resources. Perform after destroying contexts. */
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

/** Secure Hash Resources
 *
 * Awelon leverages a concept of secure hash resources for a variety
 * of purposes: binary data, anonymous functions, data stowage, and
 * inheritance or hierarchical composition of dictionaries. These 
 * binary resources are uniquely (in practice, if not theory) and
 * globally identified by a 384 bit BLAKE2b secure hash encoded as
 * 64 characters of base64url.
 * 
 * Secure hash resources support a high level of structure sharing.
 * And large resources can be divided into smaller resources to both
 * improve structure sharing and reduce memory burdens.
 *
 * What can we do with secure hash resources?
 *
 * - input resources for local storage
 * - extract resources by secure hash
 * - list undefined, referenced resources
 * 
 * Wikilon runtime may gradually garbage collect resources that are
 * not rooted by a persistent dictionary or context. A context will
 * preserve added resources against GC until cleared, reset, or 
 * destroyed.
 *
 * It is feasible to eventually configure network access to resources.
 */

/** 384 bits of base64url is exactly 64 bytes. */
#define WIKRT_HASH_SIZE 64

/** Hash a binary. 
 *
 * The 384-bit BLAKE2b secure hash algorithm is used. Data returned is
 * exactly WIKRT_HASH_SIZE bytes of base64url. We assume the buffer is
 * of sufficient size. No NUL terminal is appended.
 */ 
void wikrt_hash(char* rscID, uint8_t const*, size_t);

/** Provide a resource.
 * 
 * At this time, Wikilon runtime only has access to resources that are
 * explicitly provided. In the future, it may be possible to configure
 * access to content delivery networks or HTTP hosting services.  
 * 
 * Resources are named automatically by wikrt_hash. Thus, providing a 
 * resource requires only providing the binary. However, the hash is
 * an optional output (if not NULL) to control redundant computation.
 * Resources may be garbage collected
 *
 * Failure occurs if there isn't enough space on disk (ENOSPC) or in
 * context memory (ENOMEM) or if the resource is oversized (EFBIG). On
 * failure, we'll return false and set errno appropriately. Otherwise,
 * we return true and record the provided resource.
 *
 * NOTE: Ideally, resources should be kept small. Instead of a massive
 * binary, consider modeling a binary with fixed-size pages as a rope 
 * based data structure. Instead of a massive dictionary patch, break
 * it into smaller, reusable patches of a few hundred kilobytes each.
 */
bool wikrt_add_resource(wikrt_cx*, char* rscID, uint8_t const*, size_t);

/** Extract resource data.
 *
 * This copies a resource into provided client memory. 
 *
 * Failure occurs if the resource is larger than the buffer (EFBIG)
 * or the resource identity is unknown (ENOENT) or insufficient context
 * memory to complete the query (ENOMEM). On failure, we return false
 * and set errno appropriately. 
 *
 * On success, we return true and the resource is copied into the buffer,
 * and the size field is set to the resource size. If NULL is used for the
 * buffer, this is interpreted as a request for resource size only.
 *
 * NOTE: This function is designed to resist timing attacks, exposing at
 * most the first 60 bits of known resource IDs through latency. This is
 * important because resources are not subject to normal access control
 * yet may contain sensitive information. Resource IDs are bearer tokens.
 */
bool wikrt_get_resource(wikrt_cx*, char const* rscID, uint8_t* buffer, size_t* size);

// Idea: get resource with offset would support streaming output. But I
// don't much want streaming output unless I also have streaming inputs.

/** Scan for required resources.
 *
 * This will search for resources referenced from the current dictionary
 * and context that have not been defined. This result is returned as an
 * LF separated sequence of resource IDs in arbitrary order. Only as many
 * results as fit in the result buffer will be returned. (The expectation
 * is to batch and handle a few hundred missing resources at a time.)
 */
void wikrt_report_required_resources(wikrt_cx*, uint8_t* buffer, size_t* size);

/** Computation 
 *
 * One of the most important things we'll do is evaluate a program in
 * context of 
 */

/** Codebase Access and Update
 * 
 * An Awelon codebase is called a dictionary and defines a set words.
 * For Wikilon, I want to operate on a dictionary in several ways.
 *
 * - read or update a word definition
 * - reverse lookup clients of a word
 * - dictionary layer import or export
 * 
 * More requirements may be introduced as needed. But I'll attempt to
 * align this Wikilon runtime API with known requirements.
 *
 * A context provides an implicit transaction. Updating a definition or
 * dictionary won't become persistent until this transaction commits.
 * However, computations may be performed in context of the proposed
 * update.
 */

/** Set or clear an entire dictionary.
 * 
 * In this case we may use rscID to specify a dictionary patch. Use NULL
 * or an empty string to indicate an empty dictionary. This overwrites a
 * prior dictionary.
 *
 * The value of 'd' would typically be an empty string or NULL to refer
 * to the root dictionary. But Awelon does have simplistic support for
 * hierarchical dictionaries. By specifying dictionary "bar", you can
 * update just words of the form "foo@bar".
 */
bool wikrt_set_dict(wikrt_cx*, char const* d, char const* rscID);

/** Access a dictionary resource.
 *
 * This captures the current dictionary as a resource suitable for 
 * efficient import, export, snapshot, or hierarchical structure.
 */
bool wikrt_get_dict(wikrt_cx*, char const* d, char* rscID);


/** Load a word's definition as context program. */
bool wikrt_load_def(wikrt_cx*, char const* word);

/** Save context program to a word's definition. */
bool wikrt_save_def(wikrt_cx*, char const* word);

/** @brief Extend current program via ABC text.
 * 
 * This is a convenient way to inject a subprogram if you already have
 * a text representation for large fragments of it. This requires a
 * valid program 

but additionally stores the program
 * as it parses.
 *
 * GENERAL NOTE ON PROGRAM ENTRY
 *
 * Program entry always extends the right hand side of the existing program.
 * T
  in the context, and may be deeply embedded
 * if the contex
 

SIZE_MAX). The return value indicates
 * how much of the input program parsed correctly. 

 *
 *For a correct 
 * subprogram, with `len = wikrt_abc(cx, abc, maxlen)


 * Program entry always adds to the right hand side of the current
 * program. 
 * 
 * This is a convenient way to enter large but relatively static
 * programs. The subprogram in question must be valid bytecode,
 * no imbalanced brackets or braces, embedded texts complete, 
 * valid tokens, etc.. There are no particular constraints on 
 * tokens other than that they be valid.
 *
 * NOTE: Program entry is always left-to-right. It's possible to grow
 * a large program by injecting many small ABC strings, or to evaluate
 * incrementally between chunks.
 */

void wikrt_abc_str(wikrt_cx*, char const* abc, size_t);


  /////////////////////////
 // BASIC DATA PLUMBING //
/////////////////////////

/* I'll introduce accelerators as they're developed. */

  ///////////////////////////
 // DATA INPUT AND OUTPUT //
///////////////////////////


/** @brief Allocation of integers. (e)→(Int*e).
 *
 * Note: Wikilon runtime currently only supports a subset of integers,
 * particularly those that can be expressed with 18 decimal digits or
 * fewer (i.e. -999999999999999999..999999999999999999). This is about
 * 61 bits of integer data. Eventually, large numbers may be supported,
 * but only with sufficient demand for them.
 */
void wikrt_intro_i32(wikrt_cx*, int32_t);
void wikrt_intro_i64(wikrt_cx*, int64_t);

/** @brief Non-destructively read small integers. (Int*e)→(Int*e).
 *
 * These functions return true on success. On error, they return false
 * and output zero, min, or max (using min or max only for underflow 
 * and overflow respectively, zero for other errors).  
 */
bool wikrt_peek_i32(wikrt_cx*, int32_t*);
bool wikrt_peek_i64(wikrt_cx*, int64_t*);

/** @brief Allocation of large integers. (e)→(Int*e).
 *
 * In this case we accept regex: `0 | (-)?[1-9][0-9]*`. Wikilon runtime
 * will support numbers up to a million digits, perhaps more. Wikilon
 * runtime uses a compact variant of binary-coded decimal under the hood,
 * and so favors conversion to or from decimal representations.
 *
 * Note: This will also stop reading on a NUL character, so SIZE_MAX is
 * okay if you have NUL-terminated strings.
 *
 * Note: Wikilon runtime may fail with WIKRT_IMPL if you attempt to load
 * or use an integer outside the range it effectively supports. It will
 * not silently introduce errors by wrapping or truncating integers.
 */
void wikrt_intro_istr(wikrt_cx*, char const*, size_t);

/** @brief Non-destructively access a large integer. (Int*e)→(Int*e).
 *
 * The integer is returned as a string of regex `0 | (-)?[1-9][0-9]*`. 
 * The strlen is both input and output. As input, it is the maximum
 * buffer size. As an output, it is a recorded buffer size (on true)
 * or a required buffer size (on false). The NUL character is NOT 
 * added to the end of the output buffer, but a client can easily add
 * one via strlen.
 *
 * If the argument is a non-integer, we'll return false and strlen zero.
 */
bool wikrt_peek_istr(wikrt_cx*, char* buff, size_t* strlen); 

/** @brief Allocate a binary. (e)→(binary*e).
 *
 * The binary is modeled as a list of small integers (0..255). It may
 * use a more compact representation, e.g. an array of bytes, under 
 * the hood. A list has type `μL.((e*L) + 1)`. 
 */
void wikrt_intro_binary(wikrt_cx*, uint8_t const*, size_t);

/** @brief Incrementally read binary data. Destructive.
 *
 *    (binary*e)→(smaller binary*e)
 *
 * Destructively read binary data from the context into the client
 * buffer. Multiple sequential reads gradually consume the binary.
 */
void wikrt_read_binary(wikrt_cx*, uint8_t*, size_t*);

/** @brief Mark a value as binary. The {&binary} annotation.
 *
 *    (binary * e) → (binary * e)
 *
 * This tells a runtime that a given value should be a binary, and to
 * favor a compact byte string representation (i.e. a list containing
 * large binary fragments, instead of a cell per byte; memory savings 
 * can approach 16x.)
 *
 * If the binary is already represented as a compact byte string, this
 * will have a very low cost.
 */
void wikrt_anno_binary(wikrt_cx* cx);

// Note: use of {&binary} is okay for small to medium binaries, but
// is not suitable for very large binaries. For large binaries, the
// client may need to favor rope-like structures with value stowage. 
//
// I'm exploring ideas for a shared memory space. Binaries, texts,
// and program fragments might be represented as external resources
// related to value stowage. 

/** @brief Allocate a text.
 *
 * Text must be valid utf-8, and valid as ABC text: no control characters
 * (C0, C1, DEL) except LF, no surrogate codepoints, no replacement char.
 * A text is modeled as a list of codepoints, but may use a more compact
 * representation under the hood. Length is given in bytes. NUL-terminated
 * C strings are also accepted (use SIZE_MAX).
 */
void wikrt_intro_text(wikrt_cx*, char const* str, size_t len);

/** @brief Incrementally read a text. Destructive.
 *
 *   (text * e) → (smaller text * e)
 * 
 * Similar to wikrt_read_binary, but will read text into a utf-8 buffer.
 * The codepoints read will meet the same constraints as alloc_text, i.e. 
 * no control chars except LF, no surrogates, no incomplete codepoints.
 *
 * In case of error, we stop reading and the context enters an error state.
 * The user should statically know that the argument is a valid text. If
 * you need to control number of characters read, operate at the integer 
 * level instead of this buffer level.
 */
void wikrt_read_text(wikrt_cx*, char*, size_t* bytes);

/** @brief Mark a value as text. The {&text} annotation.
 *
 *  (text * e) → (text * e)
 * 
 * This tells a runtime that a given value should be a text, to ensure
 * a compact utf8 byte string representation, and to serialize the
 * value as embedded text (e.g. for trace or quote + block to text).
 *
 * If a value is already represented as compact text, this has a low
 * cost. Otherwise, it may perform a full copy operation to rebuild
 * the text.
 */
void wikrt_anno_text(wikrt_cx*);

/** @brief Serialization and Programming
 *
 * Introducing and extracting bytecode are the primary bases for ad-hoc
 * serialization. Arbitrary values can be serialized by first quoting 
 * them into a block then converting the block to text. Injecting bytecode
 * is also necessary prior to any useful evaluation.
 *
 *  text to block: (text*e) → (block*e)
 *  block to text: (block*e) → (text*e)
 *
 * Awelon Bytecode (ABC) is the accepted format for code. This is a subset
 * of UTF-8 unicode, with a small set of primitives and only a few types of
 * data (e.g. integers but no floating point). This minimal bytecode can be
 * accelerated by recognizing common subprograms (substrings).
 *
 * There is no implicit simplification or optimization. The text is validated
 * only insofar as ensuring it parses, not that it represents a valid program.
 * A round trip conversion (text to block to text) should return the original
 * text.
 *
 * NOTE: A block may reference stowed values (e.g. via quote and compose). Such
 * stowed value references are represented by resource tokens. cf. wikrt_peek_sv
 * for detailed information. If stowage is possible, you may need to hold another
 * reference to the block (via copy or key-value database) to prevent GC.
 */
void wikrt_text_to_block(wikrt_cx*);
void wikrt_block_to_text(wikrt_cx*);

/** @brief Wrap a value with a sealer token. (a * e)→((sealed a) * e).
 * 
 * Wikilon runtime only knows about discretionary sealers, such as
 * {:map} and {.map} to seal then subsequently unseal the value. For
 * this case, you'd use ":map" for wrap_seal.
 *
 * See wikrt_valid_token() for information on valid sealer strings.
 */
void wikrt_wrap_seal(wikrt_cx*, char const*); 

/** @brief Access a sealed value. ((sealed a) * e) → (a * e).
 *
 * This returns the sealer token into the provided buffer, which must
 * be at least WIKRT_TOK_BUFFSZ in length to eliminate risk of buffer
 * overflow. This token is NUL-terminated. On error, an empty string
 * will be returned (which is not a valid token).
 */
void wikrt_unwrap_seal(wikrt_cx*, char*);




  ////////////////////
 // MEMORY CONTROL //
////////////////////

/** @brief (a * e) → (trash * e). Annotation {&trash}.
 *
 * Use of `{&trash}` serves as a way to delete data without logically
 * dropping it. Instead, trash tells the runtime that a given value
 * will not be observed, and hence the memory may be recycled and the
 * object in question replaced by a lightweight place holder. Only 
 * substructural type is preserved. A trashed linear value serializes
 * as `[]kf{&trash}` to preserve both the substructure and the fact
 * that it's trash.
 */
void wikrt_trash(wikrt_cx*);

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

/** @brief Size for a single value.
 *
 * Given an `(a*e)` environment, this computes the size of the `(a*1)`
 * cell, i.e. the amount of memory required to copy the value (if it
 * is copyable), or the amount recovered if we drop the value. This
 * will return 0 if we don't have an `(a*e)` environment.
 */
size_t wikrt_peek_size(wikrt_cx* cx);

  ///////////////
 // DEBUGGING //
///////////////
// Debugging, Profiling, Etc..
// 
// - trace supports printf style debugging. 
// - simplistic profiling via GC stats is possible
// - TODO: stack traces, and periodic profiling traces.

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

/** Shallow reflection over values. */
typedef enum wikrt_val_type
{ WIKRT_TYPE_UNDEF      // an undefined type
, WIKRT_TYPE_INT        // any integer
, WIKRT_TYPE_PROD       // a pair of values
, WIKRT_TYPE_UNIT       // the unit value
, WIKRT_TYPE_SUM        // value in left or right
, WIKRT_TYPE_BLOCK      // block of code, a function
, WIKRT_TYPE_SEAL       // discretionary sealed value
, WIKRT_TYPE_STOW       // stowed value reference
, WIKRT_TYPE_TRASH      // placeholder for discarded value
, WIKRT_TYPE_FUTURE     // a lazy or asynchronous future value
} wikrt_val_type;

/** @brief Reflection on values.
 *
 * Given a context with type (a*e), this reflects the type of `a`.
 * WIKRT_TYPE_UNDEF is returned if `a` does not exist or the context
 * is in any error state. This operation does not modify the context.
 *
 * The primary purpose of reflection on types is to simplify debug
 * rendering. However, in general it is preferable that data to be
 * rendered comes with its own rendering models (such that rendering
 * can be performed without reflection or external tooling).
 */
wikrt_val_type wikrt_peek_type(wikrt_cx*);

  /////////////////
 // COMPUTATION //
/////////////////
/** @brief Apply function lazily. ([a→b] * (a * e)) → ((future b) * e)
 *
 * This function immediately returns a lazy future representing the
 * application of the function to its argument. Actual evaluation is
 * performed by wikrt_step_eval. Note that attributes like lazy/fork
 * do not apply to toplevel applications.
 *
 * If quoted and serialized before any evaluation, the future should
 * have form `a[a→b]{&lazy}$`. 
 */
void wikrt_apply(wikrt_cx*);

/** @brief Step an evaluation. ((future a) * e) → (((future a) + a) * e)
 * 
 * Each step performs a finite, heuristic amount of labor towards complete
 * evaluation. If evaluation successfully completes, a result is returned
 * in the right. Otherwise, an updated future is returned in the left. Or
 * there might be a runtime type or memory error, so do test wikrt_error.
 * 
 * Note: Default effort is unspecified, but sufficient to make reasonable
 * progress per step. Clients may specify the heuristic effort model via
 * wikrt_set_step_effort.
 */ 
void wikrt_step_eval(wikrt_cx*);

/** @brief Quote a value. (a * e) → ((∀e'. e'→(a*e'))*e).
 *
 * Almost any value may be quoted, including pending values. However,
 * in some cases we might not have clear substructural properties for
 * the resulting block. Wikilon runtime favors performance over precise
 * dynamic tracking of substructural attributes.
 */
void wikrt_quote(wikrt_cx*);

/** @brief Mark a block affine (non-copyable). (block*e)→(block*e). Op `f`. */
void wikrt_block_aff(wikrt_cx*);

/** @brief Mark a block relevant (non-droppable). (block*e)→(block*e). Op `k`. */
void wikrt_block_rel(wikrt_cx*);

/** @brief Explicit lazy evaluation. Annotation {&lazy}. 
 *
 *    {&lazy} :: ([a → b] * e) → ([a → (future b)] * e) 
 *    {&join} :: ((future a) * e) → (a * e)
 * 
 * Explicit laziness enables an incomplete value to be returned. The
 * value may be accessed by wikrt_step_eval or `{&join}` from within
 * another computation.
 *
 * Note: At the moment, futures are conservatively treated as linear.
 */
void wikrt_block_lazy(wikrt_cx*);

/** @brief Scalable parallel evaluation. Annotation {&fork}.
 *
 * Process functions (PF) with asynchronous futures are an effective
 * and simple basis for purely functional parallelism. 
 *
 *     PF           [i → (o * PF)]
 *     forked PF    [i → ((future o) * (forked PF))]
 *     {&fork}      (PF * e) → ((forked PF) * e)
 *     {&join}      ((future a) * e) → (a * e)
 *
 * The forked process function may be evaluated in a separate context
 * or process. When applied, the call returns 'immediately' with a
 * future result and the next PF (which represents the future state
 * of the process). The returned PF may immediately be called again,
 * effectively enqueing arguments to the process. Parallelism is 
 * achieved by doing work before joining (which synchronizes).
 * 
 * Process functions are best used together with explicit models for
 * incremental computation and concurrency, such as message passing 
 * or flow-based programming.
 *
 * Note: while immediate return is the common case, processes have
 * bounded input queues and 'push back' in case of fast producers
 * calling slower consumers. This helps control memory consumption.
 */
void wikrt_block_fork(wikrt_cx*);

/** @brief Compose two blocks. ([a→b]*([b→c]*e))→([a→c]*e). */
void wikrt_compose(wikrt_cx*);

/** @brief Add two integers. (I(a)*(I(b)*e))→(I(a+b)*e). */
void wikrt_int_add(wikrt_cx*);

/** @brief Multiply two integers. (I(a)*(I(b)*e))→(I(a*b)*e). */
void wikrt_int_mul(wikrt_cx*);

/** @brief Negate an integer. (I(a)*e)→(I(-a)*e). */
void wikrt_int_neg(wikrt_cx*);

/** @brief Divide two integers with remainder.
 *
 * (I(divisor) * (I(dividend) * e)) → (I(remainder) * (I(quotient) * e)).
 *
 * The divisor must be non-zero, otherwise the context will enter an
 * error state (a dependent type error).
 */
void wikrt_int_div(wikrt_cx*); 

/** @brief Integer comparison result. */
typedef enum wikrt_ord { WIKRT_LT = -1, WIKRT_EQ = 0, WIKRT_GT = 1 } wikrt_ord;

/** @brief Compare two integers. (I(a)*(I(b)*e)).
 *
 * This compares `b` to `a`, i.e. such that if we allocate zero then four
 * then compare, we effectively insert the comparison between the zero and
 * the four.
 */
void wikrt_int_cmp(wikrt_cx*, wikrt_ord*);

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


/** @brief Tune effort per wikrt_step_eval.
 *
 * Tune both effort model and the effort value. Larger steps, with
 * greater effort, are more efficient. But smaller steps simplify
 * debugging, isolation of errors, and enable the thread to react
 * to external events.
 *
 * Other than the time-based models, efforts are weakly deterministic
 * in the sense that the amount of work should be reproducible for
 * the given initial context. The 'blocks' and 'megabytes' options 
 * are additionally robust to changes in context size. No model is
 * precise.
 */
void wikrt_set_step_effort(wikrt_cx*, wikrt_effort_model, uint32_t effort);

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

/** @brief Larger than memory data and computations. {&stow}.
 * 
 * Stowage provides a basis for computing with larger than memory
 * data structures, serving a role similar to virtual memory but
 * with richer structure and greater precision. A `[LARGE VALUE]`
 * is generally replaced by a `[{'resourceId}]` token value, with
 * the data being serialized to a backing store (cf. wikrt_db_open).
 *
 * This feature enables representation of massive data structures as
 * first-class values, entire databases or filesystems. Trees designed
 * for filesystem use (such as log-structured merge trees) will offer
 * excellent performance when modeled with stowage. Large, flat data
 * structures may be represented by finger-tree deques or ropes.
 * 
 * Stowage is heuristic. Smaller values may be left alone, represented
 * in line. Stowage may be delayed to improve batches or avoid stowage
 * of short-lived values. Wikilon runtime will make a best effort at
 * structure sharing, gradually eliminating redundant representations.
 *
 * NOTE: Wikilon runtime secures stowage tokens by simple HMAC. Thus
 * they may be understood as secure capabilities, bearer tokens, and
 * accessed via HTTP lookup.
 */
void wikrt_stow(wikrt_cx*);

/** @brief Inline a previously stowed resource. {&load}.
 *
 * This counters a `{&stow}` action. Normally, stowed data will be
 * accessed as needed, but this can force the action early. 
 */
void wikrt_load(wikrt_cx*);

/** @brief Read persistent key from database.
 *
 * The value is copied onto the program stack. By default, all keys are
 * defined with value `#` - the empty sequence or option value. 
 */
void wikrt_key_read(wikrt_cx*, char const* key);

/** @brief Write value to key in database.
 *  
 * A value is moved from the program stack into the database or the
 * current transaction. If there is no obvious value, or the key is
 * not valid, we may return 'false'. Valid database keys have the 
 * same size and structure restrictions as tokens.
 *
 * Writing the default `#` value is equivalent to deleting the key
 * from the database.
 */
bool wikrt_key_write(wikrt_cx*, char const* key);

/** @brief Begin a transaction within the current context.
 *
 * A context may have only one open transaction. Reads and writes
 * within a transaction will be tracked until the transaction is
 * aborted or committed. Upon commit, the transaction may succeed
 * or fail. After success, concurrent conflicting transactions will
 * fail. Transactions are not guaranteed to be precise in detecting 
 * conflict. However, they do guarantee progress. At least one
 * write will succeed in case of conflicts.
 *
 * Reads and writes do not need a transaction. Without a transaction, 
 * reads and writes are still individually atomic.
 */
void wikrt_txn_create(wikrt_cx*);

/** @brief Abort context transaction.
 *
 * This abandons a transaction, releasing associated resources. There
 * is no guarantee of consistency, not even for read-only operations.
 */
void wikrt_txn_abort(wikrt_cx*);

/** @brief Attempt to commit a transaction.
 *
 * This will return 'true' if the transaction successfully commits,
 * 'false' if it's forced to abort. Either way, transaction resources
 * are released, and there is otherwise no change to context state.
 *
 * NOTE: There is no default effort to 'rewind' context upon failure.
 * If that behavior is necessary, it must be modeled explicitly.
 */
bool wikrt_txn_commit(wikrt_cx*);

/** @brief Mark transaction for durability. 
 *
 * Transactions are not durable by default, enabling work to buffer
 * and batch more efficiently. Marking a transaction durable ensures
 * durability for that transaction and its transitive dependencies.
 *
 * In practice, durable transactions probably use wikrt_db_sync(), 
 * but the client of this API shouldn't assume so.
 */
void wikrt_txn_durable(wikrt_cx*);

// Todo: consider specialized 'variable' alternatives for 
//  stream processing, queues, logs, or similar?

#define WIKILON_RUNTIME_H
#endif
