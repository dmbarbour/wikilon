/** @file wikilon-runtime.h
 *	@brief Wikilon Runtime
 *
 *	@mainpage	Wikilon Runtime
 *
 *	@section intro_sec Introduction
 *
 *  Wikilon is part of Awelon project, which explores a new model for
 *  software development. Awelon project uses its own Awelon Bytecode
 *  (ABC). This bytecode is simple and purely functional, but doesn't
 *  perform well in a naive interpreter. To recover performance, many
 *  techniques must be utilized.
 * 
 *  - Accelerators. Common subprograms (sequences of bytecode) are
 *    recognized and handled as a single opcode internally. We can
 *    accelerate collections-oriented programming, common loops,
 *    matrix math, conditional behaviors, and data plumbing.
 *  
 *  - Linearity and Move Semantics. While ABC values are logically
 *    immutable, it is possible to mutate values in place when only
 *    one reference to the value exists. Wikilon runtime makes this
 *    the default behavior because it's a good fit for ABC semantics.
 *
 *  - Compilation. We can annotate that subprograms are compiled JIT
 *    or AOT. Compilers can translate ABC to a form more suitable for
 *    modern hardware (e.g. abstract register and stack machines) and
 *    eliminate runtime data plumbing. With LLVM, it is feasible to
 *    achieve competitive performance.
 *
 *  - Large value stowage. Databases, filesystems, graphs, documents,
 *    game worlds, and more can be modeled as large immutable values.
 *    Large values are accessible for optimization and abstraction. 
 *    The common requirement for impure data systems APIs and waiting
 *    for data is reduced. LMDB is used for the backing store.
 *
 *  - Parallelism. Modulo space requirements, pure computations behave
 *    independently of evaluation order. Divide and conquer tactics 
 *    are effective if we can divide into coarse-grained tasks. ABC
 *    easily supports par/seq parallelism. 
 * 
 *  Wikilon runtime supports all of these techniques. Further, large
 *  value stowage comes with an integrated persistence model via LMDB.
 *  Wikilon state is represented using ABC values, which simplifies 
 *  potential reflection and development of software agents.
 *
 *  Effectful code can be modeled either purely, e.g. yielding with
 *  requests and a continuation, or impurely via token stops. Wikilon
 *  favors pure models as easier to simulate, test, and reuse.
 *
 *  Wikilon runtime is designed to provide very predictable performance,
 *  suitable for real-time systems if used carefully. Many features like
 *  JIT and parallelism are driven by annotations. The garbage collector
 *  has very predictable behavior, and only applies to limited nursery
 *  arenas to avoid and compact heap allocations.
 *
 *  @section usage_sec Usage
 *
 *  Create an environment. Create a context within that environment.
 *  Load some values into the context, possibly via the key-value
 *  database. Perform computations and analyze the results.
 * 
 *  @section notes_sec Notes
 *
 *  Portability: Wikilon runtime is written for use in Linux with GCC.
 *  It doesn't use much non-portable code, though. With a little work, 
 *  it should be easily adapted for other systems.
 *
 *  Floating Point Numbers: At the moment, there are no accelerators
 *  for floating point computations. I hope to eventually develop 
 *  accelerators for a simple set of floating point representations
 *  and operations that can be accelerated by hardware FPU. 
 *  
 *  Locking: The LMDB file is only safe for a single application,
 *  and only a single wikrt_env within that application. This is 
 *  guarded by a simple file lock via flock(2). 
 *
 *  Hot Backup: Currently not supported. I would like to maybe support
 *  this explicitly, eventually.
 *
 *  @section license_sec License & Copyright
 *
 *  (c) 2015 David Barbour
 *  LICENSE: BSD 3-clause
 *
 */
#ifndef WIKILON_RUNTIME_H

#include <stdint.h>
#include <stdbool.h>

/** @brief Opaque structure for overall Wikilon environment.
 * 
 * An environment includes an LMDB instance for large value stowage and
 * a simple key-value persistence layer. Additionally, the environment
 * has a pool of worker threads (one on each CPU by default) to support
 * par/seq parallelism.
 *
 * An environment may support multiple concurrent contexts.
 *
 */
typedef struct wikrt_env wikrt_env;

/** @brief Opaque structure representing substrate for computations.
 *
 * Computations occur within an evaluation context. They serve a role
 * similar to virtual machines or OS processes. The context has small
 * 'active' working set (up to four gigabytes) but may access more 
 * data via the large values stowage feature.
 *
 * Each `wikrt_cx*` should be handled in a single-threaded manner. It
 * may leverage multiple threads under the hood (par-seq parallelism),
 * but it's assumed that the API for a context is only used from only
 * one external thread at a time.
 */
typedef struct wikrt_cx wikrt_cx;

/** @brief Errors during Wikilon Runtime
 *
 * Following the normal C conventions, most functions return an error 
 * condition that allows simple policies on how to handle them. If
 * there is no error, a zero value (WIKRT_OK) is returned.
 */
typedef enum wikrt_err 
{ WIKRT_OK = 0
, WIKRT_INVAL           // bad arguments, avoidable programmer error

// External Resource Errors
, WIKRT_DBERR           // LMDB or filesystem layer errors 
, WIKRT_NOMEM           // malloc or mmap allocation error

// Special Conditions
, WIKRT_CXFULL          // context is out of memory
, WIKRT_NOLINK          // context or environment destroyed
, WIKRT_BUFFSZ          // output buffer too small

// Transactions
, WIKRT_TXN_CONFLICT    // transaction failed on conflict

// Evaluations
, WIKRT_STREAM_WAIT     // waiting on input
, WIKRT_QUOTA_STOP      // halted on time/effort quota
, WIKRT_TOKEN_STOP      // stop on unrecognized token
, WIKRT_ASSERT_FAIL     // assertion failure (op `K`)
, WIKRT_TYPE_ERROR      // generic type errors
} wikrt_err;

/** @brief Open or Create a Wikilon environment.
 *
 * The developer specifies a directory in the filesystem for persistent
 * data and stowed values. Also, a heuristic indicator for how much space
 * for persistent storage is permitted in megabyte units. This action may
 * fail, most likely for filesystem related reasons (e.g. permissions, or
 * if that directory is already in use by another process).
 *
 * If you don't need stowage or persistence, you may set the dbMaxMB to 0
 * and the dirPath to NULL. In this case, we'll skip construction of the
 * backing database. Stowage annotations will behave normally with respect
 * to copy and drop operations, but never actually stow anything. All
 * transactions will fail with WIKRT_DBERR.
 */
wikrt_err wikrt_env_create(wikrt_env**, char const* dirPath, uint32_t dbMaxMB);

/** @brief Graceful shutdown of environment and active contexts. 
 *
 * Operations on contexts should eventually abort with WIKRT_NOLINK. 
 * Contexts must still be individually destroyed to fully release 
 * their memory.
 */
void wikrt_env_destroy(wikrt_env*);

/** @brief Ensure committed transactions are pushed to disk. 
 *
 * It is recommended that a background task do this every few seconds if
 * transactions aren't usually durable. 
 */
void wikrt_env_sync(wikrt_env*);

/** @brief Create a context for computations.
 * 
 * A context consists mostly of one big mmap'd block of memory. The
 * viable range for sizes is about 4..4000 in megabyte units. We'll
 * generally require at least one megabyte for each computing thread,
 * plus a few more.
 */ 
wikrt_err wikrt_cx_create(wikrt_env*, wikrt_cx**, uint32_t sizeMB);

/** @brief Destroy a context and return its memory. */
void wikrt_cx_destroy(wikrt_cx*);

/** @brief Reset a context for use in a pooling system. 
 *
 * This returns the context to its 'freshly created' status without
 * requiring the address space to be unmapped and remapped. 
 */
void wikrt_cx_reset(wikrt_cx*);

/** @brief A context knows its parent environment. */
wikrt_env* wikrt_cx_env(wikrt_cx*);

/** @brief Supported ABC and ABCD operators as UTF-8 C string.
 *
 * The basic 42 ABC operators are
 *
 *   lrwzvcLRWZVC %^$o'kf#0123456789+*-QG?DFMK\n
 *
 * ABCD extends this set with additional opcodes represented as utf-8
 * codepoints. These are defined by expansion, ultimately into ABC.
 * An interpreter can hand-optimize those ABCD opcodes it recognizes.
 * This corresponds to the notion of 'accelerators', replacing common
 * subprograms with optimized implementations.
 */
char const* wikrt_abcd_operators();

/** @brief Expand ABCD opcodes to their definitions.
 *
 * The definition may utilize more ABCD, albeit acyclically. Pure ABC
 * operators will 'expand' to themselves, e.g. 'v' expands to "v".
 */
char const* wikrt_abcd_expansion(uint32_t opcode);

/** @brief Validate a token.
 *
 * Awelon Bytecode tokens have the following constraints:
 *
 * - valid utf-8 text
 * - no more than 63 bytes
 * - no control chars (C0, DEL, C1)
 * - no surrogate codepoints (U+D800 to U+DFFF)
 * - no replacement char (U+FFFD)
 * - no curly braces `{}`
 *
 * This function returns true if the token is valid by these rules.
 */
bool wikrt_valid_token(char const* s);


/** @brief A value reference within a context. 
 *
 * Wikilon's value model is based on Awelon Bytecode. Basic values
 * include integers, products, unit, sums, and blocks. Special case
 * data includes sealed values, stowed values, pending computations,
 * arrays, texts, and binaries. The latter three are specializations
 * of simple lists.
 *
 * Wikilon runtime optimizes for compact representation of common
 * value types - e.g. pairs, lists, small integers, unit, booleans,
 * (node + leaf) trees, and deep sums. Lists may be compacted further
 * into arrays, texts, and binaries.
 *
 * Wikilon runtime assumes linear value references, i.e. that there
 * is no aliasing. This enables in-place mutation while preserving
 * purely functional semantics. API calls that receive wikrt_val as
 * input will generally take ownership of the value unless indicated
 * otherwise in documentation.
 */ 
typedef uint32_t wikrt_val;

/** @brief Unit value is a constant value reference. 
 *
 * Unit value frequently serves in products as a placeholder for
 * extensions. It's also used for the bottom of a stack. 
 *
 * ABC code cannot reflect on whether a value is unit... it must
 * statically know that a value is unit (if it cares at all). So
 * clients of this API also shouldn't reflect on whether a value
 * is unit modulo debug traces and other places where reflection
 * is reasonable.
 */
#define WIKRT_UNIT 3

/** @brief Unit in Right is a constant value reference.
 * 
 * The unit value in the right is also the value conventionally 
 * used to represent boolean true, done, nothing, empty list.
 */
#define WIKRT_UNIT_INR 5

/** @brief Unit in Left is a constant value reference.
 * 
 * The unit value in the left is also the value conventionally 
 * used to represent boolean false. 
 */
#define WIKRT_UNIT_INL 7

/** @brief Maximum buffer size for token text.
 *
 * The maximum token size for Awelon Bytecode is 63 bytes. Wikilon
 * runtime uses a byte for a NUL-terminator to support C strings. 
 * Token text does not include the wrapping `{}` braces, just the
 * text between them.
 */
#define WIKRT_TOK_BUFFSZ 64

  ///////////////////////////
 // DATA INPUT AND OUTPUT //
///////////////////////////

/** @brief Streaming binary input.
 *
 * Streams enable construction and concurrent processing of large binary
 * values. A 'binary' is a list of integers 0..255. (A 'list' has type
 * `μL.((a*L)+1)`.) But Wikilon runtime favors a compact representation
 * for binaries as a composition of array-like segments.
 *
 * Usage: Allocate a (stream, binary) pair. Addend the stream many times.
 * Eventually 'end' the stream. Meanwhile or afterwards, process the binary
 * as a value. The stream value cannot be used after ending.
 *
 * Computations may stall with WIKRT_STREAM_WAIT indicating an effort to
 * read then end of an open stream. The stream in question is flagged so
 * wikrt_awaiting_stream returns true until addended.
 */
wikrt_err wikrt_alloc_stream(wikrt_cx*, wikrt_val* s, wikrt_val* binary);
wikrt_err wikrt_addend_stream(wikrt_cx*, wikrt_val s, uint8_t const* chunk, uint32_t size);
wikrt_err wikrt_end_stream(wikrt_cx*, wikrt_val s);
wikrt_err wikrt_awaiting_stream(wikrt_cx*, bool* bWaiting, wikrt_val const s);

/** @brief Read binary data from a list-like structure. 
 *
 * The argument to wikrt_read is a binary value, a list-like structure
 * of type `μL.((a*L)+b)` for some arbitrary `b` and where type `a` is
 * small integers in 0..255. Our read function will fill a buffer with
 * data from the binary then return the remainder. When errors occur,
 * we'll read as much as possible then return the remainder at the point
 * of error.
 */
wikrt_err wikrt_read(wikrt_cx*, wikrt_val binary, uint32_t buffSize, 
    uint32_t* bytesRead, uint8_t* buffer, wikrt_val* remainder);

/** @brief Texts to/from utf-8 binaries. 
 *
 * ABC doesn't permit arbitrary texts. A short blacklist applies:
 * 
 *  - no control chars (C0, DEL, C1) except for LF
 *  - no surrogate codeponts (U+D800 .. U+DFFF)
 *  - no replacement char (U+FFFD)
 *
 */
wikrt_err wikrt_utf8_to_text(wikrt_cx*, wikrt_val utf8, wikrt_val* text);
wikrt_err wikrt_text_to_utf8(wikrt_cx*, wikrt_val text, wikrt_val* utf8);

/** Texts to/from blocks of bytecode.
 *
 * Wrapping ABC text in a block provides an opportunity for the runtime
 * to simplify the code, perform partial evaluations, etc.. Converting
 * the block into text enables serialization of code.
 *
 * ABCD extensions are optional for both input and output. If not enabled,
 * we'll restrict input or output to pure ABC. Otherwise, we'll recognize
 * operators reported in `wikrt_abcd_operations()` as indicating common
 * subprograms.
 */
wikrt_err wikrt_text_to_block(wikrt_cx*, wikrt_val text, wikrt_val* block, bool bEnableABCD);
wikrt_err wikrt_block_to_text(wikrt_cx*, wikrt_val block, wikrt_val* text, bool bEnableABCD);

/** Alloc a short text or block from a C string literal. */
wikrt_err wikrt_alloc_text(wikrt_cx*, wikrt_val*, char const*);
wikrt_err wikrt_alloc_block(wikrt_cx*, wikrt_val*, char const*, bool bEnableABCD);

/** Allocating small integers. */
wikrt_err wikrt_alloc_i32(wikrt_cx*, wikrt_val*, int32_t);
wikrt_err wikrt_alloc_i64(wikrt_cx*, wikrt_val*, int64_t);

/** Allocate large integers from C strings, regex `0 | (-)?[1-9][0-9]*` */
wikrt_err wikrt_alloc_istr(wikrt_cx*, wikrt_val*, char const*);

/** @brief Read small integers.
 *
 * Unlike most API functions, the following have 'borrow' semantics.
 * The given wikrt_val remains available for further use. We'll return
 * WIKRT_BUFFSZ if the target is not large enough.
 */
wikrt_err wikrt_peek_i32(wikrt_cx*, wikrt_val const, int32_t*);
wikrt_err wikrt_peek_i64(wikrt_cx*, wikrt_val const, int64_t*);

/** @brief Read larger integers into a string.
 *
 * This produces a C string of regex format `0 | (-)?[1-9][0-9]*`.
 * Use of wikrt_peek_isize will compute conservatively a sufficient
 * size for our string (including the NUL terminator). Again, this
 * has borrow semantics; the integer is not destroyed. 
 */
wikrt_err wikrt_peek_istr(wikrt_cx*, wikrt_val const, uint32_t buffSize, char* buff);
wikrt_err wikrt_peek_isize(wikrt_cx*, wikrt_val const, uint32_t* sufficientBuffSize);

/** @brief Allocate or disassemble basic product types (pairs of values). */
wikrt_err wikrt_alloc_prod(wikrt_cx*, wikrt_val* p, wikrt_val fst, wikrt_val snd);
wikrt_err wikrt_split_prod(wikrt_cx*, wikrt_val p, wikrt_val* fst, wikrt_val* snd);

/** @brief Allocate or disassemble basic sum types (boolean choice of values). */
wikrt_err wikrt_alloc_sum(wikrt_cx*, wikrt_val* c, bool inRight, wikrt_val);
wikrt_err wikrt_split_sum(wikrt_cx*, wikrt_val c, bool* inRight, wikrt_val*);

/** @brief Allocate to hide a value behind a sealer token.
 *
 * Our sealer must be a valid token. Sealer tokens should have type:
 * `∀e.(a*e)→((Sealed a)*e)`, for purpose of consistent quotation and
 * serialization. Once sealed, a value is inaccessible until unsealed.
 * so this serves as a basis for data hiding or structural typing.
 *
 * Wikilon runtime knows discretionary sealers, i.e. {:foo} is undone
 * by {.foo}. Anything else requires special attention. (Note: curly
 * braces aren't included in the token text, i.e. use ":foo".)
 */
wikrt_err wikrt_alloc_seal(wikrt_cx*, wikrt_val* sv, char const* s, wikrt_val v); 

/** @brief Disassemble a sealed value into sealer token and value.
 *
 * Our buffer must have size at least WIKRT_TOK_BUFFSZ to avoid any
 * risks of buffer overfow.
 */
wikrt_err wikrt_split_seal(wikrt_cx*, wikrt_val sv, char* s, wikrt_val* v);


/** @brief Obtain shallow type information for a value.
 *
 * This is potentially useful in cases where you might deal with many
 * types of data, e.g. rendering debug outputs. However, it is not
 * recommended in general. It is too difficult within Awelon bytecode 
 * to abstract or simulate behaviors that depend on reflection.
 */
typedef enum wikrt_vtype 
{ WIKRT_VTYPE_UNIT        // unit value
, WIKRT_VTYPE_PRODUCT     // product 
, WIKRT_VTYPE_INT         // integer values
, WIKRT_VTYPE_SUM         // sum type (includes lists) 
, WIKRT_VTYPE_BLOCK       // functional bytecode  
, WIKRT_VTYPE_SEALED      // sealed values
// special cases
, WIKRT_VTYPE_STOWED      // fully stowed value refs
, WIKRT_VTYPE_PENDING     // awaiting computation
} wikrt_vtype;

wikrt_err wikrt_peek_type(wikrt_cx*, wikrt_vtype* out, wikrt_val const);

/** @brief Copy a value. 
 *
 * Wikilon runtime favors linear 'move' semantics. A value reference is
 * used without aliasing or sharing. A benefit is that pure functions 
 * can be implemented without allocations. The cost is that deep copies
 * are necessary when a value must be copied at all.
 *
 * Awelon Bytecode supports substructural types. Normally, a block marked
 * affine will not be copyable. But the C API is free to ignore such
 * constraints but must do so explicitly by indicating `bCopyAff`.
 *
 * Copies may be shallow and lazy in special cases, e.g. use of value
 * stowage or copies of pending computations. 
 */
wikrt_err wikrt_copy(wikrt_cx*, wikrt_val* copy, wikrt_val const src, bool bCopyAff);

/** @brief Drop a value.
 *
 * When done with a value, drop it. This is roughly equivalent to use
 * of `free()` in C. You may drop any value you own. Of course, if you
 * are about to destroy or reset the context, you may safely skip the
 * overheads of freeing individual values.
 *
 * The C API may freely ignore the substructural 'relevant' constraint
 * by indicating bCopyRel. If not set, we may have an error now as we
 * drop, or later when evaluation of a pending value completes. Setting
 * it also enables lazy destruction.
 */
wikrt_err wikrt_drop(wikrt_cx*, wikrt_val, bool bDropRel);

/** @brief Quote a value into a block. v → [∀e. e → (v * e)].
 *
 * Quoting a value returns a block that, when applied, regenerates
 * the value. This is exposed to users in part as a mechanism to
 * export arbitrary values as a stream of bytecode (via converting
 * the block to text then the text into a binary).
 */
wikrt_err wikrt_quote(wikrt_cx*, wikrt_val* qv, wikrt_val v);

/** @brief Mark a value for stowage.
 *
 * Value stowage serves a role similar to virtual memory. Stowed values
 * are replaced by a much smaller reference. Future access to the value
 * will re-load it into memory. Stowage makes it feasible to operate on
 * massive tree-based data structures - tries, ropes, etc. - within a
 * limited working memory.
 *
 * Values are not immediately stowed upon request. Instead, stowage will
 * usually wait for some evidence that the value will not immediately be
 * accessed. However, stowage does immediately interact with copy/drop.
 * Stowed objects are only latently copied, as need arises, and otherwise
 * we'll use reference counting.
 *
 * The `{&stow}` annotation has the same effect, applying stowage to
 * the first element of a pair.
 */
wikrt_err wikrt_stow(wikrt_cx*, wikrt_val* out, wikrt_val);


  ////////////////
 // EVALUATION //
////////////////

/** @brief Construct an evaluation. 
 *
 * This doesn't actually begin computation. Rather, it allocates and
 * prepares the workspace to perform the computation. One must proceed
 * to `wikrt_step_eval` to perform the computation.
 *
 * An evaluation may be treated as something like a lazy value. It
 * cannot readily be inspected, but it may be used in other values or
 * quoted and serialized.
 */
wikrt_err wikrt_alloc_eval(wikrt_cx*, wikrt_val*, wikrt_val arg, wikrt_val fn);

// disassembly of eval? Or leave it opaque?

/** @brief Step through an evaluation.
 *
 * Wikilon runtime requires a time or effort quota for evaluation. When
 * the quota reaches zero, we'll return WIKRT_QUOTA_STOP. The programmer
 * is free to set higher quotas. The quota is relatively coarse grained,
 * roughly corresponding to megabytes allocated during computation. Any
 * remaining quota is returned.
 *
 * In addition to quota stops, we have WIKRT_TOKEN_STOP for unrecognized 
 * tokens and possibly WIKRT_STREAM_WAIT for streaming computations. And
 * of course we'll halt on type errors, assertion failures, etc.. 
 */
wikrt_err wikrt_step_eval(wikrt_cx*, wikrt_val* evaluation, uint32_t* quota);

/** @brief Handle a token stop.
 *
 * Upon WIKRT_TOKEN_STOP we may split our evaluation into a triple: the
 * token, the argument to it, and the continuation. The continuation is
 * used as the function for wikrt_alloc_eval, but preserves evaluation
 * context to continue more efficiently than a full allocation. It may
 * be used as a normal block, however.
 *
 * Our token buffer must have size at least WIKRT_TOK_BUFFSZ.
 *
 * NOTE: In case of WIKRT_TYPE_ERROR or WIKRT_ASSERT_FAIL, Wikilon runtime
 * will implicitly treat this as stopping on an {error} and {assert} token
 * respectively to support a little introspection. However, this is not 
 * intended to be a primary debugging mechanism.
 */
wikrt_err wikrt_token_stop(wikrt_cx*, wikrt_val eval, wikrt_val* arg, char* tok, wikrt_val* cont);

// NOTE: Wikilon runtime currently provides a minimal API to support
// injecting bytecode and an initial program environment, evaluating,
// then extracting or streaming the result.
//
// later I might introduce API-level support for:
//   basic integer math (add, mul, negate, divmod, compare)
//   function ops (compose, affine, relevant)
//   annotations (par, array, shared, seamless, eqv, etc.)
//   accelerators (list processing, floating point, matrix math, etc.)


  /////////////////////////////////////
 // DATABASE AND PERSISTENCE ENGINE //
/////////////////////////////////////

/** @brief Transactional Persistence
 *
 * The Wikilon environment provides a simple key-value database as a
 * basis for persistence that interacts nicely with value stowage. 
 * Transactions on this database are atomic and isolated by default.
 * Durability is optional and consistency is left to the client.
 *
 * The keys are texts of limited size. The values are more arbitrary,
 * pretty much anything we could stow.
 *
 * Note: This database is not implicitly accessible to ABC computations.
 * Access must be modeled explicitly, like any other algebraic effect,
 * if it is to be provided at all.
 */
typedef wikrt_val wikrt_txn;

/** @brief Begin a new transaction for key-value persistence.
 */
wikrt_err wikrt_txn_begin(wikrt_cx*, wikrt_txn* dest);

// todo: support hierarchical transactions if sufficient demand

/** @brief Read a value from our key-value persistence layer.
 * 
 * This implicitly acts as a 'copy' operation on the value associated
 * with the key. Unless `bCopyAff` is set, we may fail due to copying
 * a non-copyable value. This failure may be latent, on commit.
 */
wikrt_err wikrt_txn_read(wikrt_txn*, char const* key, wikrt_val* dest, bool bCopyAff);

/** @brief Write value into our key-value persistence layer.
 * 
 * Ownership of the value is given to the transaction. This implicitly
 * acts as a 'drop' operation on the value previously associated with 
 * the key. Unless `bDropRel` is set, we may fail due to dropping a
 * non-droppable value. This failure may be latent, on commit.
 */
wikrt_err wikrt_txn_write(wikrt_txn*, char const* key, wikrt_val* val, bool bDropRel);

/** @brief Exchange a value from our key-value persistence layer.
 *
 * Swap a value currently bound to a key with the value provided. This
 * guarantees protection for substructural types and avoids intermediate
 * copies during a transaction that updates a value many times (i.e. you
 * can swap for a dummy value like unit).
 */
wikrt_err wikrt_txn_swap(wikrt_txn*, char const* key, wikrt_val* val);

/** @brief Mark a transaction for durability. 
 *
 * A 'durable' transaction will force the underlying database to
 * push content to disk. Otherwise, transactions only have the ACI
 * properties but will tend to return with reduced latency. Because
 * Wikilon runtime builds on LMDB, 
 */
void wikrt_txn_durable(wikrt_txn*);

/** @brief Abort a transaction.
 *
 * Abandon the transaction and return resources to the context.
 */
void wikrt_txn_abort(wikrt_txn*);

/** @brief Commit a transaction.
 *
 * Attempt to commit a transaction. This may fail if there are
 * conflicts with other transactions, or if the transaction context
 * has been destroyed. Succeed or fail, resources are returned to 
 * the context.
 */
wikrt_err wikrt_txn_commit(wikrt_txn*);
    // TODO: early detection of conflicts
    //       heuristic priority, etc.

/** @brief Ensure durability of prior transactions. 
 *  
 * If you don't explicitly mark transactions durable, consider calling
 * sync every five seconds or so to limit potential data loss.
 */
void wikrt_env_sync(wikrt_env*);



#define WIKILON_RUNTIME_H
#endif
