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

/** @brief Opaque structure representing a heap for computations.
 *
 * A context is a fixed-size arena for computations. Wikilon favors a
 * separate context for each toplevel comptuation, e.g. each web page
 * and background task. 
 *
 * Individually, contexts are limited to 4GB in size because we use
 * 32-bit value references within each context. However, large value
 * stowage supports manipulation of data larger than the context.
 * 
 */
typedef struct wikrt_cx wikrt_cx;

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
 * purely functional semantics. OTOH, this requires deep copies by
 * default. Some large values may use reference counting under the
 * hood, but sharing isn't exposed through the API.
 */ 
typedef uint32_t wikrt_val;

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
, WIKRT_DBERR           // persistence layer errors 
, WIKRT_NOMEM           // malloc or mmap allocation error

// Special Conditions
, WIKRT_CXFULL          // context is out of memory
, WIKRT_NOLINK          // context or environment destroyed
, WIKRT_BUFFSZ          // output buffer too small

// Transactions
, WIKRT_TXN_CONFLICT    // transaction failed on conflict

// Evaluations
, WIKRT_QUOTA_STOP      // halted on step quota
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
 */
wikrt_err wikrt_env_create(wikrt_env**, char const* dirPath, uint32_t dbMaxMB);

/** @brief Manage reference counts for an environment. 
 *
 * This helps resist concurrency errors. Using reference counts doesn't
 * prevent destruction, but operations will return WIKRT_NOLINK instead
 * of undefined behavior if destroyed early. Decref is represented by a
 * negative delta.
 *
 * An environment is destroyed implicitly if decref'd more than incref'd. 
 */
void wikrt_env_incref(wikrt_env*, int delta);

/** @brief Graceful shutdown of environment and active contexts. */
void wikrt_env_destroy(wikrt_env*);

/** @brief Create a context for computations.
 * 
 * A context consists mostly of one big mmap'd block of memory. The
 * viable range for sizes is about 4..4000 in megabyte units. The
 * context cannot be resized once created.
 */ 
wikrt_err wikrt_cx_create(wikrt_env*, wikrt_cx**, uint32_t sizeMB);

/** @brief Manage reference counts for a context.
 *
 * Using reference counts doesn't prevent destruction of the context,
 * but ensures access to a destroyed environment returns WIKRT_NOLINK
 * instead of performing undefined behavior. Decref is represented by
 * a negative delta.
 * 
 * A context is destroyed implicitly if decref'd more than incref'd. 
 */
void wikrt_cx_incref(wikrt_cx*, int delta);

/** @brief Gracefully destroy a context and recover its memory. */
void wikrt_cx_destroy(wikrt_cx*);

/** @brief A context knows its parent environment. */
wikrt_env* wikrt_cx_env(wikrt_cx*);

  ////////////////////////////
 // VALUE CONSTRUCTION API //
////////////////////////////

/** @brief Construct smaller integers.
 *
 * Note: Small integers within range of plus or minus one billion
 * will be stored in the value reference directly. Outside of this
 * range, we may allocate multiple cells of memory to store the
 * value's representation.
 */
wikrt_err wikrt_alloc_i32(wikrt_cx*, wikrt_val* out, int32_t);
wikrt_err wikrt_alloc_i64(wikrt_cx*, wikrt_val* out, int64_t);

/** @brief Construct larger integers from a string.
 *
 * Expected regex: 0 | (-)?(1-9)(0-9)*
 *
 * Currently we only support decimal input.
 */
wikrt_err wikrt_alloc_istr(wikrt_cx*, wikrt_val* out, char const*);

/** @brief Construct unit value in context. 
 *
 * While this is non-allocating, clients are not encouraged to assume
 * this in the API. I.e. go ahead and 'alloc' a new unit value every
 * time, even though they're all the same. For performance purposes,
 */
wikrt_err wikrt_alloc_unit(wikrt_cx*, wikrt_val* out);

/** @brief Construct a product type, a pair (fst * snd) of values. */
wikrt_err wikrt_alloc_prod(wikrt_cx*, wikrt_val* out, wikrt_val fst, wikrt_val snd);

/** @brief Construct a sum type, a choice (x + _) or (_ + x) of values. */
wikrt_err wikrt_alloc_sum(wikrt_cx*, wikrt_val* out, wikrt_val x, bool inRight);

/** @brief Construct a block value from a string of bytecode. 
 *
 * A block is expressed by a finite sequence of Awelon Bytecode (ABC),
 * represented here by a simple C string. In the future, we may also
 * support ABCD (ABC Deflated) accelerators. Wikilon runtime understands
 * some annotations and basic value sealing tokens, but others may cause
 * evaluation to yield to an external handler. 
 *
 * Blocks are how we express first class functional behavior in ABC
 * and in Wikilon. A block represents a purely functional program.
 * Due to properties of ABC, blocks can be composed by concatenation
 * of their bytecode.
 *
 * As a general rule, constructing a large block then applying it will
 * be more efficient compared to direct value manipulations. Wikilon
 * optimizes heavily for this use case.
 */
wikrt_err wikrt_alloc_block(wikrt_cx*, wikrt_val* out, char const* abc);
  // NOTE: I'll want a stream construction variant for large blocks.

/** @brief Construct a list represented by an array.
 *
 * Lists have recursive form `μL.((a*L)+b)`. That is, a list is a choice
 * of an element `a` and more list in the left or terminate in the right.
 * Typically `b` is type unit, but we can generalize to other terminals.
 * Wikilon's naive representation for lists is optimal for linked lists,
 * just one pointer per element.
 *
 * But we can do much better, and improve memory locality besides, by
 * using array-based representations of lists. Overhead is reduced to
 * buffer address and size information, once for the full whole array.
 * Further, we can accelerate many list processing functions: lookup,
 * update, splits, logical reversals, length, etc..
 *
 * Accelerating list processing via arrays is an important performance
 * strategy for Wikilon runtime. However, relying on very large arrays
 * (e.g. with a million elements) is not recommended. The goal should
 * be instead to use smaller arrays as a building block for ropes or
 * finger tree sequences.
 *
 */
wikrt_err wikrt_alloc_array(wikrt_cx*, wikrt_val* out, wikrt_val const* arr, uint32_t nVals);

/** @brief Construct a binary value.
 *
 * Binaries are lists of small integers in range 0..255, albeit with
 * compact representation as an array. 
 */
wikrt_err wikrt_alloc_binary(wikrt_cx*, wikrt_val* out, uint8_t const* binary, uint32_t nBytes);

/** @brief Construct text values from a utf-8, NUL-terminated C string. */
wikrt_err wikrt_alloc_text(wikrt_cx*, wikrt_val* out, char const* text);

/** @brief Test whether a text is valid for Wikilon runtime.
 *
 * Wikilon runtime expects texts to have a utf-8 encoding, presented
 * as a NUL-terminated C string, with the following constraints:
 *
 * - no control chars (C0, DEL C1) except LF
 * - no surrogate codepoints (U+D800 to U+DFFF)
 * - no replacement character (U+FFFD)
 * 
 */
bool wikrt_text_valid(char const*);

/** @brief Construct a discretionary sealed value.
 *
 * Discretionary value sealing operates by wrapping a value with a
 * sealer, a token of form {:foo}. This serves a similar role as a
 * newtype wrapper, guarding against accidental access. The value 
 * must first be unwrapped with {.foo}. A similar idea is applicable
 * to open distributed systems via cryptographic sealers.
 * 
 * The sealer here is constrained by wikrt_token_valid, and must 
 * include the prefix colon for discretionary seals. Wikilon runtime
 * does have built-in knowledge for discretionary value sealing.
 *
 * When serializing to bytecode, we assume that sealed values can be
 * represented by quoting the value and following it by the sealer
 * as we do with discretionary sealers (e.g. `#42{:foo}`). 
 */
wikrt_err wikrt_alloc_sealed(wikrt_cx*, wikrt_val* out, wikrt_val val, char const* sealer); 

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
bool wikrt_token_valid(char const* tok);

  /////////////////////////////
 // VALUE MANIPULATIONS API //
/////////////////////////////

/** @brief Copy a value. 
 *
 * Wikilon runtime favors linear 'move' semantics, where we transfer
 * ownership of a value. This allows update in place. However, this 
 * also means we must explicitly copy values if we expect to use the
 * value twice.
 *
 * Developers should assume this performs a 'deep copy' of a value.
 * Some special case will not be deep copied, instead using reference
 * counts, but that feature is generally hidden from this API and is
 * not something to heavily rely upon.
 *
 * The C API may freely ignore the substructural 'affine' constraint
 * by indicating bCopyAff. If not set, we may have an error now as we
 * copy or later if we copy a pending (lazy or parallel) value.
 */
wikrt_err wikrt_copy(wikrt_cx*, wikrt_val* copy, wikrt_val const src, bool bCopyAff);

/** @brief Delete a value.
 *
 * If you're done with a value or computation, you should drop it.
 * Of course, this is only for values you 'own'. Also, you can skip
 * this step if you're about to destroy the larger context.
 *
 * The C API may freely ignore the substructural 'relevant' constraint
 * by indicating bCopyRel. If not set, we may have an error now as we
 * drop, or later when evaluation of a pending value completes. Setting
 * it also enables lazy destruction.
 */
wikirt_err wikrt_drop(wikrt_cx*, wikrt_val, bool bDropRel);

/** @brief Quote a value.
 *
 */

// composition
// affine, relevant
// parallel, lazy?
// math: + * - Q >

// reflection


  ////////////////////////////
 // BASIC VALUE ACCESS API //
////////////////////////////

/** @brief Shallow reflection on value types. 
 *
 * Awelon bytecode assumes statically type-safe code. There are no
 * implicit conversions. Reflecting on the 'type' of a value is mostly
 * for special features like debug traces or animated evaluation.
 */
typedef enum wikrt_val_type 
{ WIKRT_VAL_UNIT = 1    // unit value
, WIKRT_VAL_PRODUCT     // product 
, WIKRT_VAL_INT         // integer values
, WIKRT_VAL_SUM         // sum type (includes lists) 
, WIKRT_VAL_BLOCK       // functional bytecode  
, WIKRT_VAL_SEALED      // sealed values
// special cases
, WIKRT_VAL_STOWED      // fully stowed values
, WIKRT_VAL_PENDING     // lazy or parallel computation
} wikrt_val_type;

wikrt_err wikrt_peek_type(wikrt_cx*, wikrt_val_type* out, wikrt_val const);

// application or evaluations (special case?)
//  stream applications?


/** @brief Read relatively small integers.
 *
 * If the destination isn't large enough we'll return WIKRT_BUFFSZ.
 * If the argument isn't an integer, we'll return WIKRT_TYPE_ERROR.
 */
wikrt_err wikrt_peek_i32(wikrt_cx*, wikrt_val const, int32_t*);
wikrt_err wikrt_peek_i64(wikrt_cx*, wikrt_val const, int64_t*);




  ////////////////////////////
 // STREAMING BYTECODE API //
////////////////////////////

/** @brief Construct a stream of bytecode.
 *
 * A stream of bytecode is an open-ended block of bytecode, of some
 * unknown size. The protocol: `alloc append* close`. If you're only
 * going to append at most once, consider directly allocating a block
 * instead.
 *
 * The stream may be used almost anywhere a block may be used - copied, 
 * applied, evaluated, quoted, etc.. But it qualifies as a PENDING value
 * until closed, and cannot be observed as a normal value.
 */
wikrt_err wikrt_stream_alloc(wikrt_val* out, wikrt_cx*);

/** @brief Append bytecode to our stream.
 *
 * Wikilon uses Awelon Bytecode (ABC), provided here as a NUL-terminated
 * C string (with UTF-8 encoding). We may later support ABCD extensions 
 * for concision, but doing so isn't a high priority at this time.
 *
 * Under the hood, Wikilon may simplify and partially evaluate bytecode.
 * But behavior for valid inputs will be preserved.
 *
 * Provided bytecode may be incomplete, e.g. stopping in the middle of
 * a block, embedded text, or character. The intention with streams is
 * to support buffered processing of bytecode input.
 */
wikrt_err wikrt_stream_append(wikrt_val* s, char const* bytecode);

/** @brief Close a bytecode stream.
 *
 * When you're done adding bytecode to a stream, close it. This is
 * necessary to complete applications, observe 
 */

/** @brief Construct a block of bytecode.
 *
 * This is equivalent to opening, filling, and closing a stream in
 * one step. This is useful if you know the bytecode ahead of time
 * and it isn't too large to easily represent in memory.
 */






/** @brief Read a large integer into a text.
 *
 * Regex: 0 | (-)?(1-9)(0-9)*       (NUL terminated)
 * 
 * If the output buffer is not of sufficient size, we'll return a
 * WIKRT_BUFFSZ. Calling wikrt_peek_isize will return the required
 * buffer size (including space for the NUL terminator).
 *
 * If the argument is not an integer, these return WIKRT_TYPE_ERROR.
 */
wikrt_err wikrt_peek_istr(wikrt_val const*, char* dst, size_t nMaxChars);
wikrt_err wikrt_peek_isize(wikrt_val const*, int* nBuffSize);

/** @brief Peek into a token-wrapped value. 
 *
 * The sealing token is copied into tok as a NUL-terminated C string.
 * If the output buffer is too small, we'll return WIKRT_BUFFSZ. A 
 * buffer of  WIKRT_TOK_BUFFSZ will prevent buffer size issues.
 *
 * If argument was not a sealed value, WIKRT_TYPE_ERROR is returned,
 * the token text is empty, and the value is simply copied to the output.
 * 
 * Note: This operation is a form of reflection, like wikrt_peek_type.
 * The same caveats apply: it's preferable to avoid this feature outside
 * of special cases like rendering values for debugging.
 */
wikrt_err wikrt_peek_seal(wikrt_val const*, size_t nMaxBytes, char* tok, wikrt_val* ); 

/** @brief 'Pop' variants dismantle their argument. 
 *
 * For many wikrt_peek_X functions, there is a similar wikrt_pop_X
 * function that dismantles the argument. This provides the caller
 * ownership of the returned elements.
 *
 * For the wikrt_pop_list variants, WIKRT_TYPE_ERROR still allows
 * partial success that pops elements off the list up to the point
 * of type error, which is returned in `rem`.
 *
 * Because 'pop' is destructive, sums can be accessed this way one
 * small step at a time (i.e. nPathBytes = 2).  to how sums are modeled, sums can be accessed via pop with
 * much smaller steps.  safely done
 * just one small step at a time. 
 */
wikrt_err wikrt_pop_prod(wikrt_val* p, wikrt_val* fst, wikrt_val* snd);
wikrt_err wikrt_pop_stack(wikrt_val* stack, size_t nStackElems, wikrt_val* pValArray, wikrt_val* rem);
wikrt_err wikrt_pop_list(wikrt_val* lst, size_t nMaxElems, wikrt_val* pValArray, size_t* nListElems, wikrt_val* rem);
wikrt_err wikrt_pop_binary(wikrt_val* binary, size_t nMaxBytes, unsigned char* dst, size_t* nBytes, wikrt_val* rem);
wikrt_err wikrt_pop_text(wikrt_val* txt, size_t nMaxBytes, char* dst, size_t* nBytes, size_t* nChars, wikrt_val* rem);
wikrt_err wikrt_pop_seal(wikrt_val* sealedVal, size_t nMaxBytes, char* tok, wikrt_val* unsealedVal);
wikrt_err wikrt_pop_sum(wikrt_val* sum, size_t nPathBytes, char* path, wikrt_val* val);

/** @brief Maximum buffer size for a token.
 *
 * The maximum token size from Awelon Bytecode is 63 bytes. Wikilon
 * runtime adds a byte for a NUL-terminator to support C strings. 
 * The token does not include the `{}` braces, just the text between
 * them.
 */
#define WIKRT_TOK_BUFFSZ 64



// TODO:
//   annotations
//     arrays, assertions
//   quoting values
//   composition of blocks
//   substructural types
//   floats and doubles
//     once we have accelerators
//   

  /////////////////////////////////////
 // DATABASE AND PERSISTENCE ENGINE //
/////////////////////////////////////

/** @brief Transactional Persistence
 *
 * The Wikilon environment includes a simple key-value database for
 * use as a persistence layer that integrates easily with large value
 * stowage. Transactions enable consistent views of multiple keys and
 * atomic updates. Durability is optional per transaction.
 *
 * Keys are texts with limited size.
 *
 * Note: This database is not implicitly accessible to ABC computations.
 * Access may be modeled explicitly, e.g. as a free monadic effect, like
 * any other effect. Semantics for the persistence layer are up to each
 * application.
 */
typedef struct wikrt_txn { wikrt_val txn; } wikrt_txn;
#define WIKRT_DB_KEY_SIZE_MAX 255

/** @brief Test validity of a proposed key.
 *
 * Keys must be valid texts, with an additional constraint of at
 * most 255 bytes in length.  
 */
bool wikrt_key_valid(char const*);

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
