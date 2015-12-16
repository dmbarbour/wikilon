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
 *  techniques are utilized.
 * 
 *  - Accelerators. Common subprograms (sequences of bytecode) are
 *    recognized and handled as a single opcode internally. We can
 *    accelerate collections-oriented programming, common loops,
 *    matrix math, conditional behaviors, and data plumbing.
 *  
 *  - Mutability. While ABC values are logically immutable, ownership
 *    of values (where a computation holds the only reference) allows
 *    computation by editing values in-place. This greatly reduces the
 *    allocation overheads. Due to substructural types, ABC encourages
 *    writing code with ownership in mind.
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
 *  LICENSE: BSD
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
 * A context is a fixed-size arena for computations. Wikilon runtime
 * currently supports 32-bit arenas, up to 4GB. The lower practical
 * bound is perhaps 4 megabytes. Large value stowage enables operating
 * on values considerably larger than the context.
 *
 * Use of many small contexts is convenient for space quotas, ensuring
 * a computation doesn't use too much space. In Wikilon, we'll tend to
 * use a pool of contexts to serve web pages. The advantage of a larger
 * context is greater potential non-copying sharing of data, especially
 * working with shared arrays. 
 * 
 */
typedef struct wikrt_cx wikrt_cx;

/** An integer type for for Wikilon addresses. */
typedef uintptr_t wikrt_addr;

/** @brief Reference to a value in a context.
 *
 * The value model is oriented around Awelon Bytecode (ABC) and Awelon
 * Object (AO). The five basic value types are integers, products, sums,
 * unit, and blocks of bytecode (which serve as first-class functions).
 * 
 * Beyond these basics, we have value sealing (which serves as an ad-hoc
 * type wrapper), and substructural types (block may be affine or relevant,
 * limiting copy and drop). Compact, specialized representations for lists
 * include arrays, chunked lists, binaries, texts.
 *
 * NOTE: Wikilon runtime is oriented around 'move' semantics. This enables
 * many pure functions to be implemented with non-allocating mutations. 
 * The cost is that 'copying' a value requires actually copying it. The 
 * presence of affine and relevant substructural types discourages AO and
 * ABC programs from using 'copy' and 'drop' for generic programming. 
 *
 * Move semantics applies also to values created through this C API. If
 * you create a value, you own it until you pass it on. You can drop the
 * value if you don't need it anymore. 
 */
typedef struct wikrt_val {
    wikrt_cx*   context;
    wikrt_addr  address;
} wikrt_val;

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
, WIKRT_DBERR           // database or filesystem related errors 
, WIKRT_NOMEM           // malloc or mmap allocation error

// Special Conditions
, WIKRT_CXFULL          // context is out of memory
, WIKRT_NOLINK          // context or environment destroyed

// Transactions
, WIKRT_TXN_CONFLICT    // transaction failed on conflict

// Evaluations
, WIKRT_QUOTA_STOP      // halted on step quota
, WIKRT_TOKEN_STOP      // stop on unrecognized token
, WIKRT_ASSERT_FAIL     // assertion failure (op `K`)
, WIKRT_TYPE_ERROR      // generic type errors


} wikrt_err;

/** @brief Create or Open a Wikilon environment.
 *
 * The developer specifies where the Wikilon environment stores data
 * on the filesystem, and how large this is permitted to grow. If the
 * database does not exist, we'll attempt to create a new one, making
 * parent directories in the filesystem as necessary.
 *
 * This action may fail, most likely for filesystem or database reasons
 * but potentially because there is insufficient address space for the
 * dbMax allocation. 
 *
 * An environment may be created without external storage by setting
 * dbMax to 0. In this case, large value stowage is ignored and any
 * txn_begin operations will fail. There is no memory-only database
 * option (modulo a memory-only filesystem). 
 */
wikrt_err wikrt_env_create(wikrt_env**, char const* dirPath, size_t dbMax);

/** @brief Manage reference counts for an environment.
 *
 * Adding to the reference count for a Wikilon runtime does not prevent
 * its destruction, but does ensure a subset of critical resources remain
 * in memory so we can return WIKRT_NOLINK errors instead of crashing.
 *
 * A newly created environment has refct=1. The destroy operation will
 * implicitly decref. If a live environment's refct is reduced to zero,
 * it is destroyed automatically. Contexts also manage the refct of the
 * environment. Hence, it is feasible to create a context then decref
 * the environment such that the environment is destroyed when there
 * are no more contexts.
 */
void wikrt_env_refct(wikrt_env*, int delta);

/** @brief Destroys an environment and free memory resources. 
 * 
 * This attempts a graceful shutdown of the environment and all its
 * contexts. A small subset of memory resources may remain in memory
 * until reference counts reduce to zero.
 *
 */
void wikrt_env_destroy(wikrt_env*);

/** @brief Create a context for computation.
 * 
 * A context includes a heap and may host tasks and transactions. Most
 * actions involving a runtime require a context. The primary parameter
 * for a context is the size of its address space, which is allocated
 * immediately.
 *
 * This operation may fail with WIKRT_NOMEM if there isn't enough address
 * space to allocate the context. It may fail with WIKRT_NOLINK if the
 * environment has been destroyed but you're still holding a reference to
 * it.
 */ 
wikrt_err wikrt_cx_create(wikrt_env*, wikrt_cx**, size_t cxSpace);

/** @brief Manage reference counts for a context.
 *
 * If concurrent destruction of a context is a risk, threads should
 * maintain a reference to their context. This doesn't prevent the
 * destruction, but rather ensures WIKRT_NOLINK errors instead of
 * crashing outright. 
 *
 * A context implicitly has refct=1 when created. Destroying the context
 * will implicitly decrement its refct; conversely, if the refct is
 * decremented to zero, the context is implicitly destroyed.
 */
void wikrt_cx_refct(wikrt_cx*, int delta);

/** @brief Destroys a context and frees its memory.
 *
 * After destroying a context, most operations on from other threads
 * (that should be holding a refct) will fail with WIKRT_NOLINK. A
 * context that is created should always be destroyed, either through
 * this operation or via decrementing the initial refct.
 */
void wikrt_cx_destroy(wikrt_cx*);

/** A context holds a reference to its environment. */
wikrt_env* wikrt_cx_env(wikrt_cx*);

/** @brief Copy a value. 
 *
 * Wikilon runtime favors 'move' semantics, with a single owner for
 * any given value reference. This enables many pure functions to be
 * implemented with non-allocating in-place mutation. However, it 
 * also requires deep copies instead of value sharing.
 *
 * In some cases - arrays, pending stowage, blocks, etc. - Wikilon
 * runtime might perform a logical copy, and only perform the true 
 * copy of the representation as needed. These cases are driven by
 * annotations. 
 *
 * The C API is not required to respect substructural types, though
 * doing so where feasible is highly recommended. If developers wish
 * to ignore affine properties, simply set `bCopyAff` to true. Note:
 * errors due to copyability are latent in case of pending values.
 */
wikrt_err wikrt_copy(wikrt_val const* src, wikrt_val* dest, bool bCopyAff);

/** @brief Drop a value. Recover context memory resources.
 * 
 * Like conventional C heaps, a context maintains its own free list.
 * Memory resources are returned to this free list, with some special
 * exceptions like values in a nursery arena. Developers should drop
 * values explicitly while they're continuing to use a context or if
 * they wish to guard substructural types. When finished, the entire
 * context may be destroyed rather than managing individual values.
 * 
 * The C API is not required to respect substructural types, though
 * doing so is recommended. If developers wish to drop relevant values,
 * it is sufficient to set the `bDropRel` flag to true.
 */
wikrt_err wikrt_drop(wikrt_val*, bool bDropRel);

/** @brief Shallow analysis of value types, for switching. 
 *
 * Outside of specialized cases (e.g. debug output or rendering, and
 * modeling reflection) we should try to stick with statically typed
 * idioms even when accessing values through the C API. 
 */
typedef enum wikrt_val_type 
{ WIKRT_VAL_UNIT        // unit value
, WIKRT_VAL_PRODUCT     // product (a pair of values)
, WIKRT_VAL_INT         // integer values
, WIKRT_VAL_SUM         // sum type (a choice of values)
, WIKRT_VAL_BLOCK       // functional bytecode  
, WIKRT_VAL_SEALED      // sealed values
// special cases
, WIKRT_VAL_STOW        // values awaiting stowage
, WIKRT_VAL_STOWED      // fully stowed values
, WIKRT_VAL_PENDING     // parallel or lazy eval
} wikrt_val_type;

/** Obtain shallow analysis of value type. */
wikrt_err wikrt_peek_type(wikrt_val const*, wikrt_val_type* out);

/** @brief Access a product of values non-destructively. 
 *
 * This takes a simple structure of the type (a*b) and returns `a`
 * in fst and `b` in snd. The original structure is preserved, so
 * the caller needs to be careful about managing ownership. If the
 * value was not a product, you'll receive WIKRT_TYPE_ERROR.
 */
wikrt_err wikrt_peek_prod(wikrt_val const* p, wikrt_val* fst, wikrt_val* snd);

/** @brief Access a stack of values non-destructively. 
 *
 * A stack has the shape (a*(b*(c*(d*e)))). If you ask for two elements,
 * you'd get `a` and `b` in the array, and `(c*(d*e))` in the leftovers.
 * If you ask for more elements than the stack contains, you'll receive
 * WIKRT_TYPE_ERROR.
 */
wikrt_err wikrt_peek_stack(wikrt_val const* stack, size_t nStackElems, wikrt_val* pValArray, wikrt_val* rem);

/** @brief Access a sum of values non-destructively.
 *
 * This takes a simple structure of the type (a+b) and returns either
 * true with `a` in the left or false with `b` in the right. If the
 * value was not a sum, you'll receive WIKRT_TYPE_ERROR.
 *
 * Note: in general, this function may need to allocate.
 */
wikrt_err wikrt_peek_sum(wikrt_val const* s, bool* bInLeft, wikrt_val* dst);

/** @brief Access a list of values non-destructively.
 *
 * A list has the structure `type List a b = ((a * List a b) + b)`.
 * We can load some number of elements into an array at a time. If
 * the argument is not a list, this fails with WIKRT_TYPE_ERROR. 
 */
wikrt_err wikrt_peek_list(wikrt_val const* lst, size_t nMaxElems, wikrt_val* pValArray, size_t* nListElems, wikrt_val* rem);

/** @brief Access a text value non-destructively.
 *
 * A text is a list of utf-8 codepoints with some constraints (forbids
 * C0 (except LF), DEL, C1, surrogates, replacement char). Because the
 * NUL value is not used in Wikilon runtime texts, we can return NUL
 * terminated C strings.
 *
 * In addition to the main output, developers may additionally receive
 * a number of bytes and utf-8 codepoints.
 */
wikrt_err wikrt_peek_text(wikrt_val const* txt, size_t nMaxBytes, char* dst, size_t* nBytes, size_t* nChars, wikrt_val* rem);

/** @brief Access a binary value non-destructively.
 *
 * A binary is a list of small integers in the range 0..255. We can
 * copy fragments of the binary up to the caller.
 */


/** @brief Read relatively small integers.
 *
 * If the destination isn't large enough we'll return WIKRT_TYPE_ERROR.
 */
wikrt_err wikrt_peek_int(wikrt_val const*, int*);
wikrt_err wikrt_peek_i32(wikrt_val const*, int32_t*);
wikrt_err wikrt_peek_i64(wikrt_val const*, int64_t*);

/** @brief Read a large integer into a text.
 *
 * Regex: 0 | (-)?(1-9)(0-9)*       (NUL terminated)
 * 
 * If the output buffer is not of sufficient size, we'll return a
 * WIKRT_TYPE_ERROR. Use wikrt_peek_isize to obtain 
 */
wikrt_err wikrt_peek_istr(wikrt_val const*, char* dst, size_t nMaxChars);
wikrt_err wikrt_peek_isize(wikrt_val const*, int* nBuffSize);

/** @brief Dismantle a product and access its elements.
 *
 * This gives the caller full ownership of the separate `fst` and `snd`
 * values while the original pair is returned to the allocator. This
 * simplifies further use of the values. Returns WIKRT_TYPE_ERROR if
 * the argument is not a product.
 */
wikrt_err wikrt_pop_prod(wikrt_val* p, wikrt_val* fst, wikrt_val* snd);

/** @brief Dismantle a stack of values and access the elements.
 * 
 * Cf. peek_stack, except the original stack is destroyed.
 */
wikrt_err wikrt_pop_stack(wikrt_val* stack, size_t nStackElems, wikrt_val* pValArray, wikrt_val* rem);

/** @brief Dismantle a sum and access the value.
 *
 * This variation is non-allocating in the current implementation.  
 */
wikrt_err wikrt_pop_sum(wikrt_val* s, bool* bInLeft, wikrt_val* dst);

/** @brief Dismantle a list and access the values.
 *
 * Cf. peek_list, except the original list is destroyed. 
 */
wikrt_err wikrt_pop_list(wikrt_val* lst, size_t nMaxElems, wikrt_val* pValArray, size_t* nListElems, wikrt_val* rem);

// thoughts: 
//  maybe some variant of sscanf pattern matching
//  support the various accelerated list operations
//

/** @brief Construct the unit value.
 *
 * In the current implementation, this doesn't actually require an
 * allocation. 
 */
wikrt_err wikrt_alloc_unit(wikrt_cx*, wikrt_val* dest);

/** @brief Construct an integer value.
 *
 * Small integers, within the range of plus or minus a billion, do not
 * require allocations. They are represented in the address.
 */
wikrt_err wikrt_alloc_int(wikrt_cx*, int, wikrt_val* dest);
wikrt_err wikrt_alloc_i32(wikrt_cx*, int32_t, wikrt_val* dest);
wikrt_err wikrt_alloc_i64(wikrt_cx*, int64_t, wikrt_val* dest);

/** @brief Construct an integer value from a text representation.
 *
 * Expected regex: 0 | (-)?(1-9)(0-9)*      (NUL terminated)
 */
wikrt_err wikrt_alloc_istr(wikrt_cx*, char const*, wikrt_val* dest);
// maybe a variant for multiple bases? nah, let's just move on for now.

/** @brief Construct an AO text value from a C string.
 * 
 * Wikilon runtime only permits valid AO texts. In particular, our
 * texts must exclude control characters (C0, C1, DEL) except LF,
 * surrogates (U+D800 to U+DFFF), and the replacement character
 * (U+FFFD). If these conditions aren't met, WIKRT_INVAL will be
 * returned.
 */
wikrt_err wikrt_alloc_text(wikrt_cx*, char const*, wikrt_val* dest);

/** @brief Construct an AO binary value.
 *
 */ 


























/** @brief Transactional Persistence
 *
 * The Wikilon environment includes a simple key-value database for
 * use as a persistence layer that integrates easily with large value
 * stowage. Transactions enable consistent views of multiple keys and
 * atomic updates. Durability is optional per transaction.
 *
 * Note: This database is not implicitly accessible to ABC computations.
 * Access may be modeled explicitly, e.g. as a free monadic effect, like
 * any other effect. Semantics for the persistence layer are up to each
 * application.
 */
typedef struct wikrt_txn { wikrt_val txn; } wikrt_txn;
#define WIKRT_DB_KEY_SIZE_MAX 255

/** @brief Access the validation code for a proposed key.
 *
 * Database keys are constrained to be valid utf-8 texts with up to
 * 255 bytes excluding control characters (C0, DEL, C1), surrogates
 * (U+D800 - U+DFFF), and the replacement character (U+FFFD).
 */
bool wikrt_db_keyvalid(char const*);

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
wikrt_err wikrt_db_read(wikrt_txn*, char const* key, wikrt_val* dest, bool bCopyAff);

/** @brief Write value into our key-value persistence layer.
 * 
 * Ownership of the value is given to the transaction. This implicitly
 * acts as a 'drop' operation on the value previously associated with 
 * the key. Unless `bDropRel` is set, we may fail due to dropping a
 * non-droppable value. This failure may be latent, on commit.
 */
wikrt_err wikrt_db_write(wikrt_txn*, char const* key, wikrt_val* val, bool bDropRel);

/** @brief Exchange a value from our key-value persistence layer.
 *
 * Swap a value currently bound to a key with the value provided. This
 * guarantees protection for substructural types and avoids intermediate
 * copies during a transaction that updates a value many times (i.e. you
 * can swap for a dummy value like unit).
 */
wikrt_err wikrt_db_swap(wikrt_txn*, char const* key, wikrt_val* val);

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
void wikrt_db_sync(wikrt_env*);


#define WIKILON_RUNTIME_H
#endif
