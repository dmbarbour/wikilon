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
 *  @section usage_sec Usage
 *
 *  Create an environment. Create a context within that environment.
 *  Create a task within that context. For persistent interactions, 
 *  create a transaction as part of your task and load some data from
 *  the built-in database. When the task is done, destroy it.
 * 
 *  @section notes_sec Notes
 *
 *  Portability: Wikilon runtime is written for use in Linux with GCC.
 *  It doesn't use much non-portable code, though. With a little work, 
 *  it should be easily adapted for other systems.
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
 * The environment contains elements shared by associated contexts.
 * This includes LMDB storage and a pool of worker threads (one per
 * CPU, with affinity) for any par/seq parallelism. There's no point
 * to having more worker threads than cores.
 *
 */
typedef struct wikrt_env wikrt_env;

/** @brief Opaque structure for a specific heap.
 *
 * A context is an arena for computations. Mostly, it consists of one
 * large address space with a set of active computations. The context
 * may be suspended at any time. If destroyed, all values associated
 * with a context become inaccessible.
 *
 * Use of multiple smaller contexts, instead of one large context, is
 * convenient for space quotas - ensuring a computation doesn't use 
 * too much space at once. Of course, large value stowage allows a
 * computation to use much more data than is held in memory at any
 * given instant.
 *
 * At the moment, Wikilon only supports 32-bit contexts (max size 
 * 4GB). Eventually, we may additionally support 64-bit contexts
 * with an option at construction time. The lower practical limit
 * for a context is perhaps 4 megabytes (one task, no parallelism). 
 */
typedef struct wikrt_cx wikrt_cx;

/** @brief Reference to a value in a context.
 * 
 * A Wikilon runtime context only supports values for use with Awelon
 * Bytecode (ABC). The five basic values include integers, products,
 * sums, unit, and functions. With these, we can represent lists,
 * trees, opaque objects, and existentials. Arrays, texts, binaries,
 * and a few other types are specialized representations for lists
 * which permit some nice acceleration of list processing functions.
 *
 * Wikilon runtime also supports discretionary value sealing (e.g. 
 * like a newtype wrapper) to help resist type errors.
 *
 * Wikilon runtime favors move and deep-copy semantics. The default
 * assumption is that we hold the only reference to any value. This
 * allows nice tricks like if we have pair (a,b) then swap operates
 * in place to form (b,a) instead of allocating a new pair. In place
 * update of arrays is possible with accelerated list processing.
 *
 * This applies also to values created through this C API. If you create
 * a value, you own it. If you pass that value into task or transaction,
 * that task now owns it. If you want to still hold a copy of the value,
 * you must copy it explicitly. I'll aim to make the API clear.
 *
 */
typedef struct wikrt_val {
    wikrt_cx*  context;
    uintptr_t  address;
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
, WIKRT_DBERR           // database related errors 
, WIKRT_NOMEM           // malloc or mmap allocation error

// Special Conditions
, WIKRT_CXFULL          // context is out of memory
, WIKRT_NOLINK          // context or environment destroyed

// Transactions
, WIKRT_CONFLICT        // transaction failed on conflict

// Evaluations
, WIKRT_TOKEN_STOP      // stop on unrecognized token
, WIKRT_QUOTA_STOP      // halted on step quota
, WIKRT_COPY_AFFINE     // copied an affine value
, WIKRT_DROP_RELEVANT   // dropped a relevant value
, WIKRT_ASSERT_FAIL     // assertion failure (operator `K`)
, WIKRT_TYPE_ERROR      // generic type errors

} wikrt_err;


/** @brief Structure representing an active computation.
 *
 * At the C API, operations on values are associated with a task. This
 * binding protects data locality, reduces synchronization overhead, 
 * ensures a reference count is held on the context, and provides a
 * location for any debug output. Tasks are single-threaded, but are
 * not bound to any particular thread.
 *
 * Under the hood, a task allocates from the heap in larger chunks for
 * data locality and to reduce synchronization on the context's free
 * list. A task also may have a 'nursery', a special region for fast
 * allocation and compacting GC that serves a similar role as a stack
 * in more conventional languages.
 */
typedef struct wikrt_task { wikrt_val task; } wikrt_task;


/** @brief Create or Open a Wikilon environment.
 *
 * The developer specifies where the Wikilon environment stores data
 * on the filesystem, and how large this is permitted to grow. If the
 * database does not exist, we'll attempt to create a new one, making
 * parent directories in the filesystem as necessary.
 *
 * This may fail due to any number of permissions errors, or if there
 * isn't enough address space, or if the database is already in use, 
 * or if the given path is invalid, etc.. In case of failure, a NULL 
 * environment is returned, along with a non-zero errno.
 *
 * An environment may be created without external storage by setting
 * dbMax to 0. In this case, large value stowage is ignored and any
 * txn_begin operations will fail. However, there is no memory-only
 * database option (modulo a memory-only filesystem). 
 */
wikrt_err wikrt_env_create(wikrt_env**, char const* dirPath, size_t dbMax);

/** @brief Destroys an environment and free its memory.
 * 
 * When you're done with an environment, destroy it. This action will
 * suspend all active computations, wait for active database writes to
 * finish, and perform a final sync. Wikilon attempts to be robust to
 * crashes, so this serves as a 'graceful' shutdown.
 *
 * The actual memory for the wikrt_env is preserved until the final
 * context is destroyed. 
 */
void wikrt_env_destroy(wikrt_env*);

/** @brief Create a context for computation.
 * 
 * A context includes a heap and may host tasks and transactions. Most
 * actions involving a runtime require a context and a task within it.
 *
 * The primary parameter for a context is the size of its address space.
 * This is allocated immediately. Eventually, contexts may support more
 * configuration, e.g. for nursery sizes and GC heuristics. But for now
 * the defaults will have to suffice. Right now, contexts are limited
 * to a 32-bit internal address space, up to 4GB in size. This limit
 * may be lifted later, but is sufficient for Wikilon's use cases.
 *
 */ 
wikrt_err wikrt_cx_create(wikrt_env*, wikrt_cx**, size_t cxSpace);

// todo: support configuration of contexts:
//  number of nurseries (or alloc dynamically?)
//  config for large vs. small nurseries

/** @brief Destroys a context and frees its memory.
 *
 * Any active computations on the context are suspended. Some memory
 * resources will be held until all 'tasks' are destroyed. 
 */
void wikrt_cx_destroy(wikrt_cx*);

/** Every context holds a reference to its environment. */
wikrt_env* wikrt_cx_env(wikrt_cx*);

/** @brief Create a new task within a context. 
 *
 * This may fail if the context is full or destroyed.
 */
wikrt_err wikrt_task_create(wikrt_cx*, wikrt_task* dest);

/** For switching on a value's general type.
 */


/** @brief Transactional Persistence
 *
 * The Wikilon environment includes a simple key-value database.
 *
 * This database is persistent, and is intended to support a persistent
 * runtime. Changes to the database are preserved in the same database
 * as our large value stowage. Durability is optional per transaction or
 * via explicit sync.
 *
 * Keys must be valid AO texts (utf8 strings minus C0 (except LF),
 * DEL, C1, U+D800-U+DFFF, and U+FFFD) and have limited size (no more
 * than 255 bytes). The values are anything we can stow.
 * 
 * NOTE: This database is not directly accessible to ABC computations.
 * Access to a database can be modeled, e.g. as a free monadic effect.
 * Doing so could be useful for constructing software agents. But the
 * normal use is just to persist dictionaries and cached computations,
 * or other application-specific content.
 */
typedef struct wikrt_txn { wikrt_val txn; } wikrt_txn;
#define WIKRT_DB_KEY_SIZE_MAX 255

/** Access the validation code for a proposed key. */
bool wikrt_db_keyvalid(char const*);

/** @brief Begin a new transaction for key-value persistence.
 *
 * A transaction must be associated with a task, which itself is 
 * bound to a particular runtime context. If the task is destroyed,
 * all transactions are implicitly aborted.
 *
 * After you start a transaction, you must eventually use 'abort' or
 * 'commit' to release the transaction. Otherwise, the transaction
 * prevents full recovery of memory from the underlying context, 
 */
wikrt_err wikrt_txn_begin(wikrt_task*, wikrt_txn*);

// TODO: potential support for hierarchical transactions (if sufficient demand)

/** @brief Read a value from our key-value persistence layer.
 * 
 * If a read succeeds, you have a copy of the value from the database
 * or a value written to the same key earlier in the transaction. The
 * read may fail if the only available copy is potentially inconsistent
 * due to a concurrent transaction.
 *
 * If this causes a transaction to copy an affine value, the action
 * succeeds but we'll also report WIKRT_COPY_AFFINE as an error. This
 * enables a caller to decide policy on whether substructural types 
 * are respected for a class of transactions.
 *
 */
wikrt_err wikrt_db_read(wikrt_txn*, char const* key, wikrt_val* dest);

/** @brief Write value into our key-value persistence layer.
 * 
 * This function will write a value to our persistence layer. The
 * transaction takes ownership of this value, but you can copy it
 * ahead of time or simply read the key. 
 *
 * If this causes a transaction to overwrite a relevant value, the
 * action succeeds but we'll get an WIKRT_DROP_RELEVANT error. This
 * enables the caller to decide policy on whether substructural types
 * are respected for a class of transactions.
 */
wikrt_err wikrt_db_write(wikrt_txn*, char const* key, wikrt_val* val);

/** @brief Exchange a value from our key-value persistence layer.
 *
 * Swap a value currently bound to a key with the value provided. This
 * guarantees protection for substructural types, and avoids intermediate
 * copies during a transaction that updates a value many times (i.e. you
 * can swap for a dummy value like unit).
 */
wikrt_err wikrt_db_swap(wikrt_txn*, char const* key, wikrt_val* val);

/** @brief Mark a transaction for durability. 
 *
 * A 'durable' transaction will force the underlying database to
 * push content to disk. Otherwise, transactions only have the ACI
 * properties but will tend to run a bit more efficiently. 
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

// just testing
void wikrt_hello(char const * s);


#define WIKILON_RUNTIME_H
#endif
