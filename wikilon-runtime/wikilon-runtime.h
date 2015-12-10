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
 *  Hot Backup: Currently not supported. It may be useful to model
 *  this explicitly.
 *
 *  @section license_sec License & Copyright
 *
 *  (c) 2015 David Barbour
 *  LICENSE: BSD
 *
 */
#ifndef WIKILON_RUNTIME_H

/** @brief Opaque structure for overall Wikilon environment.
 * 
 * The environment contains elements shared by associated contexts.
 * This includes LMDB storage and a pool of worker threads for any
 * par/seq parallelism (there's no point to having more workers 
 * than CPU cores).
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
 * with an option at construction time.
 *
 */
typedef struct wikrt_cx wikrt_cx;

/** Every context knows its environment. */
wikrt_env* wikrt_cx_env(wikrt_cx*);

/** @brief Structure representing a value in memory.
 * 
 * A value is addressed relative to a context. Low bits in the address
 * are frequently used to help interpret the value (e.g. to distinguish
 * pairs, small integers, tagged values). 
 *
 * In general, values may only be held by one thread at a time. If you
 * are holding a value, you probably own it, or at least have suspended
 * the task that holds it. If you continue computations, you'll need to
 * release the reference to the value. (Or ask for a copy if you must
 * hold onto the value.) If you don't need the value any longer, you
 * should explicitly destroy it).
 *
 * While the wikilon environment provides some functions for observing
 * and manipulating values, the intention is for most operations to
 * involve streaming bytecode: convert a value to a stream of bytecode
 * that regenerates the same value, or apply a stream of bytecode to
 * rewrite a value.
 */
typedef struct wikrt_val {
    wikrt_cx*  context;
    size_t     address;
} wikrt_val;

/** @brief Structure representing an active computation.
 *
 * Computations include a current value and a continuation. Holding
 * onto this object is necessary to obtain the result, or to push
 * a computation to completion. It also enables rendering of many
 * intermediate states.
 */
typedef struct wikrt_task { wikrt_val task; } wikrt_task;

/** @brief Structure representing a generic error condition.
 *
 * If there is no error, the zero value is returned. Otherwise, we
 * may return a number that roughly describes the error. We'll favor
 * errors in `<errno.h>` where applicable. E.g.
 *
 * - EXFULL - run out of space in a context
 * - ENODATA - undefined keys in our database
 * - ENAMETOOLONG - key too large
 * - EINVAL - invalid arguments
 * - ETIME - returned on cycle quota 
 *
 */
typedef int wikrt_errno;

/** Obtain textual descriptions of runtime errors. */
char const* wikrt_strerror(wikrt_errno);

/** @brief Create or Open a Wikilon environment.
 *
 * The developer specifies where the Wikilon environment stores data
 * on the filesystem, and how large this is permitted to grow. If the
 * database does not exist, we'll attempt to create a new one, making
 * parent directories as necessary.
 *
 * This may fail due to any number of permissions errors, or if there
 * isn't enough address space, or if the database is already in use, 
 * or if the given path is invalid, etc.. In case of failure, a NULL 
 * environment is returned, along with a non-zero errno.
 */
wikrt_errno wikrt_env_create(wikrt_env**, char const* dirPath, size_t dbMax);

/** @brief Transactional Persistence
 *
 * Each Wikilon environment includes a simple key-value database. 
 * 
 * The keys are simple strings, up to 127 bytes in length. The values
 * are any structured data we might represent in large value stowage.
 * The database serves as roots for stowage, preventing GC of content
 * referenced in the large value store.
 *
 * Wikilon uses this database for persistence. Having state represented 
 * as ABC values is convenient for programmable reflection.
 */
typedef struct wikrt_txn { wikrt_val txn; } wikrt_txn;
#define WIKRT_DB_MAX_KEY_LEN 127

/** @brief Begin a new transaction for key-value persistence.
 *
 * The transaction will be represented in the given context.
 */
wikrt_errno wikrt_txn_begin(wikrt_cx*, wikrt_txn*);

// TODO: potential support for hierarchical transactions (if sufficient demand)

/** @brief Read a value from our key-value persistence layer.
 * 
 * If a read succeeds, you have a copy of the value from the database
 * or a value written to the same key earlier in the transaction.
 *
 * If this causes a transaction to copy an affine value, the action
 * succeeds but we'll get a special WIKRT_COPY_AFFINE error. This
 * enables the caller to decide policy.
 */
wikrt_errno wikrt_db_read(wikrt_txn*, char const* key, wikrt_val* dest);

/** @brief Write value into our key-value persistence layer.
 * 
 * This function will write a value to our persistence layer. The
 * transaction takes ownership of this value, but you can copy it
 * ahead of time or simply read the key. 
 *
 * If this causes a transaction to overwrite a relevant value, the
 * action succeeds but we'll get an WIKRT_DROP_RELEVANT error. This
 * enables the caller to decide policy.
 */
wikrt_errno wikrt_db_write(wikrt_txn*, char const* key, wikrt_val* val);

/** @brief Exchange a value from our key-value persistence layer.
 *
 * Swap a value currently bound to a key with the value provided. This
 * guarantees protection for substructural types, and avoids intermediate
 * copies during a transaction that updates a value many times (i.e. you
 * can swap for a dummy value like unit).
 */
wikrt_errno wikrt_db_swap(wikrt_txn*, char const* key, wikrt_val* val);

/** @brief Obtain a list of all keys with a given prefix.
 *
 * A list of all keys with a given prefix that have a value other
 * than unit are returned. This enables browser-like explortation
 * or destruction. May return EXFULL if there isn't enough space
 * for the list.
 */
wikrt_errno wikrt_db_list(wikrt_txn*, char const* keyPrefix, wikrt_val* dest);

/** @brief Mark a transaction for durability. 
 *
 * A 'durable' transaction will force the underlying database to
 * push content to disk. Otherwise, transactions only have the ACI
 * properties but will tend to run a bit more efficiently. 
 */
void wikrt_txn_mark_durable(wikrt_txn*);

/** @brief Abort a transaction.
 *
 * All values associated with the transaction will be released from
 * context. The transaction will no longer conflict with others.
 */
void wikrt_txn_abort(wikrt_txn*);

/** @brief Commit a transaction.
 *
 * Attempt to commit a transaction. This may fail if there are
 * conflicts with other transactions. 
 */
wikrt_errno wikrt_txn_commit(wikrt_txn*);
    // TODO: early detection of conflicts
    //       heuristic priority, etc.

// just testing
void wikrt_hello(char const * s);


#define WIKILON_RUNTIME_H
#endif
