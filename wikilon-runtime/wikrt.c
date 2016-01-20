#include <assert.h>
#include "wikrt.h"

// lockfile to prevent multi-process collisions
bool init_lockfile(int* pfd, char const* dirPath) 
{
    size_t const dplen = strlen(dirPath);
    size_t const fplen = dplen + 12;
    char lockFileName[fplen];
    snprintf(lockFileName, fplen, "%s/lockfile", dirPath);
    int const fd = open(lockFileName, O_CREATE | O_RDONLY);
    if ((-1) == fd) {
        return false;
    }  
    if(0 != flock(fd, LOCK_EX|LOCK_NB)) {
        close(fd);
        return false;
    }
    (*pfd) = fd;
    return true;
}

// prepare LMDB database
bool init_db(wikrt_env* pEnv, char const* dirPath0, uint32_t dbMaxMB) {

    if((NULL == dp) || (0 == dbMaxMB)) {
        pEnv->e_db_enable = false;
        return true;
    }

    // to populate on success:
    int db_lockfile;
    MDB_env* pLMDB;
    MDB_dbi db_memory, db_keyval, db_caddrs, db_refcts, db_refct0;
    stowaddr db_last_alloc;

    // relevant constants
    int const mdbFlags = MDB_NOTLS | MDB_NOLOCK | MDB_NOMEMINIT;
    int const mdbMode  = S_IRUSR | S_IWUSR | S_IRGRP | S_IWGRP;
    int const f_memory = MDB_CREATE | MDB_INTEGERKEY;
    int const f_keyval = MDB_CREATE;
    int const f_caddrs = MDB_CREATE | MDB_INTEGERKEY | MDB_DUPSORT | MDB_DUPFIXED | MDB_INTEGERDUP;
    int const f_refcts = MDB_CREATE | MDB_INTEGERKEY;
    int const f_refct0 = MDB_CREATE | MDB_INTEGERKEY;

    // ensure "" and "." mean the same thing as directory name 
    char const* dirPath = dirPath0[0] ? dirPath0 : ".";
    size_t const dbMaxBytes = size_t(dbMaxMB) * (1024 * 1024);

    if(!init_lockfile(&db_lockfile, dirPath)) 
        goto onError;

    if( (0 != mdb_env_create(&pLMDB)))
        goto onErrCreateDB;

    MDB_txn* pTxn;
    if( (0 != mdb_env_set_mapsize(pLMDB, dbMaxBytes)) ||
        (0 != mdb_env_set_maxdbs(pLMDB, 5)) ||
        (0 != mdb_env_open(pLMDB, dirPath, mdbFlags, mdbMode)) ||
        (0 != mdb_txn_begin(pLMDB, NULL, MDB_NOSYNC, &pTxn)))
        goto onErrInitDB;

    // open our databases
    MDB_cursor* pCursor;
    if( (0 != mdb_dbi_open(pTxn, "@", f_memory, &db_memory)) ||
        (0 != mdb_dbi_open(pTxn, "/", f_keyval, &db_keyval)) ||
        (0 != mdb_dbi_open(pTxn, "#", f_caddrs, &db_caddrs)) ||
        (0 != mdb_dbi_open(pTxn, "^", f_refcts, &db_refcts)) ||
        (0 != mdb_dbi_open(pTxn, "%", f_refct0, &db_refct0)) ||
        (0 != mdb_cursor_open(pTxn, db_memory, &pCursor))      )
        goto onErrTxn;

    // obtain last allocated address
    if(0 != mdb_cursor_get(pCursor, &db_last_alloc, NULL, MDB_LAST))
        db_last_alloc = (1 << 32);
    mdb_cursor_close(pCursor);

    // commit the transaction
    if(0 != mdb_txn_commit(pTxn)) 
        goto onErrInitDB;

    // if we reach this point, we've succeeded.
    pEnv->e_db_enable = true;
    pEnv->e_db_env = pLMDB;
    pEnv->e_db_lockfile = db_lockfile;
    pEnv->e_db_memory = db_memory;
    pEnv->e_db_caddrs = db_caddrs;
    pEnv->e_db_keyval = db_keyval;
    pEnv->e_db_refcts = db_refcts;
    pEnv->e_db_refct0 = db_refct0;

    return true;   

    // we might jump to error handling below
 onErrTxn:
    mdb_txn_abort(pTxn);
 onErrInitDB:
    mdb_env_close(pLMDB);
 onErrCreateDB: 
    close(db_lockfile);
 onError:
    pEnv->e_db_enable = false;
    return false;
}

wikrt_err wikrt_env_create(wikrt_env** ppEnv, char const* dirPath, uint32_t dbMaxMB) {
    wikrt_env* const pEnv = calloc(1, sizeof(wikrt_env));
    if(NULL == pEnv) return WIKRT_NOMEM;

    pEnv->e_cxhead      = NULL;
    pEnv->e_shutdown    = false;
    pEnv->e_mutex       = PTHREAD_MUTEX_INITIALIZER;

    if(!init_db(pEnv, dirPath, dbMaxMB)) {
        free(pEnv);
        return WIKRT_DBERR;
    }

    (*ppEnv) = pEnv;
    return WIKRT_OK;
}

void wikrt_env_destroy(wikrt_env* pEnv) {

    pthread_mutex_lock(&(pEnv->e_mutex));
    // force contexts to halt with WIKRT_NOLINK
    // wait for it? need a signaling mechanism
    // delay full destruction until last context destroyed?

    // todo:
    //   final synchronization
    //   close shared resources
    //   close env
    //   destroy pthread mutex
    if(pEnv->e_db_enable) {
        pEnv->e_db_enable = false;
        mdb_env_close(pEnv->e_db_env);
        close(pEnv->e_db_lockfile);
    }
    pthread_mutex_unlock(&(pEnv->e_mutex));

    // if we're the final reference, free the environment.
    if(NULL == pEnv->e_cxhead) {
        free(pEnv);
    }
}

// trivial implementation 
void wikrt_env_sync(wikrt_env* pEnv) {
    if(pEnv->e_db_enable) {
        int const force_flush = 1;
        mdb_env_sync(pEnv->e_db_env, force_flush);
    }
}

#if 0

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
 * Note: support for incremental processing may be eliminated if it has
 * an overly complicated implementation.
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
// Special Cases
, WIKRT_VTYPE_PENDING     // ongoing evaluation
, WIKRT_VTYPE_STOWED      // value hidden
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
 * to use `wikrt_step_eval` to actually perform the computation.
 *
 * The evaluation may be understood as a 'pending' value. Such values
 * are second class: they cannot be stowed and should not be wrapped 
 * into larger structures. However, use of wikrt_quote will capture a
 * pending value as a block, representing the remaining computation.
 */
wikrt_err wikrt_alloc_eval(wikrt_cx*, wikrt_val*, wikrt_val arg, wikrt_val fn);

/** @brief Step through an evaluation.
 *
 * Each step has a 'quota', a heuristic for computational effort. This
 * quota is based on allocations rather than wall clock time, so it 
 * should be relatively consistent. When computation stalls because our
 * quota reaches zero, we will return WIKRT_QUOTA_STOP. Otherwise, the
 * remaining quota is returned.
 *
 * If we return WIKRT_OK, then computation is complete and our evaluation
 * is reduced to a value. Further evaluation steps have no additional
 * effect.
 *
 * If we return WIKRT_TOKEN_STOP, you may potentially handle the token and
 * continue. If we return WIKRT_STREAM_WAIT, we can potentially addend the
 * stream and continue. For most other errors, we cannot continue.
 */
wikrt_err wikrt_step_eval(wikrt_cx*, wikrt_val* evaluation, uint32_t* quota);

/** @brief Handle a token stop.
 *
 * Upon WIKRT_TOKEN_STOP we may split the evaluation into a triple: the
 * token, an argument to it, and a continuation. The continuation may
 * be used or serialized as a block, but is specialized for use with 
 * wikrt_alloc_eval to construct an ongoing evaluation after our argument
 * has been processed.
 *
 * The token buffer should have size at least WIKRT_TOK_BUFFSZ to avoid
 * any risk of overflow. 
 */
wikrt_err wikrt_token_stop(wikrt_cx*, wikrt_val, char* tok, wikrt_val* arg, wikrt_val* cont);

/** @brief Quote a value into a block. v → [∀e. e → (v * e)]. (') */
wikrt_err wikrt_quote(wikrt_cx*, wikrt_val, wikrt_val*);

/** @brief Compose two blocks. [a → b] → [b → c] → [a → c]. (o) */
wikrt_err wikrt_compose(wikrt_cx*, wikrt_val ab, wikrt_val bc, wikrt_val* ac);

#if 0

/** @brief Recognized block attributes. Use bitwise-or to compose. 
 *
 * Affine and relevant are substructural properties. They serve a role
 * supporting structured behavior without structured syntax. Affine 
 * blocks cannot be copied, and relevant blocks cannot be dropped, at
 * least not by ABC operators `^` and `%`. Linearity is trivially the
 * composition of affine and relevant attributes.
 *
 * Parallel blocks will be evaluated by a worker thread if available.
 * This corresponds to the {&par} annotation. Worker threads run each
 * computation to completion before continuing with another, i.e. this
 * parallelism is not a suitable basis for concurrency.
 * 
 */
typedef enum wikrt_block_attr
{ WIKRT_AFFINE = 1
, WIKRT_RELEVANT = 2
, WIKRT_LINEAR = 3    // eqv. to (WIKRT_AFFINE | WIKRT_RELEVANT)
, WIKRT_PARALLEL = 16
} wikrt_block_attr;

/** @brief Add attributes to a block value. */
wikrt_err wikrt_block_set_attr(wikrt_cx*, wikrt_val, wikrt_block_attr, wikrt_val*);

/** @brief Basic integer math operations: add, mul, negate, divmod (+*-Q) */
wikrt_err wikrt_iadd(wikrt_cx*, wikrt_val, wikrt_val, wikrt_val*);
wikrt_err wikrt_imul(wikrt_cx*, wikrt_val, wikrt_val, wikrt_val*);
wikrt_err wikrt_ineg(wikrt_cx*, wikrt_val, wikrt_val*);
wikrt_err wikrt_idiv(wikrt_cx*, wikrt_val dividend, wikrt_val divisor, 
                     wikrt_val* quotient, wikrt_val* remainder); 

/** @brief Common comparison results, expressed as enumeration. */
typedef enum wikrt_ord { WIKRT_LT = -1, WIKRT_EQ = 0, WIKRT_GT = 1 } wikrt_ord;

/** @brief Integer comparison, non-destructive. */
wikrt_err wikrt_icmp(wikrt_cx*, wikrt_val const, wikrt_ord*, wikrt_val const);


// later I might introduce API-level support for:
//   affine, relevant, parallel
//   arrays, structure assertion annotations
//   list processing accelerators
//   other accelerators? (matrix math, etc.)
#endif

  /////////////////////////////////////
 // DATABASE AND PERSISTENCE ENGINE //
/////////////////////////////////////

/** @brief Transactional Persistence
 *
 * The Wikilon environment has an integrated, persistent key-value
 * database. This serves as a basis for persistence that interacts
 * nicely with large value stowage. A transaction is required even
 * to access and update a single value.
 *
 * A transaction is represented within a context, but is not a meaningful value of any sort. 
 *
 * Note: This database is not implicitly accessible to ABC computations.
 * Access must be modeled explicitly, like any other algebraic effect,
 * if it is to be provided at all.
 */
typedef wikrt_val wikrt_txn;

/** @brief Validate transaction key.
 *
 * Transaction keys must be valid texts of limited size, at most 255
 * bytes in the utf-8 encoding.
 */
bool wikrt_valid_key(char const*);

/** @brief Create a new transaction on our key-value database. */
wikrt_err wikrt_txn_create(wikrt_cx*, wikrt_txn* dest);

/** @brief Access and update a value in the key-value database. 
 * 
 * Consistent with move semantics, we favor exchanging one value for
 * another. This counts as both a read and a write, though a few tricks
 * under the hood can recognize some read-only or write-only patterns.
 * 
 * The default value for any key is WIKRT_UNIT_INR, the conventional
 * empty container value. Updating a key to have this value effectively
 * deletes that key from the database.
 */
wikrt_err wikrt_txn_swap(wikrt_cx*, wikrt_txn, char const* key, wikrt_val* val);

/** @brief Abort and drop the transaction. */
void wikrt_txn_abort(wikrt_cx*, wikrt_txn);

/** @brief Attempt to commit a transaction.
 *
 * Commit may fail for a variety of reasons, the most likely being
 * conflict with a concurrent transaction. 
 */
wikrt_err wikrt_txn_commit(wikrt_txn*);
    // TODO: early detection of conflicts
    //       heuristic priority, etc.

/** @brief Mark a transaction for durability. 
 * 
 * By default, transactions are only weakly durable. However, we can
 * mark a transaction for a strong guarantee of durability. This will
 * transitively force ancestral transactions to also become durable.
 *
 * The cost of durable transactions is latency: we must wait for the
 * transaction data to commit to disk. Also, when transactions are
 * small, we may end up writing more pages than we would if multiple
 * non-durable transactions are batched. 
 */
void wikrt_txn_durable(wikrt_cx*, wikrt_txn);

/** @brief Ensure durability of all prior transactions. 
 *  
 * If you don't explicitly mark transactions durable, consider calling
 * sync every five seconds or so to limit potential data loss. This 
 * function returns after all prior transactions are flushed to disk.
 */
void wikrt_env_sync(wikrt_env*);

#endif

