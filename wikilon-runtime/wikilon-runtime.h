/** @file wikilon-runtime.h
 *	@brief Wikilon Runtime
 *
 *	@mainpage	Wikilon Runtime
 *
 *	@section intro_sec Introduction
 *
 *  Wikilon runtime consists of an interpreter for Awelon Bytecode (ABC)
 *  together with a persistence layer. Wikilon is part of Awelon project,
 *  which explores a new model of software development that relies on the
 *  streamable, serializable, and purely functional nature of ABC.
 *
 *  Wikilon relies on 'stowage' - a concept for pure computations over
 *  larger than memory data. Database states are represented as pure values
 *  with stowage. Integration with the persistence layer is essential for
 *  efficient stowage.
 *
 *  Wikilon Runtime should eventually support both accelerated operations
 *  for common subprograms, parallelism, and high quality just-in-time
 *  compilation. But the immediate focus is efficient interpretation.
 *
 *  @section usage_sec Usage
 *
 *  Create an environment, then a context within that environment. Load
 *  data into this context. Perform ad-hoc computations. Check for errors.
 *  Extract or store results.
 *
 *  Rather than 'fail safe' behavior, Wikilon runtime only ensures that
 *  runtime type errors, quota errors, and transaction conflict errors
 *  are confined to a context.
 *
 *  @section license_sec License & Copyright
 *
 *  (c) 2015-2016 David Barbour
 *  LICENSE: BSD 3-clause <https://opensource.org/licenses/BSD-3-Clause>
 *
 */
#ifndef WIKILON_RUNTIME_H

#include <stdint.h>
#include <stdbool.h>
#include <stddef.h>

/** @brief Opaque structure for overall Wikilon environment.
 * 
 * An environment specifies the filesystem location for large values and
 * persistence (currently via LMDB). An environment might also provide any
 * worker threads.
 */
typedef struct wikrt_env wikrt_env;

/** @brief Opaque structure representing a context for computation.
 *
 * A wikrt_cx holds a value - a 'program environment' potentially with
 * multiple implicit stacks - and may be manipulated by a single thread
 * to perform a computation. While these manipulations may be performed
 * by C code, the primary approach to manipulation is to load blocks of
 * bytecode into memory and use them to perform computations.
 */
typedef struct wikrt_cx wikrt_cx;

/** @brief Support a simple consistency check for dynamic library.
 *
 * Compare WIKRT_API_VER to wikrt_api_ver(). If they aren't the same,
 * then your app was compiled against a different interface than the
 * dynamic lib implements. This is just a simple sanity check.
 */
uint32_t wikrt_api_ver();
#define WIKRT_API_VER 20160421

/** Wikilon runtime error codes. */
typedef enum wikrt_ecode
{ WIKRT_OK = 0      // no error
, WIKRT_INVAL       // invalid use of API, client error
, WIKRT_IMPL        // incomplete implementation
, WIKRT_CXFULL      // computation failed due to space limits
, WIKRT_ETYPE       // runtime type error, div-by-zero
} wikrt_ecode;

/** @brief Check a context for runtime errors. 
 *
 * A Wikilon runtime context is either in an OK state or in an error
 * state. Once an error state is reached, all further computations are
 * considered invalid. Transactions will fail. Extraction of data will 
 * fail. Etc.. `wikrt_error(cx)` will return coarse information about
 * the earliest runtime error.
 *
 * Runtime errors outside the wikrt_ecode range are possible via 
 * wikrt_seterr.
 */
wikrt_ecode wikrt_error(wikrt_cx*);

/** @brief Mark a computation context as erroneous.
 * 
 * This will set a WIKRT_OK context into the specified error state.
 * There is no difference in behavior between error states except
 * for the wikrt_error() call. 
 */
void wikrt_set_error(wikrt_cx*, wikrt_ecode);

/** @brief Open or Create a Wikilon environment with backing database.
 *
 * The given filesystem directory is where we'll maintain persistent 
 * key-value database and hibernate our very large tree-structured
 * values. At the moment, this is an LMDB database.
 *
 * This operation returns NULL on failure. The most likely cause for
 * failure is permissions issues in creating the given directory. But
 * there may also be problems if the maxMB size is too large for mmap,
 * if we're out of memory, or if the context is already in use.
 */
wikrt_env* wikrt_env_create(char const* dirPath, uint32_t dbMaxMB);

/** @brief Destroy the environment.
 *
 * All contexts must be explicitly destroyed before the environment
 * is destroyed. 
 */
void wikrt_env_destroy(wikrt_env*);

/** @brief Ensure persistence of key-value transactions. 
 *  
 * If you don't explicitly mark transactions durable, consider calling
 * sync every five seconds or so to limit potential data loss. This 
 * function returns after all prior transactions are flushed to disk.
 */
void wikrt_env_sync(wikrt_env*);

/** @brief Create a context for computations.
 * 
 * A fresh context has the unit value. This value is manipulated by
 * functions that introduce data or perform computations. 
 *
 * Contexts are currently limited to small 'active' memory specified
 * upon construction. Use of stowed values can enable access to more
 * memory than this. At the moment we're limited to about 4GB active
 * memory due to 32-bit internal references. (This may change later.)
 */
wikrt_cx* wikrt_cx_create(wikrt_env*, uint32_t cxSizeMB);

#define WIKRT_CX_MIN_SIZE 4
#define WIKRT_CX_MAX_SIZE 4092

/** @brief Reset context to fresh condition, as if newly created. */
void wikrt_cx_reset(wikrt_cx*);

/** @brief Destroy a context, recover memory. */
void wikrt_cx_destroy(wikrt_cx*);


/** @brief A context knows its parent environment. */
wikrt_env* wikrt_cx_env(wikrt_cx*);

/** @brief Complete enumeration of Wikilon Runtime opcodes.
 * 
 * Wikilon uses Awelon Bytecode (ABC) as its serialization models for 
 * behavior and data. ABC consists of 42 primitive operators, plus text
 * literals, blocks, and tokens.
 *
 * Opcodes are valid ASCII and UTF-8 codepoints. In general, ABC has an
 * expansion as UTF-8. 
 */
typedef enum wikrt_abc
{ ABC_PROD_ASSOCL = 108  // l :: (a * (b * c)) → ((a * b) * c)
, ABC_PROD_ASSOCR = 114  // r :: ((a * b) * c) → (a * (b * c))
, ABC_PROD_W_SWAP = 119  // w :: (a * (b * c)) → (b * (a * c))
, ABC_PROD_Z_SWAP = 122  // z :: (a * (b * (c * d))) → (a * (c * (b * d)))
, ABC_PROD_INTRO1 = 118  // v :: a → (a * 1)      intro unit
, ABC_PROD_ELIM1  = 99   // c :: (a * 1) → a      elim unit
, ABC_SUM_ASSOCL  = 76   // L :: ((a + (b + c)) * e) → (((a + b) + c) * e)
, ABC_SUM_ASSOCR  = 82   // R :: (((a + b) + c) * e) → ((a + (b + c)) * e)
, ABC_SUM_W_SWAP  = 87   // W :: ((a + (b + c)) * e) → ((b + (a + c)) * e)
, ABC_SUM_Z_SWAP  = 90   // Z :: ((a + (b + (c + d))) * e) → ((a + (c + (b + d))) * e)
, ABC_SUM_INTRO0  = 86   // V :: (a * e) → ((a + 0) * e) 
, ABC_SUM_ELIM0   = 67   // C :: ((a + 0) * e) → (a * e) 
, ABC_COPY        = 94   // ^ :: (a * e) → (a * (a * e)) (for copyable a)
, ABC_DROP        = 37   // % :: (a * e) → e (for droppable a)
, ABC_SP          = 32   // (SP) :: a → a  (space for formatting)
, ABC_LF          = 10   // (LF) :: a → a  (newline for formatting)
, ABC_APPLY       = 36   // $ :: ([a→b] * (a * e)) → (b * e)
, ABC_COMPOSE     = 109  // m :: ([a→b] * ([b→c] * e)) → ([a→c] * e)
, ABC_QUOTE       = 39   // ' :: (a * e) → ([∀s.s→(a*s)] * e)
, ABC_REL         = 107  // k :: ([a→b] * e) → ([a→b]k * e) (mark block non-droppable)
, ABC_AFF         = 102  // f :: ([a→b] * e) → ([a→b]f * e) (mark block non-copyable) 
, ABC_NUM         = 35   // # :: e → (I(0) * e)  (pseudo-literal integers, e.g. `#42`)
, ABC_D1          = 49   // 1 :: (I(a) * e) → (I(10a+1) * e)
, ABC_D2          = 50   // 2 :: (I(a) * e) → (I(10a+2) * e)
, ABC_D3          = 51   // 3 :: (I(a) * e) → (I(10a+3) * e)
, ABC_D4          = 52   // 4 :: (I(a) * e) → (I(10a+4) * e)
, ABC_D5          = 53   // 5 :: (I(a) * e) → (I(10a+5) * e)
, ABC_D6          = 54   // 6 :: (I(a) * e) → (I(10a+6) * e)
, ABC_D7          = 55   // 7 :: (I(a) * e) → (I(10a+7) * e)
, ABC_D8          = 56   // 8 :: (I(a) * e) → (I(10a+8) * e)
, ABC_D9          = 57   // 9 :: (I(a) * e) → (I(10a+9) * e)
, ABC_D0          = 48   // 0 :: (I(a) * e) → (I(10a+0) * e)
, ABC_ADD         = 43   // + :: (I(a) * (I(b) * e)) → (I(a+b) * e)
, ABC_MUL         = 42   // * :: (I(a) * (I(b) * e)) → (I(a*b) * e)
, ABC_NEG         = 45   // - :: (I(a) * e) → (I(-a) * e)
, ABC_DIV         = 81   // Q :: (I(divisor) * (I(dividend) * e)) → (I(remainder) * (I(quotient) * e))
, ABC_GT          = 71   // G :: (I(A) * (I(B) * e)) → (((I(B)*I(A)) + (I(A)*I(B))) * e); (in right if B > A)
, ABC_CONDAP      = 63   // ? :: ([a→c] * ((a+b)*e)) → ((c+b)*e) (for droppable block)
, ABC_DISTRIB     = 68   // D :: (a * ((b+c) * e)) → (((a*b) + (a*c)) * e)
, ABC_FACTOR      = 70   // F :: (((a*b)+(c*d)) * e) → ((a+c)*((b+d)*e))
, ABC_MERGE       = 77   // M :: ((a+a)*e) → (a*e)
, ABC_ASSERT      = 75   // K :: ((a+b)*e) → (b*e); assert in right
} wikrt_abc;

// Thoughts: Wikilon runtime will use accelerators instead of ABCD.
// I'll want to list the accelerators that Wikilon supports.

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
 * This function assumes the input is valid utf-8, NUL-terminated as
 * a C string. It returns whether the token text is valid by the other
 * constraints. When reading a token, use a buffer of WIKRT_TOK_BUFFSZ
 * (or larger) to safely receive the token text.
 */
bool wikrt_valid_token(char const* s);
#define WIKRT_TOK_BUFFSZ 64

/** Shallow reflection over values. */
typedef enum wikrt_type
{ WIKRT_TYPE_UNDEF      // an undefined type
, WIKRT_TYPE_INT        // any integer
, WIKRT_TYPE_PROD       // a pair of values
, WIKRT_TYPE_UNIT       // the unit value
, WIKRT_TYPE_SUM        // value in left or right
, WIKRT_TYPE_BLOCK      // block of code, a function
, WIKRT_TYPE_SEAL       // discretionary sealed value
, WIKRT_TYPE_STOW       // stowed value reference
, WIKRT_TYPE_PEND       // pending lazy or parallel value
} wikrt_val_type;

/** @brief Reflection on values.
 *
 * Given a context with type (a*e), this reflects the type of `a`,
 * returning WIKRT_TYPE_UND if `a` does not exist or if the context
 * is in an error state. This operation is fail safe, it will not
 * cause an error in the context.
 *
 * NOTE: Clients of Wikilon runtime should only need reflection for
 * special cases, such as rendering ad-hoc values. Even in these cases,
 * it would be preferable for the code itself to provide rendering
 * suggestions. Please use this sparingly.
 */
wikrt_val_type wikrt_type(wikrt_cx*);

  /////////////////////////
 // BASIC DATA PLUMBING //
/////////////////////////

/** @brief Move a value from one context to another.
 * 
 * For the left context, this has type `(a*b)→b`. For the right context,
 * this has type `c→(a*c)`. The `a` value is moved from the left context
 * to the right context. Left and right contexts must not be the same.
 */
void wikrt_move(wikrt_cx*, wikrt_cx*);

/** @brief (a*e) → (a*(a*e)). ABC op `^`. 
 *
 * This has two variations. The `wikrt_copy` variant is the same as ABC 
 * op `^`, and will fail for values that are not copyable. Non-copyable
 * values include both affine blocks and pending values from laziness or
 * parallelism. The `wikrt_copyf` variant will copy any value.
 *
 * Copy can easily fail if there is not enough space in the context.
 */
void wikrt_copy(wikrt_cx*);
void wikrt_copyf(wikrt_cx*);

/** @brief Copy and Move as a combined operation.
 *
 * When we copy a value then immediately move it, we can potentially avoid
 * a large intermediate copy by combining the two operations. 
 */
void wikrt_copy_move(wikrt_cx*, wikrt_cx*);
void wikrt_copyf_move(wikrt_cx*, wikrt_cx*);

/** @brief (a*e) → e. ABC op `%`.
 *
 * This has two variations. The `wikrt_drop` variant is the same as ABC op
 * `%` and will fail if we drop values that are non-droppable - e.g. relevant
 * blocks or pending values from laziness or parallelism. The `wikrt_dropk`
 * variant will destroy normally non-droppable values.
 */
void wikrt_drop(wikrt_cx*);
void wikrt_dropk(wikrt_cx*);

/** (a*(b*c))→(b*(a*c)). ABC op `w`. */
void wikrt_wswap(wikrt_cx*);

/** (a*(b*(c*d)))→(a*(c*(b*d))). ABC op `z`. */
void wikrt_zswap(wikrt_cx*);

/** (a*(b*c))→((a*b)*c). ABC op `l`. */
void wikrt_assocl(wikrt_cx*);

/** ((a*b)*c)→(a*(b*c)). ABC op `r`. */
void wikrt_assocr(wikrt_cx*);


/** ((a+(b+c))*e)→((b+(a+c))*e). ABC op `W`. */
void wikrt_sum_wswap(wikrt_cx*);

/** ((a+(b+(c+d)))*e)→((a+(c+(b+d)))*e). ABC op `Z`. */
void wikrt_sum_zswap(wikrt_cx*);

/** ((a+(b+c))*e)→(((a+b)+c)*e). ABC op `L`. */
void wikrt_sum_assocl(wikrt_cx*);

/** (((a+b)+c)*e)→((a+(b+c))*e). ABC op `R`. */
void wikrt_sum_assocr(wikrt_cx*);

/** (a*((b+c)*e))→(((a*b)+(a*c))*e). ABC op `D`. */
void wikrt_sum_distrib(wikrt_cx*);

/** (((a*b)+(c*d))*e)→((a+c)*((b+d)*e)). ABC op `F`. */
void wikrt_sum_factor(wikrt_cx*);

// ACCELERATED DATA PLUMBING

/** (a*b)→(b*a). ABC ops `vrwlc`. Non-allocating. Fail-safe. */
void wikrt_swap(wikrt_cx*);

/** ((a+b)*e)→((b+a)*e). ABC ops `VRWLC`. */
void wikrt_sum_swap(wikrt_cx*);

/* TODO: introduce accelerators as they're developed. */

  ///////////////////////////
 // DATA INPUT AND OUTPUT //
///////////////////////////

/** @brief Allocate and eliminate unit values.
 *
 *   wikrt_intro_unit:      (a)→(1*a)       vvrwlc
 *   wikrt_elim_unit:       (1*a)→(a)       vrwlcc
 *   wikrt_intro_unit_r:    (a)→(a*1)       v
 *   wikrt_elim_unit_r:     (a*1)→(a)       c
 */
void wikrt_intro_unit(wikrt_cx*);
void wikrt_elim_unit(wikrt_cx*);
void wikrt_intro_unit_r(wikrt_cx*);
void wikrt_elim_unit_r(wikrt_cx*);

/** @brief Allocate and split 'sum' values one step at a time.
 *
 * Allocation operates on an `(a*e)` value and either returns the
 * `((a+0)*e)` or `((0+a)*e)` depending on the `inRight` parameter.
 * Split does the opposite, returning the `inRight` condition.
 * 
 * If inRight is false, this corresponds to op `V`. Otherwise to
 * `VVRWLC`. The unwrap variant provides easy access to a sum.
 *
 * In case of unwrap error, the given sum tag is not modified. You
 * can test that the operation succeeds with `!wikrt_error(cx)`. 
 */
typedef enum wikrt_sum_tag { WIKRT_INL = 0, WIKRT_INR = 1 } wikrt_sum_tag;
void wikrt_wrap_sum(wikrt_cx*, wikrt_sum_tag inRight);
void wikrt_unwrap_sum(wikrt_cx*, wikrt_sum_tag* inRight);

// TODO: consider matching deeper structure.

/** @brief Allocation of smaller integers. (e)→(Int*e). */
void wikrt_intro_i32(wikrt_cx*, int32_t);
void wikrt_intro_i64(wikrt_cx*, int64_t);

/** @brief Non-destructively read small integers. (Int*e)→(Int*e).
 *
 * These functions return true on success. On error, they return false
 * and output zero, min, or max (using min or max only for underflow 
 * and overflow respectively).  
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
 * the hood. A list has type `μL.((e*L) + t)`. In this case, the terminal
 * type `t` is unit. 
 */
void wikrt_intro_binary(wikrt_cx*, uint8_t const*, size_t);

/** @brief Incrementally read binary data. Destructive.
 *
 * Our binary is a list `μL.((e*L)+t)` of bytes (0..255). Incremental 
 * read moves data to the client's buffer destructively, so sequential
 * reads gradually consume an entire binary.
 *
 *    (binary*e)→(smaller binary*e)
 *
 * In case of error, we'll stop reading and set an error state in
 * the context. I.e. you should statically know that your argument
 * is a valid binary.
 */
void wikrt_read_binary(wikrt_cx*, uint8_t*, size_t*);

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
 * Similar to wikrt_read_binary, but will read text into a utf-8 buffer.
 * The codepoints read will meet the same constraints as alloc_text, i.e. 
 * no control chars except LF, no surrogates, no incomplete codepoints.
 *
 * In case of error, we'll stop reading and the context will enter an
 * error state. So this shouldn't be used unless you know your argument
 * should be a valid text.
 * 
 * The 'chars' parameter enables clients to limit and quickly determine 
 * the number of codepoints read, but it is optional (NULL permitted).
 */
void wikrt_read_text(wikrt_cx*, char*, size_t* bytes, size_t* chars);

/** @brief Managing bytecode.
 *
 * These are two essential functions for Wikilon runtime. Introducing text
 * containing bytecode, then parse that text into a block of code. Evaluate.
 * Conversely, quote values and convert to text as basis for serialization. 
 *
 * Awelon Bytecode (ABC) is designed to be a printable subset of UTF-8. The
 * primitive operations are within the ASCII subset, but texts, tokens, and 
 * ABCD extensions will leverage the larger UTF-8 space. See wikrt_opcode 
 * for the available opcodes (which match unicode codepoints). Anyhow, we 
 * convert blocks to and from valid texts (cf. wikrt_intro_text). 
 *
 * On success: (text*e) → (block*e)    (or inverse)
 * On failure: drops alleged text or block argument
 *
 * There is no simplification or optimization. The text is not validated beyond
 * ensuring it parses as bytecode. (E.g. type safety is not validated.) A round
 * trip conversion from text to block to text should return the original text.
 * Further processing of a block will require separate function calls.
 *
 * NOTE: The `[]` for the toplevel block is implicit, both on input and output.
 *
 * NOTE: A block may reference stowed values (e.g. via quote and compose). Such
 * stowed value references will be preserved as resource tokens. See wikrt_peek_sv
 * for more information.
 */
void wikrt_text_to_block(wikrt_cx*);
void wikrt_block_to_text(wikrt_cx*);

/** @brief Wrap a value with a sealer token. (a * e)→((sealed a) * e).
 *
 * The sealer token is provided as a simple C string. Discretionary 
 * sealers such as ":map" are understood by Wikilon runtime, as they
 * may be unsealed (by a {.map} token). Short discretionary sealers
 * (no more than 4 bytes, including the ':') have a compact encoding.
 *
 * Note that a valid token string cannot be empty. It must be between
 * one and sixty-three bytes in the utf-8 representation.
 */
void wikrt_wrap_seal(wikrt_cx*, char const*); 

/** @brief Access a sealed value. ((sealed a) * e) → (a * e).
 *
 * This returns the sealer token into the provided buffer, which must
 * be at least WIKRT_TOK_BUFFSZ in length to eliminate risk of buffer
 * overflow. This token is NUL-terminated.
 */
void wikrt_unwrap_seal(wikrt_cx*, char*);

/** @brief Mark a value for stowage. (a * e) → ((stowed a) * e).
 *
 * This corresponds to annotation {&stow}. Stowage moves a value to a backing
 * database, leaving a much smaller 'key' value in its place. The value may
 * subsequently be accessed via {&load}. This fulfills the roles of virtual
 * memory and filesystems, albeit in a manner friendly to purely functional
 * computation.
 *
 * For performance reasons, stowage is lazy. This allows a computation that
 * loads, updates, and stows a value in a tight loop to avoid unnecessary
 * traffic with the backing database. Stowage will happens heuristically. 
 * However, stowage can be forced by subsequent use of wikrt_peek_sv.
 *
 * In Wikilon runtime, stowage is implicitly coupled with structure sharing.
 * That is, values with identical representation will have the same resource 
 * identifier. However, this isn't strongly normalizing - a value may have 
 * more than one representation, and hence more than one ID. Also, after GC,
 * a value may be bound to a fresh resource ID.
 */
void wikrt_stow(wikrt_cx*);

/** @brief Load a stowed value. ((stowed a) * e) → (a * e).
 *
 * This corresponds to annotation {&load}. We'll copy the stowed value
 * from our backing database into active memory. If the value was not 
 * yet stowed due to laziness, we'll simply unwrap the stowage marker.
 * Loading a value that was not at least marked for stowage is an error. 
 *
 * Note that we can hold references to values from other runtimes, but
 * we cannot load them, at least not at the moment. Attempting to do so
 * will result in an error.
 */
void wikrt_load(wikrt_cx*);

/** @brief Inject a stowed value by resource ID. e → ((stowed a) * e)
 *
 * A typical resource token might look like {'/environmentId/resourceId},
 * and includes authentication (e.g. via HMAC). Substructural type info
 * is also included, e.g. {'kf/environmentId/resourceId} would indicate
 * a relevant (k) and affine (f) value resource ('), hence restricting 
 * use of ABC copy ^ and drop % ops.
 *
 * With wikrt_intro_sv, you'll use the token text, i.e. dropping the {}
 * curly braces but including the initial `'` character. The resource ID
 * must be a valid token (cf. wikrt_valid_token). This token is generally
 * obtained via `wikrt_peek_sv` or `wikrt_block_to_text`.
 *
 * If a resource token is not parsable, we'll treat that as an error. If 
 * it's non-local or has since been GC'd, we'll treat that instead as a
 * load-time error.
 */
void wikrt_intro_sv(wikrt_cx*, char const* resourceId);

/** @brief Access resource ID for a stowed value.
 *
 * Given context of type ∀a,e.((stowed a) * e), returns the resource
 * ID of the stowed value. Forces stowage if still lazily pending.
 *
 * The output buffer should have size at least WIKRT_TOK_BUFFSZ, and
 * is returned as a NUL-terminated C string. Due to authentication,
 * resource identifiers will typically use most of this space. If the
 * argument is not a stowed value, we'll return an empty string.
 *
 * Wikilon runtime uses cryptographic HMAC authentication to prevent 
 * forgery or guessing of resource IDs. This simplifies security, i.e.
 * supporting capability-based security models.
 * 
 * Another way to access resource IDs is use of `wikrt_block_to_text`.
 * In this case, resource IDs may appear within tokens in the text.
 *
 * IMPORTANT NOTE: Stowed value resources are garbage collected, but
 * resource IDs held outside our environment are not considered by GC
 * algorithms. To guard against GC, hold onto a context containing a
 * stowed value (e.g. for a client-server session) or leverage the
 * transactional persistence subsystem to hold the stowed value.
 */
void wikrt_peek_sv(wikrt_cx*, char* buff);

// TODO: support for fast-diffing of values that use stowage?
//
//  Or should I stick to semantic-layer diffs, e.g. keeping
//  time information, hashes, or update counters in the tree
//  structure.


// NOTE: I'll  want some support for accessing the resource ID
// for a stowed value. This would force stowage, rather than allow it to
// happen lazily. Conversely, the ability to access a value by resource ID
// could be very convenient. 

  ////////////////
 // EVALUATION //
////////////////
/** @brief Construct an evaluation. ((a→b)*(a*e)) → ((pending b) * e). 
 *
 * This prepares a lazy evaluation of `b`. To complete the evaluation,
 * repeat wikrt_step_eval on the pending value until complete.
 */
void wikrt_apply(wikrt_cx*);

/** @brief Step an evaluation.  ((pending a) * e) → (a * e).
 *
 * If called with a pending value, we process it for a given effort
 * quota - a heuristic, imprecise quota to ensure client has control
 * of CPU costs and wall clock time. 
 *
 * After each step, we'll return either 'true' indicating that another
 * step is needed, or 'false' indicating evaluation has finished. If
 * called for a non-pending value, or after an error has occurred, we
 * simply return 'false'. It may be useful to check wikrt_error after
 * getting a false result, as evaluation may have failed due to type
 * errors or other resource limits.
 */ 
bool wikrt_step_eval(wikrt_cx*, uint32_t effort);

/** @brief Quote a value. (a * e) → ((∀e'. e'→(a*e'))*e).
 *
 * Almost any value may be quoted, including pending values. However,
 * in some cases we might not have clear substructural properties for
 * the resulting block. Wikilon runtime favors performance over precise
 * dynamic tracking of substructural attributes.
 */
void wikrt_quote(wikrt_cx*);

/** @brief Mark a block affine (non-copyable). (block*e)→(block*e). */
void wikrt_block_aff(wikrt_cx*);

/** @brief Mark a block relevant (non-droppable). (block*e)→(block*e). */
void wikrt_block_rel(wikrt_cx*);

/** @brief Mark a block for parallel evaluation. (block*e)→(block*e). 
 *
 * Note: this doesn't affect a top-level evaluation, only applications
 * internal to a wikrt_step_eval. Wikilon runtime guarantees there is
 * no parallel evaluation upon returning from wikrt_step_eval.
 */
void wikrt_block_par(wikrt_cx*);

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

  /////////////////////////////////////
 // DATABASE AND PERSISTENCE ENGINE //
/////////////////////////////////////

/** @brief Validate database key.
 *
 * Modulo size, transaction keys must first be valid texts (as per
 * wikrt_intro_text), and must further obey a simple size limit of
 * no more than WIKRT_VALID_KEY_MAXLEN bytes. 
 */
bool wikrt_valid_key(char const*);
#define WIKRT_VALID_KEY_MAXLEN 255

/** @brief Begin a transaction with the current context.
 *
 * This gives the context opportunity to prepare any data structures
 * to track a transaction. Currently, a context may have only one
 * transaction active (i.e. no hierarchical transactions), i.e. you
 * must abort or commit before creating another. Read and write fail
 * if performed without a transaction.
 *
 * A transaction is not reified as a value within the context, rather
 * it is an implicit object attached to the context. It is recommended,
 * but not enforced, that effects and communications (such as wikrt_move)
 * only be performed between transactions.
 */
void wikrt_txn_create(wikrt_cx*);

/** @brief Read a value from the key-value database. (e)→(v*e). 
 *
 * Wikilon runtime provides an implicit, stowage friendly, key-value
 * database. Values held by such a database may be rather arbitrary.
 *
 * A key, if not previously written, has a default value of unit in 
 * the right (a value conventionally used for empty lists, absence
 * of a value, completion, and truth).
 */
void wikrt_txn_read(wikrt_cx*, char const* key);

/** @brief Write a value into the implicit key-value database. (v*e)→(e).
 *
 * This writes a value from the context back into the database at a named
 * location. Writing the default value - unit in right - will effectively
 * delete a key from the database.
 */
void wikrt_txn_write(wikrt_cx*, char const* key);

/** @brief Abort current transaction. */
void wikrt_txn_abort(wikrt_cx*);

/** @brief Attempt to commit active transaction.
 *
 * This operation will either return 'true' if the transaction commits
 * or 'false' if it fails, in which case it is aborted. Transactions
 * may fail due to optimistic concurrency conflicts. 
 *
 * Wikilon runtime favors optimistic concurrency as the default. If it
 * is important for throughput to constrain updates on some subset of 
 * keys, consider use of client-side structures such as a queue.
 */
bool wikrt_txn_commit(wikrt_cx*);

/** @brief Mark a transaction for durability. 
 * 
 * Transactions are 'volatile' by default - they may be lost if you
 * lose power shortly after wikrt_txn_commit. The benefit is reduced
 * latency and potential to batch transaction writes to disk. 
 *
 * If durability is important for a particular transaction, simply mark
 * it durable. This ensures that the transaction (and its dependencies)
 * are flushed to disk before we return from wikrt_txn_commit.
 *
 * Otherwise, consider periodically invoking wikrt_env_sync().
 */
void wikrt_txn_durable(wikrt_cx*);

// Todo: consider specialized 'variable' alternatives for 
//  stream processing, queues, logs, or similar?

#define WIKILON_RUNTIME_H
#endif
