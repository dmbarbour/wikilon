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
 *  Create an environment. Create a context within that environment. Load
 *  data and ABC programs into this context. Perform ad-hoc computations.
 *  Check for errors. Extract or store results. Data may be loaded from or
 *  stored into the environment's key-value database.
 *
 *  Wikilon runtime ensures most failures are confined to their context. 
 *  However, there are obvious limits to this, e.g. if the database
 *  is filled, that would hinder concurrent work with the database.
 *
 *  @section license_sec License & Copyright
 *
 *  (c) 2015-2016 David Barbour
 *  LICENSE: BSD 3-clause <https://opensource.org/licenses/BSD-3-Clause>
 *
 */
#pragma once
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
#define WIKRT_API_VER 20160803

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
 * key-value database and stow large tree-structured values. At the
 * moment, this is an LMDB database. Parents in the dirPath will be
 * created as necessary.
 *
 * This operation returns NULL on failure. Failure is most likely due
 * to permissions in creating the given directory or because the DB
 * maximum size is too large to mmap.
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

// TODO: backup or compaction functions 

/** @brief Create a context for computations.
 * 
 * A fresh context has the unit value. This value is manipulated by
 * functions that introduce data or perform computations. 
 *
 * Will return NULL if given a NULL environment. 
 */
wikrt_cx* wikrt_cx_create(wikrt_env*, uint32_t cxSizeMB);

/** @brief Release any cached resources associated with a context. 
 *
 * If you're not going to use a context for a while, signal with 
 * wikrt_cx_relax to release control of unnecessary resources. This
 * might cause a garbage collection to release memory pages, for 
 * example.
 */
void wikrt_cx_relax(wikrt_cx*);

/** @brief Reset context to fresh condition, as if newly created. */
void wikrt_cx_reset(wikrt_cx*);

/** @brief Destroy a context, recover memory. */
void wikrt_cx_destroy(wikrt_cx*);

// Maybe add some context performance tuning opportunities.

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

// NOTE: I'll eventually want to export recognized accelerators and annotations.
// I'm not sure how to best go about this, though. Maybe as a simple AO dictionary
// string, to provide a compact representation.

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
, WIKRT_TYPE_TRASH      // placeholder for discarded value
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
 * Note: Some values are logically non-copyable. Among these are affine
 * blocks and pending computations. Copy is also likely to fail due to
 * lack of space, if the context is close to full.
 */
void wikrt_copy(wikrt_cx*);

/** @brief Copy and Move as a combined operation.
 *
 * If we wikrt_copy then wikrt_move, we effectively pay for two copies.
 * By combining the two, we can avoid the intermediate copy. 
 */
void wikrt_copy_move(wikrt_cx*, wikrt_cx*);

/** @brief (a*e) → e. ABC op `%`.
 *
 * You may drop a value from the context. The value must be droppable, i.e.
 * neither linear nor relevant. If you're dealing with values that might not
 * be droppable, but you want to drop them anyway, use wikrt_trash.
 */
void wikrt_drop(wikrt_cx*);

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
void wikrt_accel_swap(wikrt_cx*);

/** ((a+b)*e)→((b+a)*e). ABC ops `VRWLC`. */
void wikrt_accel_sum_swap(wikrt_cx*);

/** (a * ((b * c) * d)) → (a * (b * (c * d))). ABC ops `wrzw`. */
void wikrt_accel_wrzw(wikrt_cx* cx);

/** (a * (b * (c * d))) → (a * ((b * c) * d)). ABC ops `wzlw`. */
void wikrt_accel_wzlw(wikrt_cx* cx);

/* I'll introduce accelerators as they're developed. */

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

/** @brief Introduce and extract 'sum' values.
 *
 * Wrap:    (a * e) → ((a+0) * e)  (if WIKRT_INL)
 *                  → ((0+a) * e)  (if WIKRT_INR)
 * Unwrap:  ((a+_) * e) → (a * e)  with WIKRT_INL
 *          ((_+a) * e) → (a * e)  with WIKRT_INR
 *
 * Note: Wikilon runtime optimizes for shallow sums on pairs or the
 * unit value. Representation for lists, booleans, and simple trees
 * of ((data*(left*right))+1) is reasonably efficient.
 */

typedef enum wikrt_sum_tag { WIKRT_INL = 0, WIKRT_INR = 1 } wikrt_sum_tag;
void wikrt_wrap_sum(wikrt_cx*, wikrt_sum_tag inRight);
void wikrt_unwrap_sum(wikrt_cx*, wikrt_sum_tag* inRight);

// TODO: consider matching deeper structure.

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

/** @brief Mark a value as binary. The {&binary} annotation.
 *
 *    (binary * e) → (binary * e)
 *
 * This tells a runtime that a given value should be a binary, and to
 * favor a compact byte string representation (i.e. a list containing
 * large binary fragments, instead of a cell per byte; memory savings 
 * can approach 16x.)
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
 * This tells a runtime that a given value should be a text, to favor
 * a compact utf8 byte string representation, and to serialize the
 * value as embedded text (e.g. for trace or quote + block to text).
 * If a value is already represented as text, this has a very low cost.
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
 * The sealer token is provided as a simple C string. Discretionary 
 * sealers such as ":map" are understood by Wikilon runtime, as they
 * may be unsealed (by a {.map} token). Short sealer strings have a 
 * more compact encoding (effectively interned).
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

/** @brief (a * e) → ((trashed a) * e). Annotation {&trash}.
 *
 * When done using a value, the normal option is to drop it. However, 
 * dropping a value is illegal for relevant or linear values. Use of 
 * {&trash} tells a runtime instead that a value will not be observed.
 * This enables memory to be recycled, replacing the value with a 
 * lightweight place holder. Any future attempt to observe the value
 * will result in a runtime type error.
 *
 * A trashed linear value serializes as `[]kf{&trash}`, i.e. preserving
 * both substructure and the 'trashed' type. The dynamic type for trashed
 * values is 'WIKRT_TYPE_TRASH'. 
 */
void wikrt_trash(wikrt_cx*);

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


  //////////////////////
 // MEMORY EXTENSION //
//////////////////////

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

/** @brief Access resource ID for a stowed value.
 *
 * Given context of type ∀a,e.((stowed a) * e), returns the resource
 * ID of the stowed value. Forces stowage if still lazily pending.
 *
 * The output buffer must have size at least WIKRT_TOK_BUFFSZ, and
 * is returned as a NUL-terminated C string. Due to authentication,
 * e.g. including an HMAC, resource identifiers will frequently use
 * much of this space. The use of authentication ensures capability
 * security for any sensitive data held via stowage.
 * 
 * Note that stowed values are garbage collected by the environment.
 * To prevent GC of a value while holding an external reference, you
 * must hold a context that continues to reference the value, or put
 * a reference to the stowed value in the key-value database.
 *
 * If the argument is not a stowed value, this function returns the
 * empty string but does not add an error to the context. 
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


  //////////////////////
 // RUNTIME FEEDBACK //
//////////////////////
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

/** Overview of a context's memory usage.
 *
 * For large contexts, Wikilon will often use a 'soft' GC threshold
 * to help control memory and cache pressures. I.e. a 200MB context
 * and a 4MB context might behave about the same for computations
 * with minimal memory requirements.
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

  ////////////////
 // EVALUATION //
////////////////
/** @brief Construct a pending value. ((a→b)*(a*e)) → ((pending b) * e).
 *
 * Apply a function (a block) to a value to produce a latent value. The
 * actual evaluation is delayed until wikrt_eval_step. A pending value
 * may be quoted (serializes as `value[continuation]{&lazy}$`), but is 
 * otherwise opaque. The exact state of the value and continuation after
 * an incomplete wikrt_step_eval is non-deterministic.
 *
 * Note: This implies explicit laziness, which is only minimally supported
 * by Wikilon runtime. Internally, a pending value may be explicitly forced
 * by {&join}, essentially a special case asynchronous value.
 */
void wikrt_apply(wikrt_cx*);

/** @brief Step an evaluation.  ((pending a) * e) → (a * e).
 *
 * Each step performs a heuristic, small, finite amount of work. The amount of
 * work is weakly deterministic, in the sense that it's implementation dependent
 * but should be consistent within the implementation (depending also on context
 * parameters, etc.).
 *
 * A step returns `true` if more steps are needed, `false` otherwise. On returning
 * false, we have either computed the value or entered an error state. Check for
 * errors with `wikrt_error`.
 */ 
bool wikrt_step_eval(wikrt_cx*);

// thoughts: I'm still thinking about how to support flexible token
// behaviors. But it isn't a priority for Wikilon runtime's use case.

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

// Laziness and Parallelism? Maybe eventually.

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
