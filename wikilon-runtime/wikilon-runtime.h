/** @file wikilon-runtime.h
 *	@brief Wikilon Runtime
 *
 *	@mainpage	Wikilon Runtime
 *
 *	@section intro_sec Introduction
 *
 *  Wikilon runtime consists of an interpreter for Awelon Bytecode (ABC),
 *  together with stowage, caching, linking layers via symbolic {tokens} 
 *  flexibly embedded in the bytecode, plus a simple persistence feature.
 *
 *  Wikilon is part of Awelon project, which explores a new model for
 *  software development. ABC is simple, streamable, serializable, purely
 *  functional, and may be implemented by local rewriting - and thus may 
 *  be evaluated and debugged without much context. The application model
 *  involves representing application state directly in a codebase.
 *
 *  Use of stowage enables multi-gigabyte data structures to be modeled as
 *  first-class values, avoiding one major pitfall of purely functional
 *  programming - i.e. eliminating need for external databases or filesystem
 *  IO. Support for caching further mitigates need for state, enabling a
 *  high level of incremental programming, so modifying the codebase doesn't
 *  recompute more than necessary.
 *
 *  Wikilon runtime aims to achieve a competitive level of performance 
 *  from ABC via parallelism, just-in-time compilation, and hand optimized
 *  acceleration for common subprograms. However, an interpreter is the
 *  primary model used.
 *
 *  @section usage_sec Usage
 *
 *  Create an environment. Create a context within that environment. Load
 *  an ABC program (including data) into the context. Evaluate in small
 *  steps. Check for errors. Extract the evaluated program or information.
 *
 *  Errors are confined to their context, and are generally represented
 *  within an evaluated program. The main unrecoverable error is to input
 *  a program larger than the context can store (because stowage is not
 *  implicit). 
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
 * persistence via a key-value store (currently via LMDB). An environment
 * may also be configured with a shared pool of worker threads for light
 * weight parallelism and similar machine-model resources.
 */
typedef struct wikrt_env wikrt_env;

/** @brief Opaque structure representing a context for computation.
 *
 * A context has a space quota, and primarily contains a 'program'.
 * Some of a context's space may be used for active debugging - logs 
 * or program snapshots, for example. Due to garbage collection, a
 * context should usually be operating at a small fraction of full
 * capacity, e.g. less than 25%.
 * 
 * From the Wikilon runtime API, a context is single-threaded. That is,
 * a context must not be used from more than one thread at a time. But
 * no thread-local storage is used, so it is safe to use a context via
 * external mutex or within an M:N threading model with work stealing.
 */
typedef struct wikrt_cx wikrt_cx;

/** @brief Support a simple consistency check for dynamic library.
 *
 * Compare WIKRT_API_VER to wikrt_api_ver(). If they aren't the same,
 * then your app was compiled against a different interface than the
 * linked object implements. This is a rather convenient sanity check.
 */
uint32_t wikrt_api_ver();
#define WIKRT_API_VER 20160913

/** @brief Create a Wikilon environment. 
 *
 * Configuration is separate. The only possible error here is to return
 * NULL because there is insufficient memory to create the environment.
 */
wikrt_env* wikrt_env_create();

/** @brief Release environment memory.
 *
 * This must occur only after associated contexts have been destroyed.
 */
void wikrt_env_destroy(wikrt_env*);


/** @brief Wikilon Runtime's Error Model
 *
 * With program rewriting, most errors are modeled *within* the program,
 * for example by use of a `[subprogram]{&error}i` pattern. Developers
 * may explicitly use an `{&error}` or `{&trash}` annotation to create
 * error objects that will become runtime errors if observed.
 *
 * A major exception 
 */

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
 * Create a fresh context that has a given 'working' memory. Note
 * that this space is less than 100% effective due to the GC model.
 * A fresh context will implicitly contain the *unit* value. 
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
#define WIKRT_TOK_BUFFSZ 256

  /////////////////////////
 // BASIC DATA PLUMBING //
/////////////////////////

/** @brief (a*e) → (a*(a*e)). ABC op `^`. 
 *
 * Note: Some values are logically non-copyable. Among these are affine
 * blocks and pending computations. Copy is also likely to fail due to
 * lack of space, if the context is close to full.
 */
void wikrt_copy(wikrt_cx*);

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
void wikrt_wrap_sum(wikrt_cx*, wikrt_sum_tag);
void wikrt_unwrap_sum(wikrt_cx*, wikrt_sum_tag*);

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

/** @brief Mark a value for stowage. (a * e) → ((stowed a) * e).
 *
 * This corresponds to annotation {&stow}. Stowage moves large values 
 * to a backing database, leaving a lightweight placeholder instead.
 * The stowed value may subsequently be accessed via {&load}. 
 *
 * Stowage enables a small context to work with big data. Databases can
 * be modeled as first class values, using data structures like tries,
 * log structured merge trees, or hitchhiker trees. Massive deques can
 * be modeled efficiently as finger tree ropes.
 *
 * A representation for a stowed value can be obtained by quoting
 * and serializing the value. In general, this representation may
 * only be used within the same environment. Also, unless there is
 * some other reference to the value (e.g. via persistence layer) 
 * the value may be garbage collected. 
 */
void wikrt_stow(wikrt_cx*);

/** @brief Load a stowed value. ((stowed a) * e) → (a * e).
 *
 * This corresponds to annotation {&load}. We'll copy the stowed value
 * from our backing database into active memory. Loading a value that 
 * was not previously stowed is an error. Loading a value that was not
 * stowed by the current runtime environment (or was subsequently GC'd) 
 * will also result in an error.
 */
void wikrt_load(wikrt_cx*);

/** @brief Garbage collection of environment-level resources.
 *
 * Most significantly, this forces GC of stowed values. Normally,
 * stowed values will be incrementally GC'd while new values are
 * stowed. This will force a full GC.
 */
void wikrt_env_gc(wikrt_env*); 

/** @brief Garbage collection of context-level resources.
 *
 * This forces immediate GC for a context's memory. Usually, GC for
 * a context occurs whenever it fills memory (with some heuristic
 * modifiers based on history of memory usage). So explicit GC is 
 * unnecessary outside of special circumstances like profiling of
 * precise memory usage.
 */
void wikrt_cx_gc(wikrt_cx*);

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

/** Effort models. */
typedef enum wikrt_effort_model 
{ WIKRT_EFFORT_BLOCKS       // effort is blocks evaluated
, WIKRT_EFFORT_GC_CYCLES    // effort is GC cycle count
, WIKRT_EFFORT_MEGABYTES    // effort is megabytes allocated
, WIKRT_EFFORT_MILLISECS    // effort is elapsed milliseconds
, WIKRT_EFFORT_CPU_TIME     // effort is CPU milliseconds
} wikrt_effort_model;

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

/** @brief Associate environment to filesystem stowage and persistence.
 *
 * This is currently implemented by LMDB, and will consume a volume of
 * memory-mapped address space corresponding to a fixed maximum amount
 * of stowage, and the maximum database file size. The directory and its
 * parents will be created if necessary.
 *
 * At this time, our environment may only have one such association,
 * is single-assignment (may not be modified after set), and should
 * be set prior to any context has need of it. Further, the database
 * is currently limited to a single process to simplify stowage GC. 
 * This property is weakly guarded by lockfile.
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

/** @brief Replace large value with database-backed placeholder.
 * 
 * This is equivalent to injecting a `{&stow}` annotation into the
 * program. In general, stowage is lazy and heuristic. A 'small'
 * value might not be replaced at all, and a 'large' value might not
 * be replaced immediately. Serializing a stowed value may force the 
 * decision.
 *
 * If a value is stowed, it will be replaced by a stowed resource token
 * that may contain some metadata about substructural attributes, data
 * type, and an HMAC for security. It's important that stowage be used
 * with transactional persistence to control against accidental GC.
 *
 * If there is no database available, the `{&stow}` indicator may be
 * left in the context.
 */
void wikrt_stow(wikrt_cx*);

/** @brief Load a previously stowed value.
 *
 * This is equivalent to injecting a `{&load}` annotation into the
 * program. When applied to a stowed value, that value is loaded into
 * memory immediately. If applied to any other value, the action is 
 * more or less a NOP. By default, stowed data is loaded as needed.
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
