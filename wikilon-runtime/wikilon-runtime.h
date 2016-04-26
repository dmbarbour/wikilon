/** @file wikilon-runtime.h
 *	@brief Wikilon Runtime
 *
 *	@mainpage	Wikilon Runtime
 *
 *	@section intro_sec Introduction
 *
 *  Wikilon is part of Awelon project, which explores a new model for
 *  software development, in which a living system is defined by a set
 *  of acyclic functions. Awelon project uses its own Awelon Bytecode
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
 *    game worlds, and more can be modeled as large immutable values
 *    that are only partially loaded into active memory. This reduces
 *    need for external persistence (e.g. no need for true filesystem
 *    access).
 *
 *  - Parallelism. Pure computations can be parallelized easily because
 *    their behavior is independent of evaluation order. Though, they
 *    may fail non-deterministically due to memory consumption.
 * 
 *  Wikilon runtime shall support these techniques. I'll also support
 *  an integrated key-value store for stowage-friendly persistence.
 *
 *  Effectful code with Wikilon runtime is modeled using Free Monads
 *  and similar techniques instead of using tokens. That is, a client
 *  can inspect a value after a computation and decide to perform some
 *  external effect and inject more data. But there are no callbacks,
 *  no stopping on arbitrary tokens. This helps ensure effects can be
 *  modeled in a pure simulation.
 *
 *  Wikilon runtime is designed to provide very predictable performance,
 *  suitable for real-time systems. Memory is managed manually via copy
 *  and drop operators, and a context's memory is separated (with regards
 *  to fragmentation etc.) from other tasks. Optimizations will be driven
 *  by programmer-controlled annotations. External parallelism is very
 *  'linear' in nature, operating on separate parts of a problem then 
 *  rejoining the pieces.
 *
 *  @section usage_sec Usage
 *
 *  Create an environment. Create a context within that environment.
 *  Load some data into the context, possibly from the key-value
 *  database. Perform computations. Extract and analyze the results.
 * 
 *  @section notes_sec Notes
 *
 *  Portability: Wikilon runtime is written for use in Linux with GCC.
 *  It doesn't use much non-portable code.
 *
 *  Implementation Limits: Wikilon runtime only supports 32-bit contexts
 *  even on a 64-bit system. I.e. a context can't have more than 4GB of
 *  active memory. Stowage can enable a lot more passive memory, limited
 *  mostly by local disk space.
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
 * An environment includes an LMDB instance for large value stowage and
 * a simple key-value persistence layer. Additionally, the environment
 * has a pool of worker threads (one on each CPU by default) to support
 * par/seq parallelism.
 *
 * An environment supports multiple concurrent contexts, representing
 * different threads and tasks.
 *
 */
typedef struct wikrt_env wikrt_env;

/** @brief Opaque structure representing a context for computation.
 *
 * A wikrt_cx holds a single value that may be manipulated by a single
 * external thread in order to perform a computation. The manipulations
 * may introduce data and process it. The 'value' in question is used
 * implicitly as a stack in most cases.
 */
typedef struct wikrt_cx wikrt_cx;

/** @brief Errors during Wikilon Runtime
 *
 * Following the normal C conventions, most functions return an error 
 * condition that allows simple policies on how to handle them. If
 * there is no error, a zero value (WIKRT_OK) is returned.
 *
 * In general, wikrt_err values may be bitwise-or'd together. 
 *
 * Many API functions are 'fail safe' and guarantee that their failure
 * does not modify the context's data.
 */
typedef enum wikrt_err 
{ WIKRT_OK           = 0
, WIKRT_INVAL        = (1<< 0)  // bad arguments, avoidable programmer error
, WIKRT_IMPL         = (1<< 1)  // incomplete implementation, runtime's error

// External Resource Errors
, WIKRT_DBERR        = (1<< 2)  // LMDB or filesystem layer errors 
, WIKRT_NOMEM        = (1<< 3)  // malloc or mmap allocation error

// Special Conditions
, WIKRT_CXFULL       = (1<< 4)  // context is out of memory
, WIKRT_BUFFSZ       = (1<< 5)  // output buffer too small

// Transactions
, WIKRT_CONFLICT     = (1<< 6)  // any transaction conflict

// Evaluations
, WIKRT_QUOTA_STOP   = (1<< 7)  // halted on time/effort quota
, WIKRT_TYPE_ERROR   = (1<< 8)  // generic runtime type errors
} wikrt_err;

/** @brief Translate a single wikrt_err to human text. */
char const* wikrt_strerr(wikrt_err);

/** @brief Support a simple consistency check for dynamic library.
 *
 * Compare WIKRT_API_VER to wikrt_api_ver(). If they aren't the same,
 * then your app was compiled against a different interface than the
 * dynamic lib implements. This is just a simple sanity check.
 */
uint32_t wikrt_api_ver();
#define WIKRT_API_VER 20160421

/** @brief Open or Create a Wikilon environment with given options. 
 *
 * The primary options here indicate where our backing store for stowage
 * and persistence will be held, and how much space we're permitted to 
 * use. At the moment, this space requires an equivalent amount of address
 * space (since I'm using a memory-mapped LMDB database). The directory
 * and size may be NULL and zero if no stowage is desired.
 * 
 */
wikrt_err wikrt_env_create(wikrt_env**, char const* dirPath, uint32_t dbMaxMB);

/** @brief Destroy the environment.
 *
 * All contexts must be explicitly destroyed before the environment
 * is destroyed. If not, this will 'abort()' instead.
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
 * This creates a new shared-nothing context in the environment with
 * a given size in megabytes. The context initially contains the unit
 * value, but may be loaded with more data via the `wikrt_intro` verbs
 * or key-value reads against the implicit database.
 *
 * Note: Wikilon runtime currently uses 32-bit references within a context.
 * This limits context's active memory to about 4GB (cf WIKRT_CX_MAX_SIZE).
 *
 * Note: Wikilon runtime contexts use considerably more address space than
 * the requested allocation. On 64-bit systems, addres space is an abundant
 * resource, so this should be a non-issue. On 32-bit systems, this reduces 
 * the effective maximum context size to a few hundred megabytes.
 */
wikrt_err wikrt_cx_create(wikrt_env*, wikrt_cx**, uint32_t cxSizeMB);

#define WIKRT_CX_MIN_SIZE 4
#define WIKRT_CX_MAX_SIZE 4092

/* Note: I originally pursued `wikrt_cx_fork()` as a basis for lightweight
 * external parallelism. At this time, however, I feel the synchronization
 * overheads, memory fragmentation, and other costs are not worthwhile. If
 * external parallelism is necessary, we can instead copy values from one
 * context to another which isn't cheap but is only paid for at specific
 * boundaries.
 */

/** @brief Destroy a context and recover memory. */
void wikrt_cx_destroy(wikrt_cx*);

/** @brief A context knows its parent environment. */
wikrt_env* wikrt_cx_env(wikrt_cx*);

/** @brief Complete enumeration Wikilon Runtime opcodes.
 * 
 * Wikilon primarily uses Awelon Bytecode (ABC) and ABC Deflated (ABCD)
 * as its serialization models for behavior and data. ABC consists of
 * 42 primitive operators (plus text and block literals), while ABCD is
 * defined by acyclic expansion ultimately into plain ABC. Under the hood
 * Wikilon may additionally use experimental ABCD accelerators, available
 * via simplification of blocks.
 *
 * ABCD serves two roles. It both compresses the serialized format and
 * serves as useful accelerators for hand-written interpreters. ABCD 
 * enables ABC to become a powerful language for collections processing,
 * linear algebra, etc.. depending on which functions are defined. The
 * development of ABCD is an ongoing project.
 *
 * ABC and ABCD opcodes also correspond to unicode character codepoints,
 * with UTF-8 being the standard serialization format.
 */
typedef enum wikrt_opcode 
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
} wikrt_opcode;

/** @brief Supported ABCD operators as utf-8 C string.
 *
 * ABC and ABCD serialize to utf-8 text. The basic 42 ABC operators 
 * are all in the ASCII range, hence requiring one byte each.
 *
 *   lrwzvcLRWZVC%^ \n$o'kf#1234567890+*-QG?DFMK
 *
 * Normally, only token texts and text literals embedded in ABC will
 * use the greater utf-8 range. However, ABCD extends ABC with opcodes
 * that are defined by expansion into ABC. Use of ABCD extensions can
 * both compress serializations and accelerate interpreted performance.
 * 
 * When requested, Wikilon runtime will provide its entire list of 
 * ABC and ABCD operators as a static utf-8 string.
 */
char const* wikrt_abcd_operators();

/** @brief Expand ABC or ABCD opcodes to their definitions.
 *
 * The 42 ABC primitives will return a string containing the same 
 * character, e.g. 'v' expands to "v". Otherwise, we'll expand to
 * at least two opcodes (possibly including more ABCD). If the
 * argument is not a recognized opcode, NULL is returned.
 */
char const* wikrt_abcd_expansion(wikrt_opcode);

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
 * constraints.
 */
bool wikrt_valid_token(char const* s);

// todo: easy function to turn a token text into a block?


/** @brief buffer size to read a token string.
 *
 * The maximum token size for Awelon Bytecode is 63 bytes. Wikilon
 * runtime adds a byte for a NUL-terminator to support C strings. 
 * Token text does not include the wrapping `{}` braces, just the
 * text between them.
 */
#define WIKRT_TOK_BUFFSZ 64

/* Note: Thoughts on Reflection
 *
 * I've experimented with a few variations of reflection for Wikilon
 * runtime. But I've not been very satisfied with the resulting API 
 * and the underlying implementation.
 * 
 * For most cases, Wikilon runtime should be assuming that computations
 * are statically typed. Even something like debug rendering of data 
 * could be tuned such that the caller provides rendering hints that
 * encode type information. 
 *
 * If necessary, clients of Wikilon runtime may use brute force, e.g.
 * use `unwrap_sum` on a suspected sum value and see if it returns a
 * WIKRT_TYPE_ERROR.
 */ 

  /////////////////////////
 // BASIC DATA PLUMBING //
/////////////////////////

/** @brief Move a value from one context to another.
 * 
 * For the left context, this has type `(a*b)→b`. For the right context,
 * this has type `c→(a*c)`. The `a` value is moved from the left context
 * to the right context. Move fails if left and right contexts are the
 * same, or if the RHS context isn't large enough. 
 */
wikrt_err wikrt_move(wikrt_cx*, wikrt_cx*);

/** @brief Metadata about substructural properties of a value.
 *
 * A 'normal' value can be copied or dropped. An affine value should
 * not be copied, and a relevant value should not be dropped. The 
 * pending substructure is returned if a value contains a lazy or
 * parallel or remote value whose substructural properties are not
 * easily computed.
 */
typedef enum wikrt_ss
{ WIKRT_SS_NORM   = 0
, WIKRT_SS_AFF    = (1 << 0)
, WIKRT_SS_REL    = (1 << 1)
, WIKRT_SS_PEND   = (1 << 2)
} wikrt_ss;

/** @brief (a*e) → (a*(a*e)). ABC op `^`. 
 *
 * This operation will succeed even for affine or pending values values, but 
 * will also report the substructural metadata. This allows the client to make
 * decisions about whether a copy was acceptable or not. Wikilon runtime doesn't
 * keep enough information to track substructure before a copy or drop is in
 * progress. If the 'ss' information is irrelevant, set it to NULL.
 *
 * This is most likely to fail with WIKRT_CXFULL, though it may fail with 
 * WIKRT_TYPE_ERROR if the parameter structure is not valid.
 */
wikrt_err wikrt_copy(wikrt_cx*, wikrt_ss*);

/** @brief Combined copy and move operation.
 *
 * This is equivalent to wikrt_copy followed by wikrt_move in one step.
 * It has performance advantages over performing copy and move in separate
 * steps.
 */
wikrt_err wikrt_copy_move(wikrt_cx*, wikrt_ss*, wikrt_cx*);

/** @brief (a*e) → e. ABC op `%`.
 *
 * As with copy, this will report substructural attributes of the value
 * that was dropped. If a relevant or pending value was dropped, that 
 * might be an error in some cases. The wikrt_ss* parameter may be NULL
 * if you don't need that output.
 *
 * The only error returned here is WIKRT_TYPE_ERROR if we lack the (a*e)
 * structure. Drop never fails for other reasons.
 */
wikrt_err wikrt_drop(wikrt_cx*, wikrt_ss*);

/** (a*(b*c))→(b*(a*c)). ABC op `w`. 
 *
 * Most of the basic product manipulations are non-allocating, and
 * hence can only fail with WIKRT_TYPE_ERROR if the context has an
 * invalid value.
 */
wikrt_err wikrt_wswap(wikrt_cx*);

/** (a*(b*(c*d)))→(a*(c*(b*d))). ABC op `z`. */
wikrt_err wikrt_zswap(wikrt_cx*);

/** (a*(b*c))→((a*b)*c). ABC op `l`. */
wikrt_err wikrt_assocl(wikrt_cx*);

/** ((a*b)*c)→(a*(b*c)). ABC op `r`. */
wikrt_err wikrt_assocr(wikrt_cx*);


/** ((a+(b+c))*e)→((b+(a+c))*e). ABC op `W`. */
wikrt_err wikrt_sum_wswap(wikrt_cx*);

/** ((a+(b+(c+d)))*e)→((a+(c+(b+d)))*e). ABC op `Z`. */
wikrt_err wikrt_sum_zswap(wikrt_cx*);

/** ((a+(b+c))*e)→(((a+b)+c)*e). ABC op `L`. */
wikrt_err wikrt_sum_assocl(wikrt_cx*);

/** (((a+b)+c)*e)→((a+(b+c))*e). ABC op `R`. */
wikrt_err wikrt_sum_assocr(wikrt_cx*);

/** (a*((b+c)*e))→(((a*b)+(a*c))*e). ABC op `D`. */
wikrt_err wikrt_sum_distrib(wikrt_cx*);

/** (((a*b)+(c*d))*e)→((a+c)*((b+d)*e)). ABC op `F`. */
wikrt_err wikrt_sum_factor(wikrt_cx*);

// ACCELERATED DATA PLUMBING

/** (a*b)→(b*a). ABC ops `vrwlc`. Non-allocating. Fail-safe. */
wikrt_err wikrt_swap(wikrt_cx*);

/** ((a+b)*e)→((b+a)*e). ABC ops `VRWLC`. */
wikrt_err wikrt_sum_swap(wikrt_cx*);

/* TODO: introduce accelerators as they're developed. */

  ///////////////////////////
 // DATA INPUT AND OUTPUT //
///////////////////////////

/** @brief Allocate and eliminate unit values. Fail-safe.
 *
 *   wikrt_intro_unit:      (a)→(1*a)       vvrwlc
 *   wikrt_elim_unit:       (1*a)→(a)       vrwlcc
 *   wikrt_intro_unit_r:    (a)→(a*1)       v
 *   wikrt_elim_unit_r:     (a*1)→(a)       c
 *
 * The 'elim' operators may fail with WIKRT_TYPE_ERROR.
 * The 'intro' operators may fail with WIKRT_CXFULL.
 * Failure is safe, in the sense that no change is made.
 */
wikrt_err wikrt_intro_unit(wikrt_cx*);
wikrt_err wikrt_elim_unit(wikrt_cx*);
wikrt_err wikrt_intro_unit_r(wikrt_cx*);
wikrt_err wikrt_elim_unit_r(wikrt_cx*);

/** @brief Allocate and split 'sum' values one step at a time. Fail-safe.
 *
 * Allocation operates on an `(a*e)` value and either returns the
 * `((a+0)*e)` or `((0+a)*e)` depending on the `inRight` parameter.
 * Split does the opposite, returning the `inRight` condition.
 * 
 * If inRight is false, this corresponds to op `V`. Otherwise to
 * `VVRWLC`. The unwrap variant provides easy access to a sum.
 *
 * Both wrap sum and unwrap sum may perform allocation. As a special case,
 * wrapping a sum around a product or unit, or unwrapping the generated
 * sum value, does not require allocation and will not fail. Unwrap only
 * allocates when working with arrays or other compact representations.
 */
typedef enum wikrt_sum_tag { WIKRT_INL = 0, WIKRT_INR = 1 } wikrt_sum_tag;
wikrt_err wikrt_wrap_sum(wikrt_cx*, wikrt_sum_tag inRight);
wikrt_err wikrt_unwrap_sum(wikrt_cx*, wikrt_sum_tag* inRight);

// TODO: consider matching deeper structure.

/** @brief Allocation of smaller integers. (e)→(Int*e). Fail-safe. */
wikrt_err wikrt_intro_i32(wikrt_cx*, int32_t);
wikrt_err wikrt_intro_i64(wikrt_cx*, int64_t);

/** @brief Non-destructively access small integers. (Int*e)→(Int*e). Fail-safe.
 *
 * The integer result is returned if possible. In case of a WIKRT_BUFFSZ
 * error, we'll appropriately return INT_MIN or INT_MAX for the size. The
 * context's value is not modified in any case.
 */
wikrt_err wikrt_peek_i32(wikrt_cx*, int32_t*);
wikrt_err wikrt_peek_i64(wikrt_cx*, int64_t*);

/** @brief Allocation of large integers. (e)→(Int*e). Fail-safe.
 *
 * In this case we accept regex: `0 | (-)?[1-9][0-9]*`. Wikilon runtime
 * will support numbers up to a million digits, perhaps more. Wikilon
 * runtime uses a compact variant of binary-coded decimal under the hood,
 * and so favors conversion to or from decimal representations.
 *
 * Note: If the text is NUL-terminated, you may use SIZE_MAX for strlen.
 */
wikrt_err wikrt_intro_istr(wikrt_cx*, char const*, size_t strlen);

/** @brief Non-destructively access a large integer. (Int*e)→(Int*e). Fail-safe.
 *
 * The integer is returned as a string of regex `0 | (-)?[1-9][0-9]*`. 
 * The strlen is both input and output. As input, it is the maximum
 * buffer size. As an output, it is the recorded buffer size (on OK)
 * or (on WIKRT_BUFFSZ) the exact required buffer size. No NUL is 
 * added.
 */
wikrt_err wikrt_peek_istr(wikrt_cx*, char* buff, size_t* strlen); 

/** @brief Allocate a binary. (e)→(binary*e). Fail-safe.
 *
 * The binary is modeled as a list of small integers (0..255). It may
 * use a more compact representation, e.g. an array of bytes, under 
 * the hood. The binary is implicitly terminated by unit. The size
 * parameter must be exact.
 */
wikrt_err wikrt_intro_binary(wikrt_cx*, uint8_t const*, size_t);

/** @brief Incrementally read binary data. Destructive.
 *
 * Our binary is a list `μL.((a*L)+b)` of bytes (0..255). Incremental 
 * read moves bytes to the client's buffer, dropping them as they're
 * read. So this is a destructive operation.
 *
 *    (binary*e)→(smaller binary*e)
 *
 * Return value is WIKRT_OK if the remaining binary is just the `+ term`
 * value. WIKRT_BUFFSZ is returned if we reached the buffer limit. If
 * the list contains a non-byte value, or if the argument is not a list,
 * we'll return WIKRT_TYPE_ERROR. In any case, we'll read as much as we
 * can before returning.
 *
 * The size_t argument is input and output. On input, it is the maximum
 * buffer size. On output, it is the number of bytes read.
 */
wikrt_err wikrt_read_binary(wikrt_cx*, uint8_t*, size_t*);

/** @brief Allocate a text. Fail-safe.
 *
 * Text must be valid utf-8, and valid as ABC text: no control characters
 * (C0, C1, DEL) except LF, no surrogate codepoints, no replacement char.
 * A text is modeled as a list of codepoints, but may use a more compact
 * representation under the hood.
 *
 * Note: A NUL-terminated C string is also accepted, in which case you may
 * use SIZE_MAX for the length parameter. NUL itself is in C0 so is not an
 * accepted character within text.
 */
wikrt_err wikrt_intro_text(wikrt_cx*, char const* str, size_t len);

/** @brief Incrementally read a text. Destructive.
 *
 * Similar to wikrt_read_binary, but will read text into a utf-8 buffer.
 * The codepoints read will meet the same constraints as alloc_text, i.e. 
 * no control chars except LF, no surrogates, no incomplete codepoints.
 * However, Wikilon runtime is ignorant of multi-codepoint characters, so
 * there is no guarantee that the codepoints read represent a complete
 * character.
 * 
 * The 'chars' parameter enables clients to limit and quickly determine 
 * the number of codepoints read, but it is optional (NULL permitted).
 */
wikrt_err wikrt_read_text(wikrt_cx*, char*, size_t* bytes, size_t* chars);

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
wikrt_err wikrt_text_to_block(wikrt_cx*);
wikrt_err wikrt_block_to_text(wikrt_cx*);

/** @brief Wrap a value with a sealer token. (a * e)→((sealed a) * e). Fail-safe.
 *
 * The sealer token is provided as a simple C string. Discretionary 
 * sealers such as ":map" are understood by Wikilon runtime, as they
 * may be unsealed (by a {.map} token). Short discretionary sealers
 * (no more than 4 bytes, including the ':') have a compact encoding.
 */
wikrt_err wikrt_wrap_seal(wikrt_cx*, char const*); 

/** @brief Access a sealed value. ((sealed a) * e) → (a * e). Fail-safe.
 *
 * This returns the sealer token into the provided buffer, which must
 * be at least WIKRT_TOK_BUFFSZ in length to eliminate risk of buffer
 * overflow. This token is NUL-terminated.
 */
wikrt_err wikrt_unwrap_seal(wikrt_cx*, char*);

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
wikrt_err wikrt_stow(wikrt_cx*);

/** @brief Load a stowed value. ((stowed a) * e) → (a * e).
 *
 * This corresponds to annotation {&load}. We'll copy the stowed value
 * from our backing database into active memory. If the value was not 
 * yet stowed due to laziness, we'll simply unwrap the stowage marker.
 */
wikrt_err wikrt_load(wikrt_cx*);

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
 * Introducing a stowed value by ID may fail for a variety of reasons.
 *
 * - value is bound to the wrong environment
 * - authentication failure of the resource ID
 * - resource ID has since been GC'd
 *
 * In these cases, I'll return WIKRT_INVAL but leave a placeholder on
 * the stack (and `wikrt_load` will fail instead). We may fail for more
 * mundane reasons - an unparseable resource token (WIKRT_TYPE_ERROR) or
 * a full context (WIKRT_CXFULL). For those, our context is unmodified.
 */
wikrt_err wikrt_intro_sv(wikrt_cx*, char const* resourceId);

/** @brief Access resource ID for a stowed value. Non-destructive.
 *
 * Given context of type ∀a,e.((stowed a) * e), returns the resource
 * ID of the stowed value. Forces stowage if still lazily pending.
 *
 * The output buffer should have size at least WIKRT_TOK_BUFFSZ, and
 * is returned as a NUL-terminated C string. Due to authentication,
 * resource identifiers will typically use most of this space.
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
wikrt_err wikrt_peek_sv(wikrt_cx*, char* buff);



// TODO: consider support for fast-diffing values that use stowage.
//  I'd want to 


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
 * use wikrt_step_eval on the pending value until complete.
 */
wikrt_err wikrt_apply(wikrt_cx*);

/** @brief Step an evaluation.  ((pending a) * e) → (a * e).
 *
 * This operation consumes an 'effort quota', which is heuristic in
 * nature but corresponds roughly to megabytes allocated (including
 * compactions). If the quota reaches zero, we return WIKRT_QUOTA_STOP
 * and our value remains pending. It's also possible we'll halt with
 * WIKRT_TYPE_ERROR or other errors. On WIKRT_OK, we have our final
 * output value.
 *
 * Under consideration: Support a WIKRT_TOKEN_STOP when evaluation
 * cannot continue because we encounter an unrecognized token. Enable
 * the client to process the token and continue evaluation.
 */
wikrt_err wikrt_step_eval(wikrt_cx*, uint32_t* effort);

/** @brief Quote a value. (a * e) → ((∀e'. e'→(a*e'))*e).
 *
 * Almost any value may be quoted, including pending values. However,
 * in some cases we might not have clear substructural properties for
 * the resulting block. Wikilon runtime favors performance over precise
 * dynamic tracking of substructural attributes.
 */
wikrt_err wikrt_quote(wikrt_cx*);

/** @brief Mark a block affine (non-copyable). (block*e)→(block*e). */
wikrt_err wikrt_block_aff(wikrt_cx*);

/** @brief Mark a block relevant (non-droppable). (block*e)→(block*e). */
wikrt_err wikrt_block_rel(wikrt_cx*);

/** @brief Mark a block for parallel evaluation. (block*e)→(block*e). 
 *
 * Note: this doesn't affect a top-level evaluation, only applications
 * internal to a wikrt_step_eval. Wikilon runtime guarantees there is
 * no parallel evaluation upon returning from wikrt_step_eval.
 */
wikrt_err wikrt_block_par(wikrt_cx*);

/** @brief Compose two blocks. ([a→b]*([b→c]*e))→([a→c]*e). */
wikrt_err wikrt_compose(wikrt_cx*);

/** @brief Add two integers. (I(a)*(I(b)*e))→(I(a+b)*e). */
wikrt_err wikrt_int_add(wikrt_cx*);

/** @brief Multiply two integers. (I(a)*(I(b)*e))→(I(a*b)*e). */
wikrt_err wikrt_int_mul(wikrt_cx*);

/** @brief Negate an integer. (I(a)*e)→(I(-a)*e). */
wikrt_err wikrt_int_neg(wikrt_cx*);

/** @brief Divide two integers with remainder.
 *
 * (I(divisor) * (I(dividend) * e)) → (I(remainder) * (I(quotient) * e)).
 *
 * The divisor must be non-zero, or we'll return WIKRT_TYPE_ERROR.
 */
wikrt_err wikrt_int_div(wikrt_cx*); 

/** @brief Integer comparison result. */
typedef enum wikrt_ord { WIKRT_LT = -1, WIKRT_EQ = 0, WIKRT_GT = 1 } wikrt_ord;

/** @brief Compare two integers. Non-destructive. (I(a)*(I(b)*e)).
 *
 * This compares `b` to `a`, matching direct allocation order (i.e. if we
 * allocate zero then four, the comparison is `zero is less than four`).
 */
wikrt_err wikrt_int_cmp(wikrt_cx*, wikrt_ord*);

  /////////////////////////////////////
 // DATABASE AND PERSISTENCE ENGINE //
/////////////////////////////////////

/** @brief Validate database key.
 *
 * Transaction keys must be valid texts of limited size, having at
 * most WIKRT_VALID_KEY_MAXLEN bytes in the utf-8 encoding, and at
 * least one byte.
 */
bool wikrt_valid_key(char const*);
#define WIKRT_VALID_KEY_MAXLEN 255

/** @brief Begin a transaction with the current context.
 *
 * At the moment, a context may have one implicit, non-hierarchical
 * transaction. This transaction must be created before database 
 * operations. After a transaction commits or aborts, a new one may
 * be started. Transactions affect only database interactions.
 *
 * Note: transactions are NOT backtracking by default. Support for
 * backtracking is feasible but requires explicitly duplicating the
 * context.
 */
wikrt_err wikrt_txn_create(wikrt_cx*);

/** @brief Read a value from the implicit key-value database. (e)→(v*e). 
 * 
 * Rather than 'undefined' keys, all valid keys are defined with an
 * implicit default value - unit in right - until otherwise set. So
 * it's easy to access any key and simply ask for its value. 
 *
 * Due to Wikilon runtime's preference for linear move semantics, every
 * read requires copying a value. This may be mitigated by stowage in
 * larger values.
 */
wikrt_err wikrt_txn_read(wikrt_cx*, char const* key);

/** @brief Write a value into the implicit key-value database. (v*e)→(e).
 *
 * This writes a value from the context back into the database at a named
 * location. Writing the default value - unit in right - will effectively
 * delete that key.
 */
wikrt_err wikrt_txn_write(wikrt_cx*, char const* key);

/** @brief Abort active transaction (if any). */
void wikrt_txn_abort(wikrt_cx*);

/** @brief Attempt to commit active transaction.
 *
 * If this fails, the transaction is implicitly aborted. Transactions
 * may fail due to conflicts, or due to writing incomplete values (a
 * pending value), etc..
 */
wikrt_err wikrt_txn_commit(wikrt_cx*);

/** @brief Mark a transaction for durability. 
 * 
 * Transactions are not durable by default, but may be marked durable
 * at any point during the transaction (e.g. based on data involved).
 * A durable transaction will require extra synchronization overheads
 * (flushing data to disk) but will be robust by the time it returns
 * to the client.
 *
 * Non-durable transactions can be made durable retroactively by use
 * of `wikrt_env_sync()`. 
 */
void wikrt_txn_durable(wikrt_cx*);

// Todo: consider specialized 'variable' alternatives for 
//  stream processing, queues, logs, or similar?

#define WIKILON_RUNTIME_H
#endif
