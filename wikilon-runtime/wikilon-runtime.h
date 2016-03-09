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
 *  - Parallelism. Modulo space requirements, pure computations behave
 *    independently of evaluation order. Divide and conquer tactics 
 *    are effective if we can divide into coarse-grained tasks. ABC
 *    easily supports par/seq parallelism. 
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
 * An environment may support multiple concurrent contexts.
 *
 */
typedef struct wikrt_env wikrt_env;

/** @brief Opaque structure representing a context for computation.
 *
 * A wikrt_cx, a wikilon runtime context, is a space and thread for
 * computation. An initial context is constructed by wikrt_cx_create()
 * to allocate the underlying memory. Use of wikrt_cx_fork() can make
 * additional lightweight contexts that share the same space limits 
 * and enable non-copying wikrt_move() between them. A context has 
 * several unsynchronized data structures, so must be used from one
 * thread at a time.
 *
 * A context binds a single implicit value, initially the unit value.
 * This value is manipulated by many API functions, which operate much
 * like a stream of Awelon Bytecode. Potential values include products,
 * sums, integers, blocks of code, sealed values, and optimized encodings
 * for texts, binaries, lists, etc..
 */
typedef struct wikrt_cx wikrt_cx;

/** @brief Errors during Wikilon Runtime
 *
 * Following the normal C conventions, most functions return an error 
 * condition that allows simple policies on how to handle them. If
 * there is no error, a zero value (WIKRT_OK) is returned.
 *
 * In general, wikrt_err values may be bitwise-or'd together.
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
, WIKRT_TXN_CONFLICT = (1<< 6)  // transaction failed on conflict

// Evaluations
, WIKRT_QUOTA_STOP   = (1<< 7)  // halted on time/effort quota
, WIKRT_TYPE_ERROR   = (1<< 8)  // generic runtime type errors
} wikrt_err;

/** @brief Translate a single wikrt_err to human text. */
char const* wikrt_strerr(wikrt_err);

/** @brief Open or Create a Wikilon environment.
 *
 * The developer specifies a directory and how much space to allocate
 * for persistent storage. This space will be used for transactions and
 * stowage, and is also allocated within the address space.
 * 
 * It is possible to create an environment without a database by setting
 * dirPath to NULL and dbMaxMB to 0. In this case, transactions fail and
 * stowage is mostly ignored. 
 *
 * wikrt_env_create will attempt to recursively create a directory if it
 * does not exist. The most likely error is WIKRT_DBERR if the directory
 * cannot be created or opened.
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
 * A context consists mostly of one big mmap'd block of memory. The
 * valid range for context size is 4..4000 in megabyte units. This
 * space is specific to the created context, though may be shared
 * with other contexts via `wikrt_cx_fork()`. 
 *
 * A newly created context will bind the basic 'unit' value.
 */ 
wikrt_err wikrt_cx_create(wikrt_env*, wikrt_cx**, uint32_t sizeMB);

#define WIKRT_CX_SIZE_MIN 4
#define WIKRT_CX_SIZE_MAX 4000

/** @brief Lightweight external parallelism.
 *
 * Use of `wikrt_cx_create()` will allocate a dedicated space each
 * time. Use of `wikrt_cx_fork()` is much more lightweight, creating
 * a context that shares the same space as its parent. `wikrt_move()`
 * between forks is very efficient. The space allocated upon create 
 * is preserved until all forks are destroyed.
 *
 * Fork is very lightweight, sufficient to use for short-lived tasks
 * or even just to carry a value from one thread to another. Only one
 * allocation is performed, and the only possible error is WIKRT_NOMEM.
 * 
 * As with wikrt_cx_create, a fork initially binds the unit value.
 *
 * On success, WIKRT_OK is returned and the context is set. Otherwise,
 * an error is returned (likely WIKRT_TYPE_ERROR or WIKRT_NOMEM) and
 * the fork's pointer is NULL.
 */
wikrt_err wikrt_cx_fork(wikrt_cx*, wikrt_cx**);

/** @brief Destroy a context and recover memory. 
 *
 * The primary space underlying a context is not destroyed until all
 * forks (from wikrt_cx_fork) are destroyed. However, destroying a
 * fork will at least release the memory bound to its contained value. 
 */
void wikrt_cx_destroy(wikrt_cx*);

/** @brief A context knows its parent environment. */
wikrt_env* wikrt_cx_env(wikrt_cx*);

/** @brief Complete enumeration of ABC and supported ABCD opcodes.
 * 
 * Wikilon uses Awelon Bytecode (ABC) and ABC Deflated (ABCD) as its
 * primary serialization models for behavior and data. ABC consists of
 * 42 primitive operators, while ABCD is defined by acyclic expansion
 * ultimately into plain ABC.
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
, ABC_COMPOSE     = 111  // o :: ([a→b] * ([b→c] * e)) → ([a→c] * e)
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
, ABC_CONDAP      = 63   // ? :: ([a→c] * ((a+b)*e)) → ((c+b)*e) (block must be droppable)
, ABC_DISTRIB     = 68   // D :: (a * ((b+c) * e)) → (((a*b) + (a*c)) * e)
, ABC_FACTOR      = 70   // F :: (((a*b)+(c*d)) * e) → ((a+c)*((b+d)*e))
, ABC_MERGE       = 77   // M :: ((a+a)*e) → (a*e)
, ABC_ASSERT      = 75   // K :: ((a+b)*e) → (b*e); assert in right
} wikrt_opcode;

/** @brief Options for bytecode serialization.
 *
 * Wikilon uses Awelon Bytecode (ABC) as its primary serialization model.
 * Any value may be quoted into a block then serialized to utf8 binary.
 * Conversely, we can compute values from bytecode.
 * 
 * While ABC is flexible, we can compress representation and accelerate
 * interpreted performance with ABCD (ABC Deflate) extensions. These use
 * a static, standardized dictionary to encode common ABC subprograms,
 * especially those amenable to hand optimization (e.g. list processing).
 *
 * We can also serialize with stowed resources, though this limits code
 * to round-tripping and requires careful design to ensure stowed data
 * is not GC'd. Stowed addresses include an HMAC to secure access to 
 * potentially sensitive data stowed by past computations.
 *
 * Use a bitwise 'or' of multiple abc options, always including 
 * WIKRT_ABC_PRIMOPS.
 */
typedef enum wikrt_abc_opts 
{ WIKRT_ABC_PRIMOPS = 1 // 42 primitive ops, texts, blocks
, WIKRT_ABC_DEFLATE = 2 // enable known ABCD extensions
, WIKRT_ABC_STOWAGE = 4 // enable stowed resource tokens
} wikrt_abc_opts;


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


/** @brief Maximum buffer size for token text.
 *
 * The maximum token size for Awelon Bytecode is 63 bytes. Wikilon
 * runtime uses a byte for a NUL-terminator to support C strings. 
 * Token text does not include the wrapping `{}` braces, just the
 * text between them.
 */
#define WIKRT_TOK_BUFFSZ 64

/** @brief Obtain or assert shallow type information for a value. 
 *
 * This is intended for debug printing and similar tasks where we 
 * don't necessarily know enough about a value to make good decisions.
 * Wikilon runtime will either *generate* a buffer up to some given
 * size and depth indicating type information, or *assert* a similar
 * type buffer.
 *
 * The format is a prefix notation (with `x` and `y` as variables):
 *
 *    *xy  - a pair with type `a` then type `b`
 *    +xy  - a sum type, likely with void in x or y
 *    _    - a void, not a valid value, used in sums
 *    .    - unit value
 *    #    - an integer 
 *    $    - a block or function 
 *    :x   - a sealed value (seal token NOT reported here)
 *    ?    - depth limit or don't care
 *    ~    - pending or stowed values, computation needed
 *
 * For example, "*#*#*#." represents a stack of 3 integers. Eventually
 * I might support repetitive type analyses, e.g. to detect lists and
 * tree structures.
 *
 * Note: Use of reflection is discouraged in general. Favor use of
 * statically predictable or at worst dependent types.
 */
typedef enum wikrt_vtype 
{ WIKRT_VTYPE_PROD = 42     // *
, WIKRT_VTYPE_SUM  = 43     // +
, WIKRT_VTYPE_UNIT = 46     // .
, WIKRT_VTYPE_VOID = 95     // _
, WIKRT_VTYPE_INT  = 35     // #
, WIKRT_VTYPE_BLOCK = 36    // $
, WIKRT_VTYPE_SEAL = 58     // :
, WIKRT_VTYPE_PEND = 126    // ~
, WIKRT_VTYPE_ANY  = 63     // ?
} wikrt_vtype;

/** @brief With bound value (a*e), obtain shallowest type of `a`. */
wikrt_err wikrt_peek_type(wikrt_cx*, wikrt_vtype*);

/** @brief With bound value (a*e), test shallow match on type of `a`. */
bool wikrt_match_type(wikrt_cx*, wikrt_vtype);

/** @brief Obtain analysis of a value's type. (Non-destructive.)
 * 
 * This variation operates on a buffer, and also limits the depth of the
 * analysis (such that clients can select between breadth and depth). 
 * This operation analyzes the value `a` in an (a*e) pair.
 *
 * The `buffLen` parameter is both an input (max buff length) and output
 * (result buff length). NUL terminal is NOT added, so reserve an extra
 * byte for that if you need to produce a C string.
 *
 * This function will fill as much as the buffer as possible with type
 * information, recording `?` upon reaching a depth limit. The end of 
 * the string can be implicitly understood as accepting any type, i.e.
 * an unlimited stream of `?`.
 */
wikrt_err wikrt_peek_typestr(wikrt_cx*, char* buff, size_t* buffLen, size_t maxDepth);

/** @brief Match type information on deep structure. (Non-destructive.)
 *
 * In this case the buffer is an input text, and we'll try to match it
 * with the value `a` in a bound (a*b) pair. If we return `true` then 
 * the type was matched. Otherwise, there was a mismatch or other error.
 *
 * Note: Setting strlen is optional for a C string. You may use SIZE_MAX
 * in that case. 
 */
bool wikrt_match_typestr(wikrt_cx*, char const* str, size_t strLen);

  /////////////////////////
 // BASIC DATA PLUMBING //
/////////////////////////

/** @brief Move a value from one context to another. 
 * 
 * For the left context, this has type `(a*b)→b`. For the right context,
 * this has type `c→(a*c)`. The `a` value is moved from the left context
 * to the right context. We'll return WIKRT_INVAL if left and right are
 * the same context. For cx_forks sharing the same underlying memory, 
 * move is extremely efficient, requiring no copies or allocation, and
 * is guaranteed to succeed. Otherwise, the value `a` may need to be
 * copied into the right-hand side context.
 *
 * This is the primary function for communication between contexts.
 *
 * Note that the `wikrt_move` call requires exclusive control over both
 * contexts. In many cases, it may be useful to create intermediate
 * 'messenger' forks to carry data between threads, i.e. such that the
 * creator pushes data to the messenger then the receiver takes data
 * from the messenger.
 */
wikrt_err wikrt_move(wikrt_cx*, wikrt_cx*);

/** @brief Metadata about substructural properties of a value.
 *
 * A 'normal' value can be copied or dropped. An affine value should
 * not be copied, and a relevant value should not be dropped. These
 * substructural properties are valuable for 
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
 * decisions about whether the copy was acceptable or not. (Wikilon runtime
 * doesn't keep enough information to track substructure until the copy is 
 * in progress.) The wikrt_ss* value may also be NULL if you don't need the
 * output.
 */
wikrt_err wikrt_copy(wikrt_cx*, wikrt_ss*);

/** @brief Combined copy and move operation. 
 *
 * This is roughly wikrt_copy followed by wikrt_move, albeit in one step.
 * The combination avoids an intermediate copy if the two contexts use 
 * different memory regions.
 */
wikrt_err wikrt_copy_move(wikrt_cx*, wikrt_ss*, wikrt_cx*);

/** @brief (a*e) → e. ABC op `%`.
 *
 * As with copy, this will report substructural attributes of the value
 * that was dropped. If a relevant or pending value was dropped, that 
 * might be an error in some cases. The wikrt_ss* parameter may be NULL
 * if you don't need that output.
 */
wikrt_err wikrt_drop(wikrt_cx*, wikrt_ss*);

/** (a*(b*c))→(b*(a*c)). ABC op `w` */
wikrt_err wikrt_wswap(wikrt_cx*);

/** (a*(b*(c*d)))→(a*(c*(b*d))). ABC op `z` */
wikrt_err wikrt_zswap(wikrt_cx*);

/** (a*(b*c))→((a*b)*c). ABC op `l`. */
wikrt_err wikrt_assocl(wikrt_cx*);

/** ((a*b)*c)→(a*(b*c)). ABC op `r`. */
wikrt_err wikrt_assocr(wikrt_cx*);

/** (a*b)→(b*a). ABC ops `vrwlc`. */
wikrt_err wikrt_swap(wikrt_cx*);

/** ((a+(b+c))*e)→((b+(a+c))*e). ABC op `W`. */
wikrt_err wikrt_sum_wswap(wikrt_cx*);

/** ((a+(b+(c+d)))*e)→((a+(c+(b+d)))*e). ABC op `Z`. */
wikrt_err wikrt_sum_zswap(wikrt_cx*);

/** ((a+(b+c))*e)→(((a+b)+c)*e). ABC op `L`. */
wikrt_err wikrt_sum_assocl(wikrt_cx*);

/** (((a+b)+c)*e)→((a+(b+c))*e). ABC op `R`. */
wikrt_err wikrt_sum_assoc(wikrt_cx*);

/** ((a+b)*e)→((b+a)*e). ABC ops `VRWLC`. */
wikrt_err wikrt_sum_swap(wikrt_cx*);

/** (a*((b+c)*e))→(((a*b)+(a*c))*e). ABC op `D`. */
wikrt_err wikrt_sum_distrib(wikrt_cx*);

/** (((a*b)+(c*d))*e)→((a+c)*((b+d)*e)). ABC op `F`. */
wikrt_err wikrt_sum_factor(wikrt_cx*);

#if 0
// consider adding many of these...

/** ((a*(b*s))*e)→((b*(a*s))*e). ABC ops `rwrwzwlwl`. */
wikrt_err wikrt_stack_wswap(wikrt_cx*);

/** ((a*(b*(c*s)))*e)→((a*(c*(b*s)))*e). ABC ops `rwrwrwzwlwlwl`. */
wikrt_err wikrt_stack_zswap(wikrt_cx*);

// `rrwll`      swap on stack
// `rwrzwll`    assocl on stack 
// `rrwzlwl`    assocr on stack
// stack-hand operations (take, put)
#endif

  ///////////////////////////
 // DATA INPUT AND OUTPUT //
///////////////////////////

/** @brief Allocate and eliminate unit values.
 *
 * Awelon Bytecode has primitive operators `v` and `c` that introduce
 * or eliminate unit values as the second element of a pair. But this
 * API favors allocation and dropping values from the left side.
 *
 *   wikrt_intro_unit:  (a)→(1*a)       vvrwlc
 *   wikrt_elim_unit:  (1*a)→(a)       vrwlcc
 *
 * This may be used together with wikrt_swap to recover the original
 * ABC operator behavior. Use of wikrt_split_unit doesn't actually
 * return any data, but it does assert we're dropping a unit value.
 */
wikrt_err wikrt_intro_unit(wikrt_cx*);
wikrt_err wikrt_elim_unit(wikrt_cx*);

/** @brief Allocate and split 'sum' values one step at a time.
 *
 * Allocation operates on an `(a*e)` value and either returns the
 * `((a+0)*e)` or `((0+a)*e)` depending on the `inRight` parameter.
 * Split does the opposite, returning the `inRight` condition.
 * 
 * If inRight is false, this corresponds to op `V`. Otherwise to
 * `VVRWLC`.
 *
 */
wikrt_err wikrt_wrap_sum(wikrt_cx*, bool inRight);
wikrt_err wikrt_unwrap_sum(wikrt_cx*, bool* inRight);

// TODO: consider matching deeper structure.

/** @brief Allocation of smaller integers. (e)→(Int*e). */
wikrt_err wikrt_intro_i32(wikrt_cx*, int32_t);
wikrt_err wikrt_intro_i64(wikrt_cx*, int64_t);

/** @brief Non-destructively access small integers. (Int*e)→(Int*e). 
 *
 * The integer result is returned if possible. In case of a WIKRT_BUFFSZ
 * error, we'll appropriately return INT_MIN or INT_MAX for the size.
 */
wikrt_err wikrt_peek_i32(wikrt_cx*, int32_t*);
wikrt_err wikrt_peek_i64(wikrt_cx*, int64_t*);

/** @brief Allocation of large integers. (e)→(Int*e).
 *
 * In this case we accept regex: `0 | (-)?[1-9][0-9]*`. Wikilon runtime
 * will support numbers up to a million digits, perhaps more. Wikilon
 * runtime uses a compact variant of binary-coded decimal under the hood,
 * and so favors conversion to or from decimal representations.
 *
 * Note: If the text is NUL-terminated, you may use SIZE_MAX for strlen.
 */
wikrt_err wikrt_intro_istr(wikrt_cx*, char const*, size_t strlen);

/** @brief Non-destructively access a large integer. (Int*e)→(Int*e).
 *
 * The integer is returned as a string of regex `0 | (-)?[1-9][0-9]*`. 
 * The strlen is both input and output. As input, it is the maximum
 * buffer size. As an output, it is the recorded buffer size (on OK)
 * or the required buffer size (on WIKRT_BUFFSZ).
 */
wikrt_err wikrt_peek_istr(wikrt_cx*, char* buff, size_t* strlen); 

/** @brief Allocate a binary. (e)→(binary*e).
 *
 * The binary is modeled as a list of small integers (0..255). It may
 * use a more compact representation, e.g. an array of bytes, under 
 * the hood. The binary is implicitly terminated by unit.
 */
wikrt_err wikrt_intro_binary(wikrt_cx*, uint8_t const*, size_t);

/** @brief Incrementally read binary data.
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

/** @brief Allocate a text. 
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

/** @brief Incrementally read a text. 
 *
 * Similar to wikrt_read_binary, but will read text into a utf-8 buffer.
 * The characters read will meet the same constraints as alloc_text, i.e. 
 * no control chars except LF, no surrogates, no incomplete chars.
 *
 * In this case, both `bytes` and `chars` indicate maximums on input and
 * actual values upon output. A utf8 codepoint may be one to four bytes,
 * so `chars` indicates a number of codepoints read.
 */
wikrt_err wikrt_read_text(wikrt_cx*, char*, size_t* bytes, size_t* chars);

/** @brief Allocate a block of Awelon Bytecode. (e)→(block * e). 
 * 
 * As with most input texts in this API, you're free to use a NUL-terminated
 * C string before the maximum size is reached.
 */
wikrt_err wikrt_intro_block(wikrt_cx*, char const*, size_t, wikrt_abc_opts);

/** @brief Wrap a value with a sealer token. (a * e)→((sealed a) * e).
 *
 * The sealer token is provided as a simple C string. Discretionary 
 * sealers such as ":map" are understood by Wikilon runtime, as they
 * may be unsealed (by a {.map} token). Short discretionary sealers
 * (no more than 4 bytes, including the ':') have a compact encoding.
 */
wikrt_err wikrt_wrap_seal(wikrt_cx*, char const*); 

/** @brief Access a sealed value. ((sealed a) * e) → (a * e).
 *
 * This returns the sealer token into the provided buffer, which must
 * be at least WIKRT_TOK_BUFFSZ in length to eliminate risk of buffer
 * overflow. This token is NUL-terminated.
 */
wikrt_err wikrt_unwrap_seal(wikrt_cx*, char*);

/** @brief Mark a value for stowage. (a * e) → ((stowed a) * e)
 *
 * Value stowage pushes a representation of the value to a backing database
 * and replaces its local representation by a small, unique key value. Use of
 * structure sharing helps mitigate space usage for common computations.
 *
 * Stowage is lazy for important performance reasons. When constructing a
 * tree with stowed nodes, nodes near the root will be accessed and updated
 * frequently and so should not be stowed immediately. Instead, stowage will
 * tend to wait until there is memory pressure or it is necessary for some
 * transaction.
 */
wikrt_err wikrt_stow(wikrt_cx*);

#if 0
// consider serialization of blocks to and from texts.
wikrt_err wikrt_text_to_block(wikrt_cx*, wikrt_abc_opts);
wikrt_err wikrt_block_to_text(wikrt_cx*, wikrt_abc_opts);
#endif

  ////////////////
 // EVALUATION //
////////////////
/** @brief Construct an evaluation. ((a→b)*(a*e)) → ((pending b) * e).
 *
 * This doesn't immediately perform the evaluation, but does prepare for
 * it. To complete the evaluation, proceed with `wikrt_step_eval` on the
 * pending value.
 */
wikrt_err wikrt_apply(wikrt_cx*);

/** @brief Step an evaluation.  ((pending a) * e) → (a * e).
 *
 * This operation consumes an 'effort quota', which corresponds roughly
 * to megabytes allocated. Quotas are only tested at internal 'apply' 
 * (operator '$' or '?') actions, so are imprecise. If the quota reaches 
 * zero, we return WIKRT_QUOTA_STOP and our value remains pending (but
 * hopefully some progress has been made).
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
 * The dividend must be non-zero, or we'll return WIKRT_TYPE_ERROR.
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
 * most WIKRT_VALID_KEY_MAXLEN bytes in the utf-8 encoding.
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
