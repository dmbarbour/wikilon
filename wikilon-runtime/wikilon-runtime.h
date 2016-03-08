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
 *  Wikilon runtime shall support these techniques. Further, large
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
#include <stddef.h>

// NOTE: Wikilon runtime is still early development. A lot of the goals
// aren't implemented yet. I'm aiming for a minimal 'spike solution' to
// start - enough to at least run a lot of useful code.

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

/** @brief Opaque structure representing substrate for computations.
 *
 * A wikrt_cx, a wikilon runtime context, represents both a space and
 * a single thread for computations. The space may be shared via fork
 * and move functions, but a wikrt_cx contains several unsynchronized
 * structures (local free lists, root sets, etc.) and must be used from
 * only one client thread at a time. (See `wikrt_cx_fork()` for use of
 * multiple external threads in one context.)
 *
 * Wikilon runtime uses a 32-bit region even on 64-bit machines. This
 * limits contexts to 4GB active data at a time, though use of stowage
 * and transactions enables access to more. (I figure that if 64-bit
 * becomes essential, it will be easy to develop a 64-bit variant of
 * this library.)
 */
typedef struct wikrt_cx wikrt_cx;

/** @brief A value reference within a context. 
 *
 * Wikilon's value model is based on Awelon Bytecode. Basic values
 * include integers, products, unit, sums, and blocks. Special case
 * data includes sealed values, stowed values, pending computations,
 * and compact representations (e.g. arrays, texts, binaries are 
 * lists `μL.((a*L)+b)` but with tight encodings).
 *
 * Wikilon runtime assumes linear value references. That is, a value
 * should be consumed exactly once after allocation. A value must be
 * explicitly copied to use it more than once. This does hinder error
 * recovery. OTOH, a lot of functions can be implemented without any
 * allocation.
 *
 * A wikrt_vref value is bound to the wikrt_cx in which it was created.
 * At this time, a wikrt_cx is limited to a small finite number of such
 * bindings - e.g. 30 items - beyond which WIKRT_CXFULL is returned for
 * further allocations. This forces clients to model a stack within the
 * context when building or processing large values. OTOH, it reduces 
 * root-set management overhead for the common case.
 *
 * Conventions:
 *
 * - wikrt_vref**: outputs, or input-outputs (i.e. leftover outputs)
 * - wikrt_vref*: input values consumed linearly, invalid after use
 * - wikrt_vref const*: input values for non-destructive peek or copy
 * 
 */ 
typedef struct wikrt_vref wikrt_vref;

/** @brief Errors during Wikilon Runtime
 *
 * Following the normal C conventions, most functions return an error 
 * condition that allows simple policies on how to handle them. If
 * there is no error, a zero value (WIKRT_OK) is returned.
 */
typedef enum wikrt_err 
{ WIKRT_OK = 0
, WIKRT_INVAL           // bad arguments, avoidable programmer error
, WIKRT_IMPL            // incomplete implementation, runtime's error

// External Resource Errors
, WIKRT_DBERR           // LMDB or filesystem layer errors 
, WIKRT_NOMEM           // malloc or mmap allocation error

// Special Conditions
, WIKRT_CXFULL          // context is out of memory
, WIKRT_BUFFSZ          // output buffer too small

// Transactions
, WIKRT_TXN_CONFLICT    // transaction failed on conflict

// Evaluations
, WIKRT_QUOTA_STOP      // halted on time/effort quota
, WIKRT_TYPE_ERROR      // generic runtime type errors
} wikrt_err;

/** @brief Translate wikrt_err to human text. */
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
 */ 
wikrt_err wikrt_cx_create(wikrt_env*, wikrt_cx**, uint32_t sizeMB);

#define WIKRT_CX_SIZE_MIN 4
#define WIKRT_CX_SIZE_MAX 4000

/** @brief External parallelism within a context.
 *
 * Use of `wikrt_cx_fork()` creates a context that shares the same
 * underlying heap as its parent. This means the space quota and
 * memory fragmentation is shared, but allows efficient movement
 * of data between the two contexts (via `wikrt_move()`). 
 *
 * This is intended for lightweight parallelism: asynhronous effects,
 * pipeline models, actors, lightweight multi-threading, etc..
 * Whereas `wikrt_cx_create()` corresponds to virtual machines or
 * distributed computations.
 */
wikrt_err wikrt_cx_fork(wikrt_cx*, wikrt_cx**);

/** @brief Destroy a context and recover memory. */
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
 * character, e.g. 'v' expands to "v". ABCD may expand acyclically to
 * use more ABCD, but repeated efforts would ultimately expand into
 * plain old ABC.
 *
 * If an opcode is not recognized, NULL is returned.
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

/** @brief Obtain shallow type information for a value.
 *
 * This is potentially useful in cases where you might deal with many
 * types of data, e.g. rendering debug outputs. However, it is not
 * recommended in general. It is preferable that most code depend on
 * statically predictable types or dependent types, as this allows 
 * much more behavior to be modeled purely within ABC.
 */
typedef enum wikrt_vtype 
{ WIKRT_VTYPE_UNIT        // unit value
, WIKRT_VTYPE_PRODUCT     // product 
, WIKRT_VTYPE_INTEGER     // integer values
, WIKRT_VTYPE_SUM         // sum type (includes lists, arrays) 
, WIKRT_VTYPE_BLOCK       // functional bytecode  
, WIKRT_VTYPE_SEALED      // sealed values
// Special Cases
, WIKRT_VTYPE_PENDING     // ongoing evaluation
, WIKRT_VTYPE_STOWED      // value hidden
} wikrt_vtype;

wikrt_err wikrt_peek_type(wikrt_cx*, wikrt_vtype* out, wikrt_vref const*);

  ///////////////////////////
 // DATA INPUT AND OUTPUT //
///////////////////////////

/** @brief Allocate a unit value. */
wikrt_err wikrt_alloc_unit(wikrt_cx*, wikrt_vref**);

/** @brief Allocate a boolean value, i.e. unit in right or left.
 *
 * This is a convenience function, equivalent to wikrt_alloc_unit()
 * followed by wikrt_alloc_sum.
 */
wikrt_err wikrt_alloc_bool(wikrt_cx*, wikrt_vref**, bool inRight);

/** @brief Allocate a small binary. 
 *
 * The binary is modeled as a list of small integers (0..255). It may
 * use a more compact representation, e.g. an array of bytes.
 */
wikrt_err wikrt_alloc_binary(wikrt_cx*, wikrt_vref**, uint8_t const*, size_t);

/** @brief Read binary data from a list-like structure.
 *
 * This will read a list of small integers (0..255) into a buffer.
 * If we return WIKRT_OK, then we've read all available data. In
 * any other case, we'll return an error code (e.g. WIKRT_BUFFSZ)
 * but we'll still read as much data as possible. 
 *
 * The unread remainder of the list is returned in 'binary'. If we
 * reach the end of the list, this should be a sum in the right.
 */
wikrt_err wikrt_read_binary(wikrt_cx*, size_t buffsz, size_t* bytesRead, uint8_t* buffer, wikrt_vref** binary);

/** Allocate a small text. 
 *
 * Text must be valid utf-8, and valid as ABC text: no control characters
 * (C0, C1, DEL) except LF, no surrogate codepoints, no replacement char.
 * A text is modeled as a list of codepoints, but may use a more compact
 * representation under the hood.
 *
 * Note: You must provide the length of the text, e.g. via `strlen()` if
 * providing a C text.
 */
wikrt_err wikrt_alloc_text(wikrt_cx*, wikrt_vref**, char const*, size_t);

/** @brief Read text data from a list-like structure.
 *
 * Similar to wikrt_read_binary, except reads text into a utf-8 buffer.
 * The read text will be valid according to ABC's text constraints (no
 * control chars except LF, no surrogates, no incomplete chars, etc.)
 */
wikrt_err wikrt_read_text(wikrt_cx*, size_t buffsz, size_t* bytesRead, 
                          size_t* charsRead, char* buffer, wikrt_vref** text);

/** Allocate a block of Awelon Bytecode. */
wikrt_err wikrt_alloc_block(wikrt_cx*, wikrt_vref**, char const*, size_t, wikrt_abc_opts);

// Todo: read a block... may need special attention.

/** @brief Allocate small integers. */
wikrt_err wikrt_alloc_i32(wikrt_cx*, wikrt_vref**, int32_t);
wikrt_err wikrt_alloc_i64(wikrt_cx*, wikrt_vref**, int64_t);

/** @brief Read small integers. 
 *
 * The following have 'copy' semantics, i.e. the integer value referenced
 * is copied into the target buffer but remains available in the context.
 * In case of WIKRT_BUFFSZ errors, we'll appropriately return INT32_MIN,
 * INT32_MAX, INT64_MIN, or INT64_MAX based on whether we've overflowed or
 * underflowed.
 */
wikrt_err wikrt_peek_i32(wikrt_cx*, wikrt_vref const*, int32_t*);
wikrt_err wikrt_peek_i64(wikrt_cx*, wikrt_vref const*, int64_t*);

/** @brief Allocate and read arbitrary integers.
 *
 * The valid regex here is `0 | (-)?[1-9][0-9]*`, i.e. a simple and
 * relatively conventional decimal encoding of arbitrary integers.
 * This enables working with very large integers. Wikilon runtime 
 * doesn't guarantee support for more than a million digits, but
 * it will fail rather than return an incorrect result.
 *
 * When reading large integers, the `strlen` parameter is both an
 * input (the size of the available buffer) and an output. On WIKRT_OK
 * we output the number of characters written. On WIKRT_BUFFSZ, the
 * strlen reference returns the minimum buffer size.
 */
wikrt_err wikrt_alloc_istr(wikrt_cx*, wikrt_vref**, char const*, size_t strlen);
wikrt_err wikrt_peek_istr(wikrt_cx*, wikrt_vref const*, char* buff, size_t* strlen); 

/** @brief Allocate or disassemble basic product types (pairs of values). */
wikrt_err wikrt_alloc_prod(wikrt_cx*, wikrt_vref** p, wikrt_vref* fst, wikrt_vref* snd);
wikrt_err wikrt_split_prod(wikrt_cx*, wikrt_vref* p, wikrt_vref** fst, wikrt_vref** snd);

/** @brief Allocate or disassemble basic sum types (boolean choice of values). */
wikrt_err wikrt_alloc_sum(wikrt_cx*, wikrt_vref** c, bool inRight, wikrt_vref*);
wikrt_err wikrt_split_sum(wikrt_cx*, wikrt_vref* c, bool* inRight, wikrt_vref**);

// Thought: maybe have a match_left and match_right variant that disassembles
//   a sum, but fails if it is not on the appropriate side? See if there is a
//   good performance difference, first...

/** @brief Allocate to hide a value behind a sealer token.
 *
 * Our sealer must be a valid token. Sealer tokens should have type:
 * `∀e.(a*e)→((Sealed a)*e)`, for purpose of consistent quotation and
 * serialization. Once sealed, a value is inaccessible until unsealed.
 * so this serves as a basis for data hiding or structural typing.
 *
 * Wikilon runtime knows about discretionary sealers, e.g. {:map} and
 * {.map} serve a similar role to type wrappers. Short discretionary
 * sealers (four utf8-bytes or fewer, including prefix ':') have an
 * optimized compact representation, and should be encouraged in most
 * use cases.
 */
wikrt_err wikrt_alloc_seal(wikrt_cx*, wikrt_vref** sv, char const* s, size_t strlen, wikrt_vref* v); 

/** @brief Disassemble a sealed value into sealer token and value.
 *
 * Our buffer must have size at least WIKRT_TOK_BUFFSZ to avoid any
 * risks of buffer overfow.
 */
wikrt_err wikrt_split_seal(wikrt_cx*, wikrt_vref* sv, char* s, wikrt_vref** v);


/** @brief Copy a value. 
 *
 * Wikilon runtime favors linear 'move' semantics. A value reference is
 * used without aliasing or sharing. A benefit is that many functions 
 * can be implemented without allocation. The cost is that explicit
 * copies of values are necessary. In ABC, the copy operator '^' is 
 * naturally explicit.
 *
 * This copy function will use an ad-hoc strategy, essentially a deep copy
 * but with potential for lazy or copy-on-write strategies where Wikilon
 * runtime is likely to profit from doing so.
 *
 * Awelon Bytecode supports substructural types. Normally, a block marked
 * affine will not be copyable. But the C API is free to ignore such
 * constraints but must do so explicitly by indicating `bCopyAff`. 
 */
wikrt_err wikrt_copy(wikrt_cx*, wikrt_vref** cpy, wikrt_vref const* src, bool bCopyAff);

/** @brief Drop a value.
 *
 * When done with a value, drop it. This is roughly equivalent to use
 * of `free()` in C. However, each context tracks its own root set of
 * values. Destroying a context will implicitly drop remaining bound
 * values.
 *
 * Developers may freely ignore substructural 'relevance' constraints 
 * by indicating bDropRel. 
 */
wikrt_err wikrt_drop(wikrt_cx*, wikrt_vref*, bool bDropRel);

/** @brief Move a value between contexts.
 *
 * Developers must use `wikrt_move()` to use a value from another context, 
 * even if that value was created by `wikrt_cx_fork()`. Moving between forks
 * is a shallow operation. 
 */
wikrt_err wikrt_move(wikrt_cx* dest, wikrt_vref**, wikrt_cx* origin, wikrt_vref*);

/** @brief Copy a value between contexts.
 *
 * Move non-destructively. One might understand `wikrt_copy()` as a move_copy
 * with source and destination contexts set to the same value.
 */
wikrt_err wikrt_remote_copy(wikrt_cx* dest, wikrt_vref**, wikrt_cx const* origin, wikrt_vref const*, bool bCopyAff);

/** @brief Mark a value for stowage.
 *
 * Value stowage serves a role similar to virtual memory with more friendly
 * persistence and address-space properties. Values are loaded and stored
 * to disk as needed. Wikilon runtime uses an embedded database for this
 * feature - currently LMDB.
 */
wikrt_err wikrt_stow(wikrt_cx*, wikrt_vref** v);

// TODO: develop and implement streaming IO models
#if 0

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
wikrt_err wikrt_addend_stream(wikrt_cx*, wikrt_val s, uint8_t const* chunk, size_t size);
wikrt_err wikrt_end_stream(wikrt_cx*, wikrt_val s);
wikrt_err wikrt_awaiting_stream(wikrt_cx*, bool* bWaiting, wikrt_val s);


// TODO: consider integrating abc-base16 compression for `bdfghjkmnpqstxyz`
// strings. Consider supporting zstd or similar compression, too. These
// could be separated from texts, applied directly to binaries.


/** @brief Serialization for arbitrary values.
 *
 * Wrapping ABC text in a block provides an opportunity for the runtime
 * to simplify the code, perform partial evaluations, etc.. Converting
 * the block into text enables serialization of code.
 */
wikrt_err wikrt_text_to_block(wikrt_cx*, wikrt_val text, wikrt_val* block, wikrt_abc_opts);
wikrt_err wikrt_block_to_text(wikrt_cx*, wikrt_val block, wikrt_val* text, wikrt_abc_opts);

#endif

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
wikrt_err wikrt_alloc_eval(wikrt_cx*, wikrt_vref**, wikrt_vref* arg, wikrt_vref* fn);

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
 * The other likely return value is WIKRT_TYPE_ERROR, which will occur
 * in case of runtime type error or assertion failure. In general, there
 * is no recovery from a type error.
 */
wikrt_err wikrt_step_eval(wikrt_cx*, wikrt_vref** evaluation, uint32_t* effort);

/** @brief Quote a value into a block. v → [∀e. e → (v * e)]. (') */
wikrt_err wikrt_quote(wikrt_cx*, wikrt_vref** qv, wikrt_vref* v);

/** @brief Compose two blocks. [a → b] → [b → c] → [a → c]. (o) */
wikrt_err wikrt_compose(wikrt_cx*, wikrt_vref** ac, wikrt_vref* ab, wikrt_vref* bc);

// Note: Originally I was planning to support 'token stops' so we could
// extend the runtime with token-driven effects. However, I'm thinking it
// may be better to model effects in other ways (e.g. monadically).
//
// Also, Wikilon itself won't use token stops so this feature would be
// very premature.

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
wikrt_err wikrt_icmp(wikrt_cx*, wikrt_val, wikrt_ord*, wikrt_val);


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
 * nicely with large value stowage. A transaction is required to 
 * access and update a bound value.
 *
 * A transaction is currently represented within a context like a
 * value. It is modified by most operations, unlike a value.
 */
typedef wikrt_vref wikrt_txn;

/** @brief Validate transaction key.
 *
 * Transaction keys must be valid texts of limited size, having at
 * most WIKRT_VALID_KEY_MAXLEN bytes in the utf-8 encoding.
 */
bool wikrt_valid_key(char const*);

#define WIKRT_VALID_KEY_MAXLEN 255

/** @brief Create a new transaction on our key-value database. */
wikrt_err wikrt_txn_create(wikrt_cx*, wikrt_txn** dest);

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
wikrt_err wikrt_txn_swap(wikrt_cx*, wikrt_txn*, char const* key, wikrt_vref** val);

/** @brief Abort and drop the transaction. */
void wikrt_txn_abort(wikrt_cx*, wikrt_txn*);

/** @brief Attempt to commit a transaction.
 *
 * Commit may fail for a variety of reasons, the most likely being
 * conflict with a concurrent transaction. 
 */
wikrt_err wikrt_txn_commit(wikrt_cx*, wikrt_txn*);

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
void wikrt_txn_durable(wikrt_cx*, wikrt_txn*);

// Todo: consider specialized 'variable' alternatives for 
//  stream processing, queues, logs, or similar?

#define WIKILON_RUNTIME_H
#endif
