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
 *  LICENSE: BSD 3-clause
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

/** @brief Opaque structure representing a heap for computations.
 *
 * A context is a fixed-size arena for computations. Wikilon runtime
 * limits contexts to four gigabytes in size (i.e. 32-bit address). 
 * This is the limit for 'active' memory resources. A computation may
 * access more than four gigabytes of data via large value stowage.
 *
 * Use of distributed processing models (where a computation uses 
 * many contexts) may be explored in the future. 
 * 
 */
typedef struct wikrt_cx wikrt_cx;

/** @brief A value reference into a context. 
 *
 * Wikilon's value model is based on Awelon Bytecode. Basic values
 * include integers, products, unit, sums, and blocks. Special case
 * data includes sealed values, stowed values, pending computations,
 * arrays, texts, and binaries. The latter three are specializations
 * of simple lists.
 *
 * Wikilon values are generally represented within a context and are
 * referenced by 32-bit addresses. However, flag bits enable direct
 * encoding of small integers, units, and booleans (unit + unit) in
 * the value reference. Here 'small integers' includes the range of
 * plus or minus one billion.
 *
 * Wikilon runtime optimizes for representing (a*b) pairs and shallow
 * sums of pairs `(a*b)+(c*d)`. This enables compact representation of
 * lists and many (node+leaf) tree structures. Deep sums are also
 * compactly represented, i.e. such that a sum of depth twelve costs 
 * no more space than a sum of depth two.
 *
 * Wikilon runtime assumes linear references. A value reference has
 * clear ownership, with a few special exceptions where reference 
 * counts will be used under the hood.

 * only a single owner. Under this assumption, semantically immutable
 * values may be implicitly mutated in place. The mutation will not
 * be observable because there is no shared reference. There are some
 * special exceptions (arrays, recently stowed values) where sharing
 * is performed.
 * 
 * T special exceptions do share data references. For sharing values include arrays, stowed values,
 *  to large values:
 * arrays, stowed values, etc.. In these cases, atomic reference 
 * counting is used under the hood.
 * 
 */ 
typedef uint32_t wikrt_val;

/** @brief Reference to a value in a context.
 *
 * The value model is oriented around Awelon Bytecode (ABC) and Awelon
 * Object (AO). The five basic value types are integers, products, sums,
 * units, and blocks of bytecode (which serve as first-class functions).
 *
 * We further have value sealing, substructural types, optimized list
 * representations (arrays, binaries, texts), large value stowage, and 
 * pending values (parallelism, laziness, streaming bytecode). 
 *
 * Wikilon runtime is oriented around 'move' semantics. A value has a
 * single 'owner' at a time, who is wholly responsible for manipulating
 * and observing it. As a benefit, this allows many functions to be
 * non-allocating. Unfortunately, it also means that 

But it does mean we use deep copies on values when copies occur.
 * Only stowed values
 *
 * 

This enables
 * many pure functions to be implemented with non-allocating mutations. 
 * The cost is that 'copying' a value requires actually copying it. The 
 * presence of affine and relevant substructural types discourages AO and
 * ABC programs from using 'copy' and 'drop' for generic programming. 
 *
 * Move semantics applies also to values created through this C API. If
 * you create a value, you own it until you pass it on. You can drop the
 * value if you don't need it anymore. 
 */


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
, WIKRT_BUFFSZ          // output buffer too small

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
 * 
 * Use a negative delta to decref.
 */
void wikrt_env_incref(wikrt_env*, int delta);

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
 * If a context is used concurrently, threads should use reference
 * counts to guard against untimely destruction of the context. A
 * newly created context has refct=1, and reducing refct to zero
 * will implicitly destroy the context.
 *
 * Explicitly destroying the context will reduce refct by one and
 * also free up as much memory as possible, so other threads will
 * begin to see WIKRT_NOLINK errors but not crash.
 concurrent destruction of a context is a risk, threads should
 * maintain a reference to their context. This doesn't prevent the
 * destruction, but rather ensures WIKRT_NOLINK errors instead of
 * crashing or undefined behavior.
 *
 * A context implicitly has refct=1 when created. Destroying the context
 * will implicitly decrement its refct; conversely, if the refct is
 * decremented to zero, the context is implicitly destroyed. Use a
 * negative delta to decref.
 */
void wikrt_cx_incref(wikrt_cx*, int delta);

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

  ////////////////////////////
 // VALUE CONSTRUCTION API //
////////////////////////////

/** Construct unit value in context. */
wikrt_err wikrt_alloc_unit(wikrt_val* out, wikrt_cx*);

/** @brief Construct integer in context.
 *
 * Note: Small integers within range of plus or minus one billion
 * will typically be stored in the address field (via flag bits and
 * alignment of addresses). But larger integers certainly require
 * an allocation.
 */
wikrt_err wikrt_alloc_int(wikrt_val* out, wikrt_cx*, int);
wikrt_err wikrt_alloc_i32(wikrt_val* out, wikrt_cx*, int32_t);
wikrt_err wikrt_alloc_i64(wikrt_val* out, wikrt_cx*, int64_t);

/** @brief Construct integer from a string representation.
 *
 * Expected regex: 0 | (-)?(1-9)(0-9)*
 *
 * Only decimal numbers are supported.
 */
wikrt_err wikrt_alloc_istr(wikrt_val* out, wikrt_cx*, char const*);

/** @brief Construct a product or pair (fst * snd) of values. 
 *
 * Both elements of the pair must be from the same context or we'll return
 * WIKRT_INVAL. The arguments are similar to those for wikrt_pop_prod, 
 * except the inputs and outputs are inverted.
 */
wikrt_err wikrt_alloc_prod(wikrt_val* out, wikrt_val* fst, wikrt_val* snd);

/** @brief Push values onto a stack.
 *
 * In this case, we construct the form (a*(b*(c*(d*e)))). The arguments
 * are similar to those for wikrt_pop_stack, except the inputs and outputs
 * are inverted.
 */
wikrt_err wikrt_alloc_stack(wikrt_val* out, size_t nStackElems, wikrt_val* pValArray, wikrt_val* initialStack);

/** @brief Construct a sum. 
 *
 * Wikilon runtime currently optimizes a single level of sums for unit
 * and product values, i.e. such that (a*b) in the left or unit in the
 * right are recorded in the reference and require the same amount of
 * space as (a*b) or unit. This optimizes representation of lists and
 * of simple (node + leaf) tree structures.
 *
 * For deeper sums, Wikilon runtime will also pack several levels per
 * allocation. Consequently, sums aren't expensive in terms of space.
 */
wikrt_err wikrt_alloc_sum(wikrt_val* out, bool bInRight, wikrt_val* val);

/** @brief Push values onto a list. 
 * 
 * These variations invert pop_list, pop_binary, pop_text functions.
 * Except we don't actually need the maximum buffer sizes, only the
 * actual number of elements. 
 *
 * For texts, we may return WIKRT_INVAL if the text is not valid.
 * See wikrt_text_valid.
 */
wikrt_err wikrt_alloc_list(wikrt_val* out, wikrt_val* pValArray, size_t nListElems, wikrt_val* initialList);
wikrt_err wikrt_alloc_binary(wikrt_val* out, unsigned char const* data, size_t nBytes, wikrt_val* initialList);
wikrt_err wikrt_alloc_text(wikrt_val* out, char const* text, wikrt_val* initialList);

/** @brief Test whether a text is valid for Wikilon runtime.
 *
 * Wikilon runtime expects texts to have a utf-8 encoding, presented
 * as a NUL-terminated C string, with the following constraints:
 *
 * - no control chars (C0, DEL C1) except LF
 * - no surrogate codepoints (U+D800 to U+DFFF)
 * - no replacement character (U+FFFD)
 * 
 */
bool wikrt_text_valid(char const*);


/** @brief Wrap a value with a token.
 *
 * You must provide a sealing token text, see wikrt_tokvalid for the
 * constraints. If the token was not valid, WIKRT_INVAL is returned.
 * This is an ownership passing function, so the value is both input
 * and output.
 *
 * The token is assumed to have type `∀e.(a * e) → (wrapped-a * e)`.
 * This affects how the wrapped value is serialized. Discretionary
 * sealers are the prototypical use case: `#42{:foo}`. Cryptographic
 * data and other special cases would instead use sealer annotation
 * {$&AES}. 
 *
 * Wikilon runtime understands discretionary sealers and sealing 
 * annotations, and will wrap values automatically when those are
 * encountered in a normal bytecode stream. Wikilon runtime also
 * knows discretionary unsealers, i.e. `{.foo}` unwraps a value
 * previously wrapped with `{:foo}`. 
 */
wikrt_err wikrt_alloc_seal(char const* tok, wikrt_val* val);

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
bool wikrt_token_valid(char const* tok);




  //////////////////////////
 // COMMON DATA PLUMBING //
//////////////////////////

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
 * to ignore affine properties, simply set `bCopyAff` to true. Errors
 * due to affine properties are latent in case of pending values.
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
 * the `bDropRel` flag should be set to true. 
 */
wikrt_err wikrt_drop(wikrt_val*, bool bDropRel);

  ////////////////////////////
 // STREAMING BYTECODE API //
////////////////////////////

/** @brief Construct a stream of bytecode.
 *
 * A stream of bytecode is an open-ended block of bytecode, of some
 * unknown size. The protocol: `alloc append* close`. If you're only
 * going to append at most once, consider directly allocating a block
 * instead.
 *
 * The stream may be used almost anywhere a block may be used - copied, 
 * applied, evaluated, quoted, etc.. But it qualifies as a PENDING value
 * until closed, and cannot be observed as a normal value.
 */
wikrt_err wikrt_stream_alloc(wikrt_val* out, wikrt_cx*);

/** @brief Append bytecode to our stream.
 *
 * Wikilon uses Awelon Bytecode (ABC), provided here as a NUL-terminated
 * C string (with UTF-8 encoding). We may later support ABCD extensions 
 * for concision, but doing so isn't a high priority at this time.
 *
 * Under the hood, Wikilon may simplify and partially evaluate bytecode.
 * But behavior for valid inputs will be preserved.
 *
 * Provided bytecode may be incomplete, e.g. stopping in the middle of
 * a block, embedded text, or character. The intention with streams is
 * to support buffered processing of bytecode input.
 */
wikrt_err wikrt_stream_append(wikrt_val* s, char const* bytecode);

/** @brief Close a bytecode stream.
 *
 * When you're done adding bytecode to a stream, close it. This is
 * necessary to complete applications, observe 
 */

/** @brief Construct a block of bytecode.
 *
 * This is equivalent to opening, filling, and closing a stream in
 * one step. This is useful if you know the bytecode ahead of time
 * and it isn't too large to easily represent in memory.
 */



  ///////////////////////////////////////////
 // GENERIC VALUE ACCESS AND CONSTRUCTION //
///////////////////////////////////////////


/** @brief Shallow analysis of value types, for switching. 
 *
 * Outside of specialized cases (e.g. debug output or rendering, and
 * modeling reflection) we should try to stick with statically typed
 * idioms even when accessing values through the C API. Use of this
 * function is an indicator that the C API is doing dynamically typed
 * behaviors.
 */
typedef enum wikrt_val_type 
{ WIKRT_VAL_UNIT = 1    // unit value
, WIKRT_VAL_PRODUCT     // product 
, WIKRT_VAL_INT         // integer values
, WIKRT_VAL_SUM         // sum type (includes lists) 
, WIKRT_VAL_BLOCK       // functional bytecode  
, WIKRT_VAL_SEALED      // sealed values
// special cases
, WIKRT_VAL_STOWED      // fully stowed values
, WIKRT_VAL_PENDING     // lazy or parallel computation
} wikrt_val_type;

/** Obtain shallow analysis of value type. */
wikrt_err wikrt_peek_type(wikrt_val const*, wikrt_val_type* out);

/** @brief Peek into a product of values. 
 *
 * This takes a simple structure of the type (a*b) and returns `a`
 * in fst and `b` in snd. The original structure is preserved, so
 * the caller needs to be careful about managing ownership. If the
 * value was not a product, you'll receive WIKRT_TYPE_ERROR.
 */
wikrt_err wikrt_peek_prod(wikrt_val const* p, wikrt_val* fst, wikrt_val* snd);

/** @brief Peek into a stack of values.
 *
 * A stack has the shape (a*(b*(c*(d*e)))). If you ask for two elements,
 * you'd get `a` and `b` in the array, and `(c*(d*e))` in the remainder.
 * If you ask for more elements than the stack contains, or if the argument
 * is not a stack, you'll receive WIKRT_TYPE_ERROR.
 *
 * Aside: With nStackElems = 1, this is equivalent to wikrt_peek_prod.
 */
wikrt_err wikrt_peek_stack(wikrt_val const* stack, size_t nStackElems, wikrt_val* pValArray, wikrt_val* rem);

/** @brief Peek at a sum value.
 *
 * Wikilon runtime packs multiple sum tags into a single cell. While
 * this is convenient for performance, it does hinder iterative access
 * to a sum without allocating a new sum tag. And it's essential that
 * peek be non-allocating.
 *
 * So, we instead peek with a fair amount of depth at once, returning 
 * a string of the form "LRLLLRLLRLLLRRR". This represents the route
 * to reach the value, reading 'left right left left left right ...'.
 * If you have a buffer size of at least WIKRT_SUM_BUFFSZ, you will
 * be able to guarantee that we can unpack at least one step. If
 * there isn't sufficient space to peek, we'll return WIKRT_BUFFSZ.
 *
 */
wikrt_err wikrt_peek_sum(wikrt_val const* sum, size_t nPathBytes, char* path, wikrt_val* val);

/** @brief Path buffer size for wikrt_peek_sum to guarantee progress. */
#define WIKRT_SUM_BUFFSZ 32


/** @brief Access a list of values non-destructively.
 *
 * NOTE: currently this API is broken because, in general, it requires
 * allocating a value in place of `rem` (e.g. when peeking into a text).
 * I may need, as with sum types, to align with natural 'chunking' of a
 * list.
 *
 * A list has the structure `λa.λb.μL.((a*L)+b)`. Some lists may use
 * a specialized representation, e.g. an array under the hood, but
 * this representation is transparent to list processing functions.
 * Annotations can control and enforce specific representations.
 *
 * A binary is a list of small integers in 0..255. A text is a list
 * of Unicode codepoints (0..1114111) with some exceptions (control
 * characters, surrogates, replacement char). Texts are always valid
 * C strings because NUL is among the characters forbidden.
 *
 * The following peek functions will copy a finite number of elements
 * from the head of the list into a local buffer for the caller. The
 * remainder of the list (or the terminal in the right) is returned 
 * via `rem`. If there is any type error, we return WIKRT_TYPE_ERROR,
 * but we also return as many elements up to the point as feasible.
 * The remaining list including the error is returned in `rem`.
 */
//wikrt_err wikrt_peek_list(wikrt_val const* lst, size_t nMaxElems, wikrt_val* pValArray, size_t* nListElems, wikrt_val* rem);
//wikrt_err wikrt_peek_binary(wikrt_val const* binary, size_t nMaxBytes, unsigned char* dst, size_t* nBytes, wikrt_val* rem);
//wikrt_err wikrt_peek_text(wikrt_val const* txt, size_t nMaxBytes, char* dst, size_t* nBytes, size_t* nChars, wikrt_val* rem);

/** @brief Read relatively small integers.
 *
 * If the destination isn't large enough we'll return WIKRT_BUFFSZ.
 * If the argument isn't an integer, we'll return WIKRT_TYPE_ERROR.
 */
wikrt_err wikrt_peek_int(wikrt_val const*, int*);
wikrt_err wikrt_peek_i32(wikrt_val const*, int32_t*);
wikrt_err wikrt_peek_i64(wikrt_val const*, int64_t*);

/** @brief Read a large integer into a text.
 *
 * Regex: 0 | (-)?(1-9)(0-9)*       (NUL terminated)
 * 
 * If the output buffer is not of sufficient size, we'll return a
 * WIKRT_BUFFSZ. Calling wikrt_peek_isize will return the required
 * buffer size (including space for the NUL terminator).
 *
 * If the argument is not an integer, these return WIKRT_TYPE_ERROR.
 */
wikrt_err wikrt_peek_istr(wikrt_val const*, char* dst, size_t nMaxChars);
wikrt_err wikrt_peek_isize(wikrt_val const*, int* nBuffSize);

/** @brief Peek into a token-wrapped value. 
 *
 * The sealing token is copied into tok as a NUL-terminated C string.
 * If the output buffer is too small, we'll return WIKRT_BUFFSZ. A 
 * buffer of  WIKRT_TOK_BUFFSZ will prevent buffer size issues.
 *
 * If argument was not a sealed value, WIKRT_TYPE_ERROR is returned,
 * the token text is empty, and the value is simply copied to the output.
 * 
 * Note: This operation is a form of reflection, like wikrt_peek_type.
 * The same caveats apply: it's preferable to avoid this feature outside
 * of special cases like rendering values for debugging.
 */
wikrt_err wikrt_peek_seal(wikrt_val const*, size_t nMaxBytes, char* tok, wikrt_val* ); 

/** @brief 'Pop' variants dismantle their argument. 
 *
 * For many wikrt_peek_X functions, there is a similar wikrt_pop_X
 * function that dismantles the argument. This provides the caller
 * ownership of the returned elements.
 *
 * For the wikrt_pop_list variants, WIKRT_TYPE_ERROR still allows
 * partial success that pops elements off the list up to the point
 * of type error, which is returned in `rem`.
 *
 * Because 'pop' is destructive, sums can be accessed this way one
 * small step at a time (i.e. nPathBytes = 2).  to how sums are modeled, sums can be accessed via pop with
 * much smaller steps.  safely done
 * just one small step at a time. 
 */
wikrt_err wikrt_pop_prod(wikrt_val* p, wikrt_val* fst, wikrt_val* snd);
wikrt_err wikrt_pop_stack(wikrt_val* stack, size_t nStackElems, wikrt_val* pValArray, wikrt_val* rem);
wikrt_err wikrt_pop_list(wikrt_val* lst, size_t nMaxElems, wikrt_val* pValArray, size_t* nListElems, wikrt_val* rem);
wikrt_err wikrt_pop_binary(wikrt_val* binary, size_t nMaxBytes, unsigned char* dst, size_t* nBytes, wikrt_val* rem);
wikrt_err wikrt_pop_text(wikrt_val* txt, size_t nMaxBytes, char* dst, size_t* nBytes, size_t* nChars, wikrt_val* rem);
wikrt_err wikrt_pop_seal(wikrt_val* sealedVal, size_t nMaxBytes, char* tok, wikrt_val* unsealedVal);
wikrt_err wikrt_pop_sum(wikrt_val* sum, size_t nPathBytes, char* path, wikrt_val* val);

/** @brief Maximum buffer size for a token.
 *
 * The maximum token size from Awelon Bytecode is 63 bytes. Wikilon
 * runtime adds a byte for a NUL-terminator to support C strings. 
 * The token does not include the `{}` braces, just the text between
 * them.
 */
#define WIKRT_TOK_BUFFSZ 64



// TODO:
//   annotations
//     arrays, assertions
//   quoting values
//   composition of blocks
//   substructural types
//   floats and doubles
//     once we have accelerators
//   

  /////////////////////////////////////
 // DATABASE AND PERSISTENCE ENGINE //
/////////////////////////////////////

/** @brief Transactional Persistence
 *
 * The Wikilon environment includes a simple key-value database for
 * use as a persistence layer that integrates easily with large value
 * stowage. Transactions enable consistent views of multiple keys and
 * atomic updates. Durability is optional per transaction.
 *
 * Keys are texts with limited size.
 *
 * Note: This database is not implicitly accessible to ABC computations.
 * Access may be modeled explicitly, e.g. as a free monadic effect, like
 * any other effect. Semantics for the persistence layer are up to each
 * application.
 */
typedef struct wikrt_txn { wikrt_val txn; } wikrt_txn;
#define WIKRT_DB_KEY_SIZE_MAX 255

/** @brief Test validity of a proposed key.
 *
 * Keys must be valid texts, with an additional constraint of at
 * most 255 bytes in length.  
 */
bool wikrt_key_valid(char const*);

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
wikrt_err wikrt_txn_read(wikrt_txn*, char const* key, wikrt_val* dest, bool bCopyAff);

/** @brief Write value into our key-value persistence layer.
 * 
 * Ownership of the value is given to the transaction. This implicitly
 * acts as a 'drop' operation on the value previously associated with 
 * the key. Unless `bDropRel` is set, we may fail due to dropping a
 * non-droppable value. This failure may be latent, on commit.
 */
wikrt_err wikrt_txn_write(wikrt_txn*, char const* key, wikrt_val* val, bool bDropRel);

/** @brief Exchange a value from our key-value persistence layer.
 *
 * Swap a value currently bound to a key with the value provided. This
 * guarantees protection for substructural types and avoids intermediate
 * copies during a transaction that updates a value many times (i.e. you
 * can swap for a dummy value like unit).
 */
wikrt_err wikrt_txn_swap(wikrt_txn*, char const* key, wikrt_val* val);

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
void wikrt_env_sync(wikrt_env*);


#define WIKILON_RUNTIME_H
#endif
