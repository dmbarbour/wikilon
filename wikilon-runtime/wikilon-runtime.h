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
 *  Locking: The LMDB database is only safe for a single application,
 *  and only a single wikrt_env within that application. This is 
 *  This is guarded by a simple file lock via flock(2), which is not
 *  reliable on a network partition. The assumption is that Wikilon
 *  will normally be the primary user of a particular filesystem.
 *  
 *  Hot Backup: Currently not supported. It may be useful to model
 *  this explicitly.
 *
 *  @section license_sec License
 *
 *  (c) 2015 David Barbour
 *  LICENSE: BSD
 *    (todo: copy license)
 *
 */


#include <stdint.h>

// Our 'wikrt_env' correspo
typedef struct wikrt_env wikrt_env;
typedef struct wikrt_cx  wikrt_cx;

// just testing
void wikrt_hello(char const * s);



