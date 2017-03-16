#pragma once
#include <stdint.h>
#include <stdbool.h>

/** A Shared Memory Ephemeron Table 
 * 
 * The ephemeron table will track a collection of reference values
 * in use across multiple processes, the intention being to guide 
 * garbage collection of these values within a shared database.
 * False positives are acceptable if they are rare.
 * 
 * I've tried this once with a counting bloom filter (CBF) but that
 * proves to be inefficient. I need to support large counts because
 * we tend to reference popular resources from many processes.
 * 
 * The current implementation is a simple counting hash table. The
 * elements recorded will be hash values, and we'll only record a
 * hash value together with a count for that hash. False positives
 * are then determined by the number of hash bits rather than the
 * number of elements. OTOH, we must resize the table on occasion.
 */  
typedef struct wikrt_eph wikrt_eph;

/** Load or Create the Ephemeron Table
 *
 * The argument here is a filename that provides the shared memory
 * address. If the file does not exist, it will be created and a
 * random address created. File locking is used as the outermost
 * lock to guard creation or destruction of the shared memory.
 */
wikrt_eph* wikrt_eph_open(char const* ephid_file_name, int mode);

/** Test Ephemeron Table for Error State
 *
 * An ephemeron table can potentially enter an error state, becomes
 * disconnected from shared memory due to a failed resize. In this
 * state, adds and removes do nothing, and tests return true. This 
 * does meet ephemeron table invariants, so it's fail-safe, but the
 * false positive rate will be very high, so it becomes useless.
 *
 * This function tests whether the table has entered the error state.
 * If so, you can raise an appropriate warning and close the table.
 * You can still lock, unlock, and close the table in error state.
 */
bool wikrt_eph_err(wikrt_eph const*);

/** Release the Ephemeron Table
 *
 * This will drop the current reference to the ephemeron table. If
 * this was the final reference to the table, we'll also release 
 * shared memory resources.
 */
void wikrt_eph_close(wikrt_eph*);

/** Lock or Unlock the Ephemeron Table
 *
 * You must lock the table before adding, testing, removing elements.
 * Be sure to unlock afterwards. Locking uses a robust pthreads mutex,
 * with some recovery logic in case another process or thread halts
 * in the middle of an important action like resizing the table.
 *
 * This lock is not recursive, and be careful to avoid deadlocks.
 */
void wikrt_eph_lock(wikrt_eph*);
void wikrt_eph_unlock(wikrt_eph*);

/** Add elements to the Ephemeron Table */
void wikrt_eph_add(wikrt_eph*, uint64_t);

/** Check for elements in the Ephemeron Table
 * 
 * This operation tests whether an element is probably part of the table.
 * False positives are possible, but should be very rare. False negatives
 * aren't possible. 
 */
bool wikrt_eph_test(wikrt_eph const*, uint64_t);

/** Remove elements from the Ephemeron Table
 *
 * This will remove an element from the table. An element is counted,
 * and must be removed as many times as added for eph_test to return
 * false. If a count overflows it cannot be removed, but the overflow
 * threshold is high (over a million) so should be a non-issue.
 */
void wikrt_eph_rem(wikrt_eph*, uint64_t);

