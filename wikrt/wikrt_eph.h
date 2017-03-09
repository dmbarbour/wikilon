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
 * random address provided. File locking is used as the outermost
 * lock to guard creation or destruction of the shared memory.
 */
wikrt_eph* wikrt_eph_open(char const* ephid, int mode);

/** Release the Ephemeron Table
 *
 * This will drop the current reference to the ephemeron table. If
 * this was the final reference to the table, we'll also release 
 * shared memory resources.
 */
void wikrt_eph_close(wikrt_eph*);

/** Lock or Unlock the Ephemeron Table
 *
 * You should lock the table before adding or checking for elements.
 * Be sure to unlock afterwards. Locking uses a robust pthreads mutex,
 * with some recovery logic in case a process or thread fails in the
 * middle of resizing the table.
 *
 * This lock is not recursive. Be careful to avoid deadlocks.
 */
void wikrt_eph_lock(wikrt_eph*);
void wikrt_eph_unlock(wikrt_eph*);

/** Add elements to the Ephemeron Table
 *
 * This operation may fail, returning false, if the table could not be
 * resized. The table must be locked by the current thread before we
 * add anything to it. 
 */
bool wikrt_eph_add(wikrt_eph*, uint64_t);

/** Check for elements in the Ephemeron Table
 * 
 * This operation tests whether an element is probably part of the table.
 * False positives are possible, but should be very rare. False negatives
 * aren't possible, assuming the value was added successfully.
 */
bool wikrt_eph_test(wikrt_eph*, uint64_t);

/** Remove elements from the Ephemeron Table
 *
 * This will remove an element from the table, albeit only if the count for
 * that element's hash has not reached an overflow status. Up to a limit,
 * if an element is removed as many times as added, it will be cleared from
 * the table such that further tests report the element is not present.
 *
 * The limit is reasonably large, over a million.
 *
 * This operation returns 'false' if the element was not counted within the
 * table.
 */
bool wikrt_eph_rem(wikrt_eph*, uint64_t);

