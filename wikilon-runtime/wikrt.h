/** This is the internal header for Wikilon runtime. 
 */

#include "wikilon-runtime.h"



/** Address Bits
 *  32 bits: 8-byte aligned addresses
 *    low bits xy0     small numbers
 *    low bits 001     tagged object
 *    low bits 101     product in left
 *    low bits 011     product in right
 *    low bits 111     product value
 *    (address 0 product = unit value)
 *
 *  64 bits: 16-byte aligned addresses (proposed)
 *    low bits xyz0     small numbers
 *    low bits 0001     tagged objects
 *    low bits 1111     normal products
 *    six options: L,R, LL, LR, RL, RR
 *    (maybe shave bits from small numbers for more options?)
 */ 
