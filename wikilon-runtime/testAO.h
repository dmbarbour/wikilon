/* I can easily extract code from AO into a C string. But it might
 * be more convenient to keep it closer to the original AO code. So
 * this file contains a few simple programs expressed in AO-like code
 * as C macros.
 */

#pragma once

#define ABC_I   "vr$c"
#define ABC_FIX "[^'mw^'zmw" ABC_I "]^'mw'm"
#define ABC_IF  "wlD[rwl]?Mrr%" ABC_I
#define ABC_DIP "wvrwlcr$vrwlc"
#define ABC_DIP2 "wzlw" ABC_DIP "r"
#define ABC_REPEAT_BODY "zw#G[%%%][r%#1-+wz" ABC_DIP2 ABC_I "]" ABC_IF
#define ABC_REPEAT "w'[" ABC_REPEAT_BODY "]wm" ABC_FIX ABC_I

