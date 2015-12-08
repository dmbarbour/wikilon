
{-# LANGUAGE ForeignFunctionInterface #-}
-- | This module wraps the `wikilon-runtime.h` for `libwikilon-runtime.so`.
-- The high performance core of our Wikilon runtime is implemented at the C
-- layer. 
module Wikilon.Runtime.H
    ( wikrt_hello
    ) where

#include <wikilon-runtime.h>

import Foreign
import Foreign.C

-- FFI
--  'safe': higher overhead, thread juggling, allows callbacks into Haskell
--  'unsafe': lower overhead, reduced concurrency, no callbacks into Haskell

-- | for now, just providing enough to test our 
foreign import ccall unsafe "wikilon-runtime.h wikrt_hello" _wikrt_hello :: CString -> IO ()

wikrt_hello :: String -> IO ()
wikrt_hello = flip withCString _wikrt_hello

