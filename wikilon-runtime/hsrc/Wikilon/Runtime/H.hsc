{-# LANGUAGE ForeignFunctionInterface, DeriveDataTypeable #-}


module Wikilon.Runtime.H
    (
    ) where

#include "wikilon-runtime.h"

import Foreign
import Foreign.C
