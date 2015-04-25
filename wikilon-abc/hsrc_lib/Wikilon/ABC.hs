{-# LANGUAGE BangPatterns, ViewPatterns, OverloadedStrings, DeriveDataTypeable #-}

-- | Wikilon's performance-tweaked internal Awelon Bytecode.
module Wikilon.ABC
    ( module Wikilon.ABC.Code
    , module Wikilon.ABC.Value
    , module Wikilon.ABC.Eval
    , module Wikilon.ABC.EncVal
    ) where

import Wikilon.ABC.Code
import Wikilon.ABC.Value
import Wikilon.ABC.Eval
import Wikilon.ABC.EncVal
