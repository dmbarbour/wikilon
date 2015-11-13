{-# LANGUAGE DeriveDataTypeable #-}
-- | The 'head' of a branching dictionary. This tightly couples the
-- dictionary content with an up-to-date reverse lookup index and a
-- version number (an update time).
module Wikilon.Dict.Head
    ( DictHead
    , dhCreate
    , dhDict
    , dhRLU
    , dhMod
    , dhUpdate
    , module Wikilon.Dict
    , module Wikilon.Dict.RLU
    , module Wikilon.Time
    ) where

import Control.Applicative
import Data.Typeable (Typeable)
import Database.VCache
import Wikilon.Dict.RLU
import Wikilon.Dict
import Wikilon.Time

-- | The head of the dictionary contains the dictionary value and a
-- reverse lookup index that is kept up-to-date. I'm not keeping this
-- index in the dictionary itself for performance reasons.
--
-- The assumption is that the dict head is the most frequently
-- accessed resource for a dictionary. 
data DictHead = DictHead
    { d_dict :: Dict    -- word lookup, primary data
    , d_rlu  :: DictRLU -- reverse token lookup
    , d_mod  :: T       -- modified time, version 
    } deriving (Typeable)

dhCreate :: VSpace -> T -> DictHead
dhCreate vc tmCreate = DictHead (dictCreate vc) (rluCreate vc) tmCreate

-- | The primary value associated with the dictionary.
dhDict :: DictHead -> Dict
dhDict = d_dict

-- | Reverse lookup index is always kept up-to-date.
dhRLU :: DictHead -> DictRLU
dhRLU = d_rlu

-- | When was the dictionary last updated? 
dhMod :: DictHead -> T
dhMod = d_mod

-- | Update the dictionary, automatically maintaining the RLU.
dhUpdate :: DictHead -> Dict -> T -> DictHead
dhUpdate dh dNew tUpd = 
    let dOld = dhDict dh in
    let dd = dictDiff dNew dOld in
    let rlu' = rluUpdate (rluDiff dd) (dhRLU dh) in
    DictHead { d_dict = dNew, d_rlu = rlu', d_mod = tUpd }

instance VCacheable DictHead where
    put (DictHead d rlu tMod) = putWord8 1 >> put d >> put rlu >> put tMod
    get = getWord8 >>= \ v -> case v of
        1 -> DictHead <$> get <*> get <*> get
        _ -> fail "Wikilon.DictSet - unknown DictHead version"

