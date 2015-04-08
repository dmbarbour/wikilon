{-# LANGUAGE DeriveDataTypeable #-}

module Wikilon.StateHist
    ( StateHist
    ) where

import Control.Applicative
import Data.Typeable (Typeable)
import Database.VCache
import Wikilon.Hist

-- | A StateHist is a history with a current value. The (T,val)
-- entry in the previous history, then, represents a previous
-- value and the time when that value *ended*.
data StateHist a = StateHist
    { _curr :: !a          -- current value
    , _prev :: !(Hist a)   -- older entries
    } deriving (Typeable)

instance (VCacheable a) => VCacheable (StateHist a) where
    put (StateHist curr prev) = put curr >> put prev
    get = StateHist <$> get <*> get
