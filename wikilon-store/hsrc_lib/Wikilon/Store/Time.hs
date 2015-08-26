{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | Time with orphaned VCacheable instances 
-- plus simple access as an STM variable...
module Wikilon.Store.Time
    ( module Wikilon.Time
    ) where

import Control.Applicative
import Wikilon.Time
import Database.VCache

-- convenient persistence for time values
instance VCacheable T where
    get = T . fromInteger <$> get
    put (T tm) = put (toInteger tm)
instance VCacheable DT where
    get = DT . fromInteger <$> get
    put (DT dt) = put (toInteger dt)



