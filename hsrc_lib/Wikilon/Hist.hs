{-# LANGUAGE DeriveDataTypeable #-}

-- | A very simple, time-ordered VCache-layer list with recent values
-- near the head. The values here may represent events or states, so
-- there isn't much semantic content. The main purpose for Hist is to
-- help control VCache chunking for systems that prefer to access the
-- recent values rather than the older ones.
module Wikilon.Hist
    ( Hist
{-
    , empty
    , insert

    , null
    , length
    
    , trim
    , decay

    , toList
    , fromList
-}
    , hist_space
    , module Wikilon.Time
    ) where

import Prelude hiding (length, null)
import Control.Exception (assert)
import Control.Applicative
import Data.Typeable (Typeable)
import qualified Data.List as L
import Database.VCache
import Wikilon.Time (T(..), DT)

-- | A finite history preserved in VCache. Assumes that recent history
-- is more frequently accessed than ancient history. 
type Hist a = Hist0 a

-- versioned model for history persistence
data Hist0 a = Hist0
    { h_space   :: !VSpace
    , h_countR  :: {-# UNPACK #-} !Int
    , h_countA  :: {-# UNPACK #-} !Int
    , h_recent  :: ![(T,a)]
    , h_ancient :: !(Maybe (VRef (Hist0 a)))
    } deriving (Typeable)

hist_space :: Hist a -> VSpace
hist_space = h_space

-- | O(1) obtain size of history
length :: Hist a -> Int
length h = (h_countA h) + (h_countR h)

-- | O(1) test whether history is empty
null :: Hist a -> Bool
null = (0 ==) . length

-- | O(1) create an empty history
empty :: VSpace -> Hist a
empty vc = Hist0 vc 0 0 [] Nothing

-- | Insert into a history. Usually, you should be adding the most
-- recent entry, in which case this is O(1). However, if the time
-- given is for an older entry, we will scan back as far as necessary
-- to insert that entry, and replace or merge it.
--
-- Heuristically, nodes near the head of our history will use smaller
-- chunk sizes than nodes near the rear of our histories.
-- insert :: (VCacheable a) => 

instance VCacheable a => VCacheable (Hist0 a) where
    put (Hist0 vc ctR ctA hR hA) = do
        putWord8 0  -- version
        put vc      -- space
        putListLen (hR,ctR)
        put ctA     -- size of ancient history
        put hA      -- ancient history (maybe ref)
    get = getWord8 >>= \ v -> case v of
        0 -> do
            vc <- get
            (hR, ctR) <- getListLen
            ctA <- get
            hA <- get
            return (Hist0 vc ctR ctA hR hA)
        _ -> fail $ histErr $ "unrecognized version " ++ show v

-- put a list and its length (mostly for symmetry with getListLen)
putListLen :: VCacheable a => ([a],Int) -> VPut ()
putListLen (lst,len) = assert (len == L.length lst) $ put lst
{-# INLINE putListLen #-}

-- obtain a list and its length 
getListLen :: VCacheable a => VGet ([a], Int)
getListLen = do
    len <- fromInteger <$> lookAhead getVarNat -- length from list encoding
    lst <- get -- obtain the list
    return (lst,len)
{-# INLINE getListLen #-}

histErr :: String -> String
histErr = (++) "Wikilon.Hist: "
