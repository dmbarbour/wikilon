{-# LANGUAGE DeriveDataTypeable #-}

-- | A very simple, time-ordered VCache-layer list with recent values
-- near the head and old values stored near the rear.
--
-- Thoughts: it might be better to generalize this, e.g. into a
-- Data.Sequence.
module Wikilon.Hist
    ( Hist
    , empty
--    , insert
    , null
    , length

{-    
    , trim
    , decay
-}
    , toList
    , fromList
    , fromOrdList


    , hist_space
    , module Wikilon.Time
    ) where

import Prelude hiding (length, null)
import Control.Exception (assert)
import Control.Applicative hiding (empty)
import Data.Function (on)
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

-- | O(N), lazily generate a list of time,val pairs from our history.
toList :: Hist a -> [(T,a)]
toList h = 
    let rh = h_recent h in
    case h_ancient h of
        Nothing -> rh
        Just v -> rh ++ toList (deref' v)

-- | O(N * lg(N)), generate a history from a list of time,val pairs. This
-- will sort the list prior. 
fromList :: VSpace -> [(T,a)] -> Hist a
fromList vc = fromOrdList vc . L.sortBy tmDesc where
    tmDesc = flip compare `on` fst

-- | O(N), generate a history from a list of time,val pairs descending in
-- time (i.e. such that the most recent time is near the head). This will
-- generate an invalid history if the list is not ordered.
fromOrdList :: VSpace -> [(T,a)] -> Hist a
fromOrdList vc = error "TODO"



-- | insert a value into the history. This will fail if the insert
-- insert :: (T,a) -> Hist a -> Maybe Hist a


instance Eq a => Eq (Hist0 a) where
    (==) = (==) `on` toList

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
