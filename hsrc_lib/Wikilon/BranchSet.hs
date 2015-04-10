{-# LANGUAGE DeriveDataTypeable #-}

module Wikilon.BranchSet
    ( BranchSet
    , module Wikilon.Branch
    ) where

import Data.Typeable (Typeable)
import Database.VCache
import Data.VCache.Trie (Trie)
import qualified Data.VCache.Trie as Trie

import Wikilon.Branch

-- | A BranchSet is a collection of named branches. 
type BranchSet = BranchSet0

-- versioned branchset model
data BranchSet0 = BranchSet0
    { b_root :: !(Trie Branch)
    , b_bct  :: {-# UNPACK #-} !Int -- total number of branches
    , b_dct  :: {-# UNPACK #-} !Int -- total number of dictionaries
    } deriving (Typeable, Eq)


instance VCacheable BranchSet0 where
    put b = do
        putWord8 0 -- version
        put (b_bct b)
        put (b_dct b)
        put (b_root b)
    get = getWord8 >>= \ v -> case v of
        0 -> do
            _bct <- get
            _dct <- get
            _root <- get
            return $! BranchSet0 
                { b_bct = _bct
                , b_dct = _dct
                , b_root = _root
                }
        _ -> fail $ branchSetErr $ "unrecognized version " ++ show v

branchSetErr :: String -> String
branchSetErr = (++) "Wikilon.BranchSet: "

