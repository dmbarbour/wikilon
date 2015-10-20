{-# LANGUAGE DeriveDataTypeable, BangPatterns, PatternGuards #-}

-- | a model for very large lists in VCache
module Data.VCache.LoB 
    ( LoB
    , lob_space
    , blockSize
    , empty
    , cons
    , uncons
    , null
    , length
    , toList
    , reverse
    , drop
    ) where

import Prelude hiding (length, null, drop, reverse, foldl)
import Control.Exception (assert)
import Control.Applicative hiding (empty)
import Data.Maybe
import qualified Data.List as L
import Data.Typeable (Typeable)
import Database.VCache

-- | At small sizes, LoB is just a list. However, when it grows beyond
-- a given threshold, it starts offloading blocks of content into VCache.
-- At larger thresholds, LoB stores block pages then block-page pages
-- and so on recursively. The total amount in main memory is O(lg(N)).
--
-- The client must choose a block size, the number of leaf elements to
-- store together in one block (at least 1).
type LoB a = LoB0 a

lob_space :: LoB a -> VSpace
lob_space = l_space

-- versioned list representations
data LoB0 a = LoB0
    { l_space       :: !VSpace
    , l_blockSize   :: {-# UNPACK #-} !Int
    , l_buffSize    :: {-# UNPACK #-} !Int
    , l_buffer      :: ![a]
    , l_bpages      :: !(Maybe (Block a, LoB (Block a)))
    } deriving (Typeable, Eq)
type Block a = VRef [a]

-- | Create an empty LoB. You must provide the block size and the
-- VSpace where the content will be stored.
empty :: VSpace -> Int -> LoB a
empty _ bsz | bsz < 1 =
    let emsg = "illegal block size: " ++ show bsz in
    error $ lobError emsg
empty vc bsz = LoB0
    { l_space = vc
    , l_blockSize = bsz
    , l_buffSize = 0
    , l_buffer = []
    , l_bpages = Nothing
    }

blockSize :: LoB a -> Int
blockSize = l_blockSize

-- | Add an element to the LoB. 
--
-- Cost is O(1) amortized, O(lg(N)) worst case.
cons :: VCacheable a => a -> LoB a -> LoB a
cons a = _cons . _collapse where
    _cons l =
        let buffer' = (a : l_buffer l) in
        let buffSize' = 1 + (l_buffSize l) in
        l { l_buffer = buffer', l_buffSize = buffSize' }
    _collapse l =
        if not (_tooBig l) then l else
        let nKeep = (l_buffSize l) - (l_blockSize l) in
        let (buff',overflow) = L.splitAt nKeep (l_buffer l) in
        assert (L.length overflow == l_blockSize l) $
        let vc = l_space l in
        let block = vref' vc overflow in
        let bpages' = case l_bpages l of
                Nothing -> Just (block, empty vc _pageSize)
                Just (b0,lb) -> Just (block, cons b0 lb)
        in
        LoB0 { l_space = vc
             , l_blockSize = (l_blockSize l)
             , l_buffSize = nKeep
             , l_buffer = buff'
             , l_bpages = bpages'
             }

-- | Test whether LoB is empty. O(1).
null :: LoB a -> Bool
null l = L.null (l_buffer l) && isNothing (l_bpages l)

-- | Compute the length of our LoB. O(lg(N)).
length :: LoB a -> Int
length l = l_buffSize l + (l_blockSize l * blockCount) where
    blockCount = maybe 0 ((1 +) . length . snd) (l_bpages l)

-- | try to remove an element from the LoB. 
--  O(1) amortized, O(lg(N)) worst case.
uncons :: LoB a -> Maybe (a, LoB a)
uncons l = case l_buffer l of
    (x:xs) -> 
        let sz' = l_buffSize l - 1 in
        let l' = l { l_buffSize = sz', l_buffer = xs } in
        Just (x, l')
    [] -> assert (0 == l_buffSize l) $ case l_bpages l of
        Nothing -> Nothing -- null list, no extra blocks
        Just (b, lb) ->
            let bpages' = uncons lb in
            let buffer' = deref' b in
            assert (L.length buffer' == l_blockSize l) $
            uncons $ LoB0 
                { l_space = (l_space l)         -- space is constant
                , l_blockSize = (l_blockSize l) -- block size is constant
                , l_buffSize = (l_blockSize l)  -- we took one block
                , l_buffer = buffer'            -- updated buffer
                , l_bpages = bpages'            -- updated block pages
                }

-- | Drop a number of elements. 
--
-- At the moment, a naive O(N) implementation is used.
-- However, this can later be optimized to O(lg(N)).
drop :: Int -> LoB a -> LoB a
drop n l =
    if (n < 1) then l else
    drop (n-1) $ maybe l snd (uncons l)

-- How large should our block pages be? A larger value means flatter
-- representation, larger buffers, more data held by the LoB directly.
_pageSize :: Int
_pageSize = 32

-- Each LoB can store up to one full page in its buffer. When we're
-- about to add one more element, we'll store the current buffer into
-- a page as a single element for the block page (repeat recursively).
_tooBig :: LoB a -> Bool
_tooBig l = l_buffSize l >= l_blockSize l

-- | lazily load LoB as a list. O(N) total, but streamable.
toList :: LoB a -> [a]
toList l = buff ++ L.concatMap deref' pages where
    buff = l_buffer l
    pages = case l_bpages l of
        Just (b, lb) -> b : toList lb
        Nothing -> []

-- | Reverse an LoB list.
reverse :: (VCacheable a) => LoB a -> LoB a
reverse l = L.foldl' (flip cons) rl0 (toList l) where
    rl0 = empty (lob_space l) (l_blockSize l)

instance VCacheable a => VCacheable (LoB0 a) where
    put l = do
        putWord8 0  -- version number
        putVSpace (l_space l)
        putVarNat (toInteger $ l_blockSize l)
        -- skip l_buffSize
        put (l_buffer l)
        put (l_bpages l)
    get = getWord8 >>= \ v -> case v of
        0 -> do
            _space <- getVSpace
            _blockSize <- fromInteger <$> getVarNat
            _buffSize <- lookAhead (fromInteger <$> getVarNat) -- size from l_buffer
            _buffer <- get
            _bpages <- get
            return $! LoB0
                { l_space = _space
                , l_blockSize = _blockSize
                , l_buffSize = _buffSize
                , l_buffer = _buffer
                , l_bpages = _bpages
                }
        _ -> error $ lobError $ "unknown version " ++ show v

lobError :: String -> String
lobError = (++) "Data.VCache.LoB (via Wikilon): "

