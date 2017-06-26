{-# LANGUAGE BangPatterns, TypeFamilies #-}

-- | Crit-bit Tree (Variant)
--
-- This is an in-memory crit-bit tree, a variant with key-value pairs
-- and that keeps the least key in the parent. This largely acts as a
-- reference for development of stowage-based KVM, but it may find
-- use elsewhere. 
--
module Wikilon.CBT
    ( CBK(..)
    , CBT(..)
    , Node(..)
    , null, member, union
    , insert, delete, lookup
    , fromList, toList
    ) where

import Prelude hiding (lookup, null)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Unsafe as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Lazy.Internal as LBS
import qualified Data.List as L
import Data.Maybe
import Data.Word 
import Data.Bits



-- | Requirements for a crit-bit key.
--
-- thoughts: it might be better to change this to a `getByte` and a
-- `length`, or perhaps a `getWord` where a word has a finite number
-- of bytes. As is, too much crit-bit logic is embedded in this class.
class (Eq k) => CBK k where
    -- | Determine bit at specified offset within key.  
    getBit  :: Int -> k -> Bool

    -- | Seek offset of first difference between two keys
    -- that is no less than the initial offset.
    critBit :: Int -> k -> k -> Maybe Int

instance CBK Word8 where
    getBit = getBitFB
    critBit = critBitFB
    {-# INLINE getBit #-}
    {-# INLINE critBit #-}

instance CBK Word16 where
    getBit = getBitFB 
    critBit = critBitFB
    {-# INLINE getBit #-}
    {-# INLINE critBit #-}

instance CBK Word32 where
    getBit = getBitFB 
    critBit = critBitFB
    {-# INLINE getBit #-}
    {-# INLINE critBit #-}

instance CBK Word64 where
    getBit = getBitFB 
    critBit = critBitFB
    {-# INLINE getBit #-}
    {-# INLINE critBit #-}

getBitFB :: FiniteBits b => Int -> b -> Bool
getBitFB n a = testBit a ((finiteBitSize a) - n)
{-# INLINE getBitFB #-}

critBitFB :: FiniteBits b => Int -> b -> b -> Maybe Int
critBitFB n a b =
    let x = (a `xor` b) `shiftL` n in
    if (zeroBits == x) then Nothing else
    Just $! (n + (countLeadingZeros x))
{-# INLINE critBitFB #-}


instance CBK BS.ByteString where
    getBit n s =
        let (q,r) = n `divMod` 8 in
        let s' = BS.drop q s in
        if BS.null s' then False else
        getBit r (BS.unsafeHead s')
    {-# INLINE getBit #-}

    critBit = begin where
        -- special handling for first byte
        begin n a b =
            let (q,r) = n `divMod` 8 in
            let a' = BS.drop q a in
            let b' = BS.drop q b in
            let hd s = if BS.null s then 0 else BS.unsafeHead s in
            case critBit r (hd a') (hd b') of
                Just off -> Just $! (n + (off - r))
                Nothing -> go2 (q + 1) (BS.drop 1 a') (BS.drop 1 b')

        -- compare whole bytes
        go2 !q !a !b =
            if BS.null a then go1 q b else
            if BS.null b then go1 q a else
            let ca = BS.unsafeHead a in
            let cb = BS.unsafeHead b in
            if (ca == cb) 
                then go2 (q+1) (BS.unsafeTail a) (BS.unsafeTail b) 
                else Just $! (8 * q) + countLeadingZeros (ca `xor` cb)

        -- search for a non-zero byte
        go1 !q !s = case BS.uncons s of
            Just (c, s')
                | (0 == c) -> go1 (q + 1) s'
                | otherwise -> Just $! (8 * q) + countLeadingZeros c
            Nothing -> Nothing
            
data CBT k v 
    = Empty
    | Root k (Node k v)
    deriving (Eq, Ord)

data Node k v 
    = Leaf v
    | Inner {-# UNPACK #-} !Int (Node k v) k (Node k v)
    deriving (Eq, Ord)

-- question: should I add size information to nodes?
--
-- I'm not sure whether O(1) sizes are useful enough when they aren't
-- being used to balance trees. But they might prove more useful for
-- the KVMs, e.g. to keep size information for the stowage nodes so we
-- can always determine size without high-latency lookups.
--
-- More generally, it might be convenient to compute monoidal meta-data
-- that is a result of composition. Size information would be a simple
-- monoid, but it seems feasible to compute useful summary data back
-- up the tree. Still, might be better to focus on the plain old data
-- type first, and create the variant as another module.

instance (Show k, Show v) => Show (CBT k v) where
    showsPrec _ m = showString "fromList " . shows (toList m)

instance (CBK k) => Monoid (CBT k v) where
    mempty = Empty
    mappend = union

instance Functor (CBT k) where
    fmap _ Empty = Empty
    fmap fn (Root k n) = (Root k (fmap fn n))

instance Functor (Node k) where
    fmap fn (Leaf v) = Leaf (fn v)
    fmap fn (Inner cb l k r) = Inner cb (fmap fn l) k (fmap fn r)

-- | Convert CBT to list. 
toList :: CBT k v -> [(k,v)]
toList Empty = []
toList (Root k n) = go [] k n where
    go p k (Inner _ l k' r) = go ((k',r):p) k l
    go p k (Leaf v) = (k,v) : more p
    more ((k',r):p) = go p k' r
    more [] = []

-- | Convert list to CBT.
fromList :: (CBK k) => [(k,v)] -> CBT k v
fromList = L.foldl' ins mempty where
    ins m (k,v) = insert k v m

null :: CBT k v -> Bool
null Empty = True
null _ = False

member :: (CBK k) => k -> CBT k v -> Bool
member k m = not (isNothing (lookup k m))

lookup :: (CBK k) => k -> CBT k v -> Maybe v
lookup k = ini where
    ini Empty = Nothing
    ini (Root lk l) = go lk l
    go lk (Inner cb l rk r) =
        if getBit cb k then go rk r 
                       else go lk l
    go lk (Leaf v) = 
        if (lk == k) then Just v 
                     else Nothing

-- update leftmost item
updL :: (v -> v) -> Node k v -> Node k v
updL fn (Leaf v) = Leaf (fn v)
updL fn (Inner cb l k r) = Inner cb (updL fn l) k r

-- insert value to left at given depth, using the given
-- key as the least key for the given node.
insL :: Int -> v -> k -> Node k v -> Node k v
insL cb = insLN cb . Leaf

-- insert node to left of tree with depth and least key 
insLN :: Int -> Node k v -> k -> Node k v -> Node k v
insLN cb n' k' = ins where
    ins (Inner ncb l k r) | (cb > ncb) = 
        Inner ncb (ins l) k r
    ins n = Inner cb n' k' n

-- insert node to right of tree
insert :: CBK k => k -> v -> CBT k v -> CBT k v
insert = insertWith const

-- Could we insert more efficiently just looking at the crit-bits?
-- note: might be better to model this as a union with a singleton.
insertWith :: CBK k => (v -> v -> v) -> k -> v -> CBT k v -> CBT k v
insertWith fn kIns vIns = ini where
    ini Empty = Root kIns (Leaf vIns)
    ini (Root k n) = uncurry Root (ins 0 k n)

    ins cb k n = case critBit cb kIns k of
        Just cb -> case getBit cb kIns of
            False -> (kIns, insL cb vIns k n)   -- insert as new least node
            True  -> (k, insR cb n)             -- insert to right after cb
        Nothing -> (k, updL (fn vIns) n)        -- update in place

    -- insR inserts to the right of the least key, 
    -- just after a specified critbit.
    insR cb (Inner ncb l k r) 
        | (cb > ncb) = Inner ncb (insR cb l) k r -- differentiates further down
        | otherwise  = uncurry (Inner cb l) (ins cb k r)
    insR cb v0@(Leaf _) = Inner cb v0 kIns (Leaf vIns) -- right of leaf

-- delete a specific node.
delete :: CBK k => k -> CBT k v -> CBT k v
delete kDel = ini where
    ini Empty = Empty
    ini (Root k n) = case critBit 0 kDel k of
        Nothing -> Empty
        Just cb -> undefined

-- | left-biased union
union :: CBK k => CBT k v -> CBT k v -> CBT k v
union = unionWith const

-- | union
-- 
-- ideally I'll provide a more efficient option than inserting each
-- element independently. But for now, I'm just making it work.
unionWith :: CBK k => (v -> v -> v) -> CBT k v -> CBT k v -> CBT k v
unionWith fn l = L.foldl' ins l . toList where
    ins m (k,v) = insertWith (flip fn) k v m

