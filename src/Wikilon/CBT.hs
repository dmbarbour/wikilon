{-# LANGUAGE BangPatterns, TypeFamilies #-}

-- | Crit-bit Tree (Variant)
--
-- This is an in-memory crit-bit tree, a variant with key-value pairs
-- and that keeps the least key in the parent. This largely acts as a
-- reference for development of stowage-based KVM, but it may find
-- use elsewhere. 
--
-- It seems implementing this variant is non-trivial, at least in the
-- sense that it requires a fair bit of book-keeping.
module Wikilon.CBT
    ( CBK(..)
    , CBT(..)
    , Node(..)
    , null, member, union
    , insert, delete, lookup
    , fromList, toList
    ) where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Unsafe as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Lazy.Internal as LBS
import Data.Maybe
import Data.Word 
import Data.Bits

-- Note: it might be that a CBT isn't quite what I want for the KVM.
-- An alternative option would be a variant of trie that keeps a
-- least key for a node. 

-- | Requirements for a crit-bit key.
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

instance CBK LBS.ByteString where
    getBit n s =
        let (q,r) = n `divMod` 8 in
        let s' = LBS.drop q s in
        if LBS.null s' then False else
        getBit r (LBS.head s')
    {-# INLINE getBit #-}

    critBit = begin where
        -- special handling for first byte
        begin n a b =
            let (q,r) = n `divMod` 8 in
            let a' = LBS.drop q a in
            let b' = LBS.drop q b in
            let hd s = if LBS.null s then 0 else LBS.head s in
            case critBit r (hd a') (hd b') of
                Just off -> Just $! ((8 * q) + off)
                Nothing -> go2 (q + 1) (LBS.drop 1 a') (LBS.drop 1 b')

        -- compare bytes for two strings
        go2 !q !a !b =
            if LBS.null a then go1 q b else
            if LBS.null b then go1 q a else
            let ca = LBS.head a in
            let cb = LBS.head b in
            if (ca == cb) 
                then go2 (q+1) (LBS.tail a) (LBS.tail b) 
                else Just $! (8 * q) + countLeadingZeros (ca `xor` cb)

        -- search for non-zero byte in single string
        go1 !q !s = case LBS.uncons s of
            Just (c, s') 
                | (0 == c)  -> go1 (q + 1) s'
                | otherwise -> Just $! (8 * q) + countLeadingZeros c
            Nothing -> Nothing

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
            
newtype CBT k v 
    = Empty
    | Root k (Node k v)
    deriving (Eq, Ord)

data Node k v 
    = Leaf v
    | Inner {-# UNPACK #-} !Int (Node k v) k (Node k v)
    deriving (Eq, Ord)

instance (Show k, Show v) => Show (CBT k v) where
    showsPrec _ = showString "fromList " . show . toList

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
toList (CBT Nothing) = []
toList (CBT (Just (k,n))) = go [] k n where
    go p k (Inner _ l k' r) = go ((k',r):p) k l
    go p k (Leaf v) = (k,v) : more p
    more ((k',r):p) = go p k' r
    more [] = []

-- | Convert list to CBT.
fromList :: (CBK k) => [(k,v)] -> CBT k v
fromList = L.foldl' ins mempty where
    ins m (k,v) = insert k v m

null :: CBT k v -> Boll
null (CBT root) = isNothing root

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
    ini Empty = Root k (Leaf v)
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
        Just cb ->


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




    , fromList, toList
    , filterPrefix



{-
-- | efficient union with a combining function
unionWith :: CBK k => (v -> v -> v) -> CBT k v -> CBT k v -> CBT k v
unionWith fn = ini where
    ini (CBT Nothing) r = r
    ini l (CBT Nothing) = l
    ini (CBT (Just (lk,l))) (CBT (Just (rk,r))) = CBT (Just (go 0 lk rk l r))
    go cb lk rk l r =
        case critBit cb lk rk of
            Nothing -> (k, goL cb l r)


    goL k (Leaf a) (Leaf b) = 
(lk, fn lv rv)
            Just ix -> if getBit ix rk 
                then (lk, Inner ix l rk r)
                else (rk, Inner ix r lk l)
    go cb0 lk rk l@(Leaf lv) r@(Inner rcb rl rrk rr) =
        let cb = min cb0 rcb in
        case critBit (min cb0 rcb
        if cb0 >= rcb then


(Leaf lv) (Leaf rv) =
        case critBit cb0 lk rk of
            Nothing -> (lk, fn lv rv)
            Just ix -> 
                let lcb = getBit ix lk in
                let lk'
                (if lcb then rk else lk, 
                let lk' = if getBit ix lk then rk else lk in
                

    , null, member, union
    , insert, delete, lookup
    , fromList, toList
    , filterPrefix

-}



