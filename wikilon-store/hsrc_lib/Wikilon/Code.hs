{-# LANGUAGE DeriveDataTypeable #-}

-- | Specializations of Wikilon.ABC to easily use VCache resources.
module Wikilon.Code
    ( ABC(..)
    , Value(..)
    , Rsc(..)
    ) where

import Data.Typeable
import Database.VCache
import qualified Wikilon.ABC as Inner
import Awelon.ABC (Quotable(..))

newtype Value = Value InnerVal deriving (Typeable, Eq)
newtype ABC = ABC InnerCode deriving (Typeable, Eq)
newtype Rsc = Rsc (VRef Value) deriving (Eq)
type InnerVal = Inner.Value Rsc
type InnerCode = Inner.ABC InnerVal

todo :: a
todo = error "Wikilon.Code: TODO!"

instance VCacheable Value where
    put = todo
    get = todo
instance VCacheable ABC where
    put = todo
    get = todo

instance Quotable Value where
    quotes = todo
instance Quotable Rsc where
    quotes = todo 

-- more todo: 
-- 
-- Simplification code to access the 

{-

-- encode ABC same as Block value
instance VCacheable ABC where
    put abc = put (Block abc 0)
    get = get >>= \ val -> case val of
        (Block abc _) -> return abc
        _ -> fail $ abcErr $ "expecting block, received " ++ show val

-- resource is simple pair
instance VCacheable Rsc where
    {-# INLINE put #-}
    {-# INLINE get #-}
    put (Rsc r f) = put r >> putWord8 f
    get = Rsc <$> get <*> getWord8


-- versioned encoding for values 
--
-- V0 does a direct encoding for resources, and optionally includes
-- a compression pass for the bytes. Compression is currently just
-- LZ4, accepting when the ratio is heuristically good enough.
data V0 = V0 { unV0 :: (Bytes, [Rsc]) } deriving Typeable
newtype ZBytes = ZBytes Bytes deriving Typeable

instance VCacheable Value where
    put = put . V0 . encodeVal
    get = (decodeVal . unV0) <$> get

instance VCacheable V0 where
    put (V0 (bs,rs)) = do
        putWord8 0  -- encode version number 
        put rs      -- value resource list
        put (ZBytes bs)
    get = getWord8 >>= \ vn -> case vn of
          0 -> do rs <- get
                  ZBytes bs <- get
                  return $! V0 (bs,rs)
          _ -> fail $ abcErr $ "unrecognized value encoding " ++ show vn

instance VCacheable ZBytes where
    put (ZBytes bs) = 
        let bytes = LBS.toStrict bs in
        -- require reasonable compression ratio or don't bother
        let accept cbytes = (5 * BS.length cbytes) < (4 * BS.length bytes) in
        case LZ4.compress bytes of
            Just cbs | accept cbs -> putWord8 1 >> put cbs
            _ -> putWord8 0 >> put bytes
    get = getWord8 >>= \ cm -> case cm of
        0 -> ZBytes <$> get
        1 -> fmap fromInteger getVarNat >>= \ nLen -> -- compressed len
             withBytes nLen $ \ p -> -- zero copy access to compressed content
                newForeignPtr_ p >>= \ fp ->
                let cbytes = BS.fromForeignPtr fp 0 nLen in
                case LZ4.decompress cbytes of
                    Just !bytes -> return $! ZBytes (LBS.fromStrict bytes)
                    Nothing -> fail $ abcErr $ "LZ4 decompression failure!"
        _ -> fail $ abcErr $ "unrecognized compression mode " ++ show cm

-}
