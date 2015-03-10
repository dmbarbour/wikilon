{-# LANGUAGE ViewPatterns #-}

-- | Wikilon uses Awelon Bytecode (ABC) to model user-defined behavior.
-- However, while ABC has many nice properties, fast interpretation is
-- not one of them. ABC is meant to be compiled. OTOH, interpretation 
-- has the advantage of simplicity.
--
-- As a compromise, Wikilon uses a 'doped' variation of ABC based on
-- precomputing values and replacing common subprograms by built-in
-- accelerators. This doped code can be converted back to pure ABC or
-- eventually ABCD for export purposes.
--
-- Wikilon's ABC also supports anonymous subprograms and values like
-- ABC achieves using {#resourceId}, except leveraging VCache instead.
-- Anonymity simplifies many garbage collection concerns.
--
module Wikilon.ABC
    ( 
    ) where

{-

import Control.Applicative ((<$>), pure)
import Control.Monad (join)
import Control.Exception (assert)
import qualified Data.Binary as B
import qualified Data.Binary.Get as B
import qualified Data.Binary.Put as B
import qualified Codec.Binary.UTF8.Generic as UTF8
import qualified Data.List as L
import qualified Data.Array.Unboxed as A
import Data.Ratio
import Data.Word (Word16)
import Data.String
import Wikilon.Char
import qualified Wikilon.ParseUtils as P


-- NOTE: Binaries can be embedded in ABC text or tokens by use of a specialized 
-- base16 alphabet: bdfghjkmnpqstxyz. This is a-z minus the vowels and `vrwlc` 
-- data plumbing. A special compression pass then encodes binaries with 0.8%
-- overhead (for large binaries) compared to a raw encoding. Some binaries can
-- be further compressed by the normal LZSS compression pass.


-- | Quotable: serves a role similar to `show` except it targets ABC
-- programs instead of raw text. Any ABC_Ext fields will expand to
-- some raw underlying ABC.
class Quotable v where 
    quotes :: v -> [Op] -> [Op]

quote :: Quotable v => v -> [Op]
quote = flip quotes []

quoteList :: Quotable v => [v] -> [Op] -> [Op]
quoteList (v:vs) = quotes v . quoteList vs
quoteList [] = id

instance Quotable NoExt where quotes = const id
instance Quotable PrimOp where quotes = (:) . ABC_Prim 
instance (Quotable ext) => Quotable (ABC_Op ext) where
    quotes (ABC_Prim op) = quotes op
    quotes (ABC_Block ops) = (:) (ABC_Block (quoteList ops [])) 
    quotes (ABC_Text txt) = (:) (ABC_Text txt)  
    quotes (ABC_Tok tok) = (:) (ABC_Tok tok)
    quotes (ABC_Ext ext) = quotes ext

instance Quotable Integer where quotes = qi'
instance (Integral i) => Quotable (Ratio i) where
    quotes r | (r < 0) = quotes (negate r) . quotes ABC_negate
             | (1 == den) = qi num
             | (1 == num) = qi den . quotes ABC_reciprocal
             | otherwise = qi num . qi den . quotes ABC_reciprocal . quotes ABC_multiply
        where den = denominator r
              num = numerator r

qi :: (Integral i) => i -> [Op] -> [Op]
qi = qi' . fromIntegral

qi' :: Integer -> [Op] -> [Op]
qi' n | (n > 0) = let (q,r) = n `divMod` 10 in qi q . quotes (opd r)
      | (0 == n) = quotes ABC_newZero
      | otherwise = qi (negate n) . quotes ABC_negate

-- quote an integer into ABC, building from right to left
opd :: Integer -> PrimOp
opd 0 = ABC_d0
opd 1 = ABC_d1
opd 2 = ABC_d2
opd 3 = ABC_d3
opd 4 = ABC_d4
opd 5 = ABC_d5
opd 6 = ABC_d6
opd 7 = ABC_d7
opd 8 = ABC_d8
opd 9 = ABC_d9
opd _ = error "invalid digit!"

skip :: a -> b -> b
skip = flip const


instance (Quotable ext) => B.Binary (ABC_Ops ext) where
    put = putABC . abc_ops
    get = ABC_Ops <$> getABC

instance (Quotable ext) => Show (ABC_Op ext) where
    showsPrec _ = showList . (:[])
    showList = shows . ABC_Ops

instance (Quotable ext) => Show (ABC_Ops ext) where
    show = UTF8.toString . B.encode

instance Show PrimOp where
    showsPrec _ = showList . (:[])
    showList = shows . fmap primOp

-- help type inference a little
primOp :: PrimOp -> Op
primOp = ABC_Prim

instance Read (ABC_Ops ext) where
    readsPrec _ s =
        let bytes = UTF8.fromString s in
        case B.runGetOrFail getABC bytes of
            Left (_bs,_ct,_emsg) -> []
            Right (brem,_ct, code) -> [(ABC_Ops code, UTF8.toString brem)]

putABC :: (Quotable ext) => [ABC_Op ext] -> B.PutM ()
putABC = mapM_ putOp

putOp :: (Quotable ext) => ABC_Op ext -> B.PutM ()
putOp (ABC_Prim op) = B.put (abcOpToChar op)
putOp (ABC_Block abc) = B.put '[' >> putABC abc >> B.put ']'
putOp (ABC_Text txt) = B.put '"' >> putMLT txt >> B.put '\n' >> B.put '~'
putOp (ABC_Tok tok) = assert (validTok tok) $ B.put '{' >> mapM_ B.put tok >> B.put '}'
putOp (ABC_Ext ops) = putABC (quote ops)

putMLT :: String -> B.PutM ()
putMLT ('\n':cs) = B.put '\n' >> B.put ' ' >> putMLT cs
putMLT (c:cs) = B.put c >> putMLT cs
putMLT [] = return ()

validTok :: String -> Bool
validTok = L.all isTokenChar

-- TODO: If possible, it would be ideal to support a streaming parse
-- even of contained blocks, such that we can immediately begin the
-- partial evaluation pass when parsing the block without waiting until
-- the full block is parsed. Potentially, this might be achieved by 
-- parsing into an intermediate stream of operations with 'in-block'
-- and 'out-block' tokens, rather than treating blocks as first-class
-- objects at this layer.
--


-- get will not return any ABC_Ext elements, so the type of ext is
-- not relevant at this point. getABC requires a little extra state
-- regarding whether we've just read a newline
getABC :: B.Get [ABC_Op ext]
getABC = P.manyC tryOp

tryOp :: B.Get (B.Get (ABC_Op ext))
tryOp = 
    B.get >>= \ c ->
    case c of
        (abcCharToOp -> Just op) -> return $ pure (ABC_Prim op)
        '[' -> return $ ABC_Block <$> readBlock 
        '{' -> return $ ABC_Tok   <$> readToken
        '"' -> return $ ABC_Text  <$> readText
        _ -> fail "input not recognized as ABC"

getOp :: B.Get (ABC_Op ext)
getOp = join tryOp

-- we've already read '['; read until ']'
readBlock :: B.Get [ABC_Op ext]
readBlock = P.manyTil getOp (P.char ']')

readToken :: B.Get String
readToken = P.manyTil (P.satisfy isTokenChar) (P.char '}')

readText :: B.Get String 
readText = 
    lineOfText >>= \ t0 ->
    P.manyTil (P.char ' ' >> lineOfText) (P.char '~') >>= \ ts ->
    return $ L.concat $ t0 : fmap ('\n':) ts

-- text to end of line...
lineOfText :: B.Get String
lineOfText = P.manyTil B.get (P.char '\n')

-- | abcSimplify performs a simple optimization on ABC code based on
-- recognizing short sequences of ABC that can be removed. E.g.
--
--   LF, SP, ww, zz, vc, cv, rl, lr, WW, ZZ, VC, CV, RL, LR
-- 
-- In addition, we translate 'zwz' to 'wzw' (same for sums).
--
abcSimplify :: [ABC_Op ext] -> [ABC_Op ext]
abcSimplify = zSimp []

zSimp :: [ABC_Op ext] -> [ABC_Op ext] -> [ABC_Op ext]
zSimp (ABC_Prim a:as) (ABC_Prim b:bs) | opsCancel a b = zSimp as bs
zSimp rvOps (ABC_Block block : ops) = zSimp (ABC_Block (abcSimplify block) : rvOps) ops
zSimp rvOps (ABC_Prim ABC_SP : ops) = zSimp rvOps ops
zSimp rvOps (ABC_Prim ABC_LF : ops) = zSimp rvOps ops
zSimp (ABC_Prim ABC_w : ABC_Prim ABC_z : rvOps) (ABC_Prim ABC_z : ops) =
    zSimp rvOps (ABC_Prim ABC_w : ABC_Prim ABC_z : ABC_Prim ABC_w : ops)
zSimp (ABC_Prim ABC_W : ABC_Prim ABC_Z : rvOps) (ABC_Prim ABC_Z : ops) =
    zSimp rvOps (ABC_Prim ABC_W : ABC_Prim ABC_Z : ABC_Prim ABC_W : ops)
zSimp rvOps (b:bs) = zSimp (b:rvOps) bs
zSimp rvOps [] = L.reverse rvOps

opsCancel :: PrimOp -> PrimOp -> Bool
opsCancel ABC_l ABC_r = True
opsCancel ABC_r ABC_l = True
opsCancel ABC_w ABC_w = True
opsCancel ABC_z ABC_z = True
opsCancel ABC_v ABC_c = True
opsCancel ABC_c ABC_v = True
opsCancel ABC_L ABC_R = True
opsCancel ABC_R ABC_L = True
opsCancel ABC_W ABC_W = True
opsCancel ABC_Z ABC_Z = True
opsCancel ABC_V ABC_C = True
opsCancel ABC_C ABC_V = True
opsCancel _ _ = False

-}
