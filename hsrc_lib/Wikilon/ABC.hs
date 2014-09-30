{-# LANGUAGE ViewPatterns #-}

-- | Support for Awelon Bytecode (ABC) and simple extensions. ABC is
-- a concatenative, securable, and streamable language designed for 
-- use in open distributed systems.
--
-- ABC isn't really designed for high-performance interpretation; it
-- is meant to be compiled on-the-fly or in advance via ABC's dynamic
-- linking and resource model. The extensions to ABC presented here
-- aim to support efficient interpretation, accelerators, partial 
-- evaluations, compression, and so on.
--
module Wikilon.ABC
    ( ABC_Op(..), PrimOp(..), Op
    , Quotable(..), quote, quoteList
    , abcOpToChar, abcCharToOp
    , abcDivMod, abcSimplify
    ) where

import Control.Applicative ((<$>))
import Text.Read (Read(..))
import qualified Text.ParserCombinators.ReadP as R
import qualified Text.ParserCombinators.ReadPrec as RP
import qualified Data.List as L
import qualified Data.Array.Unboxed as A
import Data.Ratio
import Data.Word (Word8)

data PrimOp -- 43 primitive operations
    -- basic data shuffling: twelve ops
    = ABC_l -- l :: (a*(b*c)) → ((a*b)*c)
    | ABC_r -- r :: ((a*b)*c) → (a*(b*c))
    | ABC_w -- w :: (a*(b*c)) → (b*(a*c))
    | ABC_z -- z :: (a*(b*(c*d))) → (a*(c*(b*d)))
    | ABC_v -- v :: a → (a * 1)
    | ABC_c -- c :: (a * 1) → a

    | ABC_L -- L :: (a+(b+c))*e → ((a+b)+c)*e
    | ABC_R -- R :: ((a+b)+c)*e → (a+(b+c))*e
    | ABC_W -- W :: (a+(b+c))*e → (b+(a+c))*e
    | ABC_Z -- Z :: (a+(b+(c+d)))*e → (a+(c+(b+d)))*e
    | ABC_V -- V :: a*e → (a+0)*e
    | ABC_C -- C :: (a+0)*e → a*e

    -- non-linear operations: two ops
    | ABC_copy  -- ^ :: (a*e) → (a*(a*e))    for copyable a (not affine)
    | ABC_drop  -- % :: (a*e) → e            for droppable a (not relevant)

    -- working with numbers: six ops
    | ABC_add        -- + :: (N(x)*(N(y)*e)) → (N(x+y)*e)
    | ABC_negate     -- - :: (N(x)*e) → (N(-x)*e)
    | ABC_multiply   -- * :: (N(x)*(N(y)*e)) → (N(x*y)*e) 
    | ABC_reciprocal -- / :: (N(x)*e) → (N(1/x)*e)      for non-zero x
    | ABC_divMod     -- Q :: (N(b)*(N(a)*e)) → (N(r)*(N(q)*e))
                     --      non-zero b; qb+r = a; r in range [0,b) or (b,0]
    | ABC_compare    -- > :: (N(x)*(N(y)*e)) → (((N(y)*N(x)) + (N(x)*N(y))) * e)
                     --   test if y > x, returning in right if true 
                     --   e.g. #4 #2 > results in (N(2)*N(4)) in right
    
    -- working with blocks: six ops
    | ABC_apply     -- $ :: ([x→y]*(x*e)) → (y*e)
    | ABC_condApply -- ? :: ((b@[x→x'])*((x+y)*e)) → ((x'+y)*e)  for droppable b
    | ABC_quote     -- ' :: (a*e) → ([s→(a*s)]*e)                        
    | ABC_compose   -- o :: ([x→y]*([y→z]*e)) → ([x→z]*e) 
    | ABC_relevant  -- k :: ([x→y]*e) → ([x→y]*e) mark block relevant (no drop)
    | ABC_affine    -- f :: ([x→y]*e) → ([x→y]*e) mark block affine (no copy)

    -- working with sums: four ops
    | ABC_distrib -- D :: (a*((b+c)*e)) → (((a*b) + (a*c))*e)
    | ABC_factor  -- F :: (((a*b)+(c*d))*e) → ((a+c)*((b+d)*e))
    | ABC_merge   -- M :: ((a+a')*e) → (a*e)
                  --   types may actually be different, but must be compatible
                  --   compatibility to be determined by future code 
    | ABC_assert  -- K :: (0+a)*e → (a*e)   assertion
                  --   where `C` removes a zero from construction, `K` removes
                  --   zero by assertion on some observable condition. K is for
                  --   describing contracts, assumptions, pre/post conditions.

    -- pseudo-literal numbers: eleven ops
    | ABC_newZero -- # :: e → N(0)*e        used for pseudo-literal numbers
    | ABC_d0 | ABC_d1 | ABC_d2 | ABC_d3 | ABC_d4 -- (N(x)*e) → (N(10x+d)*e)
    | ABC_d5 | ABC_d6 | ABC_d7 | ABC_d8 | ABC_d9 --  e.g. `#42` evaluates to 42

    -- whitespace identities: two ops 
    | ABC_SP | ABC_LF  -- a → a  used for formatting
    deriving (Eq, Ord, A.Ix, Enum, Bounded)

data ABC_Op ext -- 43 primitives + 3 special cases + extensions
    = ABC_Prim !PrimOp
    | ABC_Block [ABC_Op ext] -- [ops]
    | ABC_Text String -- "text\n~ embedded text 
    | ABC_Tok String -- {token} effects, resource linking, etc.
    | ABC_Ext ext -- ABCD, accelerators, quotations, laziness, etc.
    deriving (Eq, Ord) -- arbitrary ordering

data NoExt = VoidExt deriving (Ord,Eq)
type Op = ABC_Op NoExt

-- NOTE: Binaries can be embedded in ABC text or tokens by use of a specialized 
-- base16 alphabet: bdfghjkmnpqstxyz. This is a-z minus the vowels and `vrwlc` 
-- data plumbing. A special compression pass then encodes binaries with 0.8%
-- overhead (for large binaries) compared to a raw encoding. Some binaries can
-- be further compressed by the normal LZSS compression pass.

-- | abcDivMod computes the function associated with operator 'Q'
--    abcDivMod dividend divisor → (quotient, remainder)
-- Assumption: divisor is non-zero.
abcDivMod :: Rational -> Rational -> (Rational,Rational)
abcDivMod x y =
    let n = numerator x * denominator y in
    let d = denominator x * numerator y in
    let dr = denominator x * denominator y in
    let (q,r) = n `divMod` d in
    (fromInteger q, r % dr)

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

abcOpCharList :: [(PrimOp,Char)]
abcOpCharList =
    [(ABC_l,'l'), (ABC_r,'r'), (ABC_w,'w'), (ABC_z,'z'), (ABC_v,'v'), (ABC_c,'c')
    ,(ABC_L,'L'), (ABC_R,'R'), (ABC_W,'W'), (ABC_Z,'Z'), (ABC_V,'V'), (ABC_C,'C')

    ,(ABC_copy,'^'), (ABC_drop,'%')

    ,(ABC_add,'+'), (ABC_negate,'-')
    ,(ABC_multiply,'*'), (ABC_reciprocal,'/')
    ,(ABC_divMod,'Q'), (ABC_compare,'>')

    ,(ABC_apply,'$'), (ABC_condApply,'?')
    ,(ABC_quote,'\''), (ABC_compose,'o')
    ,(ABC_relevant,'k'), (ABC_affine,'f')

    ,(ABC_distrib,'D'), (ABC_factor,'F'), (ABC_merge,'M'), (ABC_assert,'K')

    ,(ABC_newZero,'#')
    ,(ABC_d0,'0'), (ABC_d1,'1'), (ABC_d2,'2'), (ABC_d3,'3'), (ABC_d4,'4')
    ,(ABC_d5,'5'), (ABC_d6,'6'), (ABC_d7,'7'), (ABC_d8,'8'), (ABC_d9,'9')
    
    ,(ABC_SP,' '), (ABC_LF,'\n')
    ]

abcOpCharArray :: A.UArray PrimOp Word8
abcOpCharArray = A.accumArray cw8 0 (minBound, maxBound) lst where
    cw8 _ = fromIntegral . fromEnum
    lst = abcOpCharList

abcCharOpArray :: A.UArray Word8 Word8
abcCharOpArray = A.accumArray pw8 maxBound (0,127) lst where
    lst = fmap swix abcOpCharList
    swix (op,c) = (cix c, op)
    cix = fromIntegral . fromEnum
    pw8 _ = fromIntegral . fromEnum

abcOpToChar :: PrimOp -> Char
abcOpToChar = toEnum . fromIntegral . (A.!) abcOpCharArray 

abcCharToOp :: Char -> Maybe PrimOp
abcCharToOp c | okay = Just $! toEnum (fromIntegral w8)
              | otherwise = Nothing 
    where okay = inBounds && (maxBound /= w8)
          inBounds = ((0 <= n) && (n <= 127))
          n = fromEnum c
          w8 = abcCharOpArray A.! fromIntegral n

instance Show PrimOp where
    showsPrec _ = showChar . abcOpToChar
    showList = showListConcat

instance (Quotable ext) => Show (ABC_Op ext) where 
    showsPrec _ (ABC_Prim op) = shows op
    showsPrec _ (ABC_Block ops) = showChar '[' . showList ops . showChar ']'
    showsPrec _ (ABC_Text txt) = showChar '"' . showEscaped txt . showChar '\n' . showChar '~'
    showsPrec _ (ABC_Tok tok) = showChar '{' . showString tok . showChar '}'
    showsPrec _ (ABC_Ext ext) = showList (quote ext)
    showList = showListConcat

-- show a list without any brackets or commas (just direct concatenation)
showListConcat :: (Show a) => [a] -> ShowS
showListConcat (x:xs) = shows x . showListConcat xs 
showListConcat [] = id

-- Escape embedded for ABC. Only LF needs be escaped.
showEscaped :: String -> ShowS
showEscaped ('\n':ss) = showChar '\n' . showChar ' ' . showEscaped ss
showEscaped (c:ss) = showChar c . showEscaped ss
showEscaped [] = id

-- Note: there will be no `ABC_Ext` elements immediately after Read.
-- So all reads are equivalent to NoExt.
instance Read (ABC_Op ext) where
    readPrec = RP.lift readOp
    readListPrec = RP.lift (R.manyTill readOp R.eof)

readOpC :: R.ReadP PrimOp
readOpC =
    R.get >>= \ c ->
    case abcCharToOp c of
        Nothing -> R.pfail
        Just opc -> return opc

readOp :: R.ReadP (ABC_Op ext)
readOp = 
    (ABC_Block <$> readBlock) R.<++
    (ABC_Text <$> readText) R.<++
    (ABC_Tok <$> readTok) R.<++
    (ABC_Prim <$> readOpC) 

readBlock :: R.ReadP [ABC_Op ext]
readBlock = R.char '[' >> R.manyTill readOp (R.char ']')

-- readText will remove the escapes (only LF is escaped)
readText :: R.ReadP String
readText = start where
    start = 
        R.char '"' >>
        textLine >>= \ t0 ->
        R.manyTill (R.char ' ' >> textLine) (R.char '~') >>= \ ts ->
        return $ L.concat (t0 : map ('\n':) ts)
    textLine = R.manyTill R.get (R.char '\n')

readTok :: R.ReadP String
readTok = R.char '{' >> R.manyTill (R.satisfy isTokChr) (R.char '}') 

isTokChr :: Char -> Bool
isTokChr c = not $ ('{' == c || '\n' == c || '}' == c)



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
