{-# LANGUAGE ViewPatterns #-}

-- | A pure model of Awelon Bytecode: no accelerators or extensions.
module Wikilon.ABC.Pure
    ( ABC(..)
    , Token
    , Op(..)
    , PrimOp(..)
    , abcDivMod
    , abcCharToOp, abcOpToChar, abcOpCharList
    , encode, encode'
    , decode

    , Quotable(..)
    , quote, quoteList, quotesList
    ) where

import Data.Monoid
import Data.Ratio
import qualified Data.ByteString.UTF8 as UTF8
import qualified Data.ByteString.Lazy.Char8 as LBS
import qualified Data.ByteString.Lazy.UTF8 as LazyUTF8
import qualified Data.ByteString.Builder as BB
import qualified Data.Array.IArray as A
import qualified Data.List as L
import Data.String (IsString(..))

import Wikilon.Text

newtype ABC = ABC { abcOps :: [Op] }
    deriving (Eq, Ord)

type Token = UTF8.ByteString

data Op -- 43 primitives + 3 special cases
    = ABC_Prim !PrimOp -- 43 primitives
    | ABC_Block ABC    -- block type
    | ABC_Text !Text   -- text literal (minus escapes)
    | ABC_Tok  {-# UNPACK #-} !Token -- {token} text
    deriving (Eq, Ord) -- arbitrary ordering

-- text is shorthand for a list of codepoints
--  where a `List foo` has type ((foo * List foo) + 1). 
--  and a codepoint is in range 0..1114111

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

    -- identities: two ops 
    | ABC_SP | ABC_LF  -- a → a  used for formatting
    deriving (Eq, Ord, A.Ix, Enum, Bounded)

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

impossible :: String -> a
impossible eMsg = error ("Wikilon.ABC.Pure: " ++ eMsg)

abcOpCharArray :: A.Array PrimOp Char
abcOpCharArray = A.accumArray ins eUndef (minBound,maxBound) abcOpCharList where
    eUndef = impossible "missing encoding for ABC PrimOp"
    ins _ c = c

abcCharOpArray :: A.Array Char (Maybe PrimOp)
abcCharOpArray = A.accumArray ins Nothing (lb, ub) lst where
    ins _ op = Just op
    lb = L.minimum (fmap snd abcOpCharList)
    ub = L.maximum (fmap snd abcOpCharList)
    lst = fmap (\(op,c) -> (c, op)) abcOpCharList

abcOpToChar :: PrimOp -> Char
abcOpToChar op = abcOpCharArray A.! op

abcCharToOp :: Char -> Maybe PrimOp
abcCharToOp c | inBounds = abcCharOpArray A.! c
              | otherwise = Nothing
    where inBounds = (lb <= c) && (c <= ub)
          (lb,ub) = A.bounds abcCharOpArray

-- | Encode pure ABC into a Lazy UTF8 ByteString
encode :: ABC -> Text
encode = BB.toLazyByteString . encode'

encode' :: ABC -> BB.Builder
encode' = mconcat . fmap _encodeOp . abcOps

_encodeOp :: Op -> BB.Builder
_encodeOp (ABC_Prim op) = BB.char8 (abcOpToChar op)
_encodeOp (ABC_Block ops) = BB.char8 '[' <> encode' ops <> BB.char8 ']'
_encodeOp (ABC_Text txt) = BB.char8 '"' <> _encodeLiteral txt <> BB.char8 '~'
_encodeOp (ABC_Tok tok) =
    BB.char8 '{'   <>
    BB.byteString tok <>
    BB.char8 '}'

_encodeLiteral :: Text -> BB.Builder
_encodeLiteral txt = 
    let (ln0,lns) = textLines txt in
    BB.lazyByteString ln0          <>
    mconcat (fmap _encodeLine lns) <>
    BB.char8 '\n'               

_encodeLine :: Text -> BB.Builder
_encodeLine l = BB.char8 '\n' <> BB.char8 ' ' <> BB.lazyByteString l

-- | Decode ABC from Text. 
--
-- Parsed ABC is represented losslessly. Any text that is not parsed
-- is returned. This implementation aims more to be fast than to offer
-- good error information.
decode :: Text -> (ABC, Text)
decode = decodeABC []

decodeABC :: [Op] -> Text -> (ABC, Text) 
decodeABC r txt = case decodeOp txt of
    Nothing -> (ABC (L.reverse r), txt)
    Just (op, txt') -> decodeABC (op:r) txt'

decodeOp :: Text -> Maybe (Op, Text)
decodeOp = maybe Nothing (uncurry decodeOp') . LBS.uncons

decodeOp' :: Char -> Text -> Maybe (Op, Text)
decodeOp' (abcCharToOp -> Just op) txt = Just (ABC_Prim op, txt)
decodeOp' '[' txt =
    let (blockOps, blockEnd) = decode txt in
    case LBS.uncons blockEnd of
        Just (']', txt') -> Just (ABC_Block blockOps, txt')
        _ -> Nothing
decodeOp' '{' txt =  
    case LBS.elemIndex '}' txt of
        Nothing -> Nothing
        Just idx -> 
            let (tokLazy, tokEnd) = LBS.splitAt idx txt in
            let tok = LBS.toStrict tokLazy in
            let txt' = LBS.drop 1 tokEnd in
            tok `seq` Just (ABC_Tok tok, txt')
decodeOp' '"' txt =
    case _decodeLiteral [] txt of
        Nothing -> Nothing
        Just (lit, litEnd) -> case LBS.uncons litEnd of
            Just ('~', txt') -> Just (ABC_Text lit, txt')
            _ -> Nothing
decodeOp' _ _ = Nothing


-- decode a literal until the final LF, allowing LFs to be escaped
-- by following with an SP. The final LF is dropped. If there is no
-- final LF, the remainder of the text is included. 
--
-- Thoughts: At the moment, this function avoids allocating bytestrings.
-- But I wonder if it might be better to copy the string to avoid binding
-- much larger underlying text. 
_decodeLiteral :: [Text] -> Text -> Maybe (Text, Text)
_decodeLiteral r txt = case LBS.elemIndex '\n' txt of
    Nothing -> Nothing -- could not find terminal
    Just idx -> 
        let txt' = LBS.drop (idx+1) txt in -- just past '\n'
        case LBS.uncons txt' of
            Just (' ', txtCont) -> -- escape prior '\n'
                let ln = LBS.take (idx+1) txt in
                _decodeLiteral (ln:r) txtCont
            _ -> 
                let lnf = LBS.take idx txt in -- final line (excludes '\n')
                let lit = LBS.concat (L.reverse (lnf:r)) in
                lit `seq` Just (lit, txt')

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

-- | The 'Quotable' class serves a role similar to 'Show', except
-- that it represents a value or behavior in Awelon Bytecode. 
class Quotable v where 
    quotes :: v -> [Op] -> [Op]

quote :: Quotable v => v -> [Op]
quote = flip quotes []
{-# INLINE quote #-}

-- | Concatenate a list of quotables.
quotesList :: Quotable v => [v] -> [Op] -> [Op]
quotesList (v:vs) = quotes v . quotesList vs
quotesList [] = id

quoteList :: Quotable v => [v] -> [Op]
quoteList = flip quotesList []
{-# INLINE quoteList #-}

instance Quotable ABC where 
    quotes abc [] = abcOps abc
    quotes abc ops = abcOps abc ++ ops
    {-# INLINABLE quotes #-}
instance Quotable PrimOp where 
    quotes = quotes . ABC_Prim
    {-# INLINE quotes #-}
instance Quotable Op where
    quotes = (:)
    {-# INLINE quotes #-} 

instance Quotable Integer where 
    quotes n | (n < 0) = quotes (negate n) . quotes ABC_negate
             | otherwise = qN n
instance (Integral i) => Quotable (Ratio i) where
    quotes r | (r < 0) = quotes (negate r) . quotes ABC_negate
             | (1 == den) = qN num
             | (1 == num) = qN den . quotes ABC_reciprocal
             | otherwise  = qN num . qN den . quotes ABC_reciprocal . quotes ABC_multiply
        where den = denominator r
              num = numerator r

-- quote a non-negative integral
qN :: (Integral i) => i -> [Op] -> [Op]
qN 0 = quotes ABC_newZero
qN n = let (q,r) = n `divMod` 10 in qN q . quotes (opd r)

-- quote an integer into ABC, building from right to left
opd :: (Integral i) => i -> PrimOp
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
opd _ = impossible "invalid digit!"

instance Show ABC where 
    showsPrec _ = showString . LazyUTF8.toString . encode 
instance Show Op where
    showsPrec _ = showList . (:[])
    showList = shows . ABC 
instance Show PrimOp where
    showsPrec _ = showChar . abcOpToChar
    showList = showString . fmap abcOpToChar

instance IsString ABC where
    fromString s =
        let bytes = LazyUTF8.fromString s in
        let (abc, brem) = decode bytes in
        if LBS.null brem then abc else
        let s' = LazyUTF8.toString brem in 
        error $ "Wikilon.ABC.Pure could not parse " ++ s'
