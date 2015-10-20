{-# LANGUAGE GeneralizedNewtypeDeriving, OverloadedStrings, ViewPatterns, BangPatterns #-}

-- | Awelon Bytecode (ABC) consists of 42 primitives, texts, tokens, 
-- and blocks. The 42 primitives cover integer maths, data plumbing,
-- conditionals, and first class functions. Tokens support extensions
-- but are typically used as pure annotations. Embedded texts model
-- arbitrary serializable data, comments, etc..
--
-- ABC is a tacit concatenative bytecode. That is, concatenation of
-- bytecode strings corresponds to composition of functions. It is
-- trivial to refactor common substrings as subprograms.
--
-- ABC serializes as simple UTF-8 text, easily streamed over a network
-- or stored in a file. This also applies to runtime constructed values.
-- ABC is intended for network use and persistent applications. Streams
-- of ABC can directly manipulate a remote API or data model.
--
-- This module focuses on just the basic structure of ABC, enough to
-- parse bytecode or serialize it. But for practical use, ABC should
-- be processed and compiled. 
--
-- See Wikilon.ABC for the practical variant.
--
-- TODO: a performance-optimized decoder (or leave this to the 
-- Wikilon.ABC variant?)
-- 
module Wikilon.ABC.Pure
    ( ABC(..)
    , Text
    , Token
    , Op(..)
    , PrimOp(..)
    , abcOpToChar
    , abcCharToOp
    , abcTokens
    , isValidABC
    , encode, encodeBB
    , decode
    , itoabc, itoabc'
    , abcTakeText
    , DecoderStack, DecoderStuck
    ) where

import Control.Arrow (first)
import Data.Maybe (isJust)
import Data.Monoid
-- import qualified Data.ByteString.UTF8 as UTF8
import qualified Data.ByteString.Lazy.Char8 as LBS
import qualified Data.ByteString.Lazy.UTF8 as LazyUTF8
import qualified Data.ByteString.Builder as BB
import qualified Data.Array.IArray as A
import qualified Data.List as L
import Data.String (IsString(..))

import Wikilon.Token
import Wikilon.Text

-- | ABC is a sequence or stream of operations.
newtype ABC = ABC { abcOps :: [Op] }
    deriving (Eq, Ord, Monoid)

-- | An ABC operation is a primitive, block, text, or token. 
data Op
    = ABC_Prim !PrimOp
    | ABC_Block ABC
    | ABC_Text !Text
    | ABC_Tok  {-# UNPACK #-} !Token
    deriving (Eq, Ord)

-- | The forty-two primitive operations.
data PrimOp 
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

    -- working with numbers: five ops
    | ABC_add        -- + :: (N(x)*(N(y)*e)) → (N(x+y)*e)
    | ABC_negate     -- - :: (N(x)*e) → (N(-x)*e)
    | ABC_multiply   -- * :: (N(x)*(N(y)*e)) → (N(x*y)*e) 
    | ABC_divMod     -- Q :: (N(b)*(N(a)*e)) → (N(r)*(N(q)*e))
                     --   assert non-zero b; qb+r = a; r in range [0,b) or (b,0]
    | ABC_compare    -- > :: (N(x)*(N(y)*e)) → (((N(y)*N(x)) + (N(x)*N(y))) * e)
                     --   test if y > x, returning in right if true 
                     --   e.g. #4 #2 > results in (N(2)*N(4)) in right

    -- pseudo-literal numbers: eleven ops
    | ABC_newZero -- # :: e → N(0)*e        used for pseudo-literal numbers
    | ABC_d0 | ABC_d1 | ABC_d2 | ABC_d3 | ABC_d4 -- (N(x)*e) → (N(10x+d)*e)
    | ABC_d5 | ABC_d6 | ABC_d7 | ABC_d8 | ABC_d9 --  e.g. `#42` evaluates to 42

    -- whitespace identities for formatting: two ops 
    | ABC_SP | ABC_LF  -- a → a  
    deriving (Eq, Ord, A.Ix, Enum, Bounded)


abcOpTable :: [(PrimOp,Char)]
abcOpTable =
    [(ABC_l,'l'), (ABC_r,'r'), (ABC_w,'w'), (ABC_z,'z'), (ABC_v,'v'), (ABC_c,'c')
    ,(ABC_L,'L'), (ABC_R,'R'), (ABC_W,'W'), (ABC_Z,'Z'), (ABC_V,'V'), (ABC_C,'C')

    ,(ABC_copy,'^'), (ABC_drop,'%')

    ,(ABC_apply,'$'), (ABC_condApply,'?')
    ,(ABC_quote,'\''), (ABC_compose,'o')
    ,(ABC_relevant,'k'), (ABC_affine,'f')

    ,(ABC_distrib,'D'), (ABC_factor,'F'), (ABC_merge,'M'), (ABC_assert,'K')

    ,(ABC_add,'+'), (ABC_negate,'-')
    ,(ABC_multiply,'*'), (ABC_divMod,'Q')
    ,(ABC_compare,'>')


    ,(ABC_newZero,'#')
    ,(ABC_d0,'0'), (ABC_d1,'1'), (ABC_d2,'2'), (ABC_d3,'3'), (ABC_d4,'4')
    ,(ABC_d5,'5'), (ABC_d6,'6'), (ABC_d7,'7'), (ABC_d8,'8'), (ABC_d9,'9')
    
    ,(ABC_SP,' '), (ABC_LF,'\n')
    ]

abcOpCharArray :: A.Array PrimOp Char
abcOpCharArray = A.accumArray ins eUndef (minBound,maxBound) abcOpTable where
    eUndef = impossible "missing encoding for ABC PrimOp"
    ins _ c = c

abcCharOpArray :: A.Array Char (Maybe PrimOp)
abcCharOpArray = A.accumArray ins Nothing (lb, ub) lst where
    ins _ op = Just op
    lb = L.minimum (fmap snd abcOpTable)
    ub = L.maximum (fmap snd abcOpTable)
    lst = fmap (\(op,c) -> (c, op)) abcOpTable

abcOpToChar :: PrimOp -> Char
abcOpToChar op = abcOpCharArray A.! op

abcCharToOp :: Char -> Maybe PrimOp
abcCharToOp c | inBounds = abcCharOpArray A.! c
              | otherwise = Nothing
    where inBounds = (lb <= c) && (c <= ub)
          (lb,ub) = A.bounds abcCharOpArray

-- | Extract possible tokens from serialized bytecode without a full
-- parse. Essentially, find {token} substrings modulo embedded texts.
abcTokens :: LazyUTF8.ByteString -> [Token]
abcTokens s = case LBS.uncons s of 
    Just (c, s') -> case c of
        '{' -> case LBS.elemIndex '}' s' of
            Just ix -> 
                let tok = LBS.toStrict (LBS.take ix s') in
                let afterTok = LBS.drop (ix + 1) s' in
                (Token tok) : abcTokens afterTok
            Nothing -> [] -- invalid ABC
        '"' -> (abcTokens . dropText) s'
        _   -> abcTokens s'
    Nothing -> []

-- Drop text after `"` up to just before the `~` after text.
dropText :: LBS.ByteString -> LBS.ByteString
dropText s = case LBS.elemIndex '\n' s of
    Nothing -> mempty
    Just ix -> 
        let s' = LBS.drop (ix + 1) s in
        case LBS.uncons s' of
            Just (' ', sCont) -> dropText sCont
            _ -> s' 

-- Consider: a fast token rewrite function? Probably not critical, but would
-- be convenient for renaming a word in a dictionary or similar.

-- | Validate serialized ABC without constructing a parse. Validates
-- tokens and texts according to provided functions. Validates block
-- structure and bytecode. 
isValidABC :: (Token -> Bool) -> (Text -> Bool) -> LazyUTF8.ByteString -> Bool
isValidABC isvTok isvTxt = v (0 :: Int) where
    v !b !s = case LBS.uncons s of
        Nothing -> (b == 0) -- validate blocks are balanced
        Just (c, afterChar) -> case c of
            '[' -> v (b + 1) afterChar
            ']' -> (b > 0) && (v (b - 1) afterChar)
            '{' -> case LBS.elemIndex '}' afterChar of
                Nothing -> False
                Just ix ->
                    let tok = LBS.toStrict (LBS.take ix afterChar) in
                    let afterTok = LBS.drop (ix + 1) afterChar in
                    (isvTok (Token tok)) && (v b afterTok)
            '"' -> let (txt, txtEnd) = abcTakeText afterChar in
                   case LBS.uncons txtEnd of
                        Just ('~', afterText) -> (isvTxt txt) && (v b afterText)
                        _ -> False
            _ -> isJust (abcCharToOp c) && (v b afterChar)


-- | Obtain text after `"` up to just before the `~` after text,
-- with escapes removed (LF SP becomes LF, final LF removed).
abcTakeText :: LBS.ByteString -> (Text, LBS.ByteString)
abcTakeText = first (mconcat . L.reverse) . t [] where
    t r s = case LBS.elemIndex '\n' s of
        Nothing -> (s:r, mempty)
        Just ix -> 
            let s' = LBS.drop (ix + 1) s in
            case LBS.uncons s' of
                Just (' ', sCont) -> 
                    let ln = LBS.take (ix + 1) s in -- line including '\n'
                    t (ln:r) sCont
                _ -> let ln = LBS.take ix s in (ln:r, s')

-- | Translate an integer to ABC that will generate it.
itoabc :: Integer -> ABC
itoabc = ABC . fmap ABC_Prim . itoabc'

itoabc' :: Integer -> [PrimOp]
itoabc' = qi where
    p = (:)
    qi n | (n < 0) = qn (negate n) [ABC_negate]
         | otherwise = qn n []
    qn 0 = p ABC_newZero
    qn n = let (q,r) = n `divMod` 10 in qn q . p (opd r)

opdArray :: A.Array Int PrimOp 
opdArray = A.listArray (0,9) $
    [ABC_d0,ABC_d1,ABC_d2,ABC_d3,ABC_d4
    ,ABC_d5,ABC_d6,ABC_d7,ABC_d8,ABC_d9]

opd :: Integer -> PrimOp
opd = (A.!) opdArray . fromInteger

-- | Serialize Awelon Bytecode into a UTF-8 text. This text isn't 
-- great for human comprehension, but it is marginally legible.
encode :: ABC -> LazyUTF8.ByteString
encode = BB.toLazyByteString . encodeBB

encodeBB :: ABC -> BB.Builder
encodeBB = mconcat . fmap encodeOpBB . abcOps

encodeOpBB :: Op -> BB.Builder
encodeOpBB (ABC_Prim op) = BB.char8 (abcOpToChar op)
encodeOpBB (ABC_Block ops) = BB.char8 '[' <> encodeBB ops <> BB.char8 ']'
encodeOpBB (ABC_Text txt) = encodeTextBB txt
encodeOpBB (ABC_Tok tok) = encodeTokenBB tok

-- | encode a token wrapped in curly braces
encodeTokenBB :: Token -> BB.Builder
encodeTokenBB (Token tok) = BB.char8 '{' <> BB.byteString tok <> BB.char8 '}'

-- | Encode multi-line text including initial " and final ~.
encodeTextBB :: Text -> BB.Builder
encodeTextBB txt =
    let (ln0,lns) = textLines txt in
    BB.char8 '"' <> BB.lazyByteString ln0 <>
    mconcat (fmap _encodeLine lns) <>
    BB.char8 '\n' <> BB.char8 '~'

_encodeLine :: Text -> BB.Builder
_encodeLine l = BB.char8 '\n' <> BB.char8 ' ' <> BB.lazyByteString l

-- | Decode Awelon Bytecode from a stream. This decoder aims more for
-- simplicity than for performance, so is strict in nature and has 
-- a sensible error handling policy. If the decoder fails, the parse
-- state is returned (as DecoderStuck). If it succeeds, the desired
-- bytecode is returned.
--
-- This decoder does not validate texts or tokens. For validation of
-- bytecode, the `isValidABC` function should be favored.
decode :: LazyUTF8.ByteString -> Either DecoderStuck ABC
decode = _decode [] [] 

-- | a stack of reverse-ordered ABC blocks.
type DecoderStack = [[Op]] 

-- | if the decoder fails, we'll return the parser state and 
-- any remaining text.
type DecoderStuck = (DecoderStack, LazyUTF8.ByteString)

_decode :: DecoderStack -> [Op] -> LazyUTF8.ByteString -> Either DecoderStuck ABC
_decode cc r s = 
    let decoderStuck = Left (r:cc,s) in
    case LBS.uncons s of
        Nothing -> case cc of
            [] -> Right (ABC (L.reverse r))
            _ -> decoderStuck -- imbalanced blocks (positive balance)
        Just (c, s') -> case c of
            (abcCharToOp -> Just op) -> _decode cc (ABC_Prim op : r) s'
            '[' -> _decode (r:cc) [] s'
            ']' -> case cc of
                (ops:cc') -> _decode cc' (block : ops) s' where
                    block = ABC_Block (ABC (L.reverse r))
                _ -> decoderStuck -- imbalanced blocks (negative balance)
            '{' -> case LBS.elemIndex '}' s' of
                Just ix -> 
                    let tok = Token (LBS.toStrict (LBS.take ix s')) in
                    let more = LBS.drop (ix + 1) s' in
                    tok `seq` _decode cc (ABC_Tok tok : r) more
                Nothing -> decoderStuck
            '"' -> 
                let (txt, eot) = abcTakeText s' in
                case LBS.uncons eot of
                    Just ('~', more) -> _decode cc (ABC_Text txt : r) more 
                    _ -> decoderStuck
            _ -> decoderStuck

instance Show ABC where 
    showsPrec _ = showString . LazyUTF8.toString . encode 
instance Show Op where
    showsPrec _ = showList . (:[])
    showList ops = shows (ABC ops) 
instance Show PrimOp where
    showsPrec _ = showChar . abcOpToChar
    showList = showString . fmap abcOpToChar

instance IsString ABC where
    fromString s = case decode (LazyUTF8.fromString s) of
        Right abc -> abc
        Left _ -> error $ abcErr $ "could not parse " ++ s

abcErr :: String -> String
abcErr = (++) "Wikilon.ABC.Pure: " 

impossible :: String -> a
impossible = error . abcErr
