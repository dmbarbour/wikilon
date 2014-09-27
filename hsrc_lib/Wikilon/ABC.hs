{-# LANGUAGE ViewPatterns #-}
-- | Support for Awelon Bytecode 
--
-- Awelon Bytecode is a concatenative language, and streamable.
-- 
-- This is the 'raw' ABC module. For efficient interpretation, I'll
-- want a variation for active ABC that can directly embed quoted
-- values and more efficiently computes compositions. But this raw
-- mode is useful as an intermediate form for input and output.
module Wikilon.ABC
    ( ABC_Op(..)
    ) where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M

import Control.Applicative ((<$>))
import Text.Read (Read(..))
import qualified Text.ParserCombinators.ReadP as R
import qualified Text.ParserCombinators.ReadPrec as RP
import qualified Data.List as L

data ABC_Op -- 43 ops + 3 special cases
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

    -- three special cases 
    | ABC_Block [ABC_Op] -- [ops]
    | ABC_Text String    -- "text\n~; constructs list 
    | ABC_Tok String     -- {token}

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
    | ABC_assert  -- K :: (0+a)*e → (a*e)   (see below)
                  --   where `C` removes a zero from construction, `K` removes
                  --   zero from assertion. K is for describing preconditions
                  --   or postconditions, contracts and dependent types.

    -- pseudo-literal numbers: eleven ops
    | ABC_newZero -- # :: e → N(0)*e        used for pseudo-literal numbers
    | ABC_d0 | ABC_d1 | ABC_d2 | ABC_d3 | ABC_d4 -- (N(x)*e) → (N(10x+d)*e)
    | ABC_d5 | ABC_d6 | ABC_d7 | ABC_d8 | ABC_d9 --   e.g. `#42` to val 42

    -- whitespace identities: two ops
    | ABC_SP | ABC_LF  -- a → a  
    deriving (Eq, Ord)

    -- NOTE: Binaries can be embedded in ABC text or tokens by use of
    -- a specialized base16 alphabet: bdfghjkmnpqstxyz. This is just
    -- a-z minus the vowels and `vrwlc` data plumbing. A specialized 
    -- compression pass will then reduce this to 0.8% overhead for a
    -- large binary.

abcOpCharList :: [(ABC_Op,Char)]
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

abcOpCharMap :: Map ABC_Op Char
abcOpCharMap = M.fromList abcOpCharList

abcCharOpMap :: Map Char ABC_Op
abcCharOpMap = M.fromList (fmap sw abcOpCharList) where
    sw (a,b) = (b,a)

abcOpToChar :: ABC_Op -> Maybe Char
abcOpToChar = flip M.lookup abcOpCharMap

abcCharToOp :: Char -> Maybe ABC_Op
abcCharToOp = flip M.lookup abcCharOpMap

instance Show ABC_Op where 
    showsPrec _ (ABC_Block ops) = showChar '[' . showList ops . showChar ']'
    showsPrec _ (ABC_Text txt) = showChar '"' . showEscaped txt . showChar '\n' . showChar '~'
    showsPrec _ (ABC_Tok tok) = showChar '{' . showString tok . showChar '}'
    showsPrec _ (abcOpToChar -> Just c) = showChar c
    showsPrec _ _ = error "Wikilon is missing ABC to Char conversion"
    showList (x:xs) = shows x . showList xs -- no spaces, commas, brackets
    showList [] = id

-- Escape embedded for ABC. Only LF needs be escaped.
showEscaped :: String -> ShowS
showEscaped ('\n':ss) = showChar '\n' . showChar ' ' . showEscaped ss
showEscaped (c:ss) = showChar c . showEscaped ss
showEscaped [] = id

instance Read ABC_Op where
    readPrec = RP.lift readOp
    readListPrec = RP.lift (R.manyTill readOp R.eof)

readOpC :: R.ReadP ABC_Op
readOpC =
    R.get >>= \ c ->
    case abcCharToOp c of
        Nothing -> R.pfail
        Just opc -> return opc

readOp :: R.ReadP ABC_Op
readOp = 
    (ABC_Block <$> readBlock) R.<++
    (ABC_Text <$> readText) R.<++
    (ABC_Tok <$> readTok) R.<++
    (readOpC) 

readBlock :: R.ReadP [ABC_Op]
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









