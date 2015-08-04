{-# LANGUAGE ViewPatterns #-}

-- | A pure model of Awelon Bytecode, no accelerators or extensions.
module Awelon.ABC
    ( ABC, abcOps, abcOpsCount, mkABC
    , Text
    , Token
    , Op(..)
    , PrimOp(..)
    , abcCharToOp, abcOpToChar, abcOpTable
    , encode
    , encodeBB
    , encodeOpBB
    , encodeTextBB
    , encodeTokenBB
    , decode, decode'
    , DecoderCont(..), DecoderStuck(..)
    , decodeLiteral
    , Quotable(..)
    , quote, quoteList, quotesList
    , primQuoteInteger

    , opsCancel
    , abcSimplify
    , tokens
    , abcTexts
    , rewriteTokens
    ) where

import Data.Monoid
import qualified Data.ByteString.UTF8 as UTF8
import qualified Data.ByteString.Lazy.Char8 as LBS
import qualified Data.ByteString.Lazy.UTF8 as LazyUTF8
import qualified Data.ByteString.Builder as BB
import qualified Data.Array.IArray as A
import qualified Data.List as L
import Data.String (IsString(..))

import Awelon.Text

-- | Bytecode is a sequence or stream of operations
--
-- I additionally keep an ops count for eval quotas. 
data ABC = ABC 
    { _abcOps      :: ![Op]
    , _abcOpsCount :: {-# UNPACK #-} !Int -- heuristic cost for quota
    } deriving (Eq, Ord)

abcOps :: ABC -> [Op]
abcOps = _abcOps

abcOpsCount :: ABC -> Int
abcOpsCount = _abcOpsCount

-- | construct ABC from just the operations. Uses length of ops
-- as estimate of cost.
mkABC :: [Op] -> ABC
mkABC ops = ABC ops (L.length ops)


-- | A token has the form {token} in ABC, and call to an external
-- procedure in the interpreter. Potentially, this enables side
-- effects, though Wikilon only uses annotations, value sealing,
-- and resource linking. Token type is usually indicated by one
-- prefix character. For example:
--
--   & - annotations, must have identity semantics but may be
--       useful for performance or type safety analysis
--   : - value sealing, discretionary unless contains $ in which
--       case the sealer is cryptographic (e.g. {:aes$key}).
--   . - unseal value from corresponding sealer
--   $ - indicates a sealed value, e.g. {$aes:fingerPrint}
--       will typically use hash of key as fingerprint
--   # - link content-addressed resources by secure hash
--   % - link named resources associated with local dictionary
--
type Token = UTF8.ByteString

data Op -- 43 primitives + 3 special cases
    = ABC_Prim !PrimOp -- 43 primitives
    | ABC_Block !ABC    -- block type
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
    | ABC_divMod     -- Q :: (N(b)*(N(a)*e)) → (N(r)*(N(q)*e))
                     --   assert non-zero b; qb+r = a; r in range [0,b) or (b,0]
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

abcOpTable :: [(PrimOp,Char)]
abcOpTable =
    [(ABC_l,'l'), (ABC_r,'r'), (ABC_w,'w'), (ABC_z,'z'), (ABC_v,'v'), (ABC_c,'c')
    ,(ABC_L,'L'), (ABC_R,'R'), (ABC_W,'W'), (ABC_Z,'Z'), (ABC_V,'V'), (ABC_C,'C')

    ,(ABC_copy,'^'), (ABC_drop,'%')

    ,(ABC_add,'+'), (ABC_negate,'-')
    ,(ABC_multiply,'*'), (ABC_divMod,'Q')
    ,(ABC_compare,'>')

    ,(ABC_apply,'$'), (ABC_condApply,'?')
    ,(ABC_quote,'\''), (ABC_compose,'o')
    ,(ABC_relevant,'k'), (ABC_affine,'f')

    ,(ABC_distrib,'D'), (ABC_factor,'F'), (ABC_merge,'M'), (ABC_assert,'K')

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

-- | Encode pure ABC into a Lazy UTF8 ByteString
encode :: ABC -> Text
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
encodeTokenBB tok = BB.char8 '{' <> BB.byteString tok <> BB.char8 '}'

-- | Encode multi-line text including initial " and final ~.
encodeTextBB :: Text -> BB.Builder
encodeTextBB txt =
    let (ln0,lns) = textLines txt in
    BB.char8 '"' <> BB.lazyByteString ln0 <>
    mconcat (fmap _encodeLine lns) <>
    BB.char8 '\n' <> BB.char8 '~'

_encodeLine :: Text -> BB.Builder
_encodeLine l = BB.char8 '\n' <> BB.char8 ' ' <> BB.lazyByteString l

-- | our decoder tracks a stack of blocks at [] boundaries
data DecoderCont 
    = DecoderDone
    | DecodeBlock [Op] DecoderCont
    deriving (Show)
-- I could also more precisely indicate errors when decoding a large
-- text, but I don't think that's as important; just getting to the
-- text is good enough.

-- | When we get stuck, we will only backtrack to the last fully 
-- parsed operator rather than backtrack across block boundaries. 
-- This should provide reasonably precise error information.
data DecoderStuck = DecoderStuck
    { dcs_text :: Text -- where did we get stuck
    , dcs_data :: [Op] -- reverse ordered ops
    , dcs_cont :: DecoderCont
    } deriving (Show)

-- | Decode ABC from Text. 
--
-- Parsed ABC is represented losslessly. If we get stuck at any point,
-- a DecoderStuck instance is returned which will include the text on
-- which it became stuck. Spaces (SP and LF) are preserved.
decode :: Text -> Either DecoderStuck ABC
decode txt = decode' $ DecoderStuck 
    { dcs_text = txt
    , dcs_data = []
    , dcs_cont = DecoderDone 
    }

-- | Decode starting from a DecoderStuck state, i.e. assuming it has
-- been tweaked so we should no longer be stuck. This allows logic
-- surrounding a decoder to try a fix and continue approach.
decode' :: DecoderStuck -> Either DecoderStuck ABC
decode' s = decoder (dcs_cont s) (dcs_data s) (dcs_text s)

decoder :: DecoderCont -> [Op] -> Text -> Either DecoderStuck ABC 
decoder cc r txt0 = 
    let decoderIsStuck = Left (DecoderStuck txt0 r cc) in
    case LBS.uncons txt0 of
        Nothing -> case cc of
            DecoderDone -> Right (mkABC (L.reverse r))
            _ -> decoderIsStuck
        Just (c, txt) -> case c of
            (abcCharToOp -> Just op) -> decoder cc (ABC_Prim op : r) txt
            '[' -> decoder (DecodeBlock r cc) [] txt
            ']' -> case cc of
                DecodeBlock ops cc' -> decoder cc' (block : ops) txt where
                    block = ABC_Block (mkABC (L.reverse r))
                _ -> decoderIsStuck
            '{' -> case LBS.elemIndex '}' txt of
                Nothing -> decoderIsStuck
                Just idx ->
                    let (lzt, tokEnd) = LBS.splitAt idx txt in
                    let bOK = not $ LBS.elem '{' lzt || LBS.elem '\n' lzt in
                    if not bOK then decoderIsStuck else
                    let tok = LBS.toStrict lzt in
                    let txt' = LBS.drop 1 tokEnd in
                    tok `seq` decoder cc (ABC_Tok tok : r) txt'
            '"' -> case decodeLiteral txt of
                Nothing -> decoderIsStuck
                Just (lit, litEnd) -> case LBS.uncons litEnd of
                    Just ('~', txt') -> decoder cc (ABC_Text lit : r) txt'
                    _ -> decoderIsStuck
            _ -> decoderIsStuck


-- | Decode ABC literal assuming the first '"' has already been taken,
-- halting after the first LF not escaped by a following SP. In ABC,
-- the character just after decoding a literal should be '~'. This 
-- will fail if there is no final LF character.
decodeLiteral :: Text -> Maybe (Text, Text)
decodeLiteral = _decodeLiteral []

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

{-
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
-}

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
    quotes n = (++) (fmap ABC_Prim qn) where
        qn = primQuoteInteger n []

-- | Quote an integer into a sequence of ABC primitives.
primQuoteInteger :: Integer -> [PrimOp] -> [PrimOp]
primQuoteInteger = qi where
    p = (:)
    qi n | (n < 0) = qn (negate n) . p ABC_negate
         | otherwise = qn n
    qn 0 = p ABC_newZero
    qn n = let (q,r) = n `divMod` 10 in qn q . p (opd r)

opdArray :: A.Array Int PrimOp 
opdArray = A.listArray (0,9) $
    [ABC_d0,ABC_d1,ABC_d2,ABC_d3,ABC_d4
    ,ABC_d5,ABC_d6,ABC_d7,ABC_d8,ABC_d9]

opd :: Integer -> PrimOp
opd = (A.!) opdArray . fromInteger

-- Monoid for ABC is concatenation, which represents composition.
instance Monoid ABC where
    mempty = ABC [] 0
    mappend a b = ABC ops ct where
        ops = abcOps a <> abcOps b
        ct = abcOpsCount a + abcOpsCount b
    mconcat lst = ABC ops ct where
        ops = L.concatMap abcOps lst
        ct = L.sum $ fmap abcOpsCount lst

instance Show ABC where 
    showsPrec _ = showString . LazyUTF8.toString . encode 
instance Show Op where
    showsPrec _ = showList . (:[])
    showList ops = shows (ABC ops minBound) 
instance Show PrimOp where
    showsPrec _ = showChar . abcOpToChar
    showList = showString . fmap abcOpToChar

instance IsString ABC where
    fromString s =
        let bytes = LazyUTF8.fromString s in
        case decode bytes of
            Right abc -> abc
            Left stuck -> 
                let s' = LazyUTF8.toString (dcs_text stuck) in
                error $ abcErr $ "could not parse " ++ s'


-- | abcSimplify performs a simple optimization on ABC code based on
-- recognizing short sequences of ABC that can be removed. E.g.
--
--   LF, SP, 
--   ww, zz, vc, cv, rl, lr, 
--   WW, ZZ, VC, CV, RL, LR
-- 
-- In addition, we translate 'zwz' to 'wzw' (and for sums)
--
-- And we'll inline [block]vr$c or v[block]$c
--
abcSimplify :: [Op] -> [Op]
abcSimplify = zSimp []


--
-- redesign thoughts: it might be better to move leftwards rather than rightwards
--  i.e. such that there is no reverse at the end, and it's easier to simplify
--  as part of a concatenation effort
zSimp :: [Op] -> [Op] -> [Op]
zSimp (ABC_Prim a:as) (ABC_Prim b:bs) | opsCancel a b = zSimp as bs
zSimp rvOps (ABC_Block block : ops) = zSimp (ABC_Block block' : rvOps) ops where
    block' = (mkABC . abcSimplify . abcOps) block
zSimp rvOps (ABC_Prim ABC_SP : ops) = zSimp rvOps ops
zSimp rvOps (ABC_Prim ABC_LF : ops) = zSimp rvOps ops
zSimp (ABC_Prim ABC_w : ABC_Prim ABC_z : rvOps) (ABC_Prim ABC_z : ops) =
    zSimp rvOps (ABC_Prim ABC_w : ABC_Prim ABC_z : ABC_Prim ABC_w : ops)
zSimp (ABC_Prim ABC_W : ABC_Prim ABC_Z : rvOps) (ABC_Prim ABC_Z : ops) =
    zSimp rvOps (ABC_Prim ABC_W : ABC_Prim ABC_Z : ABC_Prim ABC_W : ops)
zSimp (ABC_Block block : rvOps) 
      (ABC_Prim ABC_v : ABC_Prim ABC_r : ABC_Prim ABC_apply : ABC_Prim ABC_c : ops) =
    zSimp rvOps (abcOps block ++ ops)
zSimp (ABC_Block block : ABC_Prim ABC_v : rvOps)
      (ABC_Prim ABC_apply : ABC_Prim ABC_c : ops) =
    zSimp rvOps (abcOps block ++ ops)

zSimp rvOps (b:bs) = zSimp (b:rvOps) bs
zSimp rvOps [] = L.reverse rvOps

-- | compute whether two operations cancel
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

-- | Obtain a list of tokens from ABC code. Tokens are presented in
-- the same order and quantity as they exist in the original code.
tokens :: ABC -> [Token]
tokens = flip _run [] where
    _run = runABC _op 
    _op (ABC_Tok t) = (t:)
    _op (ABC_Block abc) = _run abc
    _op _ = id

-- | Obtain a list of texts from ABC code. Texts are presented in
-- the same order as they exist in the original code.
abcTexts :: ABC -> [Text]
abcTexts = flip _run [] where
    _run = runABC _op
    _op (ABC_Text t) = (t:)
    _op (ABC_Block abc) = _run abc
    _op _ = id

runABC :: (Op -> a -> a) -> ABC -> a -> a
runABC fn = flip (L.foldr fn) . abcOps

-- | Rewrite or expand tokens into arbitrary bytecode
rewriteTokens :: (Token -> [Op]) -> ABC -> ABC
rewriteTokens fn = mkABC . rw . abcOps where
    rw (ABC_Tok t : ops) = fn t ++ rw ops
    rw (ABC_Block abc : ops) = block : rw ops where
        block = ABC_Block (rewriteTokens fn abc)
    rw (op : ops) = op : rw ops
    rw [] = []

abcErr :: String -> String
abcErr = (++) "Awelon.ABC: " 

impossible :: String -> a
impossible = error . abcErr


