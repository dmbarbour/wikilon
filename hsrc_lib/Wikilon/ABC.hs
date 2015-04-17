{-# LANGUAGE BangPatterns, ViewPatterns, OverloadedStrings, DeriveDataTypeable #-}

-- | Wikilon uses Awelon Bytecode (ABC) to model user-defined behavior.
-- ABC has many nice properties for security, distribution, streaming,
-- simplicity, parallelism, and dynamic linking. 
--
-- See <https://github.com/dmbarbour/awelon/blob/master/AboutABC.md>.
--
-- ABC has a major weakness: naive interpretation is inefficient. ABC
-- must be compiled for effective use. JIT compilation is viable, as
-- is separate compilation via {#resourceId} tokens. Wikilon lacks JIT
-- compilation, though approaches using Haskell plugins or Lambdachine
-- may be viable. 
--
-- An intermediate concept for compilation to recognize a dictionary 
-- of common subprograms and accelerate them with built-in functions 
-- in the interpreter. ABCD (ABC Deflated) is will leverage this idea
-- for compression and fast interpretation of streaming code. But ABCD
-- must be standardized carefully.
--
-- Wikilon aims to improve performance by the following:
--
--  * dictionary of ABCD-like accelerated operators
--  * separate values for efficient quotation and access
--  * compact representation of the bytecode, lazy bytestrings
--  * lazy loading of very large values through VCache 
--  * fast slicing for texts, blocks, and tokens
--  * fast compression for storage of large ABC
--
-- Unlike ABCD, the internal Wikilon dictionary doesn't wait on any
-- standards committee. Unlike {#resourceId} tokens, VCache refs are
-- cheap and support implicit reference counting garbage collection.
--
module Wikilon.ABC
    ( ABC(..)
    , Op(..)
    , PrimOp(..)
    , ExtOp(..)
    , Value(..)
    , Rsc, Text, Token, Bytes, Flags

    , asUnit, asPair, asSum, asNumber, asBlock, asText
    , asListOf, fromList
    , asStackOf, asStack, fromStack
    , copyable, droppable

    , extOpTable, extCharToOp, extOpToChar, extOpToABC

    -- * attributes for blocks and links
    , flag_affine,   f_copyable
    , flag_relevant, f_droppable
    , flag_parallel, f_parallel

    -- * 


{-
    -- * primitive ABC evaluation operators.
    , op_l, op_r, op_w, op_z, op_v, op_c
    , op_L, op_R, op_W, op_Z, op_V, op_C
    , op_copy, op_drop
    , op_add, op_negate, op_multiply, op_reciprocal, op_divMod, op_compare
    , op_apply, op_condApply, op_quote, op_compose, op_relevant, op_affine
    , op_distrib, op_factor, op_merge, op_assert
    , op_newZero, op_d0, op_d1, op_d2, op_d3, op_d4, op_d5, op_d6, op_d7, op_d8, op_d9
    , op_SP, op_LF

    -- * multi-byte operators
    , op_Block, op_Text, op_Tok

    -- * Wikilon's extended operators

-}
    -- * 
    , EvalSt(..)
    , EvalErr(..)


{-
    , encodeVal, decodeVal
    , encodeVals, decodeVals
    , encodeABC, decodeABC
-}  


    ) where

import Control.Applicative
import Control.Monad
import Control.Monad.State (State)
import qualified Control.Monad.State as State
import Control.Monad.Error (ErrorT)
import qualified Control.Monad.Error as Error

-- fork/join parallelism on eval
import Control.Concurrent.MVar

import Data.Monoid
import Data.Typeable (Typeable)
import qualified Data.Array.IArray as A
import qualified Data.List as L
import Data.Word
import Data.Bits
import Data.Ratio
import Data.Char
import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Lazy.UTF8 as LazyUTF8
import qualified Data.ByteString.UTF8 as UTF8
import Database.VCache

import ABC.Basic (PrimOp(..), Quotable(..))
import qualified ABC.Basic as Pure

type Token = UTF8.ByteString
type Text = LazyUTF8.ByteString
type Bytes = LBS.ByteString

-- | Wikilon's internal representation of Awelon Bytecode (ABC). This
-- encoding has several features:
--
--  * blocks and texts are tuned for fast slicing
--  * extended op set to accelerate common functions
--  * support to lazily load large values in VCache
--  * stack of values for quotation & parallel eval
--  * compact representation compared to list of ops
--
-- Equality for bytecode is inherently structural.
data ABC = ABC 
    { abc_code :: !Bytes   -- Wikilon internal ABC variant 
    , abc_data :: [Value]  -- stack of precomputed values
    } deriving (Eq, Typeable)

-- | Individual operations from Wikilon. Wikilon uses a slightly
-- different encoding compared to ABC, and the additional context
-- (a stack of precomputed values) is used by some ExtOps and some
-- blocks. 
data Op
    -- Normal ABC
    = ABC_Prim  !PrimOp
    | ABC_Tok   !Token
    | ABC_Text  !Text
    | ABC_Block !Bytes 
    -- Extended Dictionary
    | ABC_Ext   !ExtOp 
    -- Special Manipulations
    | ABC_DataPop   -- ^ » receive data from precomp stack
    -- | ABC_DataPush  -- ^ « push data to precomp stack
    | ABC_DataBind  -- ^ ¡ inline linked resource from precomp stack
    deriving (Eq, Typeable)

-- | Extended Operations are essentially a dictionary recognized by
-- Wikilon for specialized implementations, e.g. to accelerate an
-- interpreter or support tail-call optimizations. ExtOp is similar
-- to ABCD, but doesn't require careful standardization.
--
-- Additionally, these extended operations data stack manipulations.
data ExtOp
    -- tail-call operators
    = Op_Inline -- vr$c (full inline)
    | Op_Apc    -- $c (tail call)

    -- favorite fixpoint function
    | Op_Fixpoint -- [^'ow^'zowvr$c]^'ow^'zowvr$c

    -- mirrored v,c operations
    | Op_Intro1L  -- vvrwlc
    | Op_Elim1L   -- vrwlcc
    | Op_Intro0L  -- VVRWLC
    | Op_Elim0L   -- VRWLCC

    | Op_prim_swap    -- vrwlc
    | Op_prim_mirror  -- VRWLC

    -- annotations? {&par}, {&trace}, {&≡}, etc.
    --  not sure it's worthwhile though, these aren't frequent and
    --  recognition overhead is marginal compared to the processing.

    -- stack swaps?
    -- hand manipulations?
    -- more as needed!
    deriving (Ord, Eq, Bounded, Enum, A.Ix)


-- | Values have a few basic forms:
--
--    (a * b) -- pairs    ~Haskell (a,b)
--    (a + b) -- sums     ~Haskell (Either a b)
--    1       -- unit     ~Haskell ()
--    0       -- void     ~Haskell EmptyDataDecls
--    N       -- numbers  ~Haskell Rational
--    [a→b]   -- blocks   ~Haskell functions or arrows
--    a{:foo} -- sealed   ~Haskell newtype
--
-- Blocks may be affine or relevant, and other substructural types are
-- possible. Sealed values may be cryptographic, using {$format}. 
--
-- Wikilon additionally supports lazily loaded values, i.e. such that
-- developers could model a whole multi-gigabyte filesystem as a value
-- if they desire to do so. This is achieved using VCache.
--
-- I would like to optimize representations and processing for common
-- data structures, especially vectors and matrices.
data Value
    = Number !Rational
    | Pair Value Value
    | SumL Value
    | SumR Value
    | Unit
    | Block !ABC {-# UNPACK #-} !Flags
    | Sealed !Token Value
    -- performance variants
    | Linked Rsc {-# UNPACK #-} !Flags
    | Text !Text
    deriving (Eq, Typeable)

-- | Wikilon uses VCache to model local bytecode resources. Use of
-- VCache simplifies GC issues compared to use of {#resourceId} for
-- inlined ABC or {#resourceId'kf} for large value resources.
type Rsc = VRef ABC

-- | Table of ExtOps and relevant semantics. 
--
-- I'll follow ABCD's mandate to leave the ASCII range to future ABC
-- expansions (though such expansions are very unlikely). ExtOps is
-- effectively a prototype for ABCD.
extOpTable :: [(ExtOp, Char, Pure.ABC)]
extOpTable =
    [(Op_Inline,'£',"vr$c"),(Op_Apc,'¢',"$c")
    ,(Op_Fixpoint,'¥',"[^'ow^'zowvr$c]^'ow^'zowvr$c")
    ,(Op_Intro1L,'ń',"vvrwlc"),(Op_Elim1L,'ć',"vrwlcc")
    ,(Op_Intro0L,'Ń',"VVRWLC"),(Op_Elim0L,'Ć',"VRWLCC")
    ,(Op_prim_swap,'ś',"vrwlc"),(Op_prim_mirror,'Ś',"VRWLC")
    ]

extCharOpArray :: A.Array Char (Maybe ExtOp)
extCharOpArray = A.accumArray ins Nothing (lb,ub) lst where
    ins _ op = Just op
    lb = L.minimum (fmap fst lst)
    ub = L.maximum (fmap fst lst)
    lst = fmap (\(op,c,d) -> (c,op)) extOpTable

extOpDataArray :: A.Array ExtOp (Char, Pure.ABC)
extOpDataArray = A.accumArray ins eUndef (minBound,maxBound) lst where
    lst = fmap (\(op,c,d)->(op,(c,d))) extOpTable
    eUndef = impossible "missing encoding for ABC ExtOp"
    ins _ c = c

extOpToChar :: ExtOp -> Char
extOpToChar = fst . (A.!) extOpDataArray

extOpToABC :: ExtOp -> Pure.ABC
extOpToABC = snd . (A.!) extOpDataArray

extCharToOp :: Char -> Maybe ExtOp
extCharToOp c | inBounds = extCharOpArray A.! c
              | otherwise = Nothing
    where inBounds = (lb <= c) && (c <= ub)
          (lb,ub) = A.bounds extCharOpArray

-- | Flags for blocks
--   bit 0: true if relevant (no drop with %)
--   bit 1: true if affine   (no copy with ^)
--   bit 2: true for parallelism {&par}
--   bit 3..7: Reserved. Tentatively: 
--     memoization for blocks
--     local values (cannot send in message, except in context)
--     ephemeral values (cannot store in state)
--
-- Besides the basic substructural types, it might be worth using
-- annotations to enforce a few new ones.
type Flags = Word8

flag_affine, flag_relevant, flag_parallel :: Flags
flag_relevant = 0x01
flag_affine   = 0x02
flag_parallel = 0x04

flags_include :: Flags -> Flags -> Bool
flags_include f fs = f == (f .&. fs)
{-# INLINE flags_include #-}

f_copyable :: Flags -> Bool
f_copyable = not . flags_include flag_affine
{-# INLINE f_copyable #-}

f_droppable :: Flags -> Bool
f_droppable = not . flags_include flag_relevant
{-# INLINE f_droppable #-}

f_parallel :: Flags -> Bool
f_parallel = flags_include flag_parallel
{-# INLINE f_parallel #-}

-- | Is this value copyable (not affine)
copyable :: Value -> Bool
copyable (Number _) = True
copyable (Pair a b) = copyable a && copyable b
copyable (SumL a) = copyable a
copyable (SumR b) = copyable b
copyable Unit = True
copyable (Block _ f) = f_copyable f
copyable (Sealed _ v) = copyable v
copyable (Linked _ f) = f_copyable f
copyable (Text _) = True

-- | Is this value droppable (not relevant)
droppable :: Value -> Bool
droppable (Number _) = True
droppable (Pair a b) = droppable a && droppable b
droppable (SumL a) = droppable a
droppable (SumR b) = droppable b
droppable Unit = True
droppable (Block _ f) = f_droppable f
droppable (Sealed _ v) = droppable v
droppable (Linked _ f) = f_droppable f
droppable (Text _) = True

{-# INLINE asUnit #-}
asUnit :: (Monad m) => Value -> m ()
asUnit Unit = return ()
asUnit (Linked l _) = loadValM l >>= asUnit
asUnit v = fail $ abcErr $ "expecting unit, received " ++ show v

{-# INLINE asPair #-}
asPair :: (Monad m) => Value -> m (Value,Value)
asPair (Pair a b) = return (a,b)
asPair (Linked l _) = loadValM l >>= asPair
asPair v = fail $ abcErr $ "expecting pair, received " ++ show v

{-# INLINE asSum #-}
asSum :: (Monad m) => Value -> m (Either Value Value)
asSum (SumL a) = return (Left a)
asSum (SumR b) = return (Right b)
asSum (Linked l _) = loadValM l >>= asSum
asSum (Text t) = case LazyUTF8.uncons t of
    Nothing -> return $ Right Unit
    Just (c,t') -> 
        let n = Number (fromIntegral (ord c)) in
        return $ Left (Pair n (Text t'))
asSum v = fail $ abcErr $ "expecting sum, received " ++ show v

{-# INLINE asNumber #-}
asNumber :: (Monad m) => Value -> m Rational
asNumber (Number r) = return r
asNumber (Linked l _) = loadValM l >>= asNumber
asNumber v = fail $ abcErr $ "expecting number, received " ++ show v

{-# INLINE asCharNum #-}
asCharNum :: (Monad m) => Value -> m Char
asCharNum num = 
    asNumber num >>= \ r ->
    let n = numerator r in
    let d = denominator r in
    let ok = (1 == d) && (0 <= n) && (n <= 0x10ffff) in
    if ok then return (chr (fromInteger n)) else
    fail $ abcErr $ "expecting utf8 codepoint, received " ++ show num

{-# INLINE asBlock #-}
asBlock :: (Monad m) => Value -> m (ABC, Flags)
asBlock (Block abc f) = return (abc,f)
asBlock (Linked l _) = loadValM l >>= asBlock
asBlock v = fail $ abcErr $ "expecting block, received " ++ show v

{-# INLINE asText #-}
asText :: (Monad m) => Value -> m Text
asText (Text t) = return t
asText (Linked l _) = loadValM l >>= asText
asText v = asText' mempty v 

{-# INLINE asText' #-}
asText' :: (Monad m) => BB.Builder -> Value -> m Text 
asText' !bb (Text t) = return $! (BB.toLazyByteString bb) <> t
asText' !bb s = asSum s >>= \ lr -> case lr of
    Right u -> asUnit u >> return $! BB.toLazyByteString bb
    Left p -> 
        asPair p >>= \ (n , t') -> 
        asCharNum n >>= \ c ->
        asText' (bb <> BB.charUtf8 c) t'

-- | Obtain a list from a value, with monadic failure. In this case
-- we expect an argument of form µL.((a*L)+1).
asListOf :: (Monad m) => (Value -> m a) -> Value -> m [a]
{-# INLINE asListOf #-}
asListOf fn = asListOf' [] where
    asListOf' xs = asSum >=> \ e -> case e of
        Right u -> asUnit u >> return (L.reverse xs) 
        Left p -> asPair p >>= \ (a,l) -> fn a >>= \ x -> asListOf' (x:xs) l

-- | Encode a list into a value, using ABC's underlying list structure,
-- which is λa.µL.((a*L)+1). Every item in a list is a sum.
fromList :: [Value] -> Value
{-# INLINE fromList #-}
fromList (x:xs) = SumL (Pair x (fromList xs))
fromList [] = SumR Unit


-- | Obtain a stack of values. A stack is just pairs terminated by a
-- Unit value. Within ABC, it is generally necessary that knowledge of
-- stack size be implicit in context. Values on a stack may generally
-- be of heterogeneous type.
asStackOf :: (Monad m) => (Value -> m a) -> Value -> m [a]
{-# INLINE asStackOf #-}
asStackOf fn (Pair a b) =
    fn a >>= \ x ->
    asStackOf fn b >>= \ xs ->
    return (x:xs)
asStackOf _ Unit = return []
asStackOf fn (Linked l _) = loadValM l >>= asStackOf fn
asStackOf _ v = fail $ abcErr $ "expecting stack, received " ++ show v

-- | Convert to a stack of values with error-based failure.
asStack :: Value -> [Value]
asStack (Pair a b) = a : asStack b
asStack Unit = []
asStack (Linked l _) = asStack (loadVal l)
asStack v = impossible $ "expecting stack, received " ++ show v

-- | Convert a list of values to a stack of values. Trivial.
fromStack :: [Value] -> Value
fromStack (x:xs) = (Pair x (fromStack xs))
fromStack [] = Unit


-- | Evaluator state. (Not sure exactly how much I want here.)
--
-- When we evaluate content, we have some stack of precomputed values
-- for fast quotation (which must be used precisely) and a sequence 
-- of bytes indicating continuation behavior. In some cases, for tail
-- calls, we might extend these stacks and continuations at the near
-- end.
--
-- Additionally, we have very minimal interaction with IO. While ABC
-- does have potential effects via {tokens}, Wikilon doesn't leverage
-- this feature. Instead, effects are modeled via machines, network,
-- and messaging `(InMsg*State)→(OutMsgList*State)`. However, I need
-- effects for:
--
--  * fork-join parallelism
--  * primitive debug traces
--  * timeout, interruption
--
-- At the Haskell layer, this will be modeled using unsafePerformIO.
--
data EvalSt = EvalSt
    { eval_env   :: !Value -- ^ primary input for evaluation
    , eval_stack :: !Value -- ^ precomputed values for fast load
    , eval_cont  :: !LBS.ByteString -- ^ continuation behavior
    , eval_cost  :: !Int -- ^ decide when to tick
    , eval_meta  :: !EvalMeta -- ^ relatively stable elements
    } deriving (Show)
data EvalMeta = EvalMeta
    { eval_tid   :: !Integer -- ^ thread ID; 2* 1+ for left, 2* 2+ for right
    , eval_join  :: ![JoinHandle] -- ^ threads to join
    , eval_fork  :: !(EvalSt -> IO JoinHandle)
    , eval_trace :: !(Value -> IO ()) -- ^ debug output
    , eval_tick  :: !(EvalSt -> IO Bool) -- ^ debug and interrupt support
    , eval_space :: !VSpace
    }

instance Show (EvalMeta) where
    show em = "... " ++ joinMsg where
        nJoin = L.length (eval_join em)
        joinMsg = if (nJoin > 0) then show nJoin ++ " threads" else ""

data EvalErr = EvalErr 
    { err_dump   :: !EvalSt
    , err_arg    :: !Value
    , err_msg    :: !Text
    } deriving (Show)
type EvalErrors = [EvalErr]
type JoinHandle = MVar EvalErrors

type Eval a = ErrorT EvalErrors (State EvalSt) a

runEvalMaybe :: Eval a -> Maybe a
runEvalMaybe = error "TODO"





readVarNat :: LBS.ByteString -> (Int, LBS.ByteString)
readVarNat = r 0 where
    r !n !t = case LBS.uncons t of
        Nothing -> impossible "bad VarInt in Wikilon.ABC code"
        Just (byte, t') ->
            let n' = n `shiftL` 7 .|. (fromIntegral (byte .&. 0x7f)) in
            let bDone = (0 == (byte .&. 0x80)) in
            if bDone then (n', t') else r n' t'






loadValM :: (Monad m) => Rsc -> m Value
{-# INLINE loadValM #-}
loadValM _ = fail $ abcErr $ "todo: loadValM"

loadVal :: Rsc -> Value
{-# INLINE loadVal #-}
loadVal _ = impossible "todo: loadVal"


















-- QUESTION: How are resources represented in VCache?
--
-- Wikilon's internal ABC uses 'escapes' to access precomputed data.
-- Thus, to properly encode our bytecode resources, we must reliably
-- regenerate any precomputed content.
--
-- Following ABC's example, I'll make these implicit escapes very
-- explicit and functionally meaningful. Each block will also pop a
-- value because it recursively contains some precomputed data. The
-- ability to push a value allows me to directly encode blocks and
-- avoid recursive encodings in the 'precompute the value' sections.
--
--      [       pop value for encoded block
--      »       pop value resource from stack
--      ¡       inline a bytecode resource
--
-- All link resources must be pushed to the toplevel. So let's say
-- we start with a stack of link resources, use this to compute our
-- main values stack, then use our main values stack for our ABC.
-- This avoids arbitrary recursion.
--
-- Additionally, we'll capture sizes for every block, token, and text:
--
--      [(Size)bytecode]
--      {(Size)token}
--      "(Size)textWithoutEscapes~
--
-- The size allows fast slicing. The final character provides a sanity
-- check and visual delimiter when debugging. Tokens and text may use
-- ABC base16 compression internally.
--
-- The whole stream will finally be subjected to a fast compression.
-- Candidate algorithms: LZ4, Snappy. These algorithms aren't great
-- for compaction, but ABC should be a relatively easy target. And
-- even a 50% compaction could make a useful performance difference
-- on read. (Besides, decompression simply becomes part of copying
-- the input.)
--
-- I might also add some sort of explicit interrupt or breakpoint
-- opcode.















--  * develop dictionary of built-in operators to accelerate performance
--    * each operator has simple expansion to ABC
--    * recognize or parse operator from raw ABC
--    * how to best encode? Not sure. Maybe 0xFD+VarNat?
--  * precomputed values and cheap quotations 
--    * stack of values per node 
--    * similar to how VCache uses stack of VRefs?
--  * support linked ABC resources with VCache
--    * resources involve VRefs to more ABC nodes
--    * preferably leverage same stack of values
--    * lazy loading of value resources
--  * rewrite texts, blocks, and tokens to support fast slicing
--    * "(length)(content)~
--    * [(links)(length)(content)]
--    * {(length)(content)}
--    * length and links have VarNat representation
--    * escapes are removed from the text content
--  * Compression of larger ABC at VCache layer (option?)
--    * combine texts for precomputed values and ABC for best compression
--    * Base16 compression for tokens and texts
--


-- composition is still trivial
instance Monoid ABC where
    mempty = ABC { abc_code = LBS.empty, abc_data = [] }
    mappend a b = ABC { abc_code = _code, abc_data = _data } where
        _code = abc_code a <> abc_code b
        _data = abc_data a <> abc_data b

instance Show ABC where showsPrec _ = shows . Pure.quote
instance Show Value where showsPrec _ = shows . Pure.quote

-- Value will quote as ABC that regenerates it. This includes
-- regenerating any linked or stowed value resources.
instance Quotable Value where
    quotes (Number r) = quotes r
    quotes (Text t) = quotes (Pure.ABC_Text t)
    quotes (asText -> Just t) = quotes (Pure.ABC_Text t)
    quotes (Pair a b) = quotes a . quotes b . quotes ("wl" :: Pure.ABC)
    quotes (SumL a) = quotes a . quotes ("V" :: Pure.ABC)
    quotes (SumR b) = quotes b . quotes ("VVRWLC" :: Pure.ABC)
    quotes Unit = quotes ("vvrwlc" :: Pure.ABC)
    quotes (Block abc flags) = 
        let block = quotes (Pure.ABC_Block $ Pure.ABC $ Pure.quote abc) in
        let bAff = flags_include flag_affine flags in
        let bRel = flags_include flag_relevant flags in
        let bPar = flags_include flag_parallel flags in
        let k = if bRel then quotes ABC_relevant else id in
        let f = if bAff then quotes ABC_affine else id in
        let p = if bPar then quotes (Pure.ABC_Tok "{&par}") else id in
        block . k . f . p
    quotes (Sealed tok a) = quotes a . quotes (Pure.ABC_Tok tok)
    quotes (Linked l _) = quotes (deref' l) . quotes (Pure.ABC_Tok "{&stow}")

instance Quotable ExtOp where
    quotes = quotes . extOpToABC

instance Quotable ABC where
    quotes = error $ abcErr "TODO: Quotable for Wikilon.ABC"

-- stub to prevent type errors for now
instance VCacheable ABC where
    put abc = error $ abcErr "TODO: VCacheable ABC"
    get = error $ abcErr "TODO: VCacheable ABC"

instance VCacheable Value where
    put v = error $ abcErr "TODO: VCacheable Value"
    get = error $ abcErr "TODO: VCacheable Value"


abcErr :: String -> String
abcErr = (++) "Wikilon.ABC: "

impossible :: String -> a
impossible = error . abcErr
