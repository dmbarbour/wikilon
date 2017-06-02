{-# LANGUAGE BangPatterns #-}
-- | This module is concerned with parsing or serializing the Awelon
-- dictionary representation, which essentially consists of:
--
--   secureHashOfPatch1
--   secureHashOfPatch2
--   @word1 def1
--   @word2 def2
--   @@ns secureHashOfDict
--   ...
--
-- Secure hashes in Awelon identify external resources (see Stowage
-- under Awelon.CX). A dictionary starts with a sequence of secure
-- hashes (one per line) that are logically included, followed by
-- definitons. Each definition may specify a namespace or a word,
-- indicated by prefix - `@@ns` or `@word`.  
--
-- A namespace is defined with a secure hash (or may be empty), while
-- a word should be defined with valid Awelon code. Awelon code also
-- has a very simple syntactic structure (see Awelon.Syntax). 
--
-- Order of definitions for distinct symbols is irrelevant. But if a
-- word or namespace is defined more than once, the 'last' definition
-- wins. This provides a simple, compositional patch and update model.
-- A dictionary might be treated as an append-only update log. A full
-- dictionary may be understood as a patch on the empty dictionary.
--
-- By convention, a word may be 'undefined' by defining it to itself,
-- e.g. `@foo foo`. Cyclic definitions aren't valid Awelon, but we'll
-- not call out the trivial cycle any more than we normally would an
-- undefined word. A namespace may be undefined via the empty string.
--
-- This module only detects trivial parse errors. A lot more work must
-- be done to search for cyclic definitions, type or totality errors,
-- and so on.
-- 
module Awelon.Dict.Format
    ( Dict(..), emptyDict
    , decode, DictErr
    , encodeBB, encode
    , splitDict, splitLL, findLLSep 
    ) where

import Prelude hiding (Word)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Lazy.UTF8 as LU8
import qualified Data.List as L
import Data.Int (Int64)
import Data.Word (Word8)
import qualified Data.Map as M
import Data.Monoid
import Awelon.Syntax (NS(..), Word(..), Prog(..), validWordByte)
import qualified Awelon.Syntax as P
import Awelon.Hash

-- | A parsed dictionary binary or patch file.
--
-- This representation loses irrelevant information such as whitespace
-- formatting, order of definitions, and definitions later overridden.
--
-- For the namespaces, the `Nothing` value indicates an empty string was
-- specified, so we only use a Hash value where we actually have a valid
-- hash string.
data Dict = Dict { dictInc  :: [Hash]
                 , dictNS   :: M.Map NS (Maybe Hash)
                 , dictDefs :: M.Map Word Prog
                 } deriving (Eq, Ord)

emptyDict :: Dict
emptyDict = Dict mempty mempty mempty

isWS :: Word8 -> Bool
isWS c = (10 == c) || (32 == c)

skipWS :: LBS.ByteString -> LBS.ByteString
skipWS = LBS.dropWhile isWS

readHash :: LBS.ByteString -> Maybe (Hash, LBS.ByteString)
readHash s = 
    let (h, s') = LBS.span validHashByte s in
    if (validHashLen /= LBS.length h) then Nothing else
    Just (LBS.toStrict h, s')

readHashes :: LBS.ByteString -> ([Hash], LBS.ByteString)
readHashes = rd [] . skipWS where
    rd r s = case readHash s of
        Just (h, s') -> rd (h:r) (skipWS s')
        Nothing -> (L.reverse r, s)

-- | Dictionary decode errors.
--
-- At the moment, errors are simply returned in terms of unprocessed
-- entries, or any unrecognized suffix of the header. I might need to
-- improve error reporting in the future, e.g. to include some line
-- numbers. But this should be sufficient for simple error reports.
type DictErr = (LBS.ByteString, [LBS.ByteString])

-- | Decode (parse) a dictionary binary. 
--
-- This is a lenient decoder. It parses as much as it can while
-- returning both the dictionary and parse errors.
decode :: LBS.ByteString -> (DictErr, Dict)
decode binary = ((eInc, eEnt), Dict inc ns defs) where
    (hd, bdy) = splitDict binary 
    (inc, eInc) = readHashes hd
    ents = fmap decodeEnt bdy
    eEnt = [e | Right (Right e) <- ents]
    ns = M.fromList [x | Right (Left x) <- ents]
    defs = M.fromList [x | Left x <- ents]

decodeEnt :: LBS.ByteString -> Either (Word, Prog) (Either (NS, Maybe Hash) LBS.ByteString)
decodeEnt s = 
    let badEnt = Right (Right s) in
    case LBS.uncons s of
        Just (64, s') ->
            let (w,d) = LBS.span validWordByte s' in
            if (LBS.null w) then badEnt else
            let ns = NS (Word (LBS.toStrict w)) in
            case decodeNSDef d of
                Just h -> Right (Left (ns, h))
                _ -> badEnt
        _ ->
            let (w,d) = LBS.span validWordByte s in
            if (LBS.null w) then badEnt else
            let w' = Word (LBS.toStrict w) in
            case P.decode d of
                Right prog -> Left (w', prog)
                _ -> badEnt

-- | A namespace definition is either a single hash or empty string.
-- Here 'Nothing' means parse failure, while 'Just Nothing' means we
-- received an empty string. Whitespace is trimmed.
decodeNSDef :: LBS.ByteString -> Maybe (Maybe Hash)
decodeNSDef s =
    let s' = skipWS s in
    if LBS.null s' then Just Nothing else
    case readHash s' of
        Just (h, rem) | LBS.null (skipWS rem) -> Just (Just h)
        _ -> Nothing

-- | Our encoder will write out a dictionary binary. 
encode :: Dict -> LBS.ByteString
encode = BB.toLazyByteString . encodeBB

-- | Encode with a flexible output strategy. This uses a very
-- simple and consistent space formatting.
encodeBB :: Dict -> BB.Builder
encodeBB (Dict inc ns defs) = bbInc <> bbNS <> bbDefs where
    bbInc   = mconcat $ fmap bbIncElem inc
    bbNS    = mconcat $ fmap bbNSElem (M.toList ns)
    bbDefs  = mconcat $ fmap bbDefElem (M.toList defs)
    bbIncElem h = BB.byteString h <> BB.word8 10
    bbNSElem (ns, def) = 
        BB.word8 64 <> BB.word8 64 <>  BB.byteString (wordBytes (nsWord ns)) <>
        BB.word8 32 <> maybe mempty BB.byteString def <> BB.word8 10
    bbDefElem (w, def) =
        BB.word8 64 <> BB.byteString (wordBytes w) <> BB.word8 32 <>
        P.encodeBB def <> BB.word8 10

instance Show Dict where
    showsPrec _ = showString . LU8.toString . encode

{-
    let mkE = Right . Right in
    case LBS.uncons s of
        Just (64, s') ->
            let (w,d) = LBS.span validWordByte s' in
            if LBS.null w then mkE (BadEnt s) else

            let ns = P.NS (P.Word (LBS.toStrict w)) in
            let (h,r) = LBS.span validHashByte (skipWS d) in
            let okHash = LBS.null h || isHash h in
            let okRem = LBS.null (skipWS r) in
            if not (okHash && okRem) then mkE (BadNS ns d) else

            let h' = if LBS.null h then Nothing else Just (LBS.toStrict h) in
            Right (Left (ns, h'))
        _ -> 
            let (w, d) = LBS.span validWordByte s in
            if LBS.null w then mkE (BadEnt s) else
            
            let w' = P.Word (LBS.toStrict w) in
            case P.decode d of
                Right def -> Left (w', def)
                _ -> mkE (BadDef w' d)
            

            let (
            let okDef = (LBS.null h || isHash h) && LBS.null (skipWS r) in
             
            let okH = (LBS.null h) || (isHash h) in
            let okW = not (LBS.null w) in
            let okRem = LBS.null (skipWS rem) in
            

            let ok = 
            let ns = P.NS (P.Word (LBS.toStrict w)) in
            if LBS.null w then mkE (BadEnt s) else
            if (not (LBS.null rem)) then mk
            if (LBS.null h

            let (h,rem) = LBS.span validHashByte (skipWS d) in
            
            let okDef = isHash h && LBS.null (skipWS rem) in
            if not okDef then mkE (BadNS ns d) else
            Right (Left (ns, LBS.toStrict h))
        

case LBS.uncons s of
        Just (64, s') -> decodeNS s'
        _ -> decodeWord s
  where 
    decodeNS

            let (nsBytes, nsDefBytes) = LBS.span validWordByte s' in
            let nsOK = not (LBS.null nsBytes) in
            if not nsOK then mkE (BadEnt s) else
            let nsVal = P.NS (P.Word (LBS.toStrict)) in
            let (hash, rem) = LBS.span validHashByte (skipWS nsDefBytes) in
            let defOK = isHash hash && LBS.null (skipWS rem) in
            if not defOK then mkE (BadNS nsVal nsDefBytes) else
            Right (Left (nsVal, LBS.toStrict hash))
        _ -> 
            let (wordBytes, wordDef) = LBS.span validWordByte s in


        
            
            let (h, rem) = LBS.span validHashByte (skipWS nsDef) in
            let nsOK = not (LBS.null ns) && isHash h && LBS.null (skipWS rem) in
            if not nsO
    Nothing -> mkE (BadEnt s)
    Just (c, s) -> case c of
        64
    Just (64, s') -> let (ns, def) = LBS.span 
          
-}

-- | An Awelon dictionary separates logical lines with the `LF @`
-- sequence. This function returns the index to the next `LF @`
-- line separator (returning the index for the LF). 
findLLSep :: LBS.ByteString -> Maybe Int64
findLLSep = f 0 where
    f !k s = case LBS.elemIndex 10 s of
        Nothing -> Nothing
        Just ix -> 
            let skip = (1 + ix) in
            let s' = LBS.drop skip s in
            case LBS.uncons s' of
                Just (64, _) -> Just (k + ix)
                _ -> f (k + skip) s'

-- | split the logical lines
splitLL :: LBS.ByteString -> [LBS.ByteString]
splitLL s = case findLLSep s of
    Nothing -> [s]
    Just ix -> (LBS.take ix s) : splitLL (LBS.drop (ix + 2) s)

-- | break dictionary into header and list of definition entries.
splitDict :: LBS.ByteString -> (LBS.ByteString, [LBS.ByteString])
splitDict s = case LBS.uncons s of
    Just (64, s') -> (mempty, splitLL s')
    _ -> case findLLSep s of
        Nothing -> (s, mempty)
        Just ix -> (LBS.take ix s, splitLL (LBS.drop (ix + 2) s))

-- note: we might wish to annotate line numbers in case of a parse error.

-- parse hashes
-- parse entries
-- map entries


