{-# LANGUAGE BangPatterns, PatternGuards, ViewPatterns #-}
-- | This module provides a simple import and export format for Awelon
-- dictionaries, emitting an entire dictionary as a large file. This
-- uses the following format:
--
--   @dup [r^zlwl][]
--   @dupd [rw{%dup}wl][]
--   @swap [rwrwzwlwl][]
--   @over [{%dupd} {%swap}][]
--   @dup.doc "(Copyable x) ⇒ x -- x x
--   ~[v'c]
--
-- The character @ does not appear in ABC and is not ambiguous here.
-- No escapes are needed. There are two primary structural constraints,
-- that a word is defined only after all of its dependencies, and that
-- a word is defined exactly once.
--
-- This format is called 'AODict' and uses internet media type:
--
--    text\/vnd.org.awelon.aodict
--
--
-- TODO: push most of the generic logic into wikilon-abc
--   (will need to abstract the dictionary)
module Wikilon.Store.Dict.AODict
    ( mimeType
    , dictWords
    , dictWords'
    , encode
    , encodeWords

    , splitLine
    , decodeLine
    , logicalLines

    , AODictError(..)
    , decodeAODict
    ) where

import Control.Arrow (first)
import Data.Monoid
import qualified Data.List as L
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.ByteString as BS
import qualified Data.ByteString.UTF8 as UTF8
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Lazy.UTF8 as LazyUTF8
import qualified Data.ByteString.Builder as BB
import Data.Set (Set)
import qualified Data.Set as Set

import Awelon.ABC (ABC)
import qualified Awelon.ABC as ABC
import Wikilon.Store.Dict 


-- | appropriate HTTP Content-Type or Accept type 
-- text\/vnd.org.awelon.aodict
mimeType :: BS.ByteString
mimeType = UTF8.fromString "text/vnd.org.awelon.aodict"

-- | Obtain a list of words from our dictionary ordered such that
-- all defined dependencies of a word are listed before that word
-- and all defined words are listed exactly once.
dictWords :: Dict -> [Word]
dictWords d = dictWords' d (wordsInDict d)

-- | List requested words in a dictionary together with transitive
-- dependencies. The resulting list will not contain any duplicates,
-- and words will be ordered such that transitive dependencies 
-- appear before a word (modulo cycles in an unhealthy dictionary).
dictWords' :: Dict -> [Word] -> [Word]
dictWords' d = _dictWords d mempty 

-- list words lazily
_dictWords :: Dict -> Set Word -> [Word] -> [Word]
_dictWords d pw ws@(w:ws') = 
    -- pw is printed words, prevents listing a more than once
    if Set.member w pw then _dictWords d pw ws' else -- skip
    let lDeps = L.filter (`Set.notMember` pw) $ deps d w in
    let bReady = L.null lDeps in
    if bReady
        then w : _dictWords d (Set.insert w pw) ws'
        else _dictWords d pw (lDeps ++ ws)
_dictWords _ _ [] = []

-- | Encode the entire dictionary for export into a bytestring.
encode :: Dict -> LBS.ByteString
encode d = _encode d (dictWords d)

-- | Encode a subset of the dictionary, specified by a list of root
-- words. All transitive dependencies of these words are included in
-- the export. If a requested word is not defined, it will be silently
-- skipped.
encodeWords :: Dict -> [Word] -> LBS.ByteString
encodeWords d = _encode d . dictWords' d

-- at this point, words are ordered how we need them. Undefined words
-- are skipped, and hence left undefined in the output.
_encode :: Dict -> [Word] -> LBS.ByteString
_encode d = BB.toLazyByteString . mconcat . fmap (_enw d)

_enw :: Dict -> Word -> BB.Builder
_enw d w = case lookupBytes d w of
    Nothing -> mempty
    Just bytes -> 
        BB.char8 '@' <> BB.byteString (wordToUTF8 w) <> BB.char8 ' ' 
        <> BB.lazyByteString bytes <> BB.char8 '\n'

type Bytes = LBS.ByteString

-- take a logical line splitting at next `\n@` pair.
takeLogicalLine :: Bytes -> (Bytes, Bytes)
takeLogicalLine bs = ll 0 bs where
    ll !n ss = case LBS.elemIndex 10 ss of
        Nothing -> (bs, LBS.empty) -- lacks final '\n'
        Just ix -> 
            let ln = LBS.take (n+ix) bs in
            let ss' = LBS.drop (ix+1) ss in
            case LBS.uncons ss' of
                Nothing -> (ln, LBS.empty)
                Just (64, _) -> (ln, ss')
                _ -> ll (n+ix+1) ss'

-- | select all logical lines
logicalLines :: Bytes -> [Bytes]
logicalLines bs =
    if LBS.null bs then [] else
    let (ln,bs') = takeLogicalLine bs in
    ln : logicalLines bs'

-- | split an `@word data` logical line into a (word,data) pair
splitLine :: Bytes -> Maybe (Word, Bytes)
splitLine bs =
    LBS.uncons bs >>= \ (atPrefix, wordAndDef) ->
    if (64 /= atPrefix) then Nothing else -- @ prefix
    let isWordSep c = (32 == c) || (10 == c) in
    let (wbs, bs') = LBS.break isWordSep wordAndDef in
    return (Word (LBS.toStrict wbs), LBS.drop 1 bs')

-- | try to decode an `@word abc` line of code. Does not validate
-- anything other than the ABC structure, e.g. might accept some
-- invalid words or badly typed definitions.
decodeLine :: Bytes -> Maybe (Word, ABC)
decodeLine bs =
    splitLine bs >>= \ (w, defbs) ->
    case ABC.decode defbs of
        Left _dcs -> Nothing
        Right abc -> return (w,abc)

{- TODO: when I start working with very large dictionaries, I will
   probably need to operate on chunks of reasonable size rather than
   the entire dictionary all at once. This might be achieved by 
   processing a limited number of logical lines in each step. I will
   probably need a more flexible state model for this.
   
   OTOH, I don't expect this will be an issue until AO is very
   successful. It will be a nice problem to have.
-}

-- | I want to precisely capture structural errors at the AODict
-- encoding layer, separately from Wikilon acceptance errors.
data AODictError
    = AODict_LineParse !Bytes
    | AODict_WordRedef !Word
    | AODict_WordUndef !Word !Word

instance Show AODictError where
    show (AODict_LineParse bs) = "could not parse: " ++ LazyUTF8.toString bs
    show (AODict_WordRedef w) = "cannot redefine word: " ++ show w
    show (AODict_WordUndef w uw) = "word " ++ show w ++ " uses undefined: " ++ show uw 

-- | Decode an AODict format bytestring and report any AODict-layer
-- errors. The resulting map will include all words that parse even
-- if some errors were raised (e.g. if a word was redefined or is 
-- defined out of order). 
decodeAODict :: Bytes -> ([AODictError], Map Word ABC)
decodeAODict = first L.reverse . L.foldl' accum ([],Map.empty) . logicalLines where
    accum (err,!d) (decodeLine -> Just (w,abc)) = 
        let eRedefs = if (Map.member w d) then [AODict_WordRedef w] else [] in
        let lUndefs = L.nub $ L.filter (`Map.notMember` d) $ abcWords abc in
        let eUndefs = fmap (AODict_WordUndef w) lUndefs in
        let err' = eUndefs <> eRedefs <> err in
        let d' = Map.insert w abc d in
        (err', d')
    accum r bs | LBS.null bs = r -- ignore empty lines, no error
    accum (err,d) bs = 
        let err' = AODict_LineParse bs : err in
        (err', d)

-- todo: full decode of dictionary...

