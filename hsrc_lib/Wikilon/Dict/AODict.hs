{-# LANGUAGE BangPatterns, ViewPatterns #-}
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
-- that a word is defined only after all of its defined dependencies,
-- and that a word is defined at most once.
--
-- This format is called 'AODict' and uses internet media type:
--
--    text\/vnd.org.awelon.aodict
--
module Wikilon.Dict.AODict
    ( mimeType
    , dictWords
    , dictWords'
    , encode
    , encodeWords

    , logicalLines
    , decodeLine
    , decodeAODict
    , AODictError(..)
    ) where

import Control.Arrow (first)
import Data.Monoid
import qualified Data.List as L
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.ByteString as BS
import qualified Data.ByteString.UTF8 as UTF8
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Builder as BB
import Data.Set (Set)
import qualified Data.Set as Set

import Awelon.ABC (ABC)
import qualified Awelon.ABC as ABC
import Wikilon.Dict 


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
dictWords' d = _dictWords d mempty mempty

-- list words lazily
_dictWords :: Dict -> Set Word -> Set Word -> [Word] -> [Word]
_dictWords d pw rw ws@(w:ws') = 
    -- pw is printed words, prevents listing a more than once
    -- rw is ready words, prevents searching in a cycle
    if Set.member w pw then _dictWords d pw rw ws else
    let lDeps = L.filter (`Set.notMember` pw) $ deps d w in
    let bReady = L.null lDeps || Set.member w rw in
    if bReady
        then w : _dictWords d (Set.insert w pw) rw ws'
        else _dictWords d pw (Set.insert w rw) (lDeps ++ ws)
_dictWords _ _ _ [] = []

-- | Encode the entire dictionary for export into a bytestring.
encode :: Dict -> LBS.ByteString
encode d = _encode d (dictWords d)

-- | Encode a subset of the dictionary, specified by a list of root
-- words. All transitive dependencies of these words are included in
-- the export. If a word is not defined, it will be silently skipped.
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

-- break for each @word def..., excluding the `\n` after each line.
logicalLines :: Bytes -> [Bytes]
logicalLines = ll where
    ll bs = lln bs 0 bs 
    lln bs !n ss = case LBS.elemIndex 10 ss of
        Just ix -> 
            let ss' = LBS.drop (ix+1) ss in
            case LBS.uncons ss' of
                Nothing -> LBS.take (n+ix) bs : []
                Just (64, _) -> LBS.take (n+ix) bs : logicalLines ss'
                _ -> lln bs (n+ix+1) ss'
        Nothing -> [bs] -- file terminates without \n

-- | try to decode an `@word abc` line of code. Does not validate
-- anything other than the ABC structure (e.g. might accept some
-- invalid words like `foo}`.)
decodeLine :: Bytes -> Maybe (Word, ABC)
decodeLine bs =
    LBS.uncons bs >>= \ (atPrefix, wordAndDef) ->
    if (64 /= atPrefix) then Nothing else -- @ prefix
    let isWordSep c = (32 == c) || (10 == c) in
    let (wbs, defbs) = LBS.break isWordSep wordAndDef in
    let (abc,leftOver) = ABC.decode (LBS.drop 1 defbs) in
    let bParsed = LBS.null leftOver in
    if (not bParsed) then Nothing else -- bad ABC parse
    return (Word (LBS.toStrict wbs), abc)

-- | I want to precisely capture structural errors at the AODict
-- encoding layer, separately from Wikilon acceptance errors.
data AODictError
    = AODict_LineParse Bytes
    | AODict_WordRedef Word
    | AODict_WordUndef Word Word

instance Show AODictError where
    show (AODict_LineParse bs) = "could not parse logical line: " ++ show bs
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

