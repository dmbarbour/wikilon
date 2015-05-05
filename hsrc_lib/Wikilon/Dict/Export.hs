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
--    ~[v'c]
--
-- Besides defining each word starting at a newline and escape rules
-- for LF (escape by following SP), this format is expected to ensure
-- that all dependencies for any given word are defined before that
-- word, and that every word is defined exactly once. This guards two
-- invariants for dictionaries: acyclic and fully defined.
--
-- Note: I'll create a separate but similar format for patching a
-- dictionary. This particular model is just for full dictionaries.
module Wikilon.Dict.Export
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
import Data.Maybe (mapMaybe)
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
--
-- application\/vnd.org.awelon.aodict
mimeType :: BS.ByteString
mimeType = UTF8.fromString "application/vnd.org.awelon.aodict"

-- | Obtain a list of words from our dictionary ordered such that
-- all dependencies of a word are defined before that word, and 
-- all words are listed exactly once.
dictWords :: Dict -> [Word]
dictWords d = dictWords' d (wordsInDict d)

-- | List requested words in a dictionary together with transitive
-- dependencies. The resulting list will not contain any duplicates,
-- and words will be ordered such that transitive dependencies 
-- appear before a word.
dictWords' :: Dict -> [Word] -> [Word]
dictWords' d = _dictWords d mempty

-- list words using set to prevent redundant entry
_dictWords :: Dict -> Set Word -> [Word] -> [Word]
_dictWords d lws ws@(w:ws') = 
    if Set.member w lws then _dictWords d lws ws else
    let lDeps = L.filter (`Set.notMember` lws) $ deps d w in
    if not (L.null lDeps) then _dictWords d lws (lDeps ++ ws) else
    let lws' = Set.insert w lws in
    w : _dictWords d lws' ws'
_dictWords _ _ [] = []

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
        <> _esc bytes <> BB.char8 '\n'

_esc :: LBS.ByteString -> BB.Builder
_esc = mconcat . fmap _esc' . LBS.toChunks

-- every LF is printed as LF SP.
_esc' :: BS.ByteString -> BB.Builder
_esc' bs = case BS.elemIndex 10 bs of
    Nothing -> BB.byteString bs
    Just ix -> 
        let ct = ix + 1 in
        BB.byteString (BS.take ct bs) <>
        BB.char8 ' ' <> 
        _esc' (BS.drop ct bs)

type Bytes = LBS.ByteString

-- | split a bytestring into logical lines. LF within each logical
-- line may be escaped by a following SP. So, this function will 
-- unescape each LF within a logical line, and split on LF between
-- logical lines. No information is lost: the original string can
-- be recovered by escaping each line then intercalating with "\n".
logicalLines :: Bytes -> [Bytes]
logicalLines = ll [] where
    mkLn = LBS.concat . L.reverse 
    ll acc bs = case LBS.elemIndex 10 bs of
        Nothing -> [mkLn (bs:acc)]
        Just idx -> 
            let bsNL = LBS.drop (idx+1) bs in
            case LBS.uncons bsNL of
                Just (32, bs') -> -- keep LF, drop SP
                    let ln = LBS.take (idx+1) bs in
                    ll (ln:acc) bs'
                _ -> -- drop LF, start next logical line
                    let eol = LBS.take idx bs in
                    mkLn (eol:acc) : ll [] bsNL 

-- | try to decode an `@word abc` line of code. Does not validate
-- anything other than the basic structure (e.g. might accept some
-- invalid words like `foo}`.)
decodeLine :: Bytes -> Maybe (Word, ABC)
decodeLine bs =
    LBS.uncons bs >>= \ (atPrefix, wordAndDef) ->
    if (64 /= atPrefix) then Nothing else -- @ prefix
    let isWordSep c = (32 == c) || (10 == c) in
    let (wbs, defbs) = LBS.break isWordSep wordAndDef in
    let (abc,leftOver) = ABC.decode (LBS.drop 1 defbs) in
    if (not (LBS.null leftOver)) then Nothing else -- bad ABC parse
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

-- Each word is expressed as a {%word} token in the original ABC.
_words :: ABC -> [Word]
_words = mapMaybe toWordTok . ABC.tokens where
    toWordTok bs = case UTF8.uncons bs of
        Just ('%', wbs) -> Just (Word wbs)
        _ -> Nothing

-- | Decode an AODict format bytestring and report any AODict-layer
-- errors. The resulting map will include all words that parse even
-- if some errors were raised (e.g. if a word was redefined or is 
-- defined out of order). 
decodeAODict :: Bytes -> ([AODictError], Map Word ABC)
decodeAODict = first L.reverse . L.foldl' accum ([],Map.empty) . logicalLines where
    accum (err,!d) (decodeLine -> Just (w,abc)) = 
        let eRedefs = if (Map.member w d) then [AODict_WordRedef w] else [] in
        let lUndefs = L.nub $ L.filter (`Map.notMember` d) $ _words abc in
        let eUndefs = fmap (AODict_WordUndef w) lUndefs in
        let err' = eUndefs <> eRedefs <> err in
        let d' = Map.insert w abc d in
        (err', d')
    accum r bs | LBS.null bs = r -- ignore empty lines, no error
    accum (err,d) bs = 
        let err' = AODict_LineParse bs : err in
        (err', d)

-- todo: full decode of dictionary...

