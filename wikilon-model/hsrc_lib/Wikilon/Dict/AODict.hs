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
-- a word is defined exactly once. All holes (undefined words) are 
-- included in the listing, just with an empty definition.
--
-- This format is called 'AODict' and uses internet media type:
--
--    text\/vnd.org.awelon.aodict
--
--
-- TODO: push most of the generic logic into wikilon-abc
--   (will need to abstract the dictionary)
module Wikilon.Dict.AODict
    ( mimeType
    , encode
    , encodeWords

    , splitLine
    , decodeLine
    , logicalLines

    , AODictError(..)
    , decodeAODict
    ) where

import Data.Monoid
import qualified Data.List as L
import qualified Data.ByteString as BS
import qualified Data.ByteString.UTF8 as UTF8
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Lazy.UTF8 as LazyUTF8
import qualified Data.ByteString.Builder as BB
import qualified Data.Set as Set

import Awelon.ABC (ABC)
import qualified Awelon.ABC as ABC
import Wikilon.Dict 


-- | appropriate HTTP Content-Type or Accept type 
-- text\/vnd.org.awelon.aodict
mimeType :: BS.ByteString
mimeType = UTF8.fromString "text/vnd.org.awelon.aodict"

-- | Encode the entire dictionary for export into a bytestring.
encode :: (DictView dict) => dict -> LBS.ByteString
encode d = encodeWords d (wordsInDict d)

-- | Encode a subset of the dictionary, specified by a list of root
-- words. All transitive dependencies of these words are included in
-- the export. If a requested word is not defined, it will be silently
-- skipped.
encodeWords :: (DictView dict) => dict -> [Word] -> LBS.ByteString
encodeWords d = _encode d . transitiveDepsList d

-- at this point, words are ordered how we need them
_encode :: (DictView dict) => dict -> [Word] -> LBS.ByteString
_encode d = BB.toLazyByteString . mconcat . fmap (_enw d)

_enw :: (DictView dict) => dict -> Word -> BB.Builder
_enw d w = BB.char8 '@' <> BB.byteString (wordToUTF8 w) <> BB.char8 ' '
    <> BB.lazyByteString (lookupBytes d w) <> BB.char8 '\n'

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

-- | Capture errors associated with decoding an AODict
data AODictError
    = AODict_LineParse !Bytes
    | AODict_WordRedef !Word
    | AODict_WordUndef !Word !Word
    | AODict_InsertError !InsertionError

instance Show AODictError where
    show (AODict_LineParse bs) = "could not parse line: " ++ LazyUTF8.toString bs
    show (AODict_WordRedef w) = "illegal redefinition of word `" ++ show w ++ "`"
    show (AODict_WordUndef w uw) = "word `" ++ show w ++ "` uses undefined word `" ++ show uw ++ "`" 
    show (AODict_InsertError e) = show e

-- | Decode an AODict format bytestring into a dictionary. Streaming updates.
-- Dictionary contains everything that passes safeUpdateWords even if any
-- AODict errors occur, but this is done one step at a time.
--
-- I'm going to assume that, even if the full dictionary doesn't fit into
-- memory all at once, we can at least keep a list of seen words. I might
-- change this for a bloom filter if we later need truly large dictionaries.
decodeAODict :: (DictUpdate dict) => dict -> Bytes -> ([AODictError], dict)
decodeAODict d0 bytes = results $ L.foldl' accum (mempty, mempty, d0) $ logicalLines bytes where
    results (err, _seen, !d) = (L.reverse err, d)
    accum (err, seen, !d) (decodeLine -> Just (w,abc)) =
        let eRedef = if (Set.member w seen) then [AODict_WordRedef w] else [] in
        let eUndefs = fmap (AODict_WordUndef w) $ L.nub $ 
                L.filter (`Set.notMember` seen) $ abcWords abc 
        in
        let seen' = Set.insert w seen in
        let err' = eUndefs <> eRedef <> err in
        case updateWord w abc d of
            Right d' -> (err', seen', d')
            Left e -> 
                let eInsert = fmap AODict_InsertError e in
                (eInsert <> err', seen', d) -- leaving word undefined in dictionary
    accum (err, seen, d) s = (err', seen, d) where
        err' = AODict_LineParse s : err

-- TODO: chunked processing for performance. 