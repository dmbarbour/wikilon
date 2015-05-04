
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
module Wikilon.Dict.Export
    ( mimeType
    , dictWords
    , dictWords'
    , encode
    , encodeWords

    , logicalLines

    -- , decode
    -- , decodeList
    ) where

import Data.Int
import Data.Monoid
import qualified Data.List as L
import qualified Data.ByteString as BS
import qualified Data.ByteString.UTF8 as UTF8
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Builder as BB
import Data.Set (Set)
import qualified Data.Set as Set
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

-- | split a bytestring into logical lines. No characters are
-- removed. A logical line may contain LF if followed by SP.
logicalLines :: Bytes -> (Bytes, [Bytes])
logicalLines bs = case llIndex bs of
    Nothing -> (bs, [])
    Just idx -> 
        let (ln,bs') = LBS.splitAt (idx+1) bs in
        let ll = uncurry (:) $ logicalLines bs' in
        (ln, ll)

llIndex :: Bytes -> Maybe Int64
llIndex bs = error "TODO"






