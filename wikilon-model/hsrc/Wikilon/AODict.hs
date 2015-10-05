{-# LANGUAGE BangPatterns, PatternGuards, ViewPatterns, OverloadedStrings #-}
-- | This module provides a simple import and export format for Awelon
-- dictionaries, emitting an entire dictionary as a large file. This
-- uses the following format:
--
--   @dup r^zlwl
--   @dupd rw{%dup}wl
--   @swap rwrwzwlwl
--   @over {%dupd} {%swap}
--   @dup.doc "(Copyable x) ⇒ x -- x x
--   ~l
--
-- The character @ does not appear in ABC (except in an embedded text
-- or token) and is not ambiguous at the beginning of a line. Thus, no
-- escapes are needed.
--
-- There are two primary structural constraints. First, a well-defined word
-- is listed only after all of its dependencies. Second, a word is listed at
-- most once. If a word contains any dependencies that are not defined, then
-- that word is not well-defined (e.g. it's part of a cycle, or it uses an
-- undefined word).
--
-- This format is called 'AODict' and uses internet media type:
--
--    text\/vnd.org.awelon.aodict
--
-- It is also associated with the '.ao' file extension.
--
module Wikilon.AODict
    ( mimeType
    , encode
    , encodeWords
    , loadAODict
    ) where

import Control.Monad (guard)
import Control.Arrow (first)
import Data.Monoid
import qualified Data.List as L
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy.Char8 as LBS
import qualified Data.ByteString.Builder as BB
import qualified Data.Set as Set

import Wikilon.Dict 


-- | appropriate HTTP Content-Type or Accept type 
-- text\/vnd.org.awelon.aodict
mimeType :: BS.ByteString
mimeType = "text/vnd.org.awelon.aodict"

-- | Encode a dictionary into the AODict format.
encode :: Dict -> LBS.ByteString
encode d = encodeWords d ws where
    ws = fmap fst (dictList d)

-- | Encode a subset of the dictionary, specified by a list of root
-- words that we want to ensure are listed.
encodeWords :: Dict -> [Word] -> LBS.ByteString
encodeWords d = BB.toLazyByteString . mconcat . fmap enwrd . endeps (dictLookup d)

-- Obtain an ordered list of words to encode for AODict. In case of
-- a cycle, a word will be listed only once. Note that this will keep
-- all words from a dictionary in-memory, so it might not scale for
-- multi-million word dictionaries. 
endeps :: (Word -> Maybe AODef) -> [Word] -> [(Word, AODef)]
endeps lu = accum mempty mempty where
    -- accum (visited) (cycle prevention)
    accum _ _ [] = []
    accum v c ws@(w:ws') = 
        if Set.member w v then accum v c ws' else -- already listed
        case lu w of
            Nothing -> accum (Set.insert w v) c ws' -- leave word undefined
            Just def -> -- encode word
                let lDeps = L.filter (`Set.notMember` v) (aodefWords def) in
                let bAddWord = L.null lDeps || Set.member w c in
                if bAddWord then (w,def) : accum (Set.insert w v) (Set.delete w c) ws' 
                            else accum v (Set.insert w c) (lDeps <> ws)

enwrd :: (Word, AODef) -> BB.Builder
enwrd (Word w, def) = 
    BB.char8 '@' <> BB.byteString w <> BB.char8 ' ' <> 
    BB.byteString def <> BB.char8 '\n'

type Bytes = LBS.ByteString

-- take a logical line splitting at next `\n@` pair.
takeLogicalLine :: Bytes -> (Bytes, Bytes)
takeLogicalLine bs = ll 0 bs where
    ll !n ss = case LBS.elemIndex '\n' ss of
        Nothing -> (bs, LBS.empty) -- lacks final '\n'
        Just ix -> 
            let ln = LBS.take (n+ix) bs in
            let ss' = LBS.drop (ix+1) ss in
            case LBS.uncons ss' of
                Nothing -> (ln, LBS.empty)
                Just ('@', _) -> (ln, ss')
                _ -> ll (n+ix+1) ss'

-- | select all logical lines
logicalLines :: Bytes -> [Bytes]
logicalLines bs =
    if LBS.null bs then [] else
    let (ln,bs') = takeLogicalLine bs in
    ln : logicalLines bs'

-- | split an `@word def` bytestring into a (Word,AODef) pair
-- This will also validate the Word and AODef.
parseLine :: Bytes -> Maybe (Word, AODef)
parseLine bs =
    LBS.uncons bs >>= \ (c0, wordAndDef) ->
    guard ('@' == c0) >>
    let isWordSep c = (' ' == c) || ('\n' == c) in
    let (wbs, bs') = LBS.break isWordSep wordAndDef in
    let wrd = Word $ LBS.toStrict wbs in
    guard (isValidWord wrd) >>
    let def = LBS.toStrict $ LBS.drop 1 bs' in
    guard (isValidAODef def) >>
    return (wrd,def)

-- | Load words from a fragment of AODict format into a dictionary.
-- This doesn't enforce structural constraints. The only errors 
-- detected are logical lines that fail to parse or validate (via
-- isValidWord and isValidAODef).
loadAODict :: Dict -> LBS.ByteString -> ([Bytes],Dict)
loadAODict d0 = first L.reverse . L.foldl' accum ([],d0) . logicalLines where
    accum (e,!d) s = case parseLine s of
        Nothing -> (s:e,d) -- add string to error list
        Just (wrd,def) -> (e, dictInsert d wrd def) -- add word to dictionary

