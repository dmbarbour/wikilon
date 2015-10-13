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
encodeWords d = 
    BB.toLazyByteString . 
    mconcat . fmap enwrd . 
    dictTransitiveDepsList d

-- encode defined words, skip undefined words
enwrd :: (Word, Maybe AODef) -> BB.Builder
enwrd (_, Nothing) = mempty
enwrd (Word w, Just def) = 
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
-- This doesn't check or enforce structural constraints (e.g. that
-- a word is defined before use).
loadAODict :: Dict -> LBS.ByteString -> ([Bytes],Dict)
loadAODict d0 = first L.reverse . L.foldl' accum ([],d0) . logicalLines where
    accum (e,!d) s = case parseLine s of
        Nothing -> (s:e,d) -- add string to error list
        Just (wrd,def) -> (e, dictInsert d wrd def) -- add word to dictionary

