{-# LANGUAGE ViewPatterns #-}

-- | This module focuses on word search for tab completion, spelling
-- guidance, locating words with a given prefix or suffix, and so on. 
-- That is, search based on the internal structure of words, rather
-- than external structure of how words are used.
-- 
module Wikilon.WordSearch
    ( WordSearchIndex
    , empty, insert, delete
    , member
    , wordsWithPrefix
    , wordsWithSuffix
    , AsterPattern, asterWordSearch
    -- regular expressions
    -- fuzzy search for tab completions
    -- search for spelling errors (keyboard distance, etc.)
    ) where

import Data.Maybe (maybeToList)
import qualified Data.List as L
import qualified Data.Map.Strict as M
import qualified Data.ByteString as B
import qualified Codec.Binary.UTF8.Generic as UTF8
import Wikilon.Word

-- for now, just using a brute-force search.
data WordSearchIndex = WSI
    { _content :: !(M.Map B.ByteString Word)
    }

empty :: WordSearchIndex
empty = WSI M.empty

-- | Insert a collection of words to the search.
insert :: Word -> WordSearchIndex -> WordSearchIndex
insert w wsi = WSI $ M.insert (wordToUTF8 w) w (_content wsi)

-- | Remove a collection of words from the search.
delete :: Word -> WordSearchIndex -> WordSearchIndex
delete w wsi = WSI $ M.delete (wordToUTF8 w) (_content wsi)

-- | Seek an exact match for an existing word. It the word isn't
-- found, do not intern.  
member :: String -> WordSearchIndex -> Maybe Word
member s wsi = M.lookup (UTF8.fromString s) (_content wsi)

-- | Find all words with a specific prefix.
wordsWithPrefix :: String -> WordSearchIndex -> [Word]
wordsWithPrefix [] wsi = M.elems (_content wsi)
wordsWithPrefix s wsi = 
    let prefix = UTF8.fromString s in
    let hasPrefix = B.isPrefixOf prefix . wordToUTF8 in
    let (_,exact,greater) = M.splitLookup prefix (_content wsi) in
    let consExactMatch = maybe id (:) exact in
    let wwpList = L.takeWhile hasPrefix $ M.elems greater in
    consExactMatch wwpList

-- | Find all words with a specific suffix.
wordsWithSuffix :: String -> WordSearchIndex -> [Word]
wordsWithSuffix [] wsi = M.elems (_content wsi)
wordsWithSuffix s wsi = 
    let suffix = UTF8.fromString s in
    let hasSuffix = B.isSuffixOf suffix . wordToUTF8 in
    L.filter hasSuffix $ M.elems (_content wsi)

-- | Word search with a single wildcard character '*', which will
-- match any sequence of characters. Escape asterisk with backslash.
-- This covers a lot of common search patterns.
asterWordSearch :: AsterPattern -> WordSearchIndex -> [Word]
asterWordSearch p wsi =
    let (prefix,p') = asterPrefix p in
    if (L.null p') then maybeToList $ member prefix wsi else
    let wwp = wordsWithPrefix prefix wsi in
    let dropPrefix = L.drop (L.length prefix) in
    let hasPattern = matchStr p' . dropPrefix . wordToText in
    L.filter hasPattern wwp
    
-- | Trivial search with wildcard '*'. Not very sophisticated, i.e.
-- doesn't support full regular expressions.
type AsterPattern = String

-- extract the exact-match prefix from an aster pattern 
-- to improve lookup performance. 
asterPrefix :: AsterPattern -> (String, AsterPattern)
asterPrefix = ap [] where
    ex ('*':_) = Nothing
    ex ('\\':'*':p) = Just ('*',p)
    ex (c:p) = Just (c,p)
    ex [] = Nothing
    ap r (ex -> Just (c,p)) = ap (c:r) p
    ap r p = (L.reverse r, p)

matchStr :: AsterPattern -> String -> Bool
matchStr ('*':[]) _ = True
matchStr ('\\':'*':pp) (c:ss) = ('*' == c) && matchStr pp ss
matchStr pp@('*':pp') ss@(_:ss') = matchStr pp' ss || matchStr pp ss'
matchStr (c:pp) (c':ss) = (c == c') && (matchStr pp ss)
matchStr pp ss = null pp && null ss

-- | Fuzzy search for words that use the same characters in the same
-- order, plus some ad-hoc heuristics. I'm looking for something close
-- to what Sublime text editor achieves here, to support tab completion.
--
-- Thoughts:
-- * `cc` should match `camelCase` or `cool-cat` or `soccer`
--   * but offer bonus 'punctuation points' for the former two?
-- * seems difficult to do much better than brute force search
--   * but if I can filter many words with just a few characters...
-- * may benefit a lot from memoization or machine learning
--   * e.g. initial candidates, find more candidates
--
-- I might benefit from looking into 'clustering' algorithms, i.e.
-- which are designed for clustering of images with similar features.
-- In this sense, the words can be understood as simple images, with
-- some prominent internal features.
--
-- I may also benefit from counting words for each character, or
-- perhaps even for each ordered pair of characters, such that I can
-- quickly filter candidates down to just those matching a given
-- subset.


-- requirements:
--  (a) efficiently add and remove words from search
--  (b) support fuzzy find, e.g. characters ordered within word
--  (c) find all words with a given suffix or prefix
--
-- design thoughts:
--
-- In the short term, up to perhaps 10k words and a few users, I
-- could use brute force. I might also benefit from memoizing the
-- search results and selected items on a per-session basis. This
-- option might be for the best, in the interest of getting up and
-- running early (i.e. optimize later). 
--
-- A suffix array is nearly ideal for the search requirement. It can
-- cover all the desired forms of search with reasonable efficiency.
-- It could be optimized further by a 'longest matching prefix' array.
-- Though, fuzzy find is still expensive, it would be less so with a
-- suffix array.
--
-- To add and remove words is the greater challenge. I don't want to
-- rebuild a 10k or 100k word suffix array just to add a few words.
-- 
-- I can probably assume that the word search index is linear, such
-- that the occasional O(N) rebuild is acceptable so long as the
-- total amortized cost is kept relatively low. This suggests an
-- exponential approach: combine indices so they roughly double in
-- size.
--
