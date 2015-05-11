{-# LANGUAGE OverloadedStrings #-}
-- | Primary routes and web-apps for Wikilon.
module Wikilon.WAI.Routes
    ( stringToRoute
    , dictURI, dictURIBuilder, dictLink, dictCap
    , wordURI, wordURIBuilder, wordLink, wordCap
    , Route
    ) where

import Control.Monad
import Data.Monoid
import qualified Data.List as L
import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString as BS
import qualified Data.ByteString.UTF8 as UTF8
import qualified Data.ByteString.Lazy as LBS
import qualified Network.HTTP.Types as HTTP
import Text.Blaze.Html5 ((!))
import qualified Text.Blaze.Html5 as HTML
import qualified Text.Blaze.Html5.Attributes as Attrib

import Wikilon.WAI.Utils
import Wikilon.Branch (BranchName)
import Wikilon.Dict (Word(..), isValidWord)
import Wikilon.Root

type Route = BS.ByteString

stringToRoute :: String -> Route
stringToRoute = HTTP.urlEncode False . UTF8.fromString

toRoute :: BB.Builder -> Route
toRoute = LBS.toStrict . BB.toLazyByteString

-- | URI associated with a holistic dictionary
dictURI :: Wikilon -> BranchName -> Route
dictURI wiki dictName = toRoute $ dictURIBuilder wiki dictName

dictURIBuilder :: Wikilon -> BranchName -> BB.Builder
dictURIBuilder wiki dictName = 
    BB.byteString (wikilon_httpRoot wiki) <>
    BB.byteString "/d/" <>
    HTTP.urlEncodeBuilder False dictName

-- | anchor to branch name
dictLink :: Wikilon -> BranchName -> HTML
dictLink wiki b = 
    let path = HTML.unsafeByteStringValue $ dictURI wiki b in
    let humanText = HTML.unsafeByteString b in
    HTML.a ! Attrib.href path $ humanText

-- | obtain dictionary identifier from `:d` capture URL. This will
-- also validate the dictionary identifier (dict names use same 
-- constraints as word names)
dictCap :: Captures -> Maybe BranchName
dictCap caps =
    L.lookup "d" caps >>= \ d -> 
    if not (isValidWord (Word d)) then mzero else
    return d
    
-- | URI associated for a specific word in a named dictionary
-- This includes pct-encoded escapes as necessary.
--
-- TODO: I could probably improve performance of this operation
-- by a large amount. HTTP.urlEncodeBuilder is not very efficient.
wordURI :: Wikilon -> BranchName -> Word -> Route
wordURI wiki dictName w = toRoute $ wordURIBuilder wiki dictName w

wordURIBuilder :: Wikilon -> BranchName -> Word -> BB.Builder
wordURIBuilder wiki dictName (Word wordBytes) = 
    BB.byteString (wikilon_httpRoot wiki) <>
    BB.byteString "/d/" <>
    HTTP.urlEncodeBuilder False dictName <>
    BB.byteString "/w/" <>
    HTTP.urlEncodeBuilder False wordBytes

wordLink :: Wikilon -> BranchName -> Word -> HTML
wordLink wiki b w@(Word wbs) = 
    let path = HTML.unsafeByteStringValue $ wordURI wiki b w in
    let humanText = HTML.unsafeByteString wbs in
    HTML.a ! Attrib.href path $ humanText

-- | Obtain dictionary and word via :d and :w captures. 
wordCap :: Captures -> Maybe (BranchName, Word)
wordCap cap = 
    dictCap cap >>= \ d ->
    L.lookup "w" cap >>= \ w ->
    if not (isValidWord (Word w)) then mzero else
    return (d, Word w)

