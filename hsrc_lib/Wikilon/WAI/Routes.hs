{-# LANGUAGE OverloadedStrings #-}
-- | Primary routes and web-apps for Wikilon. 
--
-- Note that this is coupled to wikilonRoutes from the Wikilon.WAI module.
module Wikilon.WAI.Routes
    ( stringToRoute
    , Route
    , dictURI, dictURIBuilder, dictLink, dictCap
    , wordURI, wordURIBuilder, wordLink, wordCap

    , uriAODict, uriAODictEdit
    , uriAODictDocs, uriAODocs, uriWikilonDocs, uriABCDocs

    , href
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
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

import Wikilon.WAI.Utils
import Wikilon.Branch (BranchName)
import Wikilon.Dict (Word(..), isValidWord)

type Route = BS.ByteString

stringToRoute :: String -> Route
stringToRoute = HTTP.urlEncode False . UTF8.fromString

toRoute :: BB.Builder -> Route
toRoute = LBS.toStrict . BB.toLazyByteString

-- | URI associated with a holistic dictionary
dictURI :: BranchName -> Route
dictURI d = toRoute $ dictURIBuilder d

-- Note: I'm going to use a base tag for wikilon_httpRoot
--  so all paths used within Wikilon can be relative to a common root.
dictURIBuilder :: BranchName -> BB.Builder
dictURIBuilder d = BB.byteString "d/" <> HTTP.urlEncodeBuilder False d

-- | anchor to branch name. Since branch name is constrained
-- to be URI and text friendly, no escapes are necessary. 
dictLink :: BranchName -> HTML
dictLink d = href (dictURI d) $ H.unsafeByteString d

-- | obtain dictionary identifier from `:d` capture URL. This will
-- also validate the dictionary identifier (dict names use same 
-- constraints as word names)
dictCap :: Captures -> Maybe BranchName
dictCap caps =
    L.lookup "d" caps >>= \ d -> 
    if not (isValidWord (Word d)) then mzero else
    return d


uriAODict :: BranchName -> Route
uriAODict d = toRoute $ dictURIBuilder d <> BB.stringUtf8 "/aodict"
    
uriAODictEdit :: BranchName -> Route
uriAODictEdit d = toRoute $ dictURIBuilder d <> BB.stringUtf8 "/aodict.edit"

uriAODictDocs, uriAODocs, uriABCDocs, uriWikilonDocs :: Route
uriAODocs       = -- "about/ao"
    "https://github.com/dmbarbour/wikilon/blob/master/docs/AboutAO.md"
uriABCDocs      = -- "about/abc"
    "https://github.com/dmbarbour/wikilon/blob/master/docs/AboutABC.md"
uriAODictDocs   = "about/aodict"
uriWikilonDocs  = -- "about/wikilon"
    "https://github.com/dmbarbour/wikilon"

-- | URI associated for a specific word in a named dictionary
-- This includes pct-encoded escapes as necessary.
--
-- TODO: I could probably improve performance of this operation
-- by a large amount. HTTP.urlEncodeBuilder is not very efficient.
wordURI :: BranchName -> Word -> Route
wordURI d w = toRoute $ wordURIBuilder d w

wordURIBuilder :: BranchName -> Word -> BB.Builder
wordURIBuilder d (Word wordBytes) = 
    dictURIBuilder d <>
    BB.byteString "/w/" <>
    HTTP.urlEncodeBuilder False wordBytes

wordLink :: BranchName -> Word -> HTML
wordLink d w@(Word wbs) = href (wordURI d w) (H.unsafeByteString wbs)

-- | Obtain dictionary and word via :d and :w captures. 
wordCap :: Captures -> Maybe (BranchName, Word)
wordCap cap = 
    dictCap cap >>= \ d ->
    L.lookup "w" cap >>= \ w ->
    if not (isValidWord (Word w)) then mzero else
    return (d, Word w)

-- | Create an HRef when we know a route is already escaped
href :: Route -> HTML -> HTML
href r = H.a ! A.href (H.unsafeByteStringValue r)



