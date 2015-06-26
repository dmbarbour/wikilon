{-# LANGUAGE OverloadedStrings, ViewPatterns #-}
-- | Primary routes and web-apps for Wikilon. 
--
-- Note that this is coupled to wikilonRoutes from the Wikilon.WAI module.
module Wikilon.WAI.Routes
    ( stringToRoute
    , Route
    , dictURIBuilder, hrefDict
    , wordURIBuilder, hrefDictWord
    , navWords

    , WikilonDictApp, dictApp
    , WikilonDictWordApp, dictWordApp

    , uriAODict
    , uriAODictEdit
    , uriAODictDocs
    , uriAODocs
    , uriWikilonDocs
    , uriABCDocs
    , uriClawDocs
    , uriAVMDocs

    , uriDict
    , uriDictWord
    , uriDictWords
    , uriDictWordsQPrefix
    , uriDictWordsList
    , uriDictWordsListQPrefix
    , uriDictWordRename

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
import qualified Network.Wai as Wai

import Wikilon.WAI.Utils
import Wikilon.Store.Branch (BranchName)
import Wikilon.Dict.Word (Word(..), isValidWord)

type Route = BS.ByteString

stringToRoute :: String -> Route
stringToRoute = HTTP.urlEncode False . UTF8.fromString

toRoute :: BB.Builder -> Route
toRoute = LBS.toStrict . BB.toLazyByteString

-- | URI associated with a holistic dictionary
uriDict :: BranchName -> Route
uriDict d = toRoute $ dictURIBuilder d

-- Note: I'm going to use a base tag for wikilon_httpRoot
--  so all paths used within Wikilon can be relative to a common root.
dictURIBuilder :: BranchName -> BB.Builder
dictURIBuilder d = BB.byteString "d/" <> HTTP.urlEncodeBuilder False d

-- | link to a dictionary branch by name, using name as the link
hrefDict :: BranchName -> HTML
hrefDict d = href (uriDict d) ! A.class_ "refDict" $ H.unsafeByteString d

-- | Link to a browseable word listing for a dictionary.
uriDictWords :: BranchName -> Route
uriDictWords d = toRoute $ dictURIBuilder d <> BB.stringUtf8 "/w"

uriDictWordsList :: BranchName -> Route
uriDictWordsList d = toRoute $ dictURIBuilder d <> BB.stringUtf8 "/w.list"

uriDictWordsQPrefix :: BranchName -> BS.ByteString -> Route
uriDictWordsQPrefix d prefix = 
    toRoute $ dictURIBuilder d <> BB.stringUtf8 "/w?prefix=" <>
        HTTP.urlEncodeBuilder True prefix

uriDictWordsListQPrefix :: BranchName -> BS.ByteString -> Route
uriDictWordsListQPrefix d prefix =
    toRoute $ dictURIBuilder d <> BB.stringUtf8 "/w.list?prefix=" <>
        HTTP.urlEncodeBuilder True prefix

-- | obtain dictionary identifier from `:d` capture URL. This will
-- also validate the dictionary identifier (dict names use same 
-- constraints as word names)
dictCap :: Captures -> Maybe BranchName
dictCap caps =
    L.lookup "d" caps >>= \ d -> 
    if not (isValidWord (Word d)) then mzero else
    return d

type WikilonDictApp = Wikilon -> BranchName -> Wai.Application
dictApp :: WikilonDictApp -> WikilonApp
dictApp app w (dictCap -> Just d) rq k = app w d rq k
dictApp _app _w cap _rq k = k $ eBadName cap

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

uriClawDocs :: Route
uriClawDocs = "about/claw"

uriAVMDocs :: Route
uriAVMDocs = "https://github.com/dmbarbour/wikilon/blob/master/docs/NetworkModel.md"

-- | URI associated for a specific word in a named dictionary
-- This includes pct-encoded escapes as necessary.
--
-- TODO: I could probably improve performance of this operation
-- by a large amount. HTTP.urlEncodeBuilder is not very efficient.
uriDictWord :: BranchName -> Word -> Route
uriDictWord d w = toRoute $ wordURIBuilder d w

uriDictWordRename :: BranchName -> Word -> Route
uriDictWordRename d w = toRoute $ wordURIBuilder d w <> BB.stringUtf8 "/rename"

wordURIBuilder :: BranchName -> Word -> BB.Builder
wordURIBuilder d (Word wordBytes) = 
    dictURIBuilder d <>
    BB.byteString "/w/" <>
    HTTP.urlEncodeBuilder False wordBytes

hrefDictWord :: BranchName -> Word -> HTML
hrefDictWord d w@(Word wbs) = href (uriDictWord d w) ! A.class_ "refDictWord" $ 
    H.unsafeByteString wbs

-- | Print a list of words under a nav tag, with a
-- simple header. Print nothing if the list is empty.
navWords :: String -> BranchName -> [Word] -> HTML
navWords _ _ [] = mempty
navWords sClass dn lWords =
    H.nav ! A.class_ (H.stringValue sClass) $ do
        H.strong (H.string sClass <> ":") <> " "
        mconcat $ L.intersperse " " $ fmap (hrefDictWord dn) lWords

-- | Obtain dictionary and word via :d and :w captures. 
dictWordCap :: Captures -> Maybe (BranchName, Word)
dictWordCap cap = 
    dictCap cap >>= \ d ->
    L.lookup "w" cap >>= \ w ->
    if not (isValidWord (Word w)) then mzero else
    return (d, Word w)


type WikilonDictWordApp = Wikilon -> BranchName -> Word -> Wai.Application
dictWordApp :: WikilonDictWordApp -> WikilonApp
dictWordApp app w (dictWordCap -> Just (d,dw)) rq k = app w d dw rq k
dictWordApp _app _w cap _rq k = k $ eBadName cap

-- | Create an HRef when we know a route is already escaped
href :: Route -> HTML -> HTML
href r = H.a ! A.href (H.unsafeByteStringValue r)

