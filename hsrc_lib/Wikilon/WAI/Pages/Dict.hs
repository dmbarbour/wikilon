{-# LANGUAGE OverloadedStrings, ViewPatterns #-}
-- | Pages for a single dictionary. This might turn into another
-- aggregation module because I want a lot of diverse operations
-- on full dictionaries.
module Wikilon.WAI.Pages.Dict
    ( dictResource
    , dictWords
    , module Wikilon.WAI.Pages.AODict
    ) where

import Control.Monad
import Data.Monoid
import qualified Data.List as L
import qualified Data.Map as Map
import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString as BS
import qualified Data.ByteString.UTF8 as UTF8
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Lazy.UTF8 as LazyUTF8
import qualified Network.HTTP.Types as HTTP
import qualified Network.HTTP.Media as HTTP
import Text.Blaze.Html5 ((!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import qualified Network.Wai as Wai
import Database.VCache

import Wikilon.WAI.Utils
import Wikilon.WAI.Routes
import Wikilon.Branch (BranchName)
import qualified Wikilon.Branch as Branch
import Wikilon.Root
import Wikilon.Time

import Wikilon.WAI.Pages.AODict
        
-- The full 'dictionary resource' will include access to
-- words, histories, issues, subscriptions, etc.. Since
-- there is a lot here, I'll need to use subdirectory URIs
-- for current words and so on.
dictResource :: WikilonApp
dictResource = justGET dictFrontPage

dictFrontPage :: WikilonApp
dictFrontPage w (dictCap -> Just dictName) rq k = dictFrontPage' dictName w rq k
dictFrontPage _w caps _rq k = k $ eBadName caps

dictFrontPage' :: BranchName -> Wikilon -> Wai.Application
dictFrontPage' dictName w _rq k = 
    readPVarIO (wikilon_dicts w) >>= \ bset ->
    let b = Branch.lookup' dictName bset in
    let d = Branch.head b in
    let status = HTTP.status200 in
    let headers = [textHtml] in
    k $ Wai.responseLBS status headers $ renderHTML $ do
    let title = H.unsafeByteString dictName 
    let tmModified = H.string $ maybe "--" show $ Branch.modified b
    H.head $ do
        htmlHeaderCommon w
        H.title title 
    H.body $ do
        H.h1 title
        -- maybe some content from the dictionary itself
        -- maybe add some banner or CSS from dictionary itself
        -- maybe a simple console-like or query application?
        -- after defining apps, probably want to revist them easily
        -- HEALTH information
            -- undefined words & cycles
            -- words that don't compile, compile errors
            -- words that are badly typed
        -- H.h2 "Recent Changes"
        -- H.h2 "Resources"
            -- H.li $ lnkDictWords dictName <> " - dictionary words"
            -- H.li $ lnkMasterDict dictName <> " - view as master"
            -- 
            -- probably want a page to create filtered AODict views
            -- probably want pages for histories and events
            -- 
            -- H.li $ lnkAODict dictName <> " - aodict format "
            -- 
        replicateM_ 10 H.br
        H.hr
        H.div ! A.id "dictFoot" ! A.class_ "footer" $ do
            H.b "Export:" <> " " <> lnkAODictFile dictName <> " " <> lnkAODictGz dictName <> H.br
            H.b "Import:" <> " " <> formImportAODict dictName <> H.br
            H.b "Modified:" <> " " <> tmModified <> H.br

-- our 
dictWords :: WikilonApp
dictWords = app where
    app = routeOnMethod
        [(HTTP.methodGet, onGet)
        ,(HTTP.methodPut, onPut)]
    onGet = branchOnOutputMedia
        [(mediaTypeTextHTML, dictWordsPage)
        ,(mediaTypeAODict, exportAODict)]
    onPut = branchOnInputMedia
        [(mediaTypeAODict, importAODict)]


-- what should the dict words page have?
--  access to recently changed words?
--  ability to delete words?
dictWordsPage :: WikilonApp
dictWordsPage = toBeImplementedLater "Dict words!"




