{-# LANGUAGE ViewPatterns, OverloadedStrings #-}


-- | just a module that gathers all the other page modules
-- 
module Wikilon.WAI.Pages
    ( wikilonRoot
    , defaultFrontPage
    , allDictionaries
    , dictResource
    , dictWords
    , dictAsAODict
    , dictPostCreate
    ) where

import Control.Arrow (first)
import Control.Applicative
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
import Wikilon.WAI.URL
import Wikilon.WAI.Routes
import Wikilon.Branch (BranchName)
import qualified Wikilon.Branch as Branch
import Wikilon.Dict (Word(..))
import qualified Wikilon.Dict as Dict
import qualified Wikilon.Dict.AODict as AODict
import Wikilon.Root
import Wikilon.Time

import Wikilon.WAI.Pages.AODict

wikilonRoot :: WikilonApp
wikilonRoot = app where
    app = justGET get where
        -- maybe add OPTIONS eventually?
    get = branchOnOutputMedia $
        [(mediaTypeTextHTML, wikilonFrontPage)
        -- maybe add a SOAP API eventually
        ]
         
-- | Our front page or primary index for Wikilon. I would like this
-- to be driven by a dictionary, but I haven't quite worked out the
-- detailed designs for this yet.
--
-- Note: I'd prefer to keep most static content out of this Wikilon executable.
-- I probably want to push most content into the dictionary, then develop some
-- initial dictionaries to help developers get started.
--
-- Explanations, tutorials, etc. will be shifted into these extra dictionaries.
--  
wikilonFrontPage :: WikilonApp
wikilonFrontPage w _cap _rq k = k $ 
    Wai.responseLBS HTTP.ok200 [textHtml] $ 
    renderHTML $ defaultFrontPage w

defaultFrontPage :: Wikilon -> HTML
defaultFrontPage w = head <> body where
    title = "Wikilon"
    head = H.head $ do
        htmlMetaCharsetUtf8
        H.title title
    body = H.body $ do
        H.h1 title
        explainWikilon
        H.h2 "Resources"
        listResources
    -- CONSIDER: providing a one-button click to import a common AO dict to get started.
    explainWikilon = do
        H.p "Wikilon is a wiki-inspired software platform and integrated development \n\
            \environment for Awelon project."
        H.p $ "See " 
            <> (H.a ! A.href "https://github.com/dmbarbour/wikilon") "github README and docs"
            <> " for details. "
    listResources = H.ul $ mapM_ H.li $ 
            [rscFrontPage
            ,rscListOfDicts
            ]
    rscFrontPage = (H.a ! A.href "/" ! A.rel "self" $ "/") <> " front page, this page"
    rscListOfDicts = (H.a ! A.href "/d" $ "/d") <> " list of dictionaries"
    masterDict = wikilon_master w
    rscMasterDict = dictLink w masterDict <> " - current master dictionary"
    frontPageWord = "wikilon:FrontPage"
    rscMasterFrontPage = wordLink w masterDict frontPageWord <> " - to configure this page"

-- | Okay, so our first resource is a collection of dictionaries.
-- There aren't any whole-collection updates at this layer, but
-- there is some ability to browse a collection.
allDictionaries :: WikilonApp
allDictionaries = app where
    app = justGET onGet
    onGet = branchOnOutputMedia
        [(mediaTypeTextHTML, listOfDictsPage)
        ,(mediaTypeTextPlain, listOfDictsText)
--      ,(mediaTypeTextCSV, listOfDictsCSV)
        ]

-- | simply list dictionaries by name, one per line.
listOfDictsText :: WikilonApp
listOfDictsText w _cap _rq k = 
    readPVarIO (wikilon_dicts w) >>= \ bset ->
    let lNames = Branch.keys bset in
    let etag = eTagN (Branch.unsafeBranchSetAddr bset) in
    k $ Wai.responseLBS HTTP.ok200 [plainText,etag] $ BB.toLazyByteString $ 
        let encName n = BB.byteString n <> BB.charUtf8 '\n' in
        mconcat (encName <$> lNames)

-- | Our list of dictionaries is another page that should be configured
-- by our clients. It will need a lot of features:
--
--   list existing dictionaries (perhaps by activity) and link to them
--   (or link to a list of dictionaries and list just the most active)
--
--   support search for dictionaries by name or content
--   provide a simple form to create an empty dictionary
--   links to geneology
--
listOfDictsPage :: WikilonApp
listOfDictsPage w _cap _rq k =
    readPVarIO (wikilon_dicts w) >>= \ bset ->
    let etag = eTagNW (Branch.unsafeBranchSetAddr bset) in
    let title = "Wikilon Dictionaries" in
    k $ Wai.responseLBS HTTP.ok200 [textHtml, etag] $ renderHTML $ do
    H.head $ do
        htmlMetaCharsetUtf8
        H.title title
    H.body $ do
        H.h1 title
        listDictsHTML w bset
        formSimpleCreateDict
        --H.div ! A.class_ "boxed" $ formSimpleCreateDict
{-
        H.h2 "Delete a Dictionary"
        formSimpleDeleteDict
-}

listDictsHTML :: Wikilon -> Branch.BranchSet -> HTML
listDictsHTML w bset = do
    H.table $ do
        H.tr $ mapM_ H.th ["Dictionary", "Versions", "Modified", "Head-ETag"]
        forM_ (Branch.toList bset) $ \ (bname,b) -> do
            let d0 = Branch.head b
            H.td $ dictLink w bname
            H.td $ H.toMarkup $ Branch.branchSize b
            H.td $ H.string $ maybe ("--") show $ Branch.modified b
            H.td $ H.toMarkup $ toInteger $ Dict.unsafeDictAddr d0
    H.p $ H.b $ "Count of Dictionaries: " <> H.toMarkup (Branch.width bset) 
    H.p $ H.b $ "Count of Versions: " <> H.toMarkup (Branch.volume bset)

dictPostCreate :: WikilonApp
dictPostCreate = app where 
    app = routeOnMethod
        [(HTTP.methodPost, onPost),(HTTP.methodGet, onGet)]
    onGet _w _cap _rq k = k $ htmlResponse HTTP.ok200 $ do
        let title = "Create a Dictionary"
        H.head $ do
            htmlMetaCharsetUtf8
            H.title title
        H.body $ do
            H.h1 title
            formSimpleCreateDict
    onPost = todo "handle POST dict create!"

formSimpleCreateDict :: HTML
formSimpleCreateDict = H.form ! A.action "/action/dictCreate" ! A.method "POST" $ do
    H.input ! A.type_ "text" ! A.required "true" ! A.name "dictName"
    H.input ! A.type_ "submit" ! A.value "Create"

formSimpleDeleteDict :: HTML
formSimpleDeleteDict = H.form ! A.action "/action/dictDelete" ! A.method "POST" $ do
    H.input ! A.type_ "text" ! A.required "true" ! A.name "dictName"
    H.input ! A.type_ "submit" ! A.value "Delete"
        
-- The full 'dictionary resource' will include access to
-- words, histories, issues, subscriptions, etc.. Since
-- there is a lot here, I'll need to use subdirectory URIs
-- for current words and so on.
dictResource :: WikilonApp
dictResource = justGET dictFrontPage

dictFrontPage :: WikilonApp
dictFrontPage = todo "Dictionary"

todo :: String -> WikilonApp
todo msg _ _ _ k = k $ Wai.responseLBS HTTP.status202 [textHtml,noCache] $ 
    renderHTML $ do 
    let msgMarkup = H.toMarkup msg
    H.head $ H.title msgMarkup
    H.body $ H.p $ (H.b "TODO: " <> msgMarkup)

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
dictWordsPage = todo "Dict words!"


