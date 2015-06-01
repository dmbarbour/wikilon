{-# LANGUAGE OverloadedStrings, ViewPatterns #-}

-- | Pages for the toplevel list-of-dictionaries
--
-- TODO: 
--   rename dictionaries
--   fork dictionaries
--   merge dictionaries    
module Wikilon.WAI.Pages.DictList
    ( allDictionaries
    , dictList
    , dictCreate
    , formSimpleCreateDict
    , ppDictName
    ) where

import Control.Applicative
import Control.Monad
import Data.Monoid
import qualified Data.List as L
import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString.UTF8 as UTF8
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Lazy.UTF8 as LazyUTF8
import qualified Network.HTTP.Types as HTTP
import Text.Blaze.Html5 ((!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import qualified Network.Wai as Wai
import Database.VCache

import Wikilon.WAI.Utils
import Wikilon.WAI.RecvFormPost
import Wikilon.WAI.Routes
import Wikilon.Branch (BranchName)
import qualified Wikilon.Branch as Branch
import Wikilon.Dict (Word(..), isValidWord)
import qualified Wikilon.Dict as Dict
import Wikilon.Root
import Wikilon.Time
import qualified Wikilon.WAI.RegexPatterns as Regex


import qualified Awelon.ABC as ABC

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

-- | restricted page for a textual list of dictionaries
dictList :: WikilonApp
dictList = justGET $ branchOnOutputMedia 
    [(mediaTypeTextPlain, listOfDictsText)]

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
    let title = H.string "Wikilon Dictionaries" in
    k $ Wai.responseLBS HTTP.ok200 [textHtml, etag] $ renderHTML $ do
    H.head $ do
        htmlHeaderCommon w
        H.title title
    H.body $ do
        H.h1 title
        listDictsHTML w bset
        H.p $ (H.b "Master Dictionary: ") <> hrefDict (wikilon_master w)
        H.hr
        H.div ! A.id "dictListFoot" ! A.class_ "footer" $ do
            H.b "New Dictionary:" <> " " <> (formSimpleCreateDict ! A.style "display:inline")
            -- RENAME
            -- FORK
            -- MERGE
{-
            H.b "Export:" <> " " <> lnkAODictList
            H.b "
            H.b "Import:" <> " " <> formImportAODict dictName <> H.br
            H.b "Modified:" <> " " <> tmModified <> H.br
-}
        --H.div ! A.class_ "boxed" $ formSimpleCreateDict
{-
        H.h2 "Delete a Dictionary"
        formSimpleDeleteDict
-}

listDictsHTML :: Wikilon -> Branch.BranchSet -> HTML
listDictsHTML _w bset = H.div ! A.id "dictTable" $ do
    H.table $ do
        H.tr $ mapM_ H.th ["Dictionary", "Versions", "Modified", "Head-ETag"]
        forM_ (Branch.toList bset) $ \ (bname,b) -> H.tr $ do 
            let d0 = Branch.head b
            H.td $ hrefDict bname
            H.td $ H.toMarkup $ Branch.branchSize b
            H.td $ maybe ("--") htmlSimpTime $ Branch.modified b
            H.td $ H.toMarkup $ toInteger $ Dict.unsafeDictAddr d0
    H.b "Count of Dictionaries:" <> " " <> H.toMarkup (Branch.width bset) <> H.br
    H.b "Count of Versions:" <> " " <> H.toMarkup (Branch.volume bset) <> H.br

-- | todo: consider authorization requirements for creating a dictionary
dictCreate :: WikilonApp
dictCreate = app where 
    app = routeOnMethod
        [(HTTP.methodPost, onPost),(HTTP.methodGet, onGet)]
    onGet w _cap _rq k = k $ htmlResponse HTTP.ok200 $ do
        let title = "Create a Dictionary"
        H.head $ do
            htmlHeaderCommon w
            H.title title
        H.body $ do
            H.h1 title
            formSimpleCreateDict
    onPost = recvFormPost onFormPost
    onFormPost :: PostParams -> WikilonApp
    onFormPost (ppDictName -> Just d) w _cap _rq k = do
        -- authorize (TODO) then create
        _ <- createDict w d
        -- then goto the named dictionary (be it new or old)
        k $ gotoDict w d
    onFormPost _pp _w _cap _rq k = k $ eBadRequest "invalid dictName"

gotoDict :: Wikilon -> BranchName -> Wai.Response
gotoDict w d = 
    let status = HTTP.seeOther303 in
    let location = (HTTP.hLocation, wikilon_httpRoot w <> uriDict d) in
    let headers = [location, textHtml] in
    let title = H.string $ "Redirect to Dictionary " ++ UTF8.toString d in
    Wai.responseLBS status headers $ renderHTML $ do
    H.head $ do
        htmlHeaderCommon w
        H.title title
    H.body $ do
        H.h1 title
        H.p $ "If you aren't automatically redirected, goto " <> hrefDict d


-- Create a valid AO definition that simply exports text
--  e.g. "text\n~[v'c]
aoTextDef :: ABC.Text -> ABC.ABC
aoTextDef txt = ABC.mkABC [ABC.ABC_Text txt, ABC.ABC_Block "v'c"]

-- | Create a dictionary with an initial entry, or return
-- without changing anything.
createDict :: Wikilon -> BranchName -> IO Bool
createDict w d =
    let vc = vcache_space (wikilon_store w) in
    getTime >>= \ tNow ->
    runVTx vc $ 
        readPVar (wikilon_dicts w) >>= \ bset ->
        let b0 = Branch.lookup' d bset in
        let bNew = Dict.null (Branch.head b0) in
        if not bNew then return False else
        let tNowText = LazyUTF8.fromString $ show tNow in
        let initWords = [("dictMeta:timeCreated", aoTextDef tNowText)] in
        case Dict.insert (Branch.head b0) initWords of
            Left _ -> return False -- shouldn't happen...
            Right d' -> do
                let b' = Branch.update (tNow,d') b0 
                let bset' = Branch.insert d b' bset 
                writePVar (wikilon_dicts w) bset'
                return True

ppDictName :: PostParams -> Maybe BranchName
ppDictName ps = 
    L.lookup "dictName" ps >>= \ p ->
    let d = LBS.toStrict (postParamContent p) in
    if not (isValidWord (Word d)) then mzero else
    return d

-- | a simple form for creation of a dictionary
formSimpleCreateDict :: HTML
formSimpleCreateDict = H.form ! A.action "d.create" ! A.method "POST" $ do
    let aoWord = A.pattern $ H.stringValue Regex.aoWord 
    H.input ! A.type_ "text" ! A.required "true" ! A.name "dictName" ! aoWord
    H.input ! A.type_ "submit" ! A.value "Create"


