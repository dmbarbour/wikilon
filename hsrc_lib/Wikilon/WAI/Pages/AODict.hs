{-# LANGUAGE ViewPatterns, OverloadedStrings #-}
-- | Pages for AODict import & export
--
-- todo: also support POST for AODict files
module Wikilon.WAI.Pages.AODict
    ( dictAsAODict
    , exportAODict
    , importAODict
    ) where

import qualified Data.List as L
import qualified Data.Map as Map
import qualified Network.HTTP.Types as HTTP
import qualified Network.HTTP.Media as HTTP
--import Text.Blaze.Html5 ((!))
import qualified Text.Blaze.Html5 as H
--import qualified Text.Blaze.Html5.Attributes as A
import qualified Network.Wai as Wai
import Database.VCache

import Wikilon.WAI.Utils
import Wikilon.WAI.Routes
import qualified Wikilon.Branch as Branch
import qualified Wikilon.Dict as Dict
import qualified Wikilon.Dict.AODict as AODict
import Wikilon.Root
import Wikilon.Time

-- | endpoint that restricts media-type to just mediaTypeAODict
dictAsAODict :: WikilonApp
dictAsAODict = app where
    app = routeOnMethod [(HTTP.methodGet, onGet),(HTTP.methodPut, onPut)]
    onGet = branchOnOutputMedia [(mediaTypeAODict, exportAODict)]
    onPut = branchOnInputMedia [(mediaTypeAODict, importAODict)]

-- | our primary export model for dictionaries. 
--
-- TODO: I may need authorization for some dictionaries.
-- TODO: support GZIP encoding.
exportAODict :: WikilonApp
exportAODict w (dictCap -> Just dictName) _rq k = do
    branchSet <- readPVarIO (wikilon_dicts w)
    let d = Branch.head $ Branch.lookup' dictName branchSet
    let etag = eTagN $ Dict.unsafeDictAddr d
    let aodict = (HTTP.hContentType, HTTP.renderHeader mediaTypeAODict)
    let status = HTTP.ok200
    let headers = [aodict,etag]
    k $ Wai.responseLBS status headers $ AODict.encode d
exportAODict _ caps _ k = k $ eBadName caps

-- | load a dictionary into Wikilon from a single file. 
--
-- TODO: I may need authorization for some dictionaries.
-- TODO: support GZIP encoding.
importAODict :: WikilonApp 
importAODict w (dictCap -> Just dictName) rq k = 
    Wai.lazyRequestBody rq >>= \ body ->
    let (err, wordMap) = AODict.decodeAODict body in
    let bHasError = not (L.null err) in
    let onError = k $ aodImportErrors $ fmap show err in
    if bHasError then onError else
    let vc = vcache_space (wikilon_store w) in
    case Dict.insert (Dict.empty vc) (Map.toList wordMap) of
        Left insertErrors -> k $ aodImportErrors $ fmap show insertErrors
        Right dictVal -> do
            tNow <- getTime 
            runVTx vc $ 
                modifyPVar (wikilon_dicts w) $ \ bset ->
                    let b0 = Branch.lookup' dictName bset in
                    let b' = Branch.update (tNow, dictVal) b0 in
                    Branch.insert dictName b' bset
            k okNoContent
importAODict _ caps _ k = k $ eBadName caps

aodImportErrors :: [String] -> Wai.Response
aodImportErrors errors = 
    Wai.responseLBS HTTP.badRequest400 [textHtml, noCache] $ 
    renderHTML $ do
    let title = "Import Error"
    H.head $ do
        htmlMetaCharsetUtf8
        htmlMetaNoIndex
        H.title title
    H.body $ do
        H.h1 title
        H.p "Content rejected. Do not resubmit without changes."
        H.p "Error Messages:"
        H.ul $ mapM_ (H.li . H.string) errors
