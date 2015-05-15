{-# LANGUAGE ViewPatterns, OverloadedStrings #-}
-- | Pages, links, and forms for AODict import & export
module Wikilon.WAI.Pages.AODict
    ( dictAsAODict
    , dictAsAODictGzip
    , exportAODict
    , exportAODictGzip
    , exportAODictRaw
    , importAODict
    , recvAODictFormPost

    , lnkAODict
    , lnkAODictFile
    , lnkAODictGz
    , formExportAODict
    , formExportAODictGz
    , formImportAODict
    ) where

import Data.Monoid
import Data.Word (Word8)
import qualified Data.List as L
import qualified Data.Map as Map
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Network.HTTP.Types as HTTP
import qualified Network.HTTP.Media as HTTP
import Text.Blaze.Html5 ((!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import qualified Network.Wai as Wai
import Database.VCache
import qualified Codec.Compression.GZip as GZip

import Wikilon.WAI.Utils
import Wikilon.WAI.Routes
import Wikilon.WAI.RecvFormPost
import Wikilon.Branch (BranchName)
import qualified Wikilon.Branch as Branch
import Wikilon.Dict.Word
import qualified Wikilon.Dict as Dict
import qualified Wikilon.Dict.AODict as AODict
import Wikilon.Root
import Wikilon.Time

-- | endpoint that restricts media-type to just mediaTypeAODict
dictAsAODict :: WikilonApp
dictAsAODict = app where
    app = routeOnMethod [(HTTP.methodGet, onGet),(HTTP.methodPut, onPut),(HTTP.methodPost, onPost)]
    onGet = branchOnOutputMedia [(mediaTypeAODict, exportAODict)]
    onPut = branchOnInputMedia [(mediaTypeAODict, importAODict)]
    onPost = recvFormPost recvAODictFormPost

-- | endpoint that restricts media type to `.ao.gz` content.
dictAsAODictGzip :: WikilonApp
dictAsAODictGzip = app where
    app = routeOnMethod [(HTTP.methodGet, onGet),(HTTP.methodPut, onPut),(HTTP.methodPost, onPost)]
    onGet = branchOnOutputMedia [(mediaTypeGzip, exportAODictGzip)]
    onPut = branchOnInputMedia [(mediaTypeGzip, importAODictGzip)]
    onPost = recvFormPost recvAODictFormPost -- discriminates Gzip internally

-- | our primary export model for dictionaries. 
--
-- OPTIONS:
--  ?asFile   -- export as file
--  ?words=a,b,c -- select given root words
--
-- TODO: I may need authorization for some dictionaries.
exportAODict :: WikilonApp
exportAODict = enableGzipEncoding exportAODictRaw

exportAODictRaw :: WikilonApp
exportAODictRaw w (dictCap -> Just dictName) rq k = do
    bset <- readPVarIO (wikilon_dicts w)
    let d = Branch.head $ Branch.lookup' dictName bset
    let etag = return $ eTagN $ Dict.unsafeDictAddr d
    let aodict = return (HTTP.hContentType, HTTP.renderHeader mediaTypeAODict)
    -- allow 'asFile' option in query string for file disposition
    let bAsFile = L.elem "asFile" $ fmap fst $ Wai.queryString rq
    let asFile = if not bAsFile then [] else return $
            ("Content-Disposition" 
            ,mconcat ["attachment; filename=", dictName, ".ao"])
    let status = HTTP.ok200
    let headers = aodict <> etag <> asFile
    let lWords = selectWords rq
    let body = if L.null lWords then AODict.encode d else 
               AODict.encodeWords d lWords
    k $ Wai.responseLBS status headers body
exportAODictRaw _ caps _ k = k $ eBadName caps

selectWords :: Wai.Request -> [Word]
selectWords rq = case L.lookup "words" (Wai.queryString rq) of
    Just (Just bs) -> fmap Dict.Word $ L.filter (not . BS.null) $ BS.splitWith spc bs
    _ -> []

-- split on LF CR SP Comma
spc :: Word8 -> Bool
spc c = (10 == c) || (13 == c) || (32 == c) || (44 == c)

-- | export as a `.ao.gz` file.
exportAODictGzip :: WikilonApp
exportAODictGzip w (dictCap -> Just dictName) rq k = do
    bset <- readPVarIO (wikilon_dicts w)
    let d = Branch.head $ Branch.lookup' dictName bset
    let etag = return $ eTagN $ Dict.unsafeDictAddr d
    let aodict = return (HTTP.hContentType, HTTP.renderHeader mediaTypeGzip)
    let asFile = return $
            ("Content-Disposition" 
            ,mconcat ["attachment; filename=", dictName, ".ao.gz"])
    let status = HTTP.ok200
    let headers = aodict <> etag <> asFile
    let lWords = selectWords rq
    let body = if L.null lWords then AODict.encode d else 
               AODict.encodeWords d lWords
    k $ Wai.responseLBS status headers $ GZip.compress body
exportAODictGzip _w caps _rq k = k $ eBadName caps

-- receive a dictionary via Post, primarily for importing .ao or .ao.gz files
recvAODictFormPost :: PostParams -> WikilonApp
recvAODictFormPost (ppAODict -> Just body) w (dictCap -> Just dictName) rq k =
    importAODict' dictName body w rq k
recvAODictFormPost pp _w captures _rq k =
    case ppAODict pp of  -- a little error diagnosis 
        Nothing -> k $ eBadRequest "missing 'aodict' parameter"
        Just _ -> k $ eBadName captures

ppAODict :: PostParams -> Maybe LBS.ByteString
ppAODict = fmap postParamContentUnzip . L.lookup "aodict"

-- | load a dictionary into Wikilon from a single file.
-- This may delete words in the original dictionary.
-- 
-- TODO: I may need authorization for some dictionaries.
importAODict :: WikilonApp 
importAODict w (dictCap -> Just dictName) rq k = 
    Wai.lazyRequestBody rq >>= \ body ->
    importAODict' dictName body w rq k
importAODict _ caps _ k = k $ eBadName caps

importAODictGzip :: WikilonApp
importAODictGzip w (dictCap -> Just dictName) rq k =
    Wai.lazyRequestBody rq >>= \ gzBody ->
    let body = GZip.decompress gzBody in
    importAODict' dictName body w rq k
importAODictGzip _w caps _rq k = k $ eBadName caps

-- | 
importAODict' :: Branch.BranchName -> LBS.ByteString -> Wikilon -> Wai.Application
importAODict' dictName body w _rq k =
    let (err, wordMap) = AODict.decodeAODict body in
    let bHasError = not (L.null err) in
    let onError = k $ aodImportErrors $ fmap show err in
    if bHasError then onError else
    let vc = vcache_space (wikilon_store w) in
    case Dict.insert (Dict.empty vc) (Map.toList wordMap) of
        Left insertErrors -> 
            k $ aodImportErrors $ fmap show insertErrors
        Right dictVal -> do
            tNow <- getTime 
            runVTx vc $ 
                modifyPVar (wikilon_dicts w) $ \ bset ->
                    let b0 = Branch.lookup' dictName bset in
                    let b' = Branch.update (tNow, dictVal) b0 in
                    Branch.insert dictName b' bset
            k okNoContent

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



lnkAODict, lnkAODictFile, lnkAODictGz :: BranchName -> HTML
formExportAODict, formExportAODictGz :: BranchName -> HTML
formImportAODict :: BranchName -> HTML

lnkAODict d = 
    let uri = uriAODict d in
    H.a ! A.href (H.unsafeByteStringValue uri) $ 
        H.unsafeByteString $ "aodict"
lnkAODictFile d =
    let uri = uriAODict d <> "?asFile" in
    H.a ! A.href (H.unsafeByteStringValue uri) $
        H.unsafeByteString $ d <> ".ao"
lnkAODictGz d = 
    let uri = uriAODictGz d in
    H.a ! A.href (H.unsafeByteStringValue uri) $ 
        H.unsafeByteString $ d <> ".ao.gz"

-- | form allows for filtered export.
formExportAODict d = 
    let uriAction = uriAODict d in
    H.form ! A.method "GET" ! A.action (H.unsafeByteStringValue uriAction) $ do
        let select = H.input ! A.type_ "text" ! A.name "words"
        let asFile = H.input ! A.type_ "checkbox" ! A.name "asFile" ! A.value "✔" 
        let export = H.input ! A.type_ "submit" ! A.value "Export"
        H.string "Select: " <> select
        H.string "Download: " <> asFile 
        export

formExportAODictGz d =
    let uriAction = uriAODict d in
    H.form ! A.method "GET" ! A.action (H.unsafeByteStringValue uriAction) $ do
        let select = H.input ! A.type_ "text" ! A.name "words"
        let export = H.input ! A.type_ "submit" ! A.value "Export (.gz)"
        H.string "Select: " <> select <> export

formImportAODict d =
    let uriAction = H.unsafeByteStringValue $ uriAODict d in
    let style = "display:inline" in
    H.form ! A.method "POST" ! A.enctype "multipart/form-data" ! A.action uriAction ! A.style style $ do
        let lAccept = ".ao,.ao.gz,text/vnd.org.awelon.aodict"
        H.input ! A.type_ "file" ! A.name "aodict" ! A.accept lAccept
        H.input ! A.type_ "submit" ! A.value "Import File"
