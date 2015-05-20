{-# LANGUAGE ViewPatterns, PatternGuards, OverloadedStrings #-}
-- | Pages, links, and forms for AODict import & export
--
-- TODO: variation supporting import/export with full history.
-- (This might require a different format for performance...
-- maybe a sequence of patches as a tar file?)
module Wikilon.WAI.Pages.AODict
    ( dictAsAODict
    , importAODict
    , exportAODict

    , lnkAODict
    , lnkAODictFile
    , lnkAODictGz
    , formImportAODict
    , htmlAODictFormat
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
import Wikilon.Dict.Text (listTextConstraintsForHumans)
import qualified Wikilon.Dict as Dict
import qualified Wikilon.Dict.AODict as AODict
import Wikilon.Root
import Wikilon.Time

-- | endpoint that restricts media-type to just mediaTypeAODict
dictAsAODict :: WikilonApp
dictAsAODict = app where
    app = routeOnMethod [(HTTP.methodGet, onGet),(HTTP.methodPut, onPut),(HTTP.methodPost, onPost)]
    onGet = exportAODict
    onPut = branchOnInputMedia [(mediaTypeAODict, importAODict), (mediaTypeGzip, importAODictGzip)]
    onPost = recvFormPost recvAODictFormPost

-- | support an "asFileGz" selection
exportAODict :: WikilonApp
exportAODict w cap rq k =
    let bAsGz = L.elem "asFileGz" $ fmap fst $ Wai.queryString rq in
    let optGz = (mediaTypeGzip, exportAODictGzip) in
    let optText = (mediaTypeAODict, enableGzipEncoding exportAODict') in
    let lOpts = if bAsGz then [optGz] else [optText, optGz] in
    branchOnOutputMedia lOpts w cap rq k


-- | our primary export model for dictionaries. 
--
-- OPTIONS:
--  ?asFile   -- export as file
--  ?words=a,b,c -- select given root words
--
-- TODO: I may need authorization for some dictionaries.
exportAODict' :: WikilonApp
exportAODict' w (dictCap -> Just dictName) rq k = do
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
exportAODict' _ caps _ k = k $ eBadName caps


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
recvAODictFormPost pp@(ppAODict -> Just body) w (dictCap -> Just dictName) rq k = do
    let dest = maybe (wikilon_httpRoot w <> dictURI dictName) id (ppOrigin pp)
    let location = (HTTP.hLocation, dest)
    let okSeeDict = Wai.responseLBS HTTP.seeOther303 [location, textHtml, noCache] $ renderHTML $ do
            let title = H.string "Import Succeeded"
            H.head $ do
                htmlHeaderCommon w
                H.title title
            H.body $ do
                H.h1 title
                H.p $ "The import of " <> dictLink dictName <> "succeeded."
                H.p $ "You should be automatically redirected to the dictionary page."
    importAODict' okSeeDict dictName body w rq k
recvAODictFormPost pp _w captures _rq k =
    case ppAODict pp of  -- a little error diagnosis 
        Nothing -> k $ eBadRequest "missing 'aodict' parameter"
        Just _ -> k $ eBadName captures

ppAODict :: PostParams -> Maybe LBS.ByteString
ppAODict = getPostParamUnzip "aodict"

ppOrigin :: PostParams -> Maybe BS.ByteString
ppOrigin = fmap LBS.toStrict . getPostParam "origin"

-- | load a dictionary into Wikilon from a single file.
-- This may delete words in the original dictionary.
-- 
-- TODO: I may need authorization for some dictionaries.
importAODict :: WikilonApp 
importAODict w (dictCap -> Just dictName) rq k = 
    Wai.lazyRequestBody rq >>= \ body ->
    importAODict' okNoContent dictName body w rq k
importAODict _ caps _ k = k $ eBadName caps

importAODictGzip :: WikilonApp
importAODictGzip w (dictCap -> Just dictName) rq k =
    Wai.lazyRequestBody rq >>= \ gzBody ->
    let body = GZip.decompress gzBody in
    importAODict' okNoContent dictName body w rq k
importAODictGzip _w caps _rq k = k $ eBadName caps

importAODict' :: Wai.Response -> Branch.BranchName -> LBS.ByteString -> Wikilon -> Wai.Application
importAODict' onOK dictName body w _rq k =
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
            k onOK

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
        H.hr
        htmlAODictFormat

htmlAODictFormat :: HTML
htmlAODictFormat = H.div ! A.id "aodictFormat" ! A.class_ "advice" $ do
    H.h2 "General Format"
    H.p "Content must use the aodict format. i.e. (@word abcdef\\n)* where\n\
        \each abcdef is a definition written in Awelon Bytecode (ABC)."
    H.h3 "ABC"
    H.p "ABC consists of 43 primitive operators, blocks, tokens, and texts."
    H.ul $ do
        H.li "operators: SP LF lrwzvc%^$o'kf#0123456789+*/-QLRWZVC?DFMK>"
        H.li "blocks contain ABC between square brackets, e.g. [vrwlc]"
        H.li "text literals have regex form: [\"].*(\\n[ ].*)*\\n~" 
        H.li "tokens use text between curly braces, e.g. {foo}"
    H.p "However, only a subset of ABC is permitted within AO dictionaries."
    H.h4 "Words Constraints"
    H.ul $ mapM_ (H.li . H.string) listWordConstraintsForHumans
    H.h4 "Token Constraints"
    H.ul $ do
        H.li "{%word} - dependency on another word in dictionary"
        H.li "{&anno} - annotation, e.g. for performance or safety"
        H.li "{:foo} - discretionary sealer, excludes '$'"
        H.li "{.foo} - discretionary unsealer, excludes '$'"
    H.p "Tokens must be from these classes. Additionally, tokens must\n\
        \be valid according to the Word Constraints."
    H.h4 "Text Constraints"
    H.p "Text literal constraints were initially motivated to simplify CRLF\n\
        \conversions. But avoiding control characters seems wise in general."
    H.ul $ mapM_ (H.li . H.string) listTextConstraintsForHumans
    H.h3 "Structural Constraints"
    H.p "The aodict format has structural constraints to guard against\n\
        \cycles and undefined words, and simplify efficient processing."
    H.ul $ do
        H.li "words are defined before use"
        H.li "words are not redefined"
    H.h3 "ABC Definition"
    H.p "An ABC definition should have type: ∀e.(e → ∃v.([v→[a→b]]*(v*e))). The\n\
        \intermediate value 'v' serves as an abstract syntax or DSL structure.\n\
        \The resulting [a→b] is the compiled functional meaning of the word." 


lnkAODict, lnkAODictFile, lnkAODictGz :: BranchName -> HTML
formImportAODict :: Route -> BranchName -> HTML

lnkAODict d = 
    let uri = uriAODict d in
    H.a ! A.href (H.unsafeByteStringValue uri) $ 
        H.unsafeByteString $ "aodict"
lnkAODictFile d =
    let uri = uriAODict d <> "?asFile" in
    H.a ! A.href (H.unsafeByteStringValue uri) $
        H.unsafeByteString $ d <> ".ao"
lnkAODictGz d = 
    let uri = uriAODict d <> "?asFileGz" in
    H.a ! A.href (H.unsafeByteStringValue uri) $ 
        H.unsafeByteString $ d <> ".ao.gz"

formImportAODict r d =
    let uri = uriAODict d in
    let uriAction = H.unsafeByteStringValue uri in
    let style = "display:inline" in
    H.form ! A.method "POST" ! A.enctype "multipart/form-data" ! A.action uriAction
           ! A.id "aodictFileImport" ! A.style style $ do
        let lAccept = ".ao,.ao.gz,text/vnd.org.awelon.aodict"
        H.input ! A.type_ "file" ! A.name "aodict" ! A.accept lAccept ! A.required "true"
        let origin = H.unsafeByteStringValue r
        H.input ! A.type_ "hidden" ! A.name "origin" ! A.value origin
        H.input ! A.type_ "submit" ! A.value "Import File"

