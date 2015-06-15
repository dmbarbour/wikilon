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
    , aodictDocHTML
    , aodictDocs
    ) where

import Data.Monoid
import Data.Word (Word8)
import qualified Data.List as L
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
import Wikilon.Store.Branch (BranchName)
import qualified Wikilon.Store.Branch as Branch
import Wikilon.Dict.Word
import Wikilon.Dict.Text (listTextConstraintsForHumans)
import qualified Wikilon.Store.Dict as Dict
import qualified Wikilon.Dict.AODict as AODict
import Wikilon.Store.Root
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
exportAODict' = dictApp $ \w dictName rq k -> do
    bset <- readPVarIO (wikilon_dicts $ wikilon_model w)
    let d = Branch.head $ Branch.lookup' dictName bset
    let etag = eTagN $ Dict.unsafeDictAddr d
    let aodict = (HTTP.hContentType, HTTP.renderHeader mediaTypeAODict)
    -- allow 'asFile' option in query string for file disposition
    let bAsFile = L.elem "asFile" $ fmap fst $ Wai.queryString rq
    let fattach = if bAsFile then "attachment; " else "inline; "
    let fname = mconcat [fattach, "filename=", dictName, ".ao"]
    let disp = ("Content-Disposition", fname) 
    let status = HTTP.ok200
    let headers = [etag,aodict,disp]
    let lWords = selectWords rq
    let body = if L.null lWords then AODict.encode d else 
               AODict.encodeWords d lWords
    k $ Wai.responseLBS status headers body

selectWords :: Wai.Request -> [Word]
selectWords rq = case L.lookup "words" (Wai.queryString rq) of
    Just (Just bs) -> fmap Dict.Word $ L.filter (not . BS.null) $ BS.splitWith spc bs
    _ -> []

-- split on LF CR SP Comma
spc :: Word8 -> Bool
spc c = (10 == c) || (13 == c) || (32 == c) || (44 == c)

-- | export as a `.ao.gz` file.
exportAODictGzip :: WikilonApp
exportAODictGzip = dictApp $ \w dictName rq k -> do 
    bset <- readPVarIO (wikilon_dicts $ wikilon_model w)
    let d = Branch.head $ Branch.lookup' dictName bset
    let etag = eTagN $ Dict.unsafeDictAddr d
    let aodict = (HTTP.hContentType, HTTP.renderHeader mediaTypeGzip)
    let disp = ("Content-Disposition"
            ,mconcat ["attachment; filename=", dictName, ".ao.gz"])
    let status = HTTP.ok200
    let headers = [etag,aodict,disp]
    let lWords = selectWords rq
    let body = if L.null lWords then AODict.encode d else 
               AODict.encodeWords d lWords
    k $ Wai.responseLBS status headers $ GZip.compress body

-- receive a dictionary via Post, primarily for importing .ao or .ao.gz files
recvAODictFormPost :: PostParams -> WikilonApp
recvAODictFormPost pp@(ppAODict -> Just body) = dictApp $ \ w dictName rq k -> do
    let dest = maybe (wikilon_httpRoot w <> uriDict dictName) id (ppOrigin pp)
    let location = (HTTP.hLocation, dest)
    let okSeeDict = Wai.responseLBS HTTP.seeOther303 [location, textHtml, noCache] $ renderHTML $ do
            let title = H.string "Import Succeeded"
            H.head $ do
                htmlHeaderCommon w
                H.title title
            H.body $ do
                H.h1 title
                H.p $ "The import of " <> hrefDict dictName <> "succeeded."
                H.p $ "You should be automatically redirected to the dictionary page."
    importAODict' okSeeDict dictName body w rq k
recvAODictFormPost _ = \ _w _cap _rq k -> k $ eBadRequest "missing 'aodict' parameter" 

ppAODict :: PostParams -> Maybe LBS.ByteString
ppAODict = getPostParamUnzip "aodict"

ppOrigin :: PostParams -> Maybe BS.ByteString
ppOrigin = fmap LBS.toStrict . getPostParam "origin"

-- | load a dictionary into Wikilon from a single file.
-- This may delete words in the original dictionary.
-- 
-- TODO: I may need authorization for some dictionaries.
importAODict :: WikilonApp 
importAODict = dictApp $ \w dictName rq k -> 
    Wai.lazyRequestBody rq >>= \ body ->
    importAODict' okNoContent dictName body w rq k

importAODictGzip :: WikilonApp
importAODictGzip = dictApp $ \w dictName rq k ->
    Wai.lazyRequestBody rq >>= \ gzBody ->
    let body = GZip.decompress gzBody in
    importAODict' okNoContent dictName body w rq k

importAODict' :: Wai.Response -> Branch.BranchName -> LBS.ByteString -> Wikilon -> Wai.Application
importAODict' onOK dictName body w _rq k =
    let vc = vcache_space (wikilon_store $ wikilon_model w) in
    let (err, dictVal) = AODict.decodeAODict (Dict.empty vc) body in
    let bHasError = not (L.null err) in
    if bHasError then k $ aodImportErrors w $ fmap show err else do
    tNow <- getTime
    runVTx vc $ 
        modifyPVar (wikilon_dicts $ wikilon_model w) $ \ bset ->
            let b0 = Branch.lookup' dictName bset in
            let b' = Branch.update (tNow, dictVal) b0 in
            Branch.insert dictName b' bset
    k onOK

aodImportErrors :: Wikilon -> [String] -> Wai.Response
aodImportErrors w errors = 
    let status = HTTP.badRequest400 in
    let headers = [textHtml, noCache] in
    Wai.responseLBS status headers $ renderHTML $ do
    let title = "Import Error"
    H.head $ do
        htmlHeaderCommon w
        htmlMetaNoIndex
        H.title title
    H.body $ do
        H.h1 title
        H.p "Content rejected. Do not resubmit without changes."
        H.p "Error Messages:"
        H.ul $ mapM_ (H.li . H.string) errors
        let lnDocs = H.a ! A.href (H.unsafeByteStringValue uriAODictDocs) $ "aodict documentation"
        H.p $ "See " <> lnDocs <> " for general information on formatting." 

aodictDocs :: WikilonApp
aodictDocs = basicWebPage $ \ w _ _ -> do
    H.head $ do
        htmlHeaderCommon w
        H.title "text/vnd.org.awelon.aodict"
    H.body ! A.class_ "docs" ! A.style "margin: 0 auto; width: 960px" $ 
        aodictDocHTML

aodictDocHTML :: HTML
aodictDocHTML = do
    H.h1 "The AODict Format"
    H.p $ H.b "Internet Media Type:" <> " text/vnd.org.awelon.aodict"
    H.p $ H.b "Filename Extensions:" <> " .ao,.ao.gz (gzipped)"
    H.p "AODict is an import/export format for Awelon Object (AO) dictionaries."
    H.h2 "General Format"
    H.p "The AODict file contains a simple list of `@word definition LF` entries.\n\
        \Each word is defined exactly once, and each word is defined before use.\n\
        \Definitions use a subset of Awelon Bytecode (ABC). Simple example:"
    H.pre . (H.code ! A.lang "aodict") $
        "@swap [rwrwzwlwl][]\n\
        \@swapd [rw {%swap} wl][]\n\
        \@dup [r^zlwl][]\n\
        \@fixpoint ['[^'ow^'zowvr$c]^'owo][]\n\
        \@helloMultiLine\n\
        \\"Hello,\n\
        \ this is a multi-line definition!\n\
        \~[v'c]\n\
        \"
    H.p "The structure of ABC ensures there is never any ambiguity that the '@'\n\
        \begins a new entry. There is no need for escapes, and we don't need to\n\
        \parse ABC to split entries. The word and definition may be divided by a\n\
        \a space or linefeed (ASCII 32 or 10)."
    H.p "While a general ABC parser can process any AO definition, AO doesn't\n\
        \permit arbitrary ABC. In particular, there are constraints on tokens\n\
        \and text literals. Words are also constrained. See below."
    H.h3 "Words Constraints"
    H.ul $ mapM_ (H.li . H.string) listWordConstraintsForHumans
    H.h3 "Awelon Bytecode (ABC)"
    H.p "ABC consists of 43 primitive operators, blocks, tokens, and texts."
    H.ul $ do
        H.li "operators: SP LF lrwzvc%^$o'kf#0123456789+*/-QLRWZVC?DFMK>"
        H.li "blocks contain ABC between square brackets, e.g. [vrwlc]"
        H.li "text literals have regex form: [\"].*(\\n[ ].*)*\\n~" 
        H.li "tokens use text between curly braces, e.g. {foo}"
    let abcDocs = H.a ! A.href (H.unsafeByteStringValue uriABCDocs) $ "ABC documentation"
    H.p $ "See " <> abcDocs <> " for detailed information." 
    H.p "However, only a subset of ABC is permitted within AO dictionaries."
    H.h4 "Token Constraints"
    H.ul $ do
        H.li "{%word} - dependency on another word in dictionary"
        H.li "{&anno} - annotation, e.g. for performance or safety"
        H.li "{:foo} - discretionary sealer, excludes character '$'"
        H.li "{.foo} - discretionary unsealer, excludes character '$'"
    H.p "Tokens must be from these classes. Additionally, tokens must\n\
        \be valid according to the Word Constraints."
    H.h4 "Text Constraints"
    H.p "Text literal constraints were initially motivated to simplify CRLF\n\
        \conversions, e.g. when editing a dictionary through HTML forms. But\n\
        \avoiding control characters seems wise in general."
    H.ul $ mapM_ (H.li . H.string) listTextConstraintsForHumans
    H.p "If you must represent binary data, favor base 16 with variant alphabet\n\
        \'bdfghjkmnpqstxyz' (as elements 0..15). A compression pass can easily\n\
        \reduce this to binary when streaming or storing ABC."
    H.h3 "Structural Constraints"
    H.p "The aodict format has structural constraints to guard against\n\
        \cycles and undefined words, and simplify efficient processing."
    H.ul $ do
        H.li "words are defined before use"
        H.li "words are not redefined"
    H.p "When editing fragments of an AO dictionary, these two constraints\n\
        \are relaxed."
    H.h3 "Type Constraints"
    H.p "Each definition should have the general form:"
    H.pre . H.code $
        "type Def a b = ∀e.(e → ∃v.([v→[a→b]] * (v * e)))"
    H.p "The meaning of the word is the pure function of type [a→b]. However,\n\
        \the intermediate value 'v' serves an important role for structured\n\
        \editing, staged programming, and user-defined languages. The function\n\
        \[v→[a→b]] serves as a compiler. We compile the value into its meaning.\n\
        \Frequently, the compiler is the identity function `[]`, which is valid\n\
        \when the value `v` is directly a function. Another trivial compiler is\n\
        \`[v'c]`, which simply quotes value 'v' and exports a structure. Complex\n\
        \compilers will usually be factored into words, e.g. `[{%fooLang}]`."
    H.p "In a development dictionary, it is not uncommon for some words to be\n\
        \undefined or incompletely defined. If a definition does not compile\n\
        \because it is incomplete, this is considered a 'hole' in the dictionary.\n\
        \A good development environment helps developers recognize and fill holes."
    H.p "In a healthy dictionary, every word should compile, and additionally the\n\
        \meaning functions should typecheck and pass other analyses."
    H.h2 "Editing of AO Dictionaries"
    H.p "The AODict format is not suitable for direct human reading and editing."
    H.p "At small scales, it is feasible to edit AODict directly, assuming you\n\
        \well know ABC and your dictionary. But in practice a dictionary grows\n\
        \large, dense, noisy, intractable in the AODict format. An editor may\n\
        \help developers by supporting edits for ad-hoc fragments of a dictionary."
    H.p "AO is designed for structure editors and environments that can present\n\
        \editable views for words based around the intermediate structures of\n\
        \definitions"


lnkAODict, lnkAODictFile, lnkAODictGz :: BranchName -> HTML
formImportAODict :: Route -> BranchName -> HTML

lnkAODict d = 
    let uri = uriAODict d in
    H.a ! A.href (H.unsafeByteStringValue uri) $ 
        H.unsafeByteString $ "AODict"
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


