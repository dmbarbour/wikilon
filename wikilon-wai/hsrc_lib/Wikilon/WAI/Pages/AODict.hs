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
    H.body ! A.class_ "docs" $ 
        aodictDocHTML

aodictDocHTML :: HTML
aodictDocHTML = do
    H.h1 "The AODict Format"

    H.p $ H.b "Internet Media Type:" <> " text/vnd.org.awelon.aodict"
    H.p $ H.b "Filename Extensions:" <> " .ao,.ao.gz (gzipped)"

    H.p "AODict is an import/export format for Awelon Object (AO) dictionaries."

    H.h2 "General Structure"

    let awelonBytecode = href uriABCDocs "Awelon Bytecode (ABC)" 
    H.p $ "The AODict file contains a simple list of `@word definition LF` entries.\n\
          \Each word is defined exactly once, and each word is defined before use.\n\
          \Definitions use a subset of " <> awelonBytecode <> ". Simple example:"

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
        \parse ABC to split entries. The word is separated from the definition\n\
        \by a space or linefeed (ASCII 32 or 10)."

    H.h2 "Type of Definition"
    H.p "A definition is a string of bytecode with the general type:"
    H.pre . H.code $
        "type Def a b = ∃v.∀e.(e → ([v→[a→b]] * (v * e)))"
    H.p "The definition effectively has two parts:"
    H.ul $ do
        H.li "An ad-hoc structured value of type `v`."
        H.li "A compiler function of type `v→[a→b]`"
    
    H.p $ "The separation of the value `v` and the compiler function provides\n\
          \a simple foundation for staged programming that doesn't rely on\n\
          \partial evaluation. It may also be leveraged for domain specific\n\
          \languages and structured editing (see editing, below). This value\n\
          \is compiled into a pure function [a→b], the meaning of the word."
    
    H.p $ "The common, trivial case: our compiler function is identity [], and\n\
          \the value is already a function of type [a→b]. A minimal definition\n\
          \is `[][]`, which specifies an identity function."

    H.h3 "The AODef Format"

    H.p $ H.b "Internet Media Type:" <> " text/vnd.org.awelon.aodef"
    H.p $ H.b "Filename Extensions:" <> " .aodef (rarely applicable)"

    H.p $ "When a definition is taken by itself, apart from the context of the\n\
          \dictionary, I call that the `aodef` format. Because aodef is strictly\n\
          \a subset of ABC, we could use generic bytecode types. But explicitly\n\
          \indicating the `aodef` format is convenient for tooling."

    H.h3 "Incomplete Definitions"

    H.p "An incomplete definition is represented by depending on the tacit argument\n\
        \labeled `e` in the definition type descriptor. Effectively, we express that\n\
        \a definition depends on some unspecified, to-be-provided value. Thus, we can\n\
        \model definitions with 'holes' in them. An undefined word is just the extreme\n\
        \case where the definition is one big hole."

    H.p "These tacit holes have utility in software development contexts:"
    H.ul $ do
        H.li "suitable for top-down, exploratory, test-driven development"
        H.li "infer types and constraints based on usage and tests"
        H.li "automatic search for values that meet constraints"
        H.li "interactive programming, programming-by-example"
        H.li "fill-in-the-gap skeletons, templates, form-based modules"

    H.p "Without incomplete definitions, we're effectively restricted to bottom-up\n\
        \development models. Bottom-up is concrete but not always convenient."
    
    H.p "Outside of development contexts, incomplete definitions may be forbidden.\n\
        \For example, a curated dictionary will generally only contain complete and\n\
        \well-typed definitions, perhaps even with nice properties like termination\n\
        \or correctness proofs."

    H.h2 "Token and Text Constraints"

    H.p $ "AO definitions use a " <> (H.em "strict subset") <> " of ABC. The limits\n\
          \primarily constrain tokens and texts. The constraints on tokens ensure\n\
          \purity and portability, while the other constraints simplify interaction\n\
          \with structured editors and documentation."

    H.h3 "Token Constraints"
    H.p "Tokens are restricted to four cases, indicated by prefix:"
    H.ul $ do
        H.li "{%word} - dependency on another word in dictionary"
        H.li "{&anno} - annotation, e.g. for performance or safety"
        H.li "{:foo} - discretionary sealer, exclude character '$'"
        H.li "{.foo} - discretionary unsealer, exclude character '$'"
    H.p "After removing the prefix, all tokens must be valid as words:"
    H.ul $ mapM_ (H.li . H.string) listWordConstraintsForHumans

    H.h3 "Text Constraints"
    H.p "Text is constrained to simplify interaction with editors and HTML."
    H.ul $ mapM_ (H.li . H.string) listTextConstraintsForHumans

    H.p "If developers wish to include control characters, they'll probably\n\
        \need to model their own escapes and post-process the text with a\n\
        \word."

    H.h3 "Representing Binary Data"
    H.p "Shoving binary data into a dictionary should be pretty rare, though it\n\
        \may be convenient for data-intensive resources (textures, images, sound).\n\
        \The recommended representation for binary is base16 text. It is easy to\n\
        \compress base16 for storage or transmission, especially if we recognize\n\
        \a specific base16 alphabet. The favored alphabet for Awelon project is\n\
        \`bdfghjkmnpqstxyz`, excluding `aeiou` to avoid spelling inane words and\n\
        \`vrwlc` to prevent interference with data plumbing."

    H.h2 "Structural Constraints"
    H.p "The aodict format has structural constraints to guard against\n\
        \cycles and simplify efficient processing."
    H.ul $ do
        H.li "words are defined before use"
        H.li "words are not redefined"
    H.p "These constraints apply to a file or stream describing an entire\n\
        \dictionary. They may be relaxed for editing a fragment of the\n\
        \dictionary, e.g. a few words at a time. Also, 'defined' here might\n\
        \involve an incomplete definition for contexts that accept one."

    H.h2 "Editing of AO Dictionaries"

    H.p "The AODict and AODef formats are not suitable for direct human reading\n\
        \and editing. Use of curly braces and prefixes, e.g. {%foo}, is painful\n\
        \to type (too much shift key). Even the simplest definition `[][]` is a\n\
        \little painful to read. Definitions and dictionaries should be edited\n\
        \through human-friendly, structured views."
    
    H.p "There are at least two viable options for editable structure in AO:"
    H.ol $ do
        H.li "We can 'parse' bytecode into higher level texts or structures."
        H.li "We can 'evaluate' intermediate structure `v` from definitions."
    
    let commandLanguageForAwelon = href uriClawDocs "command language for awelon (claw)"
    H.p $ "The " <> commandLanguageForAwelon <> " leverages the first\n\
          \idea. For definitions of form `[command][]`, we parse the command\n\
          \into a Forth-like sequence of words and numbers. Claw is usually\n\
          \rendered and manipulated as text. However, claw's parse and render\n\
          \functions can feasibly be extended for structure editors, e.g. to\n\
          \support graphs, diagrams, sliders, color pickers."
    
    H.p $ "The second option involves rendering and directly manipulating the\n\
          \existential value `v` computed when compiling a definition. Updates\n\
          \can be modeled as appending the definition, resulting in an updated\n\
          \value or compiler function. The value is rendered for the user. It\n\
          \is feasible to model rich objects for `v` that have their own update\n\
          \and rendering logics, and protect their own invariants."

    H.p $ "These options overlap in utility, but have different strengths. My\n\
          \hope is that they fill different niches. Eventually. The immediate\n\
          \focus is on command language views and extensions."

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


