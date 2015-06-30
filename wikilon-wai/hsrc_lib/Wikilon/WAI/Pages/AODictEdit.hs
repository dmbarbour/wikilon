{-# LANGUAGE OverloadedStrings, ViewPatterns, PatternGuards #-}
-- | Editing AO directly is not a great interface for programming.
-- But it isn't intolerable, either, at least for getting started.
--
-- To make it more tolerable, developers are able to edit just a
-- few words at a time, and to select a few words to pre-load the
-- editor.
--
-- While I've contemplated a few ways to make this more robust
-- against potential conflicts, at the moment I'm just going to
-- assume a single user editing a dictionary. (I can try to shift
-- conflict management into the DVCS layer for now.)
--
-- TODO: push logic into separate modules oriented around 
-- analysis of code and construction of great error reports.
--
-- IDEA: It seems feasible to create a richer editor with 
-- extra features, e.g. to rename a word or delete one by
-- writing something like `@word delete` or `@word rename foo`.
-- However, mixing responsibilities does complicate and mix
-- responses. I'll avoid this for now.
--
module Wikilon.WAI.Pages.AODictEdit
    ( appAODictEdit
    , formAODictLoadEditor
    , formAODictEdit
    ) where

import Control.Monad
import Control.Applicative
import Data.Monoid
import Data.Maybe (mapMaybe)
import Data.Either (lefts, rights)
import qualified Data.List as L
import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString.Lazy.Char8 as LBS
import qualified Data.ByteString.Lazy.UTF8 as LazyUTF8
import qualified Network.HTTP.Types as HTTP
import Text.Blaze.Html5 ((!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import qualified Network.Wai as Wai
import Database.VCache


import Awelon.ABC (ABC)
import qualified Awelon.ABC as ABC

import Wikilon.WAI.Utils
import Wikilon.WAI.Routes
import Wikilon.WAI.Conflicts
import Wikilon.WAI.RecvFormPost
import qualified Wikilon.WAI.RegexPatterns as Regex
import Wikilon.Store.Branch (BranchName, Branch)
import qualified Wikilon.Store.Branch as Branch
import Wikilon.Dict.Word
import Wikilon.Store.Dict (Dict)
import qualified Wikilon.Store.Dict as Dict
import qualified Wikilon.Dict.AODict as AODict
import Wikilon.Store.Root
import Wikilon.Time


-- | Provide a form that will pre-load the editor. If given a non-empty
-- list of words, its default value will contain just that word.
formAODictLoadEditor :: [Word] -> BranchName -> HTML
formAODictLoadEditor ws dictName =
    let uri = uriAODictEdit dictName in
    let uriAction = H.unsafeByteStringValue uri in
    H.form ! A.method "GET" ! A.action uriAction ! A.id "formLoadAODictEdit" $ do
        let content = 
                if L.null ws then A.placeholder "#foo: bar baz" else
                let wsText = L.intercalate " " $ fmap wordToText ws in
                A.value $ H.stringValue $ wsText 
        let pattern = A.pattern $ H.stringValue Regex.aoWordList
        H.input ! A.type_ "text" ! A.name "words" ! content ! pattern
        H.input ! A.type_ "submit" ! A.value "Load Editor"

mkAOText :: [(Word, LazyUTF8.ByteString)] -> LazyUTF8.ByteString
mkAOText = BB.toLazyByteString . mconcat . fmap encPair where
    encPair ((Word w), s) = 
        BB.charUtf8 '@' <> BB.byteString w <> BB.charUtf8 ' ' <> 
        BB.lazyByteString s <> BB.charUtf8 '\n'

-- | Create an editor form given some initial content.
--
-- At the moment I don't provide much feedback for success,
-- but I'll see if exposing the modified-time value helps.
--
-- I'll permit editing of the modified time, e.g. so users
-- can learn how conflicts would appear without much work
-- to create them.
formAODictEdit :: LBS.ByteString -> BranchName -> Maybe T -> HTML
formAODictEdit preload dictName mbT = 
    let uri = uriAODictEdit dictName in
    let uriAction = H.unsafeByteStringValue uri in
    -- giving blaze-html opportunity to escape the contents
    H.form ! A.method "POST" ! A.action uriAction ! A.id "formAODictEdit" $ do
        H.textarea ! A.name "update" ! A.rows "20" ! A.cols "70" ! A.required "required" $
            H.string $ LazyUTF8.toString preload
        H.br
        let tmVal = H.stringValue $ maybe "--" show mbT
        H.strong "Edit Origin: "
        H.input ! A.type_ "text" ! A.name "editOrigin" ! A.value tmVal
        H.string " "
        H.input ! A.type_ "submit" ! A.value "Submit"

appAODictEdit :: WikilonApp
appAODictEdit = app where
    app = routeOnMethod [(HTTP.methodGet, onGet),(HTTP.methodPost, onPost)]
    onGet = branchOnOutputMedia [(mediaTypeTextHTML, editorPage)]
    onPost = branchOnOutputMedia [(mediaTypeTextHTML, recvFormPost recvAODictEdit)]

queriedWordList :: HTTP.Query -> [Word]
queriedWordList = L.nub . L.concatMap getWords where
    getWords ("words", Just bs) = extractWordList bs
    getWords _ = []

-- | obtain page to edit the AO code
editorPage :: WikilonApp
editorPage = dictApp $ \ w dictName rq k -> do
    bset <- readPVarIO (wikilon_dicts $ wikilon_model w)
    let b = Branch.lookup' dictName bset
    let d = Branch.head b
    let tMod = Branch.modified b
    let lWords = queriedWordList (Wai.queryString rq)
    let lContent = L.zip lWords (Dict.lookupBytes d <$> lWords)
    let sContent = mkAOText lContent
    let status = HTTP.ok200
    let etag = eTagN (Dict.unsafeDictAddr d)
    let headers = [textHtml, etag]
    let title = "Edit Dictionary"
    k $ Wai.responseLBS status headers $ renderHTML $ do
        H.head $ do
            htmlHeaderCommon w
            H.title title 
        H.body $ do
            H.h1 title
            formAODictEdit sContent dictName tMod 
            H.hr
            H.strong "Dictionary:" <> " " <> hrefDict dictName
            navWords "Words" dictName lWords
            formAODictLoadEditor lWords dictName

type Line = Either LBS.ByteString (Word, ABC) 

parseLines :: LBS.ByteString -> [Line]
parseLines = fmap _parse . AODict.logicalLines where
    _parse ln = maybe (Left ln) Right $ AODict.decodeLine ln

-- my error analysis isn't entirely precise, but I can at least dig
-- to operations within blocks.
reportParseError :: LBS.ByteString -> HTML
reportParseError s = H.pre ! A.class_ "parseErrorReport" $ H.code ! A.lang "aodict" $ do
    case AODict.splitLine s of
        Nothing -> styleParseError $ H.string (LazyUTF8.toString s)
        Just (w, defbs) -> do
            let remText = either ABC.dcs_text (const LBS.empty) (ABC.decode defbs) 
            let lenOK = LBS.length defbs - LBS.length remText 
            let okText = LBS.take lenOK defbs 
            "@" <> H.string (show w) <> " " <> H.unsafeLazyByteString okText
            styleParseError $ H.string $ LazyUTF8.toString remText

styleParseError :: HTML -> HTML
styleParseError h = H.span 
    ! A.class_ "parseError" 
    ! A.style "background-color: LightCoral;" 
    $ h

-- | I should probably develop a more semantic merge for ABC definitions.
-- But for the moment, at least a structural merge will help developers
-- highlight the differences!
--
-- NOTE: the current 'diff' algorithm sucks at reporting deletions.
-- ALSO: font color is insufficient. I need background or border colors.
reportConflictABC :: BranchName -> Dict -> Dict -> Word -> ABC -> Maybe HTML
reportConflictABC dictName dOrig dHead w abc = 
    let bsOrig = Dict.lookupBytes dOrig w in
    let bsHead = Dict.lookupBytes dHead w in
    if bsOrig == bsHead then Nothing else Just $ do -- no change
    -- return an HTML description of the conflict
    -- let sOrig = LazyUTF8.toString bsOrig
    H.strong $ "@" <> hrefDictWord dictName w
    reportConflicts (LazyUTF8.toString bsOrig) (LazyUTF8.toString bsHead) (show abc)

histDict :: Branch -> Maybe T -> Dict
histDict b Nothing = Dict.empty vc where
    vc = Dict.dict_space $ Branch.head b
histDict b (Just t) = Branch.histDict b t

-- TODO: refactor. heavily. 100 lines is too much.
recvAODictEdit :: PostParams -> WikilonApp
recvAODictEdit pp 
  | (Just updates) <- getPostParam "update" pp
  , (Just tMod) <- (parseTime . LazyUTF8.toString) <$> getPostParam "editOrigin" pp
  = dictApp $ \ w dictName _rq k ->
    let parsed = parseLines updates in
    let lErr = lefts parsed in
    let lUpdates = rights parsed in
    let onParseError = 
            let status = HTTP.badRequest400 in
            let headers = [textHtml, noCache] in
            let title = "Parse Error" in
            k $ Wai.responseLBS status headers $ renderHTML $ do
                H.head $ do
                    htmlMetaNoIndex
                    htmlHeaderCommon w
                    H.title title
                H.body $ do
                    H.h1 title
                    H.p "Some content did not parse.\n\
                        \Do not resubmit without changes."
                    H.h2 "Description of Errors"
                    forM_ lErr $ \ e -> reportParseError e <> H.br
                    H.h2 "Edit and Resubmit"
                    formAODictEdit updates dictName tMod
    in
    if not (L.null lErr) then onParseError else
    -- if we don't exit on parse errors, we can move on.
    getTime >>= \ tNow ->
    let vc = vcache_space $ wikilon_store $ wikilon_model w in
    join $ runVTx vc $ 
        readPVar (wikilon_dicts $ wikilon_model w) >>= \ bset ->
        let b = Branch.lookup' dictName bset in
        let dHead = Branch.head b in
        let dOrig = histDict b tMod in
        let report = uncurry $ reportConflictABC dictName dOrig dHead in
        let lConflict = if (dOrig == dHead) then [] else mapMaybe report lUpdates in
        let onConflict =
                let status = HTTP.conflict409 in
                let headers = [textHtml, noCache] in
                let title = "Edit Conflicts" in
                return $ k $ Wai.responseLBS status headers $ renderHTML $ do
                    H.head $ do
                        htmlMetaNoIndex
                        htmlHeaderCommon w
                        H.title title
                    H.body $ do
                        H.h1 title
                        H.p "A concurrent edit has modified words you're manipulating."
                        let sEditOrigin = maybe "--" show tMod
                        let sLastUpdate = maybe "--" show (Branch.modified b)
                        H.p $ H.strong "Edit Origin: " <> H.string sEditOrigin
                        H.p $ H.strong "Head Version: " <> H.string sLastUpdate
                        H.h2 "Change Report"
                        H.p "Currently just some string diffs. (Todo: structural or semantic diffs.)"
                        mconcat $ fmap (<> H.br) lConflict
                        H.h2 "Edit and Resubmit"
                        H.p "At your discretion, you may resubmit without changes."
                        formAODictEdit updates dictName (Branch.modified b)
        in
        if not (L.null lConflict) then onConflict else
        -- if we don't exit on edit conflicts, we can attempt to update the dictionary!
        case Dict.safeUpdateWords lUpdates dHead of
            Left insErrors -> -- INSERT ERRORS
                let status = HTTP.conflict409 in
                let headers = [textHtml, noCache] in
                let title = "Content Conflicts" in
                return $ k $ Wai.responseLBS status headers $ renderHTML $ do
                    H.head $ do
                        htmlMetaNoIndex
                        htmlHeaderCommon w
                        H.title title
                    H.body $ do
                        H.h1 title
                        H.p "The proposed update is structurally problematic."
                        H.h2 "Problems"
                        H.ul $ forM_ insErrors $ \ e ->
                            H.li $ H.string $ show e
                        H.h2 "Edit and Resubmit"
                        H.p "If you can easily do so from here."
                        formAODictEdit updates dictName tMod
            Right dUpd -> do -- EDIT SUCCESS!
                let b' = Branch.update (tNow, dUpd) b 
                let bset' = Branch.insert dictName b' bset 
                writePVar (wikilon_dicts $ wikilon_model w) bset' -- commit the update
                markDurable -- hand-written updates should be durable

                -- prepare our response, including edit success information.
                let status = HTTP.seeOther303
                let lWords = fmap (wordToUTF8 . fst) lUpdates
                let editor = mconcat $ [uriAODictEdit dictName, "?words="]
                             ++ (L.intersperse "+" lWords)
                let dest = (HTTP.hLocation, wikilon_httpRoot w <> editor) 
                let headers = [textHtml, noCache, dest] 
                let title = "Edit Success" 
                return $ k $ Wai.responseLBS status headers $ renderHTML $ do
                    H.head $ do
                        htmlMetaNoIndex
                        htmlHeaderCommon w
                        H.title title
                    H.body $ do
                        H.h1 title
                        H.p $ "Return to " <> href editor "the editor" <> "."
recvAODictEdit _pp = \ _w _cap _rq k -> k $ eBadRequest $ 
    "POST: missing 'update' or 'editOrigin' parameters"

