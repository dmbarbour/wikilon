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
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Lazy.UTF8 as LazyUTF8
import qualified Network.HTTP.Types as HTTP
import Text.Blaze.Html5 ((!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import qualified Network.Wai as Wai
import qualified Data.Algorithm.Diff as Diff
import qualified Data.Algorithm.Diff3 as Diff3
import Database.VCache


import Awelon.ABC (ABC)
import qualified Awelon.ABC as ABC

import Wikilon.WAI.Utils
import Wikilon.WAI.Routes
import Wikilon.WAI.RecvFormPost
import qualified Wikilon.WAI.RegexPatterns as Regex
import Wikilon.Branch (BranchName, Branch)
import qualified Wikilon.Branch as Branch
import Wikilon.Dict.Word
import Wikilon.Dict (Dict)
import qualified Wikilon.Dict as Dict
import qualified Wikilon.Dict.AODict as AODict
import Wikilon.Root
import Wikilon.Time

-- | Provide a form that will pre-load the editor. If given a non-empty
-- list of words, its default value will contain just that word.
formAODictLoadEditor :: [Word] -> BranchName -> HTML
formAODictLoadEditor ws dictName =
    let uri = uriAODictEdit dictName in
    let uriAction = H.unsafeByteStringValue uri in
    H.form ! A.method "GET" ! A.action uriAction ! A.id "formAODictLoadEditor" $ do
        let content = 
                if L.null ws then A.placeholder "foo bar baz" else
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
formAODictEdit :: LBS.ByteString -> BranchName -> Maybe T -> HTML
formAODictEdit preload dictName mbT = 
    let uri = uriAODictEdit dictName in
    let uriAction = H.unsafeByteStringValue uri in
    -- giving blaze-html opportunity to escape the contents
    H.form ! A.method "POST" ! A.action uriAction ! A.id "formAODictEdit" $ do
        H.textarea ! A.name "update" ! A.rows "20" ! A.cols "70" ! A.required "required" $
            H.string $ LazyUTF8.toString preload
        --let vOrigin = H.unsafeByteStringValue origin
        --H.input ! A.type_ "hidden" ! A.name "origin" ! A.value vOrigin
        let tmVal = H.stringValue $ maybe "--" show mbT
        H.input ! A.type_ "hidden" ! A.name "modified" ! A.value tmVal
        H.br
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

extractWordList :: BS.ByteString -> [Word]
extractWordList = L.filter isValidWord . fmap Word . BS.splitWith spc where
    spc c = (32 == c) || (44 == c) -- spaces and commas

loadBytes :: Dict -> Word -> LBS.ByteString
loadBytes d = maybe LBS.empty id . Dict.lookupBytes d


-- | obtain page to edit the AO code
editorPage :: WikilonApp
editorPage w (dictCap -> Just dictName) rq k = do
    bset <- readPVarIO (wikilon_dicts w)
    let b = Branch.lookup' dictName bset
    let d = Branch.head b
    let tMod = Branch.modified b
    let lWords = queriedWordList (Wai.queryString rq)
    let lContent = L.zip lWords (loadBytes d <$> lWords)
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
            let hrefAODict = href uriAODictDocs "AODict format"
            H.p $ "Edit an ad-hoc fragment of dictionary in " <> hrefAODict <> "."
            formAODictEdit sContent dictName tMod 
            H.h2 "Reload Editor"
            H.p $ "Load words into the editor to view or edit." <>
                  H.b "Warning:" <> " editor content will be lost."
            formAODictLoadEditor lWords dictName <> H.br
            H.hr
            unless (L.null lWords) $ do
                H.nav $ do
                    H.strong "Words:" 
                    forM_ lWords $ \ aow -> " " <> wordLink dictName aow
                H.br
            H.strong "Dictionary:" <> " " <> dictLink dictName
editorPage _ caps _ k = k $ eBadName caps

type Line = Either LBS.ByteString (Word, ABC) 

parseLines :: LBS.ByteString -> [Line]
parseLines = fmap _parse . AODict.logicalLines where
    _parse ln = maybe (Left ln) Right $ AODict.decodeLine ln

-- my error analysis isn't very good, but I can at least try to
-- highlight an error. 
reportParseError :: LBS.ByteString -> HTML
reportParseError s = H.pre $ H.code ! A.lang "aodict" $ do
    let cursor = H.strong ! A.class_ "parseErrorCursor" ! A.style "color:red;font-weight:900" $ "(@)"
    case AODict.splitLine s of
        Nothing -> cursor <> H.string (LazyUTF8.toString s)
        Just (w, defbs) -> do
            let (abc, leftOver) = ABC.decode defbs
            "@" <> H.string (show w) <> " " <> H.string (show abc) 
            cursor
            H.string $ LazyUTF8.toString leftOver



-- | I should probably develop a more semantic merge for ABC definitions.
-- But for the moment, at least a structural merge will help developers
-- highlight the differences!
--
-- NOTE: the current 'diff' algorithm sucks at reporting deletions.
-- ALSO: font color is insufficient. I need background or border colors.
reportConflict :: BranchName -> Dict -> Dict -> (Word, ABC) -> Maybe HTML
reportConflict dictName dOrig dHead (w, abc) = 
    let bsOrig = loadBytes dOrig w in
    let bsHead = loadBytes dHead w in
    if bsOrig == bsHead then Nothing else Just $ do -- no change
    -- return an HTML description of the conflict
    -- let sOrig = LazyUTF8.toString bsOrig
    H.strong $ "@" <> wordLink dictName w
    H.b "Head Version:"
    headBox $ H.string (LazyUTF8.toString bsHead)
    H.b "2-Way String Merge (Head and Edit):"
    merge2Box $ twoWayMerge (LazyUTF8.toString bsHead) (show abc)
    H.b "3-Way String Merge:"
    merge3Box $ threeWayMerge (LazyUTF8.toString bsHead) (LazyUTF8.toString bsOrig) (show abc)

twoWayMerge :: String -> String -> HTML
twoWayMerge sHead sEdit = 
    let lChunks = Diff.getGroupedDiff sHead sEdit in
    forM_ lChunks $ \ chunk -> case chunk of
        Diff.First s -> styleHead $ H.string s
        Diff.Second s -> styleEdit $ H.string s
        Diff.Both s _ -> styleOrig $ H.string s

threeWayMerge :: String -> String -> String -> HTML
threeWayMerge sHead sOrig sEdit =
    let lChunks = Diff3.diff3 sHead sOrig sEdit in
    forM_ lChunks $ \ chunk -> case chunk of
        Diff3.LeftChange s -> styleHead $ H.string s
        Diff3.RightChange s -> styleEdit $ H.string s
        Diff3.Unchanged s -> styleOrig $ H.string s 
        Diff3.Conflict h o e -> styleConflict $ do
            barrierConflict "("
            styleHead $ H.string h
            barrierConflict "|"
            styleOrig $ H.string o
            barrierConflict "|"
            styleEdit $ H.string e
            barrierConflict ")"

headBox, merge2Box, merge3Box :: HTML -> HTML
headBox = codeBox "diffHeadBox" "border-color:Navy;border-style:dashed;border-width:thin" 
merge2Box = codeBox "diffMerge2Box" "border-color:Indigo;border-style:dashed;border-width:thin"
merge3Box = codeBox "diffMerge3Box" "border-color:SeaGreen;border-style:dashed;border-width:thin"

codeBox :: String -> String -> HTML -> HTML
codeBox _class _style h =
    H.pre ! A.style (H.stringValue _style) $ 
    H.code ! A.class_ (H.stringValue _class) ! A.lang "abc" $ h

styleHead, styleEdit, styleOrig :: HTML -> HTML
styleHead = H.span ! A.class_ "diffHead" ! A.style "background-color:DarkSeaGreen"
styleEdit = H.span ! A.class_ "diffEdit" ! A.style "background-color:Thistle"
styleOrig = id

styleConflict, barrierConflict :: HTML -> HTML
styleConflict = H.span ! A.class_ "diffConflict" ! A.style "border-color:DarkOrange;border-style:solid;border-width:medium"
barrierConflict = H.span ! A.class_ "diffConflictSep" ! A.style "color:DarkOrange;font-weight:bolder"

            
histDict :: Branch -> Maybe T -> Dict
histDict b Nothing = Dict.empty vc where
    vc = Dict.dict_space $ Branch.head b
histDict b (Just t) = Branch.histDict b t

-- TODO: refactor. heavily. 100 lines is too much.
recvAODictEdit :: PostParams -> WikilonApp
recvAODictEdit pp w cap _rq k 
  | (Just updates) <- getPostParam "update" pp
  , (Just tMod) <- (parseTime . LazyUTF8.toString) <$> getPostParam "modified" pp
  , (Just dictName) <- dictCap cap
  = let parsed = parseLines updates in
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
                    H.p "Some update content did not parse.\n\
                        \Do not resubmit without changes."
                    H.h2 "Description of Errors"
                    forM_ lErr $ \ e -> reportParseError e <> H.br
                    H.h2 "Edit and Resubmit"
                    formAODictEdit updates dictName tMod
    in
    if not (L.null lErr) then onParseError else
    -- if we don't exit on parse errors, we can move on.
    getTime >>= \ tNow ->
    let vc = vcache_space $ wikilon_store w in
    join $ runVTx vc $ 
        readPVar (wikilon_dicts w) >>= \ bset ->
        let b = Branch.lookup' dictName bset in
        let dHead = Branch.head b in
        let dOrig = histDict b tMod in
        let lConflict = if (dOrig == dHead) then [] else      
                mapMaybe (reportConflict dictName dOrig dHead) lUpdates
        in
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
                        H.h2 "Change Report"
                        H.p "Currently just some string diffs. Todo: structural and semantic diffs."
                        forM_ lConflict $ \ report -> report <> H.br
                        H.h2 "Edit and Resubmit"
                        H.p "At your discretion, you may resubmit without changes."
                        formAODictEdit updates dictName (Branch.modified b)
        in
        if not (L.null lConflict) then onConflict else
        -- if we don't exit on edit conflicts, we can attempt to update the dictionary!
        case Dict.insert dHead lUpdates of
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
                        H.p "At least if you can easily do so from here."
                        formAODictEdit updates dictName tMod
            Right dUpd -> do -- EDIT SUCCESS!
                let b' = Branch.update (tNow, dUpd) b 
                let bset' = Branch.insert dictName b' bset 
                writePVar (wikilon_dicts w) bset' -- commit the update
                markDurable -- hand-written updates should be durable
                -- prepare our response:
                let status = HTTP.seeOther303 
                let editor = uriAODictEdit dictName 
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
recvAODictEdit _pp _w cap _rq k =
    case dictCap cap of
        Nothing -> k $ eBadName cap
        Just _ -> k $ eBadRequest "POST: missing update or modified parameters"

{-
form

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





-- IDEAS:
-- for AODict editing
-- supply ability to ask for a list of words (e.g. in a GET field)
-- fill the edit box with the current definitions of these words
-- automatically add associated word links for dependencies 
-- include an 'overwrite' list for words in the editor.





-- | dictEditAO is a very simplistic editor that requires input via
-- the 'aodict' format. This isn't a convenient format for direct human
-- use
dictEditAO :: WikilonApp
dictEditAO = app where
    app = routeOnMethod
        [(HTTP.methodGet, onGet)
        ,(HTTP.methodPost, onPost)]
    onGet = branchOnOutputMedia [(mediaTypeTextHTML, onGetHTML)]
    onGetHTML w (dictCap -> Just dictName) rq k =
        k $ Wai.responseLBS HTTP.ok200 [textHtml] $ renderHTML $ do
        let title = H.string $ "Edit " ++ UTF8.toString dictName
        H.head $ do
            htmlHeaderCommon w
            H.title title
        H.body $ do
            H.h1 title
            formDictEdit (Wai.rawPathInfo rq) dictName
            H.hr
            H.string "view " <> lnkAODict dictName <> H.br
            H.string "return to " <> dictLink dictName <> H.br
    onGetHTML _w captures _rq k = k $ eBadName captures
    onPost = recvFormPost recvDictEditAO


-- 

recvDictEdit :: PostParams -> WikilonApp
recvDictEdit pp@(ppUpdate -> Just body) w (dictCap -> Just dictName) rq k = do
    let defaultDest = wikilon_httpRoot w <> dictURI dictName
    let dest = (HTTP.hLocation, maybe defaultDest id (ppOrigin pp))
    
    let vc = wikilon_store w 
    let vsp = vcache_space vc
    join $ runVTx vsp $ do
        bset <- readPVar (wikilon_dicts w) 
        let b = Branch.lookup' dictName bset
        

-- | error 400 for edit
badEditRequest :: [String] -> Route -> Wikilon -> Wai.Response
badEditRequest errs dest w = 
    Wai.responseLBS HTTP.badRequest400 [textHtml] $ renderHTML $ do
    let title = "
    H.head $ do
        htmlHeaderCommon w
        H
    

    
    
    runVtx 

    update <- parseDictEdit w dictName body
    result <- processDictEdit w dictName body 
    case result of 
        Left errors -> 
            k $ Wai.responseLBS HTTP.badRequst400 [textHtml] $ renderHTML $ do
            let title = H.string $ "Edit Rejected"
            H.head $ do
                htmlHeaderCommon w
                H.title title
            H.body $ do
                H.h1 title
                H.p "The edit request was rejected due either to the structure\n\
                    \of the request or the
                
                 
            let status = HTTP.badRequest400 in
            let headers = [textHtml] in
            k $ Wai.
            

    let vc = wikilon_store w in
    join $ runVtx 
    k $ Wai.responseLBS HTTP.accepted202 [plainText] $ "todo: run edit"
recvDictEdit pp _w captures _rq k =
    case ppUpdate pp of
        Nothing -> k $ eBadRequest "missing 'update' parameter"
        Just _ -> k $ eBadName captures

ppUpdate :: PostParams -> Maybe LBS.ByteString
ppUpdate = getPostParamUnzip "update"

ppOrigin :: PostParams -> Maybe BS.ByteString
ppOrigin = fmap LBS.toStrict . getPostParam "origin"


formDictEditAO :: Route -> BranchName -> HTML
formDictEditAO r d =
    let uri = uriDictEdit d in
    let uriAction = H.unsafeByteStringValue uri in
    H.form ! A.id "aodictEdit" ! A.method "POST" ! A.action uriAction $ do
        H.input ! A.type_ "textarea" ! A.name "update"  ! A.required "true" 
                ! A.rows "24" ! A.cols "70" 
                ! A.placeholder "@helloWorld \"Hello, World!\n\
                                \~[v'c]\n\
                                \@swap [rwrwzwlwl][]"
        let origin = H.unsafeByteStringValue r
        H.input ! A.type_ "hidden" ! A.name "origin" ! A.value origin
        H.input ! A.type_ "submit" ! A.value "Update"

-}







