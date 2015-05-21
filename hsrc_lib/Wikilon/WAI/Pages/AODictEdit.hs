{-# LANGUAGE OverloadedStrings, ViewPatterns #-}
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
module Wikilon.WAI.Pages.AODictEdit
    ( appAODictEdit
    , formAODictLoadEditor
    , formAODictEdit
    ) where

import Control.Applicative
import Data.Monoid
import qualified Data.List as L
import qualified Data.ByteString as BS
import qualified Data.ByteString.UTF8 as UTF8
import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Lazy.UTF8 as LazyUTF8
import qualified Network.HTTP.Types as HTTP
import Text.Blaze.Html5 ((!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import qualified Network.Wai as Wai
import Database.VCache

import Wikilon.WAI.Utils
import Wikilon.WAI.Routes
import Wikilon.WAI.RecvFormPost
import Wikilon.WAI.RegexPatterns
import Wikilon.Branch (BranchName)
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
                if L.null ws then A.placeholder "foo,bar,baz" else
                let wsText = L.intercalate "," $ fmap wordToText ws in
                A.value $ H.stringValue $ wsText 
        let pattern = A.pattern $ H.stringValue aoWordList
        H.input ! A.type_ "text" ! A.name "words" ! content ! pattern
        H.input ! A.type_ "submit" ! A.value "Load Editor"

mkAOText :: [(Word, LazyUTF8.ByteString)] -> LazyUTF8.ByteString
mkAOText = BB.toLazyByteString . mconcat . fmap encPair where
    encPair ((Word w), s) = 
        BB.charUtf8 '@' <> BB.byteString w <> BB.charUtf8 ' ' <> 
        BB.lazyByteString s <> BB.charUtf8 '\n'

-- | Create an editor form given some initial content. 
formAODictEdit :: [(Word, LazyUTF8.ByteString)] -> BranchName -> Maybe T -> HTML
formAODictEdit ws dictName mbT = 
    let uri = uriAODictEdit dictName in
    let uriAction = H.unsafeByteStringValue uri in
    -- giving blaze-html opportunity to escape the contents
    let content = LazyUTF8.toString $ mkAOText ws in -- pre-escaped contents
    H.form ! A.method "POST" ! A.action uriAction ! A.id "formAODictEdit" $ do
        H.textarea ! A.name "update" ! A.rows "20" ! A.cols "70" ! A.required "required" $
            H.string $ content
        --let vOrigin = H.unsafeByteStringValue origin
        --H.input ! A.type_ "hidden" ! A.name "origin" ! A.value vOrigin
        let tmVal = H.stringValue $ maybe "--" show mbT
        H.input ! A.type_ "hidden" ! A.name "modified" ! A.value tmVal
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
    let status = HTTP.ok200
    let headers = [textHtml]
    let title = H.string $ "Edit " ++ UTF8.toString dictName ++ " Dictionary"
    k $ Wai.responseLBS status headers $ renderHTML $ do
        H.head $ do
            htmlHeaderCommon w
            H.title title 
        H.body $ do
            H.h1 title
            let hrefAODict = H.a ! A.href (H.unsafeByteStringValue uriAODictDocs)
            H.p $ "Edit an ad-hoc fragment of " <> hrefAODict "AODict" <> " content."
            formAODictEdit lContent dictName tMod 
            H.h2 "Reload Editor"
            H.p $ "Load one or more words into the editor for viewing or editing."
                <> H.b "Warning:" <> " you will lose current editor contents."
            formAODictLoadEditor lWords dictName
editorPage _ caps _ k = k $ eBadName caps

recvAODictEdit :: PostParams -> WikilonApp
recvAODictEdit _ = toBeImplementedLater "receive aodict.edit"

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







