{-# LANGUAGE OverloadedStrings, ViewPatterns #-}
-- | Pages for a single dictionary. This might turn into another
-- aggregation module because I want a lot of diverse operations
-- on full dictionaries.
--
-- Note: another format that might be useful for import/export is
-- a tar file, expanding into one file per word?
module Wikilon.WAI.Pages.Dict
    ( dictResource
    , dictWords
    , dictEdit
    , formDictEdit
    , module Wikilon.WAI.Pages.AODict
    ) where

import Data.Monoid
import qualified Data.ByteString.UTF8 as UTF8
import qualified Data.ByteString.Lazy as LBS
import qualified Network.HTTP.Types as HTTP
import Text.Blaze.Html5 ((!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import qualified Network.Wai as Wai
import Database.VCache

import Wikilon.WAI.Utils
import Wikilon.WAI.Routes
import Wikilon.WAI.RecvFormPost
import Wikilon.Branch (BranchName)
import qualified Wikilon.Branch as Branch
import Wikilon.Root

import Wikilon.WAI.Pages.AODict
        
-- The full 'dictionary resource' will include access to
-- words, histories, issues, subscriptions, etc.. Since
-- there is a lot here, I'll need to use subdirectory URIs
-- for current words and so on.
dictResource :: WikilonApp
dictResource = app where
    app = justGET onGet 
    onGet = branchOnOutputMedia
        [(mediaTypeTextHTML, dictFrontPage)
        ]

dictFrontPage :: WikilonApp
dictFrontPage w (dictCap -> Just dictName) rq k = dictFrontPage' dictName w rq k
dictFrontPage _w caps _rq k = k $ eBadName caps

dictFrontPage' :: BranchName -> Wikilon -> Wai.Application
dictFrontPage' dictName w rq k = 
    readPVarIO (wikilon_dicts w) >>= \ bset ->
    let b = Branch.lookup' dictName bset in
    --let d = Branch.head b in
    let status = HTTP.status200 in
    let headers = [textHtml] in
    k $ Wai.responseLBS status headers $ renderHTML $ do
    let title = H.unsafeByteString dictName 
    let tmModified = maybe "--" htmlSimpTime $ Branch.modified b
    let origin = Wai.rawPathInfo rq
    H.head $ do
        htmlHeaderCommon w
        H.title title 
    H.body $ do
        H.h1 title
        H.p "This is an AO dictionary front page. I'm still figuring out what should go here."
        H.h2 "Recent Events"
        H.h2 "Dictionary Health"
        H.h2 "Resources"
        H.ul $ do
            H.li $ lnkAODict dictName <> " see dictionary (very primitive)"
        -- maybe some content from the dictionary itself
        -- maybe add some banner or CSS from dictionary itself
        -- maybe a simple console-like or query application?
        -- after defining apps, probably want to revist them easily
        -- HEALTH information
            -- undefined words & cycles
            -- words that don't compile, compile errors
            -- words that are badly typed
        -- H.h2 "Recent Changes"
        -- H.h2 "Resources"
            -- H.li $ lnkDictWords dictName <> " - dictionary words"
            -- H.li $ lnkMasterDict dictName <> " - view as master"
            -- 
            -- probably want a page to create filtered AODict views
            -- probably want pages for histories and events
            -- 
            -- H.li $ lnkAODict dictName <> " - aodict format "
            -- 
        H.h2 "Quick Edit"
        formDictEdit origin dictName
        H.hr
        H.div ! A.id "dictFoot" ! A.class_ "footer" $ do
            H.b "Export:" <> " " <> lnkAODictFile dictName <> " " <> lnkAODictGz dictName <> H.br
            H.b "Import:" <> " " <> formImportAODict origin dictName <> H.br
            H.b "Modified:" <> " " <> tmModified <> H.br

-- | dictEdit is a very simplistic editor for AO.
dictEdit :: WikilonApp
dictEdit = app where
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
    onPost = recvFormPost recvDictEdit

recvDictEdit :: PostParams -> WikilonApp
recvDictEdit (ppUpdate -> Just body) w (dictCap -> Just dictName) rq k =
    k $ Wai.responseLBS HTTP.accepted202 [plainText] $ "todo: run edit"
recvDictEdit pp _w captures _rq k =
    case ppUpdate pp of
        Nothing -> k $ eBadRequest "missing 'update' parameter"
        Just _ -> k $ eBadName captures

ppUpdate :: PostParams -> Maybe LBS.ByteString
ppUpdate = getPostParamUnzip "update"


formDictEdit :: Route -> BranchName -> HTML
formDictEdit r d =
    let uri = uriDictEdit d in
    let uriAction = H.unsafeByteStringValue uri in
    H.form ! A.id "dictEdit" ! A.method "POST" ! A.action uriAction $ do
        H.input ! A.type_ "textarea" ! A.name "update"  ! A.required "true" 
                ! A.rows "24" ! A.cols "70" 
                ! A.placeholder "@helloWorld \"Hello, World!\n\
                                \~[v'c]\n\
                                \@swap [rwrwzwlwl][]"
        let origin = H.unsafeByteStringValue r
        H.input ! A.type_ "hidden" ! A.name "origin" ! A.value origin
        H.input ! A.type_ "submit" ! A.value "Update"



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
dictWordsPage = toBeImplementedLater "Dict words!"




