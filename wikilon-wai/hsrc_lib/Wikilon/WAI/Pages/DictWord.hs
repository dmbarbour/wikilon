{-# LANGUAGE OverloadedStrings #-}

module Wikilon.WAI.Pages.DictWord
    ( dictWord
    , dictWordAODef
    , dictWordEdit
    --, formDictWordEdit
    , dictWordRename
    , formDictWordRename
    ) where

import Control.Applicative
import Control.Monad
import Data.Monoid
import qualified Data.List as L
import qualified Data.ByteString.Lazy as LBS
import qualified Network.HTTP.Types as HTTP
import qualified Network.HTTP.Media as HTTP
import Text.Blaze.Html5 ((!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import qualified Network.Wai as Wai
import Database.VCache

import Wikilon.Time
import Wikilon.Dict.Word

import Wikilon.WAI.Utils
import Wikilon.WAI.Routes
import Wikilon.WAI.RecvFormPost
import qualified Wikilon.WAI.RegexPatterns as Regex
import qualified Wikilon.Store.Dict as Dict
import Wikilon.Store.Branch (BranchName)
import qualified Wikilon.Store.Branch as Branch
import Wikilon.Store.Root

dictWord :: WikilonApp
dictWord = app where
    app = routeOnMethod $
        [(HTTP.methodGet, onGet)
        ,(HTTP.methodPut, onPut)
        ,(HTTP.methodDelete, delDictWord)
        -- I might eventually want to Post update actions?
        ]
    onGet = branchOnOutputMedia $
        [(mediaTypeTextHTML, getDictWordPage)
        ,(mediaTypeAODef, getDictWordAODef)
        ]
    onPut = branchOnInputMedia $
        [(mediaTypeAODef, putDictWordAODef)
        ]

-- | an endpoint that forces content to the 'aodef' media type.
dictWordAODef :: WikilonApp
dictWordAODef = app where
    app = routeOnMethod $
        [(HTTP.methodGet, onGet)
        ,(HTTP.methodPut, onPut)
        ,(HTTP.methodDelete, delDictWord)
        ]
    onGet = branchOnOutputMedia [(mediaTypeAODef, getDictWordAODef)]
    onPut = branchOnInputMedia [(mediaTypeAODef, putDictWordAODef)]

-- a simple form to rename a word.
formDictWordRename :: BranchName -> Word -> HTML
formDictWordRename d w =
    let uri = H.unsafeByteStringValue $ uriDictWordRename d w in
    let ws = wordToUTF8 w in
    H.form ! A.method "POST" ! A.action uri ! A.id "formDictWordRename" $ do
        H.fieldset ! A.id "formDictWordRenameFieldset" ! A.style "display: inline" $ do
            H.legend ("Rename " <> H.unsafeByteString ws) 
            let wv = H.unsafeByteStringValue ws 
            let rx = H.stringValue Regex.aoWord 
            H.input ! A.type_ "text" ! A.name "target" ! A.value wv ! A.pattern rx
            H.input ! A.type_ "submit" ! A.value "Rename"

dictWordRename :: WikilonApp
dictWordRename = app where
    app = routeOnMethod [(HTTP.methodGet, onGet), (HTTP.methodPost, onPost)]
    onGet = branchOnOutputMedia [(mediaTypeTextHTML, pageWordRename)]
    onPost = branchOnOutputMedia [(mediaTypeTextHTML, recvFormPost recvWordRename)]
    
-- a page with just the form to rename a word...
pageWordRename :: WikilonApp
pageWordRename = dictWordApp $ \ w dn dw _rq k ->
    let status = HTTP.ok200 in
    let headers = [textHtml] in
    let title = "Rename Word" in
    k $ Wai.responseLBS status headers $ renderHTML $ do
        H.head $ do
            htmlHeaderCommon w
            H.title title
        H.body $ do
            formDictWordRename dn dw
            H.br
            renameMeta

renameMeta :: HTML
renameMeta = do
    H.p $ H.strong "Effects:" <> " after renaming, references to the original word\n\
          \are rewritten to the new target word, the original word is undefined,\n\
          \and the target word has the original's definition.\n\
          \"
    H.p $ H.strong "Limitations:" <> " to rename a word, the target word must\n\
          \either be undefined or have a byte-for-byte identical definition.\n\
          \In the latter case, the two words are merged.\n\
          \"

recvWordRename :: PostParams -> WikilonApp
recvWordRename pp
  | (Just wt) <- Word . LBS.toStrict <$> getPostParam "target" pp
  , isValidWord wt
  = dictWordApp $ \ w dn wo _rq k ->
    let vc = vcache_space $ wikilon_store $ wikilon_model w in
    getTime >>= \ tNow ->
    join $ runVTx vc $ 
        let dicts = wikilon_dicts $ wikilon_model w in
        readPVar dicts >>= \ bset ->
        let b = Branch.lookup' dn bset in
        let d = Branch.head b in
        let tryRename = Dict.safeRenameWord wo wt d <|> Dict.safeMergeWords wo wt d in
        case tryRename of
            Nothing -> 
                let status = HTTP.conflict409 in
                let headers = [textHtml, noCache] in
                let title = "Word Rename Conflict" in
                return $ k $ Wai.responseLBS status headers $ renderHTML $ do
                    H.head $ do
                        htmlMetaNoIndex
                        htmlHeaderCommon w
                        H.title title
                    H.body $ do
                        H.h1 title
                        H.p $ H.strong "origin: " <> " " <> hrefDictWord dn wo
                        H.p $ H.strong "target: " <> " " <> hrefDictWord dn wt
                        H.p "Rename failed due to distinct definition of target.\n\
                            \For rename to succeed, target word must be undefined\n\
                            \or byte-for-byte identical to definition at origin."
            Just d' -> do
                let b' = Branch.update (tNow, d') b 
                let bset' = Branch.insert dn b' bset 
                writePVar dicts bset' -- commit update
                markDurable -- push to disk
                -- prepare our response:
                let status = HTTP.seeOther303 
                let dest = (HTTP.hLocation, wikilon_httpRoot w <> uriDictWord dn wt) 
                let headers = [textHtml, noCache, dest] 
                let title = "Rename Successful" 
                return $ k $ Wai.responseLBS status headers $ renderHTML $ do
                    H.head $ do
                        htmlMetaNoIndex
                        htmlHeaderCommon w
                        H.title title
                    H.body $ do
                        H.h1 title
                        H.p $ "renamed to: " <> hrefDictWord dn wt 
recvWordRename _ = \ _w _cap _rq k -> 
    k $ eBadRequest $ "invalid target word"


dictWordEdit :: WikilonApp
dictWordEdit = toBeImplementedLater "Support POST form editing of words"

putDictWordAODef :: WikilonApp
putDictWordAODef = toBeImplementedLater "PUT word via AODef definition"
{-
putDictWordAODef = dictWordApp $ \ w dn dw rq k ->
    Wai.lazyRequestBody rq >>= \ body ->
    readPVar
-}
    

delDictWord :: WikilonApp
delDictWord = toBeImplementedLater "HTTP Deletion of individual Word"

-- | Return just the AO definition. This will always succeed, though
-- it may return an empty string for an undefined word.
getDictWordAODef :: WikilonApp
getDictWordAODef = dictWordApp $ \ w dn dw _rq k ->
    readPVarIO (wikilon_dicts $ wikilon_model w) >>= \ bset ->
    let hMedia = (HTTP.hContentType, HTTP.renderHeader mediaTypeAODef) in
    let status = HTTP.ok200 in
    let headers = [hMedia] in
    let b = Branch.lookup' dn bset in
    let d = Branch.head b in 
    k $ Wai.responseLBS status headers $ Dict.lookupBytes d dw 

getDictWordPage :: WikilonApp
getDictWordPage = dictWordApp $ \ w dn dw _rq k ->
    readPVarIO (wikilon_dicts $ wikilon_model w) >>= \ bset ->
    let b = Branch.lookup' dn bset in
    let d = Branch.head b in 
    let abc = Dict.lookup d dw in
    let status = HTTP.ok200 in
    let headers = [textHtml] in
    let title = H.unsafeByteString (wordToUTF8 dw) in
    k $ Wai.responseLBS status headers $ renderHTML $ do
        H.head $ do
            htmlHeaderCommon w
            H.title title
        H.body $ do
            H.h1 title
            -- TODO: switch to an editable text area here? at least for now!
            H.textarea ! A.class_ "aodef" ! A.readonly "readonly" 
                ! A.lang "abc" ! A.placeholder "(undefined)"
                ! A.rows "20" ! A.cols "70" $ H.string (show abc)
            let lDeps = L.nub $ Dict.abcWords abc 
            let lClients = Dict.wordClients d dw 
            -- navDocWords dictName dictWord
            H.hr
            navWords "Dependencies" dn lDeps
            navWords "Dependents" dn lClients
            H.nav $ H.strong "Dictionary" <> " " <> hrefDict dn
            H.br
            H.br
            formDictWordRename dn dw
            H.p "to delete: clear definition to empty string"
            H.h2 "TODO"
            H.p "access to compiled word and type information (or errors),\n\
                \expanded definition suitable for rendering and structured editing,\n\
                \samples where word is used with known values in other definitions,\n\
                \animations of word's construction and word in use, compile words of\n\
                \appropriate types to javascript or SVG or raster images or simple\n\
                \web applications, etc."

navWords :: String -> BranchName -> [Word] -> HTML
navWords _ _ [] = mempty
navWords sClass dn lWords =
    H.nav ! A.class_ (H.stringValue sClass) $ do
        H.strong (H.string sClass) 
        forM_ lWords $ \ w -> " " <> hrefDictWord dn w

