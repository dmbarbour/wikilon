{-# LANGUAGE OverloadedStrings #-}

module Wikilon.WAI.Pages.DictWord
    ( dictWord
    , dictWordClients
    , module Wikilon.WAI.Pages.DictWord.Rename
    , module Wikilon.WAI.Pages.DictWord.ClawDef
    , module Wikilon.WAI.Pages.DictWord.AODef
    ) where

import Control.Monad
import Data.Monoid
import qualified Data.List as L
import qualified Data.ByteString.Builder as BB
import qualified Network.HTTP.Types as HTTP
import Text.Blaze.Html5 ((!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

import qualified Network.Wai as Wai
import Database.VCache

import Awelon.ClawCode


import Wikilon.WAI.Utils
import Wikilon.WAI.Routes
import qualified Wikilon.Store.Dict as Dict
import qualified Wikilon.Store.Branch as Branch
import Wikilon.Store.Root

import Wikilon.WAI.Pages.DictWord.Rename
import Wikilon.WAI.Pages.DictWord.AODef
import Wikilon.WAI.Pages.DictWord.ClawDef

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
        ,(mediaTypeClaw, getDictWordClawDef)
        ]
    onPut = branchOnInputMedia $
        [(mediaTypeAODef, putDictWordAODef)
        ,(mediaTypeClaw, putDictWordClawDef)
        ]

-- | This is our basic non-interactive web page for words.
--
-- For the moment, I'm going to assume that most words will be
-- implemented using claw code. However, I'll also provide the
-- ABC variant.
-- I'm not sure what should go on this page, so I'm just
-- adding content for now. Later, I'll try to streamline it.
--
-- * view and edit code for individual words
--  * view and edit as Claw code, when feasible
-- *  
--  
getDictWordPage :: WikilonApp
getDictWordPage = dictWordApp $ \ w dn dw _rq k ->
    readPVarIO (wikilon_dicts $ wikilon_model w) >>= \ bset ->
    let b = Branch.lookup' dn bset in
    let d = Branch.head b in 
    let tm = Branch.modified b in
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
            H.strong "Dictionary:" <> " " <> hrefDict dn
            let lDeps = L.nub $ Dict.abcWords abc 
            let lClients = Dict.wordClients d dw 
            let displayClients = case L.drop 10 lClients of
                    [] -> navWords "Clients" dn lClients
                    _ -> navWords "Clients" dn (L.take 10 lClients) <>
                         href (uriDictWordClients dn dw) "(full client list)" <> H.br
            navWords "Depends" dn lDeps
            displayClients
            formDictWordClawDefEdit dn dw tm abc
            H.h3 "AO Definition"
            H.pre $ H.code ! A.class_ "viewAODef" ! A.lang "aodef" $ H.string $ show abc
            let editor = mconcat $ [uriAODictEdit dn, "?words=", wordToUTF8 dw] 
            H.small $ href editor "Edit AO Definition"

            H.hr
            formDictWordRename dn dw
            H.small $ "TODO: compilation, type, health information,\n\
                \access to structured AO definition and structure editing,\n\
                \use cases, tests, visualizations, animations of code,\n\
                \render words (of known types) as application or images...\n\
                \" 

-- | Note: deletion of a word should be identical to putting an empty
-- definition.
delDictWord :: WikilonApp
delDictWord = toBeImplementedLater "HTTP Deletion of individual Word"


-- | Reverse Lookup for a particular word, i.e. find all
-- words that reference this one.
dictWordClients :: WikilonApp
dictWordClients = app where
    app = routeOnMethod [(HTTP.methodGet, onGet)]
    onGet = branchOnOutputMedia $
        [(mediaTypeTextHTML, getPage)
        ,(mediaTypeTextPlain, getTextList)
        ]
    getClients w dn dw =
        readPVarIO (wikilon_dicts $ wikilon_model w) >>= \ bset ->
        let d = Branch.head $ Branch.lookup' dn bset in
        let lClients = Dict.wordClients d dw in
        return lClients
    getPage = dictWordApp $ \ w dn dw _rq k ->
        getClients w dn dw >>= \ lClients ->
        let status = HTTP.ok200 in
        let headers = [textHtml] in
        k $ Wai.responseLBS status headers $ renderHTML $ do
            H.head $ do
                htmlHeaderCommon w
                H.title $ H.unsafeByteString $ wordToUTF8 dw <> " clients" 
            H.body $ do
                H.h1 $ "Clients of " <> hrefDictWord dn dw 
                H.ol $ forM_ lClients $ \ wc ->
                    H.li $ hrefDictWord dn wc
    getTextList = dictWordApp $ \ w dn dw _rq k ->
        getClients w dn dw >>= \ lClients ->
        let status = HTTP.ok200 in
        let headers = [plainText] in
        let wbb = (<> BB.char8 '\n') . BB.byteString . wordToUTF8 in
        let txt = BB.toLazyByteString $ mconcat $ fmap wbb $ lClients in
        k $ Wai.responseLBS status headers txt

