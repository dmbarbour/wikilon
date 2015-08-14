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
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy.UTF8 as LazyUTF8
import qualified Data.ByteString.Builder as BB
import qualified Network.HTTP.Types as HTTP
import Text.Blaze.Html5 ((!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

import qualified Network.Wai as Wai
import Database.VCache

import qualified Awelon.ClawCode as Claw
import qualified Awelon.Base16 as B16
import Awelon.ABC (ABC)
import qualified Awelon.ABC as ABC


import Wikilon.WAI.Utils
import Wikilon.WAI.Routes

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

-- Ideally, a lot should go into presenting a word to a user.
--
-- (a) present a command language view if appropriate.
-- (b) include health issues and errors in presentation.
-- (c) last resort is just the raw AO-layer definition.
-- (d) captions, temporal metadata, etc. if available.
-- (e) integration with 
--
-- For the moment, I'm making due with much less. However, in the
-- long run, I'll probably want a consistent default view for words
-- in many contexts (including REPL-based forums), even if I have
-- multiple views in specialized contexts.
viewClawOrAODef :: ABC -> HTML
viewClawOrAODef abc =
    let bUndefined = L.null $ ABC.abcOps abc in
    if bUndefined then H.p "This word is undefined." else
    case parseClawDef abc of
        Nothing -> do
            H.pre ! A.lang "aodef" $ H.code $ H.string $ show abc
            H.small $ H.strong "viewing as: " <>  href uriAODictDocs "aodef" <> H.br
        Just cc -> do
            let sClaw = LazyUTF8.toString $ Claw.encode cc
            H.pre ! A.lang "claw" $ H.code $ H.string sClaw
            H.small $ H.strong "viewing as: " <> href uriClawDocs "claw" <> H.br

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
    let abc = Dict.lookup d dw in
    let h = Dict.lookupVersionHash d dw in
    let hs = (BS.pack . (35:) . B16.encode . BS.unpack) h in 
    let status = HTTP.ok200 in
    let headers = [textHtml] in
    let title = H.string "Word: " <> H.unsafeByteString (Dict.wordToUTF8 dw) in
    k $ Wai.responseLBS status headers $ renderHTML $ do
        H.head $ do
            htmlHeaderCommon w
            H.title title
        H.body $ do
            H.h1 title
            viewClawOrAODef abc

            H.hr
            let ver = H.span ! A.class_ "versionHash" $ H.unsafeByteString hs
            H.strong "Dictionary:" <> " " <> hrefDict dn
            let lDeps = L.nub $ Dict.abcWords abc 
            let lClients = Dict.wordClients d dw
            let maxClientsHere = 12 
            let displayClients = case L.drop maxClientsHere lClients of
                    [] -> navWords "Clients" dn lClients
                    _ -> navWords "Clients" dn (L.take maxClientsHere lClients) <>
                         href (uriDictWordClients dn dw) "(full client list)" <> H.br
            navWords "Depends" dn lDeps
            displayClients

            -- links to editors (no longer editing inline)
            H.nav $ do
                H.strong "Edit:"
                let editName = ("name", uriDictWordRename dn dw)
                let editAO = ("aodef", uriAODictEditWords dn [dw])
                let editClaw = ("claw", uriClawDefEdit dn dw)
                let lEditors = [editName, editClaw, editAO] 
                forM_ lEditors $ \ (s,uri) -> " " <> href uri s

            H.strong "SecureHash:" <> " " <> ver <> H.br

            H.hr ! A.class_ "todo"
            H.div ! A.class_ "todo" $ "TODO: compilation, type, health information,\n\
                \access to structured AO definition and structure editing,\n\
                \dictonary app views based on type or naming conventions,\n\
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
                H.title $ H.unsafeByteString $ Dict.wordToUTF8 dw <> " clients" 
            H.body $ do
                H.h1 $ "Clients of " <> hrefDictWord dn dw 
                H.ol $ forM_ lClients $ \ wc ->
                    H.li $ hrefDictWord dn wc
    getTextList = dictWordApp $ \ w dn dw _rq k ->
        getClients w dn dw >>= \ lClients ->
        let status = HTTP.ok200 in
        let headers = [plainText] in
        let wbb = (<> BB.char8 '\n') . BB.byteString . Dict.wordToUTF8 in
        let txt = BB.toLazyByteString $ mconcat $ fmap wbb $ lClients in
        k $ Wai.responseLBS status headers txt

