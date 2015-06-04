{-# LANGUAGE OverloadedStrings #-}

module Wikilon.WAI.Pages.DictWord
    ( dictWord
    , dictWordAODef
    , dictWordRename
    , formDictWordRename
    ) where


import Control.Monad
import Data.Monoid
import Data.Maybe
import qualified Data.ByteString.Lazy as LBS
import qualified Network.HTTP.Types as HTTP
import qualified Network.HTTP.Media as HTTP
import Text.Blaze.Html5 ((!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import qualified Network.Wai as Wai
import Database.VCache

import Wikilon.WAI.Utils
import Wikilon.WAI.Routes
import Wikilon.Dict.Word
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

putDictWordAODef :: WikilonApp
putDictWordAODef = toBeImplementedLater "HTTP Put of Word definition"

delDictWord :: WikilonApp
delDictWord = toBeImplementedLater "HTTP Deletion of individual Word"

-- | return just the definition
getDictWordAODef :: WikilonApp
getDictWordAODef = dictWordApp $ \ w dn dw _rq k ->
    readPVarIO (wikilon_dicts $ wikilon_model w) >>= \ bset ->
    let hMedia = (HTTP.hContentType, HTTP.renderHeader mediaTypeAODef) in
    let b = Branch.lookup' dn bset in
    let d = Branch.head b in 
    let mbDef = Dict.lookupBytes d dw in
    case mbDef of
        Nothing -> k $ Wai.responseLBS HTTP.notFound404 [hMedia] LBS.empty
        Just abc -> k $ Wai.responseLBS HTTP.ok200 [hMedia] abc

getDictWordPage :: WikilonApp
getDictWordPage = dictWordApp $ \ w dn dw _rq k ->
    readPVarIO (wikilon_dicts $ wikilon_model w) >>= \ bset ->
    let b = Branch.lookup' dn bset in
    let d = Branch.head b in 
    let mbDef = Dict.lookup d dw in
    let status = if isNothing mbDef then HTTP.notFound404 else HTTP.ok200 in
    let headers = [textHtml] in
    let title = H.unsafeByteString (wordToUTF8 dw) in
    k $ Wai.responseLBS status headers $ renderHTML $ do
        H.head $ do
            htmlHeaderCommon w
            H.title title
        H.body $ do
            H.h1 title
            case mbDef of
                Nothing -> H.p "The requested word is not defined."
                Just abc -> do
                    H.p "Definition: "
                    (H.pre ! A.class_ "aodef") $ (H.code ! A.lang "abc") $ 
                        H.string (show abc)
            let lDeps = maybe [] Dict.abcWords mbDef 
            let lClients = Dict.usedBy d dw 
            navWords "Dependencies" dn lDeps
            navWords "Dependents" dn lClients
            -- navDocWords dictName dictWord
            H.hr
            H.p "TODO: access to compiled word and type information (or errors),\n\
                \expanded definition suitable for rendering and structured editing,\n\
                \samples where word is used with known values in other definitions,\n\
                \animations of word's construction and word in use, compile words of\n\
                \appropriate types to javascript or SVG or raster images or simple\n\
                \web applications, etc."
            H.p "TODO: delete word, rename word"

navWords :: String -> BranchName -> [Word] -> HTML
navWords _ _ [] = mempty
navWords sClass dn lWords =
    H.nav ! A.class_ (H.stringValue sClass) $ do
        H.strong (H.string sClass) 
        forM_ lWords $ \ w -> " " <> hrefDictWord dn w


dictWordRename :: WikilonApp
dictWordRename = toBeImplementedLater "rename word"

formDictWordRename :: HTML
formDictWordRename = error "todo"

