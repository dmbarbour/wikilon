{-# LANGUAGE OverloadedStrings #-}

module Wikilon.WAI.Pages.DictWord
    ( dictWord
    , module Wikilon.WAI.Pages.DictWord.Rename
    , module Wikilon.WAI.Pages.DictWord.ClawDef
    , module Wikilon.WAI.Pages.DictWord.AODef
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
            -- user experience here?
            --  either provide BOTH editors, cleanly separated
            --  or primarily edit as claw code.
            H.textarea ! A.class_ "aodef" ! A.readonly "readonly" 
                ! A.lang "abc" ! A.placeholder "(undefined)"
                ! A.rows "10" ! A.cols "70" $ H.string (show abc)
            let lDeps = L.nub $ Dict.abcWords abc 
            let lClients = Dict.wordClients d dw 
            -- navDocWords dictName dictWord
            H.br
            H.br
            H.hr
            H.strong "Dictionary:" <> " " <> hrefDict dn
            navWords "Depends" dn lDeps
            navWords "Clients" dn lClients
            formDictWordRename dn dw
            H.br
            H.small $ H.strong "TODO:" <> " compilation, type, health information,\n\
                \access to structured AO definition and structure editing,\n\
                \use cases, tests, visualizations, animations of code,\n\
                \render words (of known types) as application or images...\n\
                \" 

-- | Note: deletion of a word should be identical to putting an empty
-- definition.
delDictWord :: WikilonApp
delDictWord = toBeImplementedLater "HTTP Deletion of individual Word"


