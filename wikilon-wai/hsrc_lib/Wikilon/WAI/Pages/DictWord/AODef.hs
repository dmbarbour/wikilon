{-# LANGUAGE OverloadedStrings #-}

module Wikilon.WAI.Pages.DictWord.AODef
    ( dictWordAODef
    , getDictWordAODef
    , putDictWordAODef
    , dictWordAODefEdit
    , formDictWordAODefEdit
    ) where

import Control.Applicative
import Control.Monad
import Data.Monoid
import qualified Data.List as L
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Lazy.UTF8 as LazyUTF8
import qualified Network.HTTP.Types as HTTP
import qualified Network.HTTP.Media as HTTP
import Text.Blaze.Html5 ((!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import qualified Network.Wai as Wai
import Database.VCache

import Awelon.ABC

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


-- | an endpoint that forces content to the 'aodef' media type.
dictWordAODef :: WikilonApp
dictWordAODef = app where
    app = routeOnMethod $
        [(HTTP.methodGet, onGet)
        ,(HTTP.methodPut, onPut)
        ]
    onGet = branchOnOutputMedia [(mediaTypeAODef, getDictWordAODef)]
    onPut = branchOnInputMedia [(mediaTypeAODef, putDictWordAODef)]

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


putDictWordAODef :: WikilonApp
putDictWordAODef = toBeImplementedLater "PUT word via AODef definition"

-- | a page with just the edit form, and also the recipient of POST
-- actions to update a word via the AO definition.
dictWordAODefEdit :: WikilonApp
dictWordAODefEdit = toBeImplementedLater "AODef edit page"

formDictWordAODefEdit :: BranchName -> Word -> Maybe T -> ABC -> HTML
formDictWordAODefEdit d w t abc = formAODefEdit' d w t (encode abc)

formAODefEdit' :: BranchName -> Word -> Maybe T -> LazyUTF8.ByteString -> HTML
formAODefEdit' d w t sInit =
    let uriAction = H.unsafeByteStringValue $ uriAODefEdit d w in
    H.form ! A.method "POST" ! A.action uriAction ! A.id "formAODefEdit" $ do
        H.textarea ! A.name "command" ! A.rows "4" ! A.cols "60" $
            H.string $ LazyUTF8.toString sInit -- escapes string for HTML
        H.br
        let tmVal = H.stringValue $ maybe "--" show t
        H.strong "Edit Origin: "
        H.input ! A.type_ "text" ! A.name "editOrigin" ! A.value tmVal
        H.string " "
        H.input ! A.type_ "submit" ! A.value "Edit AO Definition"


