{-# LANGUAGE OverloadedStrings #-}

module Wikilon.WAI.DefaultCSS
    ( resourceDefaultCSS
    --, defaultCSS
    ) where


import Data.Monoid
import qualified Network.HTTP.Types as HTTP
import qualified Network.Wai as Wai
import Wikilon.WAI.Utils
import Wikilon.Store.Root

resourceDefaultCSS :: WikilonApp
resourceDefaultCSS = app where
    app = routeOnMethod [(HTTP.methodGet, onGet)]
    onGet = branchOnOutputMedia [(mediaTypeCSS, onGetCSS)]
    onGetCSS w _cap _rq k = 
        let status = HTTP.ok200 in
        let headers = [textCSS] in
        -- for the moment using a file response for fast editing
        let path = (wikilon_home . wikilon_model) w <> "css" in
        k $ Wai.responseFile status headers path Nothing

-- Note: I might later push the CSS into the VCache or model to avoid
-- direct dependency on the filesystem and simplify editing of CSS. 

