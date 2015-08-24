{-# LANGUAGE OverloadedStrings #-}

module Wikilon.WAI.DefaultCSS
    ( resourceDefaultCSS
    --, defaultCSS
    ) where


import Data.Monoid
import qualified Network.HTTP.Types as HTTP
import qualified Network.Wai as Wai
import Wikilon.WAI.Utils

-- TODO: 
-- I'll be pushing all resources like this into the dictionary. I'll
-- need to leverage cached computations to make this work effectively.
-- I might also want to leverage cookies or user information in some
-- manner, e.g. to allow per-user CSS.

resourceDefaultCSS :: WikilonApp
resourceDefaultCSS = app where
    app = routeOnMethod [(HTTP.methodGet, onGet)]
    onGet = branchOnOutputMedia [(mediaTypeCSS, onGetCSS)]
    onGetCSS w _cap _rq k = 
        let status = HTTP.ok200 in
        let headers = [textCSS] in
        -- for the moment using a file response for fast editing
        let path = wikilon_home w <> "css" in
        k $ Wai.responseFile status headers path Nothing

-- Note: I might later push the CSS into the VCache or model to avoid
-- direct dependency on the filesystem and simplify editing of CSS. 
-- Further, CSS should probably be provided by dictionaries (i.e.
-- as a simple text output, with cached computation.). 

