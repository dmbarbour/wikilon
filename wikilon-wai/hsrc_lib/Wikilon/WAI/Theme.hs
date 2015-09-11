{-# LANGUAGE OverloadedStrings #-}

-- | I want a few basic theme pages to be tied to dictionaries, and
-- perhaps selected by use of cookies. E.g. one cookie might set the
-- current 'master' dictionary, while our theme is a path or URL that
-- defines a specific dictionary and word, e.g. 
--
--      \/d\/master\/w\/wikilon.theme.dark
--
-- This format follows the URL format for dictionaries and words.
-- 
-- pair that defines where we get our CSS. E.g. `\/d\/master\/w\/wikilon.theme.dark`
module Wikilon.WAI.Theme
    ( resourceCSS
    , selectTheme
    -- , resourceFavicon
    ) where


import Data.Monoid
import Data.Maybe (fromMaybe)
import qualified Data.List as L
-- import qualified Data.ByteString.Lazy as LBS
import qualified Network.HTTP.Types as HTTP
import qualified Network.Wai as Wai
import Wikilon.WAI.Utils
import Wikilon.WAI.Routes (parseWordPath)
import Wikilon.WAI.Cookies (parseCookies)

selectTheme :: Wai.Request -> (BranchName, Word)
selectTheme = fromMaybe def . cookieTheme where
    def = ("master", "wikilon.css") -- default theme source
    cookieTheme rq = 
        L.lookup "Cookie" (Wai.requestHeaders rq) >>= \ sCookies ->
        let lCookies = parseCookies sCookies in
        L.lookup "theme" lCookies >>= \ sTheme ->
        parseWordPath sTheme

-- TODO: 
-- I'll be pushing all resources like this into the dictionary. I'll
-- need to leverage cached computations to make this work effectively.
-- I might also want to leverage cookies or user information in some
-- manner, e.g. to allow per-user CSS.

resourceCSS :: WikilonApp
resourceCSS = app where
    app = routeOnMethod [(HTTP.methodGet, onGet)]
    onGet = branchOnOutputMedia [(mediaTypeCSS, onGetCSS)]
    onGetCSS _ _ _ k = k $ Wai.responseLBS HTTP.status404 [] mempty
{-
    onGetCSS w _cap rq k = join $ wikilon_action w $  
        let (dn,dw) = selectTheme rq in
        loadDict dn >>= \ d ->
        let h = lookupVersionHash dw d in
        let status = HTTP.ok200 in
        let headers = [textCSS, eTagHash h] in
        let cacheKey = "css" <> h in
        -- TODO: compute css
        let css = error "TODO" cacheKey in 
        return $ k $ Wai.responseLBS status headers $ LBS.fromStrict css
        -- select dict/word by cookie
-}      
 
-- Note: I might later push the CSS into the VCache or model to avoid
-- direct dependency on the filesystem and simplify editing of CSS. 
-- Further, CSS should probably be provided by dictionaries (i.e.
-- as a simple text output, with cached computation.). 

