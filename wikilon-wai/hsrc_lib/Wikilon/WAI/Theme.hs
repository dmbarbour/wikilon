{-# LANGUAGE OverloadedStrings, ViewPatterns #-}

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

import Control.Monad
import Data.Monoid
import Data.Maybe (fromMaybe)
import qualified Data.List as L
-- import qualified Data.ByteString.Lazy as LBS
import qualified Network.HTTP.Types as HTTP
import qualified Network.Wai as Wai
import qualified Data.ByteString as BS
import Wikilon.Compile
import qualified Awelon.ABC as ABC
import Awelon.ABC.Eval
import Wikilon.WAI.Utils
import Wikilon.WAI.Routes (parseWordPath)
import Wikilon.WAI.Cookies (parseCookies)

-- | looking for Cookie: theme=\/d\/master\/w\/wikilon.css (or alternative URL)
selectTheme :: Wai.Request -> (BranchName, Word)
selectTheme = fromMaybe def . cookieTheme where
    def = ("master", "wikilon.css") -- default theme source
    cookieTheme rq = 
        L.lookup HTTP.hCookie (Wai.requestHeaders rq) >>= \ sCookies ->
        let lCookies = parseCookies sCookies in
        L.lookup "theme" lCookies >>= \ sTheme ->
        parseWordPath sTheme



-- TODO: 
-- I'll be pushing all resources like this into the dictionary. I'll
-- need to leverage cached computations to make this work effectively.
-- I might also want to leverage cookies or user information in some
-- manner, e.g. to allow per-user CSS.
--
-- The current implementation uses a very naive evaluator for the CSS
-- string. But I can transparently replace it later.

resourceCSS :: WikilonApp
resourceCSS = app where
    app = routeOnMethod [(HTTP.methodGet, onGet)]
    onGet = branchOnOutputMedia [(mediaTypeCSS, onGetCSS)]
    onGetCSS w _ rq k = join $ wikilon_action w $
        let (dn,dw) = selectTheme rq in
        loadDict dn >>= \ d ->
        let cacheKey = lookupVersionHash d dw <> cssSuffix in
        let status = HTTP.ok200 in
        let headers = [textCSS, eTagHash cacheKey] in
        cacheBytes cacheKey (computeCSS d dw) >>= \ css ->
        return $ k $ Wai.responseLBS status headers css

-- Version suffix for CSS computation. Update this if the 
-- CSS computation type or result changes. 
cssSuffix :: BS.ByteString
cssSuffix = ".css.v0"

computeCSS :: Dict m -> Word -> W m Bytes
computeCSS d w = 
    let cmd = ABC.mkABC [ABC.ABC_Tok ("%" <> unWord w)] in
    let q = 10000000 in -- decently large quota for the ref. compile.
    -- todo: cached, incremental compilation... 
    -- currently using a stupidly naive compile
    let abc = ABC.abcOps $ referenceCompile d q cmd in
    case basicEval Unit (Cont abc Awelon.ABC.Eval.Return) q of
        Right (Pair (toText -> Just css) _) -> return css
        _ -> return "// failed to compute CSS"

