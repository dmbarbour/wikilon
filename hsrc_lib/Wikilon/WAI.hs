{-# LANGUAGE OverloadedStrings #-}

-- | Web Application Interface bindings for Wikilon. 
--
-- Using Network.Wai.Route to easily cover common cases.
module Wikilon.WAI 
    ( waiApp
    ) where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Lazy.UTF8 as LazyUTF8
import qualified Network.HTTP.Types as HTTP
import qualified Network.Wai as Wai
import qualified Network.Wai.Route.Tree as Tree
import Wikilon.WAI.Routes
import Wikilon.WAI.Utils (plainText, noCache)
import Wikilon.Root

waiApp :: Wikilon -> Wai.Application
waiApp w rq k = 
    -- require: request is received under Wikilon's httpRoot
    let nRootLen = BS.length (wikilon_httpRoot w) in
    let (uriRoot,uriPath) = BS.splitAt nRootLen (Wai.rawPathInfo rq) in
    if uriRoot /= wikilon_httpRoot w then k notFound else
    -- otherwise handle the request normally
    let t = Tree.fromList wikilonRoutes in
    let s = Tree.segments uriPath in
    case Tree.lookup t s of
        -- most apps will use Network.Wai.Route
        Just route -> app w cap rq k where
            app = Tree.value route
            cap = Tree.captured $ Tree.captures route
        -- ad-hoc special cases
        Nothing -> case s of
            ("dev":"echo":_) -> echo rq k
            _ -> k notFound

-- resource not found
notFound :: Wai.Response
notFound = Wai.responseLBS HTTP.notFound404 [] LBS.empty

-- | Echo request (for development purposes)
echo :: Wai.Application
echo rq k = k response where
    response = Wai.responseLBS HTTP.ok200 [plainText,noCache] body
    body = LazyUTF8.fromString $ show rq

