{-# LANGUAGE OverloadedStrings #-}
-- | Web Application Interface bindings for Wikilon.
module Wikilon.WAI 
    ( waiApp
    ) where

import qualified Data.ByteString.Lazy.UTF8 as LazyUTF8
import qualified Network.Wai as Wai
import qualified Network.HTTP.Types as HTTP
import Wikilon.Root

waiApp :: Wikilon -> Wai.Application
waiApp = const helloApp

helloApp :: Wai.Application
helloApp req reply = reply response where
    response = Wai.responseLBS status plainText body
    body = txt2bin $ show (Wai.requestHeaders req)
    status = HTTP.status200
    plainText = [(HTTP.hContentType,"text/plain")]
    txt2bin = LazyUTF8.fromString


