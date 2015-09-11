{-# LANGUAGE OverloadedStrings #-}
-- | Basic support for cookies over WAI request and response headers.
--
-- Wikilon isn't going to bother with sophisticated cookies. I only need
-- a few basics for setting the user or master dictionary. 
module Wikilon.WAI.Cookies
    ( setCookieNVP
    , parseCookies
    ) where

import Data.Monoid
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS
import qualified Network.HTTP.Types as HTTP

-- | Set a cookie with a given name, value, and path. Assumes the name,
-- value, and path are all valid strings. Expiration is set to ~seven
-- years in the future.
setCookieNVP :: ByteString -> ByteString -> ByteString -> HTTP.Header
setCookieNVP n v p = ("Set-Cookie", s) where
    s = mconcat [n, "=", v, "; Path=", p, "; Max-Age=220752000"]

-- | We receive a `Cookie: string` header. This string may contain
-- several cookies separated by semicolons.
parseCookies :: ByteString -> [(ByteString, ByteString)]
parseCookies s =
    if BS.null s then [] else
    case BS.elemIndex ';' s of
        Nothing -> [parseCookie s]
        Just ix -> parseCookie (BS.take ix s) : parseCookies (BS.drop (ix+1) s)

-- | parse a single cookie.
parseCookie :: ByteString -> (ByteString, ByteString)
parseCookie s = (k,v) where
    k = BS.dropWhile (== ' ') spk
    (spk,v) = case BS.elemIndex '=' s of
        Just ix -> (BS.take ix s, BS.drop (ix+1) s)
        Nothing -> (s, mempty)
