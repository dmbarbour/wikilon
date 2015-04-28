{-# LANGUAGE OverloadedStrings #-}

-- | Web Application Interface bindings for Wikilon.
--
-- At the moment I'm using Network.Wai.Route, which is simple, flat,
-- and reasonably efficient.
module Wikilon.WAI 
    ( waiApp
    ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString.UTF8 as UTF8
import qualified Data.ByteString.Lazy.UTF8 as LazyUTF8
import qualified Network.HTTP.Types as HTTP
-- import qualified Network.HTTP.Types.Header as HTTP
import qualified Network.Wai as Wai
import qualified Network.Wai.Route as WaiRoute
import Wikilon.Root

waiApp :: Wikilon -> Wai.Application
waiApp w = WaiRoute.route waiRoutes where
    waiRoutes = fmap toWaiRoute wikilonRoutes
    toWaiRoute (s,app) = (UTF8.fromString s, app w)

wikilonRoutes :: [(String, WikilonApp)]
wikilonRoutes =
    [("/",frontPage)
    ,("/d",listDicts)
    ,("/d/:d",oneDict)
    ,("/d/:d/w",dictWords)
    ,("/d/:d/w/:w",dictWord)
    ,("/u",listUsers)
    ,("/u/:u",oneUser)
    ,("/about",about)
    ,("/echo",echo)
    ,("/echo/:p1of1",echo)
    ,("/echo/:p1of2/:p2of2",echo)
    ,("/echo/:p1of3/:p2of3/:p3of3",echo)
    ]

type WikilonApp = Wikilon -> Captures -> Wai.Application
type Captures = [(ByteString, ByteString)]

plainText :: HTTP.Header
plainText = (HTTP.hContentType,"text/plain")

todoApp :: String -> WikilonApp
todoApp s _w _cap _req reply = reply $ 
    Wai.responseLBS HTTP.status404 [plainText] $
    LazyUTF8.fromString $ "TODO! " ++ s

frontPage = todoApp "front page"
listDicts = todoApp "list of dictionaries"
oneDict = todoApp "singular dictionary"
dictWords = todoApp "list words in dictionary"
dictWord = todoApp "singular word in dictionary"
listUsers = todoApp "list of users"
oneUser = todoApp "user page"
about = todoApp "about page"

-- | echo is just a convenient testing path
echo :: WikilonApp
echo _w cap req reply = reply $ 
    Wai.responseLBS HTTP.status200 [plainText] $ 
    LazyUTF8.fromString $ show req ++ "\n\n" ++ show cap
