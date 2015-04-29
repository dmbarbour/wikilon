{-# LANGUAGE OverloadedStrings #-}

-- | Web Application Interface bindings for Wikilon. 
--
-- Using Network.Wai.Route to easily cover common cases.
module Wikilon.WAI 
    ( waiApp
    ) where

import Control.Arrow (first)
import Control.Applicative
import qualified Data.List as L
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.UTF8 as UTF8
import qualified Data.ByteString.Lazy.UTF8 as LazyUTF8
import qualified Network.HTTP.Types as HTTP
-- import qualified Network.HTTP.Types.Header as HTTP
import qualified Network.Wai as Wai
import qualified Network.Wai.Route.Tree as Tree
import Wikilon.Root

type Captures = [(ByteString, ByteString)]
type WikilonApp = Wikilon -> Captures -> Wai.Application

-- Primary routes for the Wikilon web service.
wikilonRoutes :: [(ByteString, WikilonApp)]
wikilonRoutes = fmap (first UTF8.fromString) $ 
    [("/",frontPage)
    ,("/d",listOfDicts)
    ,("/d/:d",singleDict)
    ,("/d/:d/w",dictWordsList)
    ,("/d/:d/w/:w",singleDictWord)
    ,("/u",listOfUsers)
    ,("/u/:u",singleUser)
    ]

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
        -- some special cases
        Nothing -> case s of
            ("dev":"echo":_) -> echo rq k
            _ -> k notFound

-- resource not found
notFound :: Wai.Response
notFound = Wai.responseLBS HTTP.status404 [] LBS.empty

-- Route based on method. Also provides default implementations
-- for OPTIONS and HEAD.
routeOnMethod :: [(HTTP.Method, WikilonApp)] -> WikilonApp
routeOnMethod lms w cap rq k = body where
    m = Wai.requestMethod rq
    body = case L.lookup m lms of
        Just app -> app w cap rq k
        Nothing -> 
            if (m == HTTP.methodOptions) then k options else
            if (m == HTTP.methodHead) then tryHEAD else
            k notAllowed
    options = Wai.responseLBS HTTP.status200 [allow] LBS.empty
    notAllowed = Wai.responseLBS HTTP.status405 [allow] LBS.empty
    allow = ("Allow", BS.intercalate ", " (fst <$> lms))
    tryHEAD = case L.lookup HTTP.methodGet lms of
        Nothing -> k notAllowed
        Just app -> app w cap rq $ \ r ->
            let status = Wai.responseStatus r in
            let hdrs = Wai.responseHeaders r in
            k $ Wai.responseLBS status hdrs LBS.empty


frontPage, getFrontPage :: WikilonApp
listOfDicts, getListOfDicts :: WikilonApp
singleDict, getDict :: WikilonApp
dictWordsList, getDictWords, putDictWords :: WikilonApp
singleDictWord, getDictWord, putDictWord :: WikilonApp
listOfUsers, getListOfUsers :: WikilonApp
singleUser, getUser :: WikilonApp

frontPage = routeOnMethod 
    [(HTTP.methodGet, getFrontPage)
    ]
listOfDicts = routeOnMethod
    [(HTTP.methodGet, getListOfDicts)
    --,(HTTP.methodPATCH, patchListOfDicts)
    ]
singleDict = routeOnMethod 
    [(HTTP.methodGet, getDict)
    ]
dictWordsList = routeOnMethod
    [(HTTP.methodGet, getDictWords)
    ,(HTTP.methodPut, putDictWords)
    -- patch?
    ]
singleDictWord = routeOnMethod
    [(HTTP.methodGet, getDictWord)
    ,(HTTP.methodPut, putDictWord)
    -- patch?
    ]
listOfUsers = routeOnMethod
    [(HTTP.methodGet, getListOfUsers)
    -- patch?
    ]
singleUser = routeOnMethod
    [(HTTP.methodGet, getUser)
    ]

getFrontPage = todo "front page"
getListOfDicts = todo "list of dicts"
getDict = todo "dict page"
getDictWords = todo "words in dict"
putDictWords = todo "PUT words in dict"
getDictWord = todo "word in dict"
putDictWord = todo "PUT word in dict"
getListOfUsers = todo "list of users"
getUser = todo "get user"


todo :: String -> WikilonApp
todo msg _ _ _ k = k $ Wai.responseLBS HTTP.status202 [plainText,noCache] $ 
    LazyUTF8.fromString $ "Developer TODO: " ++ msg

-- | Echo request (for development purposes)
echo :: Wai.Application
echo rq k = k response where
    response = Wai.responseLBS HTTP.status200 [plainText,noCache] body
    body = LazyUTF8.fromString $ show rq

plainText :: HTTP.Header
plainText = (HTTP.hContentType,"text/plain")

noCache :: HTTP.Header
noCache = (HTTP.hCacheControl, "no-cache")

