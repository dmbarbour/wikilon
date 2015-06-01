{-# LANGUAGE OverloadedStrings #-}

-- | Web Application Interface bindings for Wikilon. 
--
-- Using Network.Wai.Route to easily cover common cases.
module Wikilon.WAI 
    ( wikilonWaiApp
    , wikilonRoutes
    ) where

import Control.Arrow (first)
import Control.Exception (catch)
import qualified Data.ByteString as BS
import qualified Data.ByteString.UTF8 as UTF8
import qualified Data.ByteString.Lazy.UTF8 as LazyUTF8
import qualified Network.HTTP.Types as HTTP
import qualified Network.Wai as Wai
import qualified Network.Wai.Route.Tree as Tree
import Wikilon.WAI.Types
import Wikilon.WAI.Routes
import Wikilon.WAI.Pages
import Wikilon.WAI.Utils (plainText, noCache, eServerError, eNotFound)
import Wikilon.Branch (BranchName)
import Wikilon.Root

-- | List of routes for the Wikilon web service.
wikilonRoutes :: [(Route, WikilonApp)]
wikilonRoutes = fmap (first UTF8.fromString) $ 
    [("/", wikilonRoot)
    ,("/d", allDictionaries)
    ,("/d/:d", dictResource)
    ,("/d/:d/aodict", dictAsAODict)
    ,("/d/:d/aodict.edit", appAODictEdit)

    ,("/d/:d/w", dictWords)
    ,("/d/:d/w/:w", dictWord)
    ,("/d/:d/w/:w/rename", dictWordRename)

    ,("/d.create", dictCreate)
    --,("/d.merge", dictMerge)

    --,("/d/:d/hist", dictHist)

    --,("/d/:d/w(create)", dictWordPostCreate)
    --,("/d/:d/w(delete)", dictWordPostDelete)

    --,("/d/:d/w/:w", dictWord)
    --,("/d/:d/w/:w/name", 
    --,("/d/:d/w/:w/deps", dictWordDeps)
    --,("/d/:d/w/:w/clients", dictWordClients)
    --,("/d/:d/wlist", dictWordList)
    --,("/d/:d/wdeps", dictWordListDeps)
    --,("/d/:d/wclients", dictWordListClients)
    -- todo: historical versions
    
--    ,("/u",listOfUsers)
--    ,("/u/:u",singleUser)


    -- administrative
    ,("/admin/dbHealth", dbHealth)

    -- built-in documentation
    ,("/about/aodict", aodictDocs)
    ,("/about/claw", clawDocs)

    -- special endpoints to force media types
    ,("/d.list", dictList)
    ,("/d/:d/w.list", dictWordsList)

    -- generic endpoints
--    ,("/favicon", resourceFavicon)
    ]

-- | The primary wikilon web service. Any exceptions will be logged
-- for administrators and return a 500 response.
wikilonWaiApp :: Wikilon -> Wai.Application
wikilonWaiApp w rq k = catch (baseWikilonApp w rq k) $ \ e -> do 
    logSomeException w e
    k $ eServerError "unhandled exception (logged for admin)"

baseWikilonApp :: Wikilon -> Wai.Application
baseWikilonApp w rq k =
    -- require: request is received under Wikilon's httpRoot
    let nRootLen = BS.length (wikilon_httpRoot w) in
    let (uriRoot,uriPath) = BS.splitAt nRootLen (Wai.rawPathInfo rq) in
    let badPath = k $ eServerError "inconsistent path" in
    if uriRoot /= wikilon_httpRoot w then badPath else
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
            ("d":dictPath:"wiki":_) -> remaster dictPath w rq k
            _ -> k $ eNotFound rq 

-- | allow 'views' of Wikilon using a different master dictionary.
--
-- \/d\/foo\/wiki\/  -  view wikilon via the 'foo' dictionary
--
-- While a single dictionary is the default master for Wikilon, it
-- is always possible to treat other dictionaries as the new master.
-- This does add some access costs, but those should be marginal.
--
remaster :: BranchName -> Wikilon -> Wai.Application
remaster dictPath w rq k =
    -- "d/fooPath/wiki/" adds 8 + length "fooPath" to old prefix. 
    -- We must urlDecode "fooPath" to recover the dictionary name.
    let prefixLen = 8 + BS.length dictPath + BS.length (wikilon_httpRoot w) in
    let _httpRoot = BS.take prefixLen (Wai.rawPathInfo rq) in
    let _master = HTTP.urlDecode False dictPath in
    let w' = w { wikilon_master = _master, wikilon_httpRoot = _httpRoot } in
    baseWikilonApp w' rq k

-- | Echo request (for development purposes)
echo :: Wai.Application
echo rq k = k response where
    response = Wai.responseLBS HTTP.ok200 [plainText,noCache] body
    body = LazyUTF8.fromString $ show rq

