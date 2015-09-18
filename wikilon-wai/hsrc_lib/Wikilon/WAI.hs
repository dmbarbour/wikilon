{-# LANGUAGE OverloadedStrings #-}

-- | Web Application Interface bindings for Wikilon. 
--
-- Using Network.Wai.Route to easily cover common cases.
module Wikilon.WAI 
    ( wikilonWaiApp
    , wikilonRoutes
    , module Wikilon.WAI.Types
    ) where

import Control.Arrow (first)
import Control.Exception (catch, SomeException)
import qualified Data.ByteString as BS
import qualified Data.ByteString.UTF8 as UTF8
import qualified Network.Wai as Wai
import qualified Network.Wai.Route.Tree as Tree
import Wikilon.WAI.Types
import Wikilon.WAI.Routes
import Wikilon.WAI.Pages
import Wikilon.WAI.Theme
import Wikilon.WAI.Utils

-- | List of routes for the Wikilon web service.
wikilonRoutes :: [(Route, WikilonApp)]
wikilonRoutes = fmap (first UTF8.fromString) $ 
    [("/", wikilonRoot)
    ,("/d", allDictionaries)
    ,("/d/:d", dictResource)
    ,("/d/:d/aodict", dictAsAODict)
    ,("/d/:d/aodict.edit", appAODictEdit)

    ,("/d/:d/repl", dictRepl)

    ,("/d/:d/w", dictWords)
    ,("/d/:d/w/:w", dictWord)
    ,("/d/:d/w/:w/rename", dictWordRename)
    ,("/d/:d/w/:w/delete", dictWordDelete)
    ,("/d/:d/w/:w/clients", dictWordClients)
--    ,("/d/:d/w/:w/abc", dictWordCompile)
    ,("/d/:d/w/:w/aodef", dictWordAODef)
    ,("/d/:d/w/:w/aodef.edit", dictWordAODefEdit)
    ,("/d/:d/w/:w/clawdef", dictWordClawDef)
    ,("/d/:d/w/:w/clawdef.edit", dictWordClawDefEdit)
    --,("/d/:d/w/:w/abc", dictWordABC)

    -- ("/d/:d/fork", dictFork)

    ,("/d.create", dictCreate)
    --,("/d/:d/hist", dictHist)

    -- thoughts:
    --  I could use /d/:d/name and /d/:d/w/:w/name 
    --  as handles for renaming
    
--    ,("/u",listOfUsers)
--    ,("/u/:u",singleUser)


    -- administrative
    --,("/admin/dbHealth", dbHealth)

    -- built-in documentation
    ,("/about/aodict", aodictDocs)
    ,("/about/claw", clawDocs)

    -- special endpoints to force media types
    ,("/d.list", dictList)
    ,("/d/:d/w.list", dictWordsList)

    -- generic endpoints
--    ,("/favicon", resourceFavicon)
    ,("/css", resourceCSS)
    ]

-- | The primary wikilon web service. Any exceptions will be logged
-- for administrators and return a 500 response.
wikilonWaiApp :: Wikilon -> Wai.Application
wikilonWaiApp w rq k = catch (baseWikilonApp w rq k) $ \ e -> do 
    wikilon_error w (showRequestException rq e)
    k $ eServerError ("unhandled exception in server: " ++ show e)

showRequestException :: Wai.Request -> SomeException -> String
showRequestException rq e = 
    "Exception: " ++ show e ++ "\n  WAI Request: " ++ show rq

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
        Nothing -> k $ eNotFound rq

-- Thoughts:
--  rather than remastering a dictionary via URL
--  consider doing so via cookie or query parameter
