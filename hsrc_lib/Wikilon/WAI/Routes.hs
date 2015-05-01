
-- | Primary routes and web-apps for Wikilon.
module Wikilon.WAI.Routes
    ( wikilonRoutes
    ) where

import Control.Arrow (first)
import Data.ByteString (ByteString)
import qualified Data.ByteString.UTF8 as UTF8
import qualified Data.ByteString.Lazy.UTF8 as LazyUTF8
import qualified Network.HTTP.Types as HTTP
import qualified Network.Wai as Wai
import Wikilon.WAI.Utils


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

    -- Note: maybe have directories for CSS, Images, Scripts
    --  after I decide how they're developed, e.g. via dictionary & cache
    --  could scripts could be cached views of dictionary code? browser AVMs?

    -- Maybe a /robots.txt
    --  nah... Wikilon is not necessarily at root.
    --  I can use <meta name="robots" content="noindex, nofollow"/> as needed.
    ]

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



-- Now I probably need to `routeOnAccept`. Either that, or I need to
-- find some way to convert types appropriately. But routeOnAccept is
-- probably the better option here.

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


