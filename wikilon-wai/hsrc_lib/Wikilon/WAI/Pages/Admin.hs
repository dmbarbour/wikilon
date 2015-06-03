
module Wikilon.WAI.Pages.Admin
    ( dbHealth
    ) where

import qualified Data.ByteString.Lazy.UTF8 as LazyUTF8
import qualified Network.HTTP.Types as HTTP
import qualified Network.Wai as Wai
import Database.VCache

import Wikilon.WAI.Utils
import Wikilon.Store.Root


dbHealth :: WikilonApp
dbHealth = justGET $ branchOnOutputMedia $
    [(mediaTypeTextPlain, dbHealthText)]

dbHealthText :: WikilonApp
dbHealthText w _cap _rq k =
    let vc = wikilon_store w in 
    vcacheStats (vcache_space vc) >>= \ vcstat -> 
    k $ Wai.responseLBS HTTP.ok200 [plainText] $ 
        LazyUTF8.fromString $ show vcstat





