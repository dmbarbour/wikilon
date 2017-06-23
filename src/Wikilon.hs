
-- | Wikilon ultimately provides a WAI web service.
module Wikilon
    ( mkWikilonApp
    , WSOpts
    , setAdminPass, getAdminPass
    , setKeyPrefix, getKeyPrefix
    ) where

import qualified Data.ByteString as BS 
import Wikilon.DB (DB)
import qualified Network.Wai as Wai

data WSOpts = WSOpts
  { ws_admin  :: Maybe BS.ByteString
  , ws_prefix :: BS.ByteString
  , ws_db     :: DB
  } 

initOpts :: DB -> WSOpts
initOpts db = WSOpts
  { ws_admin  = Nothing
  , ws_prefix = BS.empty
  , ws_db     = db
  }

-- | Administration of Wikilon
--
-- The 'admin' user is a special case in Wikilon, available only if
-- a password is set upon initialization, and having full authority
-- to configure persistent administrative users. If the password here
-- is Nothing, then the user 'admin' cannot log in.
setAdminPass :: Maybe BS.ByteString -> WSOpts -> WSOpts
setAdminPass v ws = ws { ws_admin = v }

getAdminPass :: WSOpts -> Maybe BS.ByteString
getAdminPass = ws_admin

-- | Setting a key prefix enables a DB to potentially be shared with
-- other tasks, or even other Wikilon instances.
setKeyPrefix :: BS.ByteString -> WSOpts -> WSOpts
setKeyPrefix v ws = ws { ws_prefix = v }

getKeyPrefix :: WSOpts -> BS.ByteString
getKeyPrefix = ws_prefix


-- | The Wikilon web service
mkWikilonApp :: WSOpts -> IO Wai.Application
mkWikilonApp opts = undefined



