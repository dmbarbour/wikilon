
-- | Wikilon ultimately provides a WAI web service. 
module Wikilon
    ( mkWaiApp
    , WSOpts, defaultOpts
    , setAdmin, getAdmin
    ) where

import Data.Monoid
import Data.ByteString (ByteString)
import Wikilon.DB (DB)
import Network.Wai (Application)
import qualified Network.Wai as Wai

-- | Extra configuration for the Wikilon service.
--
-- Most configuration will be performed at runtime, by users or agents
-- with administrative authorities. But a few features may need to be
-- handled up front.
data WSOpts = WSOpts
  { ws_admin  :: Maybe ByteString
  } 

defaultOpts :: WSOpts
defaultOpts = WSOpts
  { ws_admin  = mempty
  }

-- | Administration of Wikilon
--
-- The 'admin' account is reserved for administrative bootstrapping
-- and troubleshooting. Enabled by setting a password at startup. This
-- password is usable only for the process lifetime.
setAdmin :: Maybe ByteString -> WSOpts -> WSOpts
setAdmin v ws = ws { ws_admin = v }

getAdmin :: WSOpts -> Maybe ByteString
getAdmin = ws_admin

-- other potential options:
--
-- support for web service composition (very Low priority)
--   access to a higher level rep. of application
--   prefix for keys in the DB
--   extra prefix for URLs
--
-- The following might be better configured at runtime by
-- administrative agents. 
--
-- Integration of external resources:
--   files, and resources in the filesystem
--   distributed computation setup, authorities
--     cooperative Wikilon nodes
--     creation of VMs
--     OpenCL clouds
--   backups
--   logging
--
-- Tab icons, default CSS, etc..


-- | The Wikilon web service
mkWaiApp :: WSOpts -> DB -> IO Application
mkWaiApp opts db = undefined



