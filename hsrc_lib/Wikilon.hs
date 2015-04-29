-- | Wikilon is a persistent web server implemented above Warp and 
-- VCache. Wikilon hosts multiple Awelon project dictionaries and
-- live-programmed abstract virtual machines. Users can create new
-- web services directly within Wikilon.
-- 
module Wikilon
    ( loadInstance
    , Args(..)
    , App(..)
    ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString.UTF8 as UTF8
import qualified Network.Wai as Wai
import Database.VCache
import qualified Wikilon.Root as W
import qualified Wikilon.WAI as W

-- | Currently, Wikilon doesn't take many arguments. 
-- Just a place to persist its data.
data Args = Args
    { store    :: !VCache       -- ^ persistent state goes here if possible
    , home     :: !FilePath     -- ^ otherwise persistent state goes here
    , httpRoot :: !ByteString   -- ^ URI path, e.g. "" or "\/wiki"
    }

-- | A web app and a volatile administrative code. 
data App = App 
    { waiApp    :: !Wai.Application
    , adminCode :: !UTF8.ByteString
    }

-- | Load an existing Wikilon instance, or initialize a new one, 
-- whose identity and persistence is associated with the given 
-- directory. 
loadInstance :: Args -> IO App
loadInstance args = do
    w <- W.loadWikilon (home args) (store args) (httpRoot args)
    return (App (W.waiApp w) (W.adminCode w))



