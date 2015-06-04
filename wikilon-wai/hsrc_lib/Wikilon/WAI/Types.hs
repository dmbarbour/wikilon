
module Wikilon.WAI.Types 
    ( Captures
    , WikilonApp
    , Wikilon(..)
    , Middleware
    ) where

import Data.ByteString (ByteString)
import qualified Network.Wai as Wai

import Wikilon.Store.Root
import Wikilon.Store.Branch (BranchName)

type Captures = [(ByteString, ByteString)]
type WikilonApp = Wikilon -> Captures -> Wai.Application
type Middleware = WikilonApp -> WikilonApp

-- | a Wikilon WAI instance is constructed from the abstract model
-- plus a little web-app configuration 
data Wikilon = Wikilon
    { wikilon_httpRoot  :: !ByteString   -- ^ raw URI root for web services
    , wikilon_master    :: !BranchName   -- ^ master dictionary for toplevel pages
    , wikilon_model     :: !WikilonStore -- ^ for model state and events 
    }



