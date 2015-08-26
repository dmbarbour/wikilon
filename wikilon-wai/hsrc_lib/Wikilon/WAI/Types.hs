{-# LANGUAGE Rank2Types #-}
module Wikilon.WAI.Types 
    ( Captures
    , WikilonApp
    , Wikilon(..)
    , Middleware
    , module Wikilon.Model
    ) where

import Data.ByteString (ByteString)
import qualified Network.Wai as Wai
import Wikilon.Model


-- This is kind of ugly at the moment. I really need something
-- more like a generic widgets and routing model with AJAX support
-- built in automatically, cf. Snap and MFlow.

type Captures = [(ByteString, ByteString)]
type WikilonApp = Wikilon -> Captures -> Wai.Application
type Middleware = WikilonApp -> WikilonApp

-- | a Wikilon WAI instance is constructed from the abstract model
-- plus a tiny amount of web-app configuration. Ultimately, all state
-- will be part of the model, usually via dictionaries.
data Wikilon = Wikilon
    { wikilon_httpRoot  :: !ByteString   -- ^ URI root for web services
    , wikilon_master    :: !BranchName   -- ^ master dictionary for resources
    , wikilon_action    :: !ModelRunner  -- ^ abstract queries and updates
    , wikilon_home      :: !FilePath     -- ^ until we push css into dictionary
    , wikilon_error     :: !(String -> IO ()) -- ^ simplistic error reporting
    }

