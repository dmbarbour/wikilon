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
-- built in automatically, cf. Snap and MFlow. I'd also like to
-- capture a lot of the extra structure into a monad, e.g. to 
-- hide the query and return a response.

type Captures = [(ByteString, ByteString)]
type WikilonApp = Wikilon -> Captures -> Wai.Application
type Middleware = WikilonApp -> WikilonApp

-- | a Wikilon WAI instance is constructed from the abstract model
-- plus a tiny amount of web-app configuration. Ultimately, all state
-- will be part of the model, usually via dictionaries.
data Wikilon = Wikilon
    { wikilon_httpRoot  :: !ByteString        -- ^ URI root for web services
    , wikilon_action    :: !ModelRunner       -- ^ abstract queries and updates
    , wikilon_error     :: !(String -> IO ()) -- ^ simplistic error reporting
    }

