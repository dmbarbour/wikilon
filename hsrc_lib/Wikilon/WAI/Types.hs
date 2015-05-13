
module Wikilon.WAI.Types 
    ( Captures
    , WikilonApp
    , Wikilon
    , Middleware
    ) where

import Data.ByteString (ByteString)
import qualified Network.Wai as Wai
import Wikilon.Root

type Captures = [(ByteString, ByteString)]
type WikilonApp = Wikilon -> Captures -> Wai.Application
type Middleware = WikilonApp -> WikilonApp
