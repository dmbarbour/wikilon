
module Wikilon.WAI.Types 
    ( Captures
    , WikilonApp
    , Wikilon
    ) where

import Data.ByteString (ByteString)
import qualified Network.Wai as Wai
import Wikilon.Root

type Captures = [(ByteString, ByteString)]
type WikilonApp = Wikilon -> Captures -> Wai.Application

