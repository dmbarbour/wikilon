
module Wikilon.Model
    ( Wikilon(..)
    ) where

import Database.VCache
import Wikilon.Word
import Wikilon.Dict
import Wikilon.AODict

data Wikilon = Wikilon
    { wikilon_vcache :: VCache
    } 



