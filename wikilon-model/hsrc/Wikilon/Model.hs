
module Wikilon.Model
    ( Wikilon(..)
    , module Wikilon.Word
    , module Wikilon.Token
    ) where

import Database.VCache
import Wikilon.Word
import Wikilon.Token
import Wikilon.Dict
import Wikilon.AODict


--

-- 

data Wikilon = Wikilon
    { wikilon_vcache :: VCache
    } 



