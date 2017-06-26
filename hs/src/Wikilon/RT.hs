
-- | Wikilon runtime types.
module Wikilon.RT
    ( RTE(..)
    ) where

import Control.Monad.Trans.Reader
import Wikilon.DB (DB)
import qualified Wikilon.DB as DB

data RTE = RTE
    { rte_db :: DB
    }

-- Adding some form of checkpoint model to our RTE seems potentially
-- useful.


