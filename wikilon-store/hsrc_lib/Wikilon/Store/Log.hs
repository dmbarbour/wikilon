

-- | Wikilon mostly keeps dictionaries. But logs are useful for
-- tracking recent events. The main difficulty with logs is that
-- it isn't obvious what can be removed from them. For now, I'll
-- leave it to humans. Though, it might be interesting to train
-- a neural network from log data when discarding old info.
--
module Wikilon.Store.Log
    (
    ) where

