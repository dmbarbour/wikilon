-- | Abstract Wikilon Runtime Context
--
-- Wikilon operates in a similar context as Awelon, but we also need
-- some access to persistent data storage in form of a key-value DB.
-- And it might need some access to secure entropy and other features.
module Wikilon.CX
    ( Storage(..)
    , module Awelon.CX
    ) where

import Data.ByteString.Lazy (ByteString)
import Awelon.CX

-- | Access to a trivial key-value persistent storage.
--
-- These operations may be implicitly transactional, and likely
-- should be in most use cases. Keys might be rewritten a little,
-- e.g. to model subdirectories or to secure hash oversized keys.
class Storage m where
    kvGet :: ByteString -> m ByteString
    kvPut :: ByteString -> ByteString -> m ()

-- TODO:
--
-- ASAP, I need to implement the concrete context, even if the
-- implementation is not optimal it should reach a useful working
-- condition.
--

