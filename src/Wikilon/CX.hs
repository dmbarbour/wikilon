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

-- | Access to a key-value registry (usually persistent)
--
-- Storage operates on a simple key-value registry, representing a
-- persistent store such as a filesystem or database. In Wikilon,
-- this is probably Wikilon.DB. 
class Storage m where
    regGet :: ByteString -> m ByteString
    regPut :: ByteString -> ByteString -> m ()


-- NOTE: other potential features to consider are checkpointing or
-- backtracking. But I'd prefer to wait for a real requirement.

