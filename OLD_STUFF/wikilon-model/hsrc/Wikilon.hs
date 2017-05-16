-- | Root (or prelude) for Wikilon library.
module Wikilon
    ( loadWikilon
    , module Wikilon.Model
    ) where

import Wikilon.Model
import Database.VCache

-- | load a Wikilon model, returning a command runner. Persistent 
-- state is preserved in the given VCache. The load action may
-- create some background tasks and other volatile resources.
loadWikilon :: VCache -> IO Runner
loadWikilon = undefined

{- data W = W

run :: VCache -> Action w a -> IO (Result a)
run = error "todo"
-}
