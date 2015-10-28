module Wikilon.Model.Result
    ( Result(..)
    ) where

-- | Possible results from interacting with a Wikilon model.
-- (These generally aren't exposed directly to our users.
data Result a
    = OK a
    -- todo:
    --  not found? redirect?
    --  authorization errors (e.g. a 401 result)
    --  computation timeouts (e.g. a 202 result)
    | Fail String   -- generic failure
    deriving (Show)
