

-- | This is the value model that Wikilon will be using internally.
module Wikilon.Value
    (
    ) where




-- | Values that can be represented in ABC, with a simple
-- extension for larger than memory values. Later, I might
-- add additional extensions for fast vector and matrix
-- processing and similar.
data V 
    = N !Integer        -- number (integer)
    | P V V             -- product of values
    | L V | R V         -- sum of values (left or right)
    | U                 -- unit value
    | B ABC Flags       -- block value
    | S !Token V        -- sealed and special values
    | T !Text           -- embedded text value
    | X (VRef V) Flags  -- external value resource
    deriving (Eq, Typeable)


