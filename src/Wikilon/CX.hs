-- | A Wikilon Context provides access to an Awelon dictionary.
--
-- Awelon dictionaries can potentially be much larger than memory.
-- Further, a dictionary may be associated with a variety of cached
-- computations. It is difficult to treat the cache or dictionary
-- as a Haskell value. Instead we access these values via monadic
-- context.
--
module Wikilon.CX
    ( 
    ) where



