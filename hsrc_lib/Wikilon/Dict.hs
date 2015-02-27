
-- | A dictionary is simply a map from words to definitions. 
--
-- In the Wikilon dictionary, a definition has a structured value and
-- a compiler function, both modeled in Awelon Bytecode (ABC). Access
-- to other words in the dictionary is achieved by {%word} tokens. The
-- word tokens will be compiled away by inlining word definitions.
--
-- I also want indices for dictionaries, but it isn't clear how much
-- should be kept with each snapshot vs. how much should be computed
-- on an as-needed basis, or at higher levels e.g. to support search
-- across multiple branches, or just the most recent version of the
-- dictionary. So, for now, I'll leave indexing alone.
-- 
module Wikilon.Dict
    (
    ) where
