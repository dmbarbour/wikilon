
-- | The 'property lookup' is a generic, flexible, and expensive 
-- variation of the 'reverse lookup'. Compared to reverse lookup,
-- I give up precision to better support background computation. 
-- 
-- * given a property, find a list of associated words.
-- * given a word, find a list of associated properties.
-- * propagate invalidation and computation for properties.
--
module Wikilon.Dict.PLU
    (
    ) where

