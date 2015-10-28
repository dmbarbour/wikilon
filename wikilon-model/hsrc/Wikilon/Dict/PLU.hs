
-- | The 'property lookup' is a generic, flexible, and expensive 
-- variation of the 'reverse lookup'. Compared to reverse lookup,
-- I give up precision to better support background computation. 
-- 
-- * given a property, find a list of associated words.
-- * given a word, find a list of associated properties.
-- * propagate invalidation and computation for properties.
--
-- Thoughts: maybe it would be better to model properties in a
-- more generic module, separate from the dictionary? Something
-- more like an incremental datalog?
module Wikilon.Dict.PLU
    (
    ) where

