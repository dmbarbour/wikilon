
-- | Stowage-supported Tries
--
-- Tries with stowage can model entire databases as first-class values,
-- using the secure hash stowage references to load data as needed. But
-- they also introduce some performance challenges: construction and GC
-- of stowage resources has a high cost that we cannot afford to ignore.
--
-- Aside: LSM trees offer batching more naturally, but have difficulties
-- for other nice features like structure sharing and quick diffs. That
-- isn't a problem for second-class databases, but once databases become
-- first-class values, it's highly convenient to have efficient diffs and
-- and similar properties.
--
-- In any case, this module provides a bytes-to-bytes trie, essentially
-- a first-class key-value database that doesn't parse the data. Values
-- may root other stowage resources, but keys must not. Large values are
-- implicitly stowed to ensure predictable node sizes.
--
module Wikilon.Trie
    (
    ) where

-- | A simple trie with bytestring data. 

