
-- | First-Class Key-Value Databases
--
-- Wikilon.DB offers a mutable key-value database with stowage and GC.
-- This KVM models a key-value database model above stowage, enabling
-- database values to potentially be larger than memory.
-- 
-- The tree structure used here is a variant of the crit-bit tree. The
-- main differences from conventional crit-bit tree:
--
-- - least key is held by parent, to support full tree diffs and merges
-- - each key is associated with a single value (which may be empty)
-- - keys, values, nodes may be stowed rather than referenced in memory
--
-- In context of stowage, keys and values in the KVM may freely reference
-- other stowage resources. But there is a potential challenge: operations
-- on a KVM may fail if a resource is unrecognized, and this failure is 
-- impure.
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
module Wikilon.KVM
    (
    ) where

-- | A simple trie with bytestring data. 

