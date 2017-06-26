
-- | First-Class Key-Value Databases
--
-- Wikilon.DB offers a mutable key-value database with stowage and GC.
-- This KVM models a key-value map above stowage, enabling first class
-- database values to potentially be larger than memory.
-- 
-- The tree structure used for KVM is a variant of a crit-bit tree or
-- trie. Differences from conventional crit-bit tree:
--
-- - least key is held by parent, to support full tree diffs and merges
-- - each key is associated with a binary value (which may be empty)
-- - keys, values, nodes may be stowed outside of volatile memory
--
-- Keys and values within the KVM are free to reference other stowage 
-- resources. But keys mustn't have any trailing null bytes.
--
-- Batched updates are important: it's inefficient to allocate lots of
-- short-lived nodes at the stowage layers. This module will aim to make
-- batching and buffering relatively simple and easy.
--
-- First-class database values offer a lot of benefits over conventional
-- key-value databases: histories, forking, diffs, composition. Wikilon
-- relies on KVM for most data indexing and processing. 
--
module Wikilon.KVM
    ( Key, Val

    ) where

import qualified Data.ByteString.Lazy as LBS
import Control.Exception

type Key = LBS.ByteString
type Val = LBS.ByteString

-- our crit-bit tree doesn't distinguish keys with trailing NULLs,
-- instead treating every key as having an infinite extent of zero
-- bits following the final non-zero bit.
--
-- Since we can't distinguish keys with trailing NULLs, we also should
-- not accept them into our trees.
validKey :: Key -> Bool
validKey s = LBS.null s || (0 /= LBS.last s)





-- | A simple trie with bytestring data. 

