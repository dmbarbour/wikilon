
-- | This is a Trie implemented for VCache. The keys, in this case,
-- are simple bytestrings - they've got to be serialized anyway.
-- Since LMDB and VCache work well with larger nodes, this Trie has
-- a high branching factor. 
module Data.VCache.Trie
    (
    ) where


