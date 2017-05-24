
-- | Awelon Dictionary Format
--
-- Awelong language specifies a simple, DVCS-inspired dictionary
-- format that is designed for scalability and structure sharing
-- between versions and developers. It looks roughly like this:
--
--     secureHashOfPatch1
--     secureHashOfPatch2
--     @word1 def1
--     @word2 def2
--     @@ns secureHashOfSubDict
--     @word3 def3
--
-- Awelon uses secure hashes to identify binary resources instead of
-- filenames or URLs. The resources identified by the initial hashes
-- are logically inlined. The hash after `@ns` supports hierarchical
-- containmenent of one dictionary within another. At the top level,
-- a dictionary may be precisely versioned by a root secure hash. The
-- last definition of any symbol wins. 
--
-- Thus we have a deeply immutable, persistent data structure that 
-- can be updated using flexible update models. But this flexibility
-- does create some challenges for indexing. We cannot assume sorted
-- words, for example.
--
-- And as for scale, Awelon's application model unifies the database
-- and the codebase. Thus, we must think about dictionaries the size
-- of popular forums or Wikipedia, at least in the long term. Scale
-- creates its own challenges, since we cannot expect to keep a full
-- codebase in memory, nor even an index on the codebase.
--
-- Ideally, I will want several indices:
--
-- * quickly find definition for a word
-- * reverse lookup, find uses of a word
-- * find words matching a suffix or prefix
-- * fuzzy find, spellcheck: words by similarity
-- * full text search
-- * cached evaluated definitions, types, optimized link defs, etc.
--  * word to a deep-version behavior hash? (a vanderhoof versum?)
-- * reverse type lookup, hoogle style for code completion
-- 
-- Also, I will also want to keep histories for dictionaries, and to
-- merge updates on similar dictionaries, and support transactional
-- updates, and so on. Everything will need to be incremental, so I
-- can quickly account for changes or differences in code. Memoized
-- computation of partial, composable indices is a promising approach,
-- though it may constrain which indices can efficiently be maintained.
--
-- There is much to do here. It's very daunting. But I'll just add one
-- feature at a time, as needed.
--
module Awelon.Dict
    (
    ) where


