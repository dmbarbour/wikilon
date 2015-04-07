
-- | 
--
-- 
-- 
-- This module provides data structures to help track the branching and
-- histories of dictionaries.
--
-- At the
--
module Wikilon.Branch
    (
    ) where


-- | A specific branch of a dictionary will have:
-- 
-- * a head version
-- * historical versions
-- * some information about origins
--
--data Branch

-- Thoughts:
--
-- While I do want support for branching, I think I might also need
-- first-class support for merging. The question, then, is how we 
-- should indicate which other branches from which any given branch
-- is 'interested' in receiving merges from.
--
-- I want to track enough metadata about branches that I can support
-- easy merges. Essentially, I want something like a geneology. 







