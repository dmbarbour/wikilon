
-- | Renaming words is a common refactoring that is relatively easy
-- to automate, assuming we can perform lookups and reverse lookups
-- in context of the dictionary.
data Awelon.Dict.Rename 
   -- ( renameWord, renameWords
   -- , renameDict, renameDicts
   -- ) where

-- TODO:
--
-- Renaming is not a O(1) operation for an Awelon dictionary. But
-- it is relatively cheap.
--  given a dictionary or the suitable context
--  given a set of name rewrites
--  generate a single 'patch' for the dictionary that performs the rename
--
-- R
