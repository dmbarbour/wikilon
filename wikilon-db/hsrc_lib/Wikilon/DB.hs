
-- | Wikilon DB is essentially an adaptation of Berkeley DB adding
-- some features for structured values and structure sharing. This
-- allows Wikilon DB to keep a history via copy-on-write with the
-- normal structure sharing.
-- 
module Wikilon.DB 
    (
    ) where


-- todo:
--  structured values
--  value references
--  staging
--    opening a database
--    running transactions
--    closing a database
--  concurrent transactions
--  working with history
--  branching (and merging?)
--
-- Question: Do I want to support more than one "map", like Berkeley DB?
--  might keep it simple for now, just one map.
