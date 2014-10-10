
-- | A 'Session' represents a long-running interaction with a Wiki.
--
-- Typically, a session is for a single user. Though, Wikilon does not
-- prevent multi-user shared sessions. A single user may have many 
-- sessions, e.g. for different tasks. The relationship between users
-- and sessions is pretty loose... Wikilon doesn't actually have a 
-- concept of 'User' except as a potential authentication measure.
--
-- Sessions will usually be obtained by starting as a guest in a guest
-- friendly wiki, or by invite or delegation from an existing session.
-- 
-- Most sessions are persistent. The exception is a 'volatile' guest
-- session, which is aimed to lower the barrier for new guests but is
-- volatile by default because our guest might decide to log in as a
-- proper user. A guest is free to promote a volatile session to a
-- persistent one, indicating explicit intention to continue as a new
-- user.
--
-- A session is where we keep user preferences, workspaces and working
-- definitions, and most web-app state. 
--
-- A session provides a nearly transactional view of the wiki, but one
-- that must be maintained - e.g. through merging updates from other
-- sessions and users. So long as there are no conflicts, the merges
-- can happen automatically. 
-- 
module Wikilon.Session
    (
    ) where



