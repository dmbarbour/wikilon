
-- | A Wiki will have a set of users. The assumption is that users
-- will have 'bursty' interactions with the Wiki. 
--
--  
-- interaction with a wiki - active for a few hours, gone for a few
-- days, back again. While active, a user may have a large number
-- of active views of the Wiki.
--
-- The Wikilon User is identified by a randomly generated string.
-- In addition, a user will have some local state. 
-- Some basic information about the user's interactions will be 
-- recorded over time. 
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
module Wikilon.User
    (
    ) where



