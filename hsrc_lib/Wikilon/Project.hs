
-- | The 'project' is the main social unit of Wikilon. A project has
-- an AO dictionary, corresponding to a line of transactional updates.
-- And a project may have multiple active users, each with their own
-- active sessions. Typically, a Wikilon instance will be involved with
-- just one or a few projects. 
--
-- A project will also be a *distributed* concept. Multiple Wikilon 
-- instances may participate in a single project via replication and
-- synchronization. This synchronization will occur automatically by
-- use of web APIs.
--
-- In addition to the main dictionary, a project may have some state
-- to support interactions between users of the same project. At the
-- very least, this will cover concepts such as issues, bug reports, 
-- pull requests, and so on. A project might be understood to provide
-- a set of extensible services to its active users.
--
-- 
-- 
module Wikilon.Project
    (
    ) where



