
-- | The 'Wiki' is the main social unit of Wikilon. A Wikilon instance
-- may host multiple Wikis. Each Wiki has a single dictionary, which
-- may have been forked from another Wiki. However, forking should not
-- be encouraged. The vision for Awelon project is that a single Wiki
-- should host thousands of projects and users, developing a dictionary
-- of a million words to cover nearly every use case imaginable.
--
-- Forking is necessary for some use cases, of course. For example, a
-- private software development group might wish to fork a public Wiki
-- as a basis for their own projects. Wikis should eventually support
-- good APIs for this behavior, and for 'giving back' to the community
-- via pull requests and such.
--
-- Long term, a large Wiki should become a distributed object, with
-- hundreds of compatible processes contributing to it, remaining
-- synchronized through web sockets and web APIs.
-- 
-- The main stateful element of a Wiki is the dictionary. However,
-- Wikis do contain other state for such purposes as pull requests,
-- issue trackers, long-running services, and so on. User sessions
-- will similarly have access to state. 
-- 
module Wikilon.Wiki
    (
    ) where



