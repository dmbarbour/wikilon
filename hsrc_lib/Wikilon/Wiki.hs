
-- | The 'Wiki' is the main social unit of Wikilon.
--
-- The Wikilon web server may host multiple Wikis. Each Wiki should
-- be understood as a *distributed* object, not tied to any particular
-- process or machine. A wiki may be replicated or forked, and those
-- are distinct concepts.
--
-- The primary state of a Wiki is the dictionary. Each Wiki has a 
-- dictionary, represented by a bounded sequence of transactions.
-- In addition to the dictionary, a Wiki may have a great deal of
-- auxillary state, e.g. for bug reports, feature requests, pending
-- transactions, long-running services.
--
-- Some resources (cache, dictionary transactions, scheduler) shall 
-- be shared across multiple Wiki instances on the same machine.
--
-- A significant responsibility of the Wiki is tracking when a word
-- is updated - not just directly, but transitively. Performing this
-- task efficiently, and with minimal latency, is the basis for live
-- programming.
-- 
module Wikilon.Wiki
    (
    ) where



