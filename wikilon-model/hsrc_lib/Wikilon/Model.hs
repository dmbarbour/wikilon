
-- | The Wikilon 'Model' provides an abstract interface for Wikilon.
-- Wikilon will receive and process event messages, and respond to
-- messages asynchronously (if at all). Batches of events may be
-- processed atomically, i.e. present Wikilon as an abstract virtual
-- machine.  
--
module Wikilon.Model
    ( module Wikilon.Events
    ) where


import Wikilon.Events


