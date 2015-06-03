
-- | An abstract interface for Wikilon.
--
-- The goal here is to present Wikilon in a manner compatible with an
-- abstract virtual machine on a network that accepts domain layer
-- messages or events. All responses from this machine are asynchronous.
-- Long-running subscriptions are also viable.
--
-- Note: user model and authentication may require an additional machine.
--
module Wikilon.Model
    (
    ) where

