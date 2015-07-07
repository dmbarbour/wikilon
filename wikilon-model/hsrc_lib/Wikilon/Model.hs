
-- | The Wikilon 'Model' provides an abstract interface for Wikilon.
-- Wikilon will receive and process event messages, and respond to
-- messages asynchronously (if at all). Batches of events may be
-- processed atomically, i.e. present Wikilon as an abstract virtual
-- machine.  
--
module Wikilon.Model
    ( 
    ) where

-- TODO:
--
--  define a set of update and query messages
--  decide how to return query responses to Haskell
--  decide how to integrate subscriptions and RDP
--


-- A web service will communicate with our model by presenting a
-- sequence of messages, some of which will cause responses. In
-- some cases, I'll want subscriptions as well. Messages will be
-- processed by an abstract machine, the data model, but may be
-- batched as a simplistic basis for atomic updates and queries.
--
-- For feedback, I should present each client as a separate abstract
-- machine, though which may be short-lived or volatile. Capabilities
-- for such a machine could be given a volatile address, e.g. using
-- the load count for the wikilon store.
--
-- Eventually, I'll need subscriptions. But for now I can probably
-- focus on simple query-response behaviors, which suggest futures.

-- model requires:
--  capabilities concepts
--  serializable capabilities
--  toplevel capabilities (e.g. basic query/response)
--  ability to create temporary capabilities
--    







