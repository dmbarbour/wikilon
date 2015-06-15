
-- | Events should capture every command to Wikilon, allowing us to
-- easily log operations and view update logs in the browser. Also,
-- modeling explicit events should make it easier to track authority,
-- context, and origin information.
--
module Wikilon.Events
    ( 
    ) where

-- What I'd like to do is gradually ween wikilon-wai off of direct
-- access to wikilon-store and to instead use events.


-- type DictName = UTF8.ByteString

-- Which events do we need?
--
-- * Dictionary Creation Events
-- * Dictionary Edit Events
-- * Dictionary View Events
--
-- * User and Group Creation 
-- * User and Group Authority Management
--
-- * Evaluation 
--  * short running (supply result immediately?)
--  * long running (need intermediate and debug views?) 
-- * Acquisition of Cached or Compiled Views

{-
data Event
    = CreateDict DictName
    | 
-}



