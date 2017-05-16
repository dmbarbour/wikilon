{-# LANGUAGE GADTs, TypeFamilies, Rank2Types #-}
-- | The Wikilon 'Model' is an abstract monadic API. This hides 
-- most implementation details from the client.
module Wikilon.Model
    ( Result, Action, Runner(..)
    , isOK
    , module Wikilon.Dict
    , module Wikilon.Word
    , module Wikilon.Token
    ) where

import Wikilon.Word
import Wikilon.Token
import Wikilon.Dict
import Wikilon.AODict

import Wikilon.Model.Action
import Wikilon.Model.Result

-- | Run a given action. Here, the `forall w` constrains our action
-- to be valid in all possible Wikilon models. (This prevents
-- us from exporting content tied to a wiki.)  
newtype Runner = Runner (forall a . (forall w . Action w a) -> IO (Result a))

isOK :: Result a -> Maybe a
isOK (OK a) = Just a
isOK _ = Nothing

