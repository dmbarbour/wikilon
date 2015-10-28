{-# LANGUAGE GADTs, TypeFamilies #-} 
module Wikilon.Model.Action
    ( Action(..)
    ) where

import Wikilon.Word
import Wikilon.Token
import Wikilon.Dict
import Wikilon.Dict.Set
import Wikilon.AODict
import Wikilon.Time

-- | This is a complete abstract API for a Wikilon instance. It is 
data Action w a where
    -- composition
    Return :: a -> Action w a
    Bind :: Action w a -> (a -> Action w b) -> Action w b

    -- authority
    -- WithAuth :: Auth -> Action w a -> Action w a
    
    -- time (constant for an action)
    GetTime :: Action w T

    -- working with multiple dictionaries
    ListBranches :: Action w [DictName] 
    LoadBranch :: DictName -> Action w DictRef

     
    -- import/export of dictionaries (via AODict format)

    -- basic views
    


