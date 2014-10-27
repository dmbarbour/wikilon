
-- | The Wiki has:
--
-- * an AO dictionary with words as pages
-- * active and inactive users (sessions)
-- * long-running services and behaviors
-- * persistent storage via acid-state
--
-- In addition to the pages, the Wiki must support state for many
-- purposes: bug reports, feature requests, pending transactions,
-- cache, ABC resources, and similar. Further, in Wikilon's role as
-- an IDE, the Wiki should be a programmable software platform. 
-- 
-- Long term, I'd like for a wiki to be a distributed object, i.e.
-- with multiple Wikilon hosts contributing to and voting on its
-- future state. But I think we can leave that for later.
-- 
module Wikilon.Wiki
    ( Wiki, newWiki
    ) where

import Control.Applicative 

import qualified Data.Binary as Binary
import qualified Data.Serialize as C
import qualified Data.SafeCopy as SC

import Data.ByteString (ByteString)
import Wikilon.Secret
import Wikilon.DictST
import Wikilon.DictTX

type Wiki = Wiki0

data Dictionary = Dict 
    { _dictHist    :: ![DictTX]                 -- primary content
    , _dictSize    :: {-# UNPACK #-} !Int       -- cached: length of history list
    , _dictState   :: {-# UNPACK #-} !DictST    -- cached: current state of dictionary
    , _dictCount   :: {-# UNPACK #-} !Int       -- cached: total count of transactions
    }

data Wiki0 = Wiki0
    { _secret   :: !ByteString
    , _fullDict :: !Dictionary
    , _dictMax  :: {-# UNPACK #-} !Int
    -- TODO:
    --   users
    --   auxillary state
    --   logs and reports
    --   cached computations (not stored)
    }

{-
instance SC.SafeCopy Dict of
    errorTypeName _ = "Wikilon.Wiki.Dict"
    version = 0
    kind = SC.base
  -}  

defaultDictSize :: Int
defaultDictSize = 1000

-- | Create a 'new' wiki, with empty dictionary and hidden secret.
newWiki :: IO Wiki
newWiki =
    newSecret >>= \ sec ->
    return $ Wiki0 
                { _secret = sec
                , _fullDict = Dict [] 0 emptyDict 0
                , _dictMax = defaultDictSize
                }



