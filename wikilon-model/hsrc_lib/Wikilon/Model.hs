{-# LANGUAGE TypeFamilies, FlexibleContexts, ExistentialQuantification #-}

-- | The Wikilon Model is an abstract interface for Wikilon. This
-- API supports atomic groups of confined queries and updates with
-- purely functional glue, i.e. a monadic interface with the idea
-- that the monad may be serialized and sent to the model directly.
-- Eventually, I may need to add support for streaming interfaces
-- and reactive continuous queries, e.g. as extensions.
--
-- Conceptually, Wikilon is hosted by a separate abstract machine,
-- which is why queries and udpates are confined to computations
-- that may occur on just that machine. The long term goal is to
-- model Wikilon within Wikilon, as an abstract virtual machine,
-- to compile the AVM and remove the Haskell layer entirely.
--
-- TODO: figure out authorization concerns, perhaps at the level
-- of whole monadic operations.
--
-- NOTE: A lot of this replicates the Wikilon.Dict interface,
-- except that the monadic context allows a more natural use of
-- cache, logging, change tracking, side-effects, etc.. at the
-- cost of implicit laziness or streaming. I'll eventually 
-- deprecate the pure interface on the dict type.
--
module Wikilon.Model
    ( WikilonModel
    , ModelAction(..)
    , ModelAuth(..)
    , ModelRunner
    , BranchName, Branch, Dict
    , BranchingDictionary(..), branchSnapshot
    , module Wikilon.Dict
    , module Wikilon.Time

    {-
    , ABC, Token, Word, Bytes
    , DictView(..)
    , dictWordDeps, dictWordDepsT, abcWords
    , SecureHash
    , DictRLU(..), dictWordClients, dictWordClientsT
    , WordPrefix, DictSplitPrefix(..), dictSplitPrefixW
    , DictUpdate(..)
    , dictUpdateWord, dictUpdateWords
    , dictDeleteWord, dictDeleteWords
    , dictRenameWord
    , matchWord, matchSimpleRedirect
    , testForMalformedDef, testForCycle
    -}

    
    ) where

import Control.Monad
import Control.Applicative
import Wikilon.Time
import Wikilon.SecureHash
import Wikilon.Dict 

type BranchName = Word

-- | Wikilon model is defined by a toplevel API.
class ( Functor m
      , Applicative m
      , Monad m
      , BranchingDictionary m
      ) => WikilonModel m

-- | A model action is generic to the machine and monad type, m. It
-- corresponds to a free monad with a limited set of available 
-- behaviors. In general, an action will be computed atomically,
-- though we may mark some tasks for background processing.
data ModelAction a = forall m . WikilonModel m => ModelAction (m a)

-- | I'll eventually need a good model for authority. I'd prefer to
-- use object capability model, but that will probably coexist with
-- user-level authentication (e.g. such that users can create caps
-- to share authority).
data ModelAuth = Guest

-- | We'll ultimately run our model actions via the IO monad.
type ModelRunner a = ModelAuth -> ModelAction a -> IO a

-- | Wikilon has some opaque value types, which might be understood
-- similarly to file handles or ADTs. Very large dictionaries may be
-- lazily loaded, but provide a pure value interface. Branches are
-- modeled as mutable constructs, albeit addend-only.
--
data family Dict (m :: * -> *)
data family Branch (m :: * -> *)

-- | Wikilon has a branching dictionary model. Branches are named under
-- the same restrictions as dictionary words (to easily fit HTML). If a
-- branch has not been defined, we can treat that as an empty dictionary.
class (Dictionary (Dict m)) => BranchingDictionary m where
    -- | Load a branch given its name. Authentication might be performed here.
    loadBranch :: BranchName -> m (Branch m)

    -- | Return the most recent dictionary for a branch.
    branchHead :: Branch m -> m (Dict m)

    -- | Update the dictionary associated with the branch.
    branchUpdate :: Branch m -> Dict m -> m ()

    -- | Obtain all available snapshots for a branch between two time values.
    -- There is no strong requirement that a branch keeps more than the head
    -- value, but if we do keep more we must be able to access them.
    --
    -- Our history is accessed in terms of:
    --
    --   (snapshot, [(tmUpdate, previousSnapshot)])
    --
    -- The tmUpdate values should fall between the requested times.
    branchHistory :: Branch m -> T -> T -> m (Dict m, [(T,Dict m)])

-- | Obtain the snapshot of a dictionary at a specific time. 
branchSnapshot :: (BranchingDictionary m, Monad m) => Branch m -> T -> m (Dict m) 
branchSnapshot b t = liftM fst $ branchHistory b t t 

