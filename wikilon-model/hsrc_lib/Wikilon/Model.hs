{-# LANGUAGE GADTs, TypeFamilies, FlexibleContexts, Rank2Types #-}

-- | Wikilon is accessed via an abstract API. This API supports 
-- atomic groups of confined queries and updates with functional
-- glue (e.g. monadic). Queries and updates are confined by this
-- API, i.e. in the sense that they cannot access other machines
-- or resources. A set of update actions and queries is atomic 
-- whenever feasible. 
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
    ( BranchName, Branch, Dict, DictRep
    , ModelRunner
    , W(..)
    , loadBranch, listBranches
    , branchHead, branchUpdate, branchModified, branchHistory
    , loadDict, loadDictAndTime, branchSnapshot
    , getTransactionTime
    , newEmptyDictionary
    --, WikilonModel, ModelRunner
    --, BranchingDictionary(..), CreateDictionary(..)
    --, GlobalErrorLog(..), logSomeException, logException
    , module Wikilon.Dict
    , module Wikilon.Time
    ) where

import Control.Monad
import Control.Applicative
import Wikilon.Time (T)
import Wikilon.SecureHash
import Wikilon.Dict.Object
import Wikilon.Dict

type BranchName = Word

-- | Wikilon has some opaque value types, which might be understood
-- similarly to file handles or ADTs. Very large dictionaries may be
-- lazily loaded, but provide a pure value interface. Branches are
-- modeled as mutable constructs, albeit addend-only with a hidden
-- decay model.
type family DictRep m
type family Branch m

-- | a Dict value has a machine-dependent representation.
type Dict m = DictObj (DictRep m)

type ModelRunner = forall a . (forall m . W m a) -> IO a

-- I need a runner that hides the implementation-type of the model.
--
-- I can't say: 
--
--     Runner a = forall m . Model m => m a -> IO a
--
-- This would say that our Runner must accept any implementation of
-- the model, which would be ridiculous. I need a specific instance
-- of the model, but one not known to the caller.
--
-- The simplest approach might be the best: an intermediate language
-- for the model, with some hidden types. Will data families work?
-- I'm not sure. But something like this does seem appropriate:
-- 
--    Runner a = forall m . W m a -> IO a
--    data family Dict m
--    data family Branch m
-- 
-- However, we must expose computations on the dictionary of a hidden
-- type. Again, typeclasses seem inadequate:
--
--    Runner a = forall m . Dictionary (Dict m) => Action m a -> IO a
--
-- Because we wouldn't be able to prove Dictionary for an unknown 
-- instance. The alternative is to avoid typeclasses here, too, e.g.
-- by making `Dict m` a more specific data type that encapsulates the
-- interface and a hidden value type, or to provide access via the
-- monadic actions. 
--
-- A point to consider is that whatever interface I use here should
-- be easily reflected in a network of abstract virtual machines via
-- Awelon Bytecode. Typeclasses do not exist in ABC because they are
-- global in nature. So, it makes sense to avoid typeclasses at this
-- layer.

-- | The Wikilon model API, presented as a monad with a bunch of
-- concrete commands. There might be better ways to express this
-- (free monad, continuation monad, Data.Machine.Type, etc.) but
-- this should be enough to get started quickly.
data W m a where 
    Return :: a -> W m a
    Bind :: W m a -> (a -> W m b) -> W m b

    -- basics
    ListBranches :: W m [BranchName]

    LoadBranch :: BranchName -> W m (Branch m)
    BranchHead :: Branch m -> W m (Dict m)
    BranchModified :: Branch m -> W m T
    BranchHistory :: Branch m -> T -> T -> W m (Dict m, [(T, Dict m)])
    BranchUpdate :: Branch m -> Dict m -> W m ()

    -- miscellaneous 
    NewEmptyDictionary :: W m (Dict m)
    GetTransactionTime :: W m T

    
instance Monad (W m) where
    return = Return
    (Return a) >>= f = f a
    op >>= f = Bind op f
instance Applicative (W m) where
    pure = return
    (<*>) = ap
instance Functor (W m) where
    fmap f op = op >>= return . f


-- | Load a branch given its name. Authentication might be performed here.
loadBranch :: BranchName -> W m (Branch m)
loadBranch = LoadBranch

-- | Obtain list of branch names with content or history. 
listBranches :: W m [BranchName]
listBranches = ListBranches

-- | Return the most recent dictionary on a branch.
branchHead :: Branch m -> W m (Dict m)
branchHead = BranchHead

-- | Update the dictionary associated with the branch.
branchUpdate :: Branch m -> Dict m -> W m ()
branchUpdate = BranchUpdate

-- | Obtain all available snapshots for a branch between two time values.
-- There is no strong requirement that a branch keeps more than the head
-- value, but if we do keep more we must be able to access them.
--
-- Our history is accessed in terms of:
--
--   (snapshot, [(tmUpdate, previousSnapshot)])
--
-- The tmUpdate values should fall between the requested times.
branchHistory :: Branch m -> T -> T -> W m (Dict m, [(T,Dict m)])
branchHistory = BranchHistory

-- | Obtain the time snapshot for the current branch head.
-- Will return minBound if no updates have been applied.
branchModified :: Branch m -> W m T
branchModified = BranchModified

-- | Load the current dictionary given the branch name.
loadDict :: BranchName -> W m (Dict m) 
loadDict dn = loadBranch dn >>= branchHead

-- | Load dictionary and its modified time together.
loadDictAndTime :: BranchName -> W m (Dict m, T)
loadDictAndTime dn =
    loadBranch dn >>= \ b ->
    branchHead b >>= \ d ->
    branchModified b >>= \ t ->
    return (d,t)

-- | Obtain the snapshot of a dictionary at a specific time. 
branchSnapshot :: Branch m -> T -> W m (Dict m) 
branchSnapshot b t = liftM fst $ branchHistory b t t 

-- | Obtain an empty dictionary value with an abstract 
-- representation associated with machine `m`. This should
-- be an effectively pure operation.
newEmptyDictionary :: W m (Dict m)
newEmptyDictionary = NewEmptyDictionary

-- | Obtain the transaction time that would be associated with
-- branch updates and so on.
getTransactionTime :: W m T
getTransactionTime = GetTransactionTime


-- TODO
--  abstract support for caching, memoization, interning
--  abstract internal model of Awelon Bytecode (`WBC m`?)
--  event logs, issue tracking, etc. (perhaps modeled as dictionaries?)
--  hmac signature support? (long term and per-session?)
--  encryption support?



