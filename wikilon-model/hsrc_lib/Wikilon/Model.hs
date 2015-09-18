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
    , Bytes, cacheBytes

    , deleteDictWord

    --, WikilonModel, ModelRunner
    --, BranchingDictionary(..), CreateDictionary(..)
    --, GlobalErrorLog(..), logSomeException, logException
    , module Wikilon.Dict
    , module Wikilon.Time
    ) where

import Control.Monad
import Control.Applicative
import Data.Monoid
import Wikilon.Time (T)
import Wikilon.SecureHash
import Wikilon.Dict.Object
import Wikilon.Dict
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS

-- | Dictionary branches and forks use the same naming systems as
-- the words, e.g. for easy integration with HTML and URLs.
type BranchName = Word

-- | Wikilon has some opaque value types, which might be understood
-- similarly to file handles or ADTs. Very large dictionaries may be
-- lazily loaded, but provide a pure value interface. Branches are
-- modeled as mutable constructs, albeit addend-only with a hidden
-- decay model.
type family DictRep m
type family Branch m

-- | For now, cache keys will just be opaque bytestrings. This seems
-- a good option: bytestrings are serializable, have canonical form,
-- and are easy to use with tries and secure hashes and so on. 
type Key = BS.ByteString

-- | Cached bytestrings, allowing for lazy loading or computation.
type Bytes = LBS.ByteString

-- | a Dict value has a machine-dependent representation.
type Dict m = DictObj (DictRep m)

-- | When running a wikilon transaction (W) I don't know
-- the implementation-specific details (m).
type ModelRunner = forall a . (forall m . W m a) -> IO a

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

    -- TODO:
    -- values or types
    -- evaluation or type checking
    -- caching binaries and values/types
    CacheBytes :: Key -> W m Bytes -> W m Bytes
    -- value key...

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

-- | Obtain a cached bytestring, or compute the bytestring then
-- store it. The bytestring should have a constant value for the
-- given key. Typically, the key should involve a secure hash or
-- version hash of the relevant source and inputs. A simple prefix
-- or suffix can cover the computation and version numbers.
cacheBytes :: Key -> W m Bytes -> W m Bytes
cacheBytes = CacheBytes

-- | Delete a word under a named branch.
deleteDictWord :: BranchName -> Word -> W m ()
deleteDictWord dn dw = 
    loadBranch dn >>= \ br ->
    branchHead br >>= \ d0 ->
    let d' = unsafeUpdateWord dw mempty d0 in
    branchUpdate br d'




-- TODO
--  efficient access to pre-cached word semantics
--  abstract support for caching, memoization, interning
--  abstract internal model of Awelon Bytecode (`WBC m`?)
--  event logs, issue tracking, etc. (perhaps modeled as dictionaries?)
--  hmac signature support? (long term and per-session?)
--  encryption support?



