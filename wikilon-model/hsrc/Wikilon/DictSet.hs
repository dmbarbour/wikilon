{-# LANGUAGE DeriveDataTypeable #-}

-- | Wikilon hosts more than one dictionary. This is necessary for
-- DVCS-style forking and merging, to model 'working' dictionaries
-- apart from the more stable versions. Individually, each named
-- dictionary has a history. 
--
-- In terms of the web service, a dictionary has a URL \/d\/dictName.
-- And an individual word has URL \/d\/dictName\/w\/word. 
--
module Wikilon.DictSet
    ( DictName
    , DictSet
    , DictRef
    , DictHead

    , getDictRef
    , setDictRef
    , delDictRef
    , newDictRef

    , loadDictHead
    , loadDictHist
    , loadDictVer
    , updateDict

    , dhDict
    , dhRLU
    , dhMod

    , module Wikilon.Time
    , module Wikilon.Dict
    ) where

import Control.Applicative
import Data.Typeable (Typeable)
-- import qualified Data.ByteString as BS
import Data.VCache.Trie (Trie)
import qualified Data.VCache.Trie as Trie
import Database.VCache
import Data.VCache.LoB (LoB) 
import qualified Data.VCache.LoB as LoB
import Wikilon.Dict
import Wikilon.DictRLU
import Wikilon.Time

-- | Dictionary names should be valid words.
type DictName = Word

-- | Wikilon hosts a finite collection of named dictionaries.
newtype DictSet = DictSet (Trie DictRef)
    deriving (Eq, Typeable)

getDictRef :: DictName -> DictSet -> Maybe DictRef
getDictRef (Word w) (DictSet t) = Trie.lookup w t

setDictRef :: DictName -> DictRef -> DictSet -> DictSet
setDictRef (Word w) r (DictSet t) = DictSet (Trie.insert w r t)

delDictRef :: DictName -> DictSet -> DictSet
delDictRef (Word w) (DictSet t) = DictSet (Trie.delete w t)

-- | Each named dictionary has a head, a history, and additional 
-- metadata, authority management, cached computations, etc.. 
--
-- The history for a dictionary shall utilize exponential decay after
-- it grows too large (much larger than a few hundred point samples).
-- So what I'm really after is more like a breadcrumb trail to provide
-- greater robustness against loss of information.
--
-- I'm modeling each dictionary with its own set of variables to help
-- avoid synchronization bottlenecks. 
--
-- For cached computations, I want:
--
--  * reverse lookup index to find words by which tokens they use.
--  * fast access to precompiled words
--  * index for fuzzy find for words by substring
--  * quick list of undefined, cyclic, or badly typed words
--  * index for finding words by type (combine with prior?)
--
-- Fortunately, it's easy to add cached computations later, as needed.
--
data DictRef = DictRef
    { d_head :: PVar DictHead
    , d_hist :: PVar (LoB (T, Dict))   
    -- , d_auth :: PVar Auth
    } deriving (Typeable)

-- | The head of the dictionary contains the dictionary value, a
-- reverse lookup index that is always kept up-to-date (because it's
-- critical for cache invalidation), and a time value that doubles as
-- a unique, monotonic version number. 
--
-- The assumption is that the dict head is the most frequently
-- accessed resource for a dictionary. 
data DictHead = DictHead
    { d_dict :: Dict    -- word lookup, primary data
    , d_rlu  :: DictRLU -- reverse token lookup
    , d_mod  :: T       -- modified time, version 
    } deriving (Typeable)

-- | Create a new, empty DictRef. 
newDictRef :: T -> VTx DictRef
newDictRef tNow = 
    getVTxSpace >>= \ vc ->
    let dh = DictHead { d_dict = dictCreate vc
                      , d_rlu  = rluCreate vc
                      , d_mod  = ceilingSeconds tNow
                      }
    in
    newPVar dh >>= \ _head ->
    newPVar (LoB.empty vc 32) >>= \ _hist ->
    return $ DictRef
        { d_head = _head
        , d_hist = _hist
        }

-- | Obtain the head version of the dictionary.
loadDictHead :: DictRef -> VTx DictHead
loadDictHead = readPVar . d_head

-- | Obtain the dictionary's history.
--
-- Each element in this list is read as 'before time T the
-- dictionary had value Dict', with more recent times near
-- the head of the list.
--
-- The dictionary history will correspond to 
--
-- This history will be incomplete due to exponential decay
-- strategies that gradually remove old content.
loadDictHist :: DictRef -> VTx [(T,Dict)]
loadDictHist = fmap LoB.toList . readPVar . d_hist

-- seek a time-decreasing series for a particular version
hseek :: a -> [(T, a)] -> T -> a
hseek _ ((tm,v):h) tgt | (tm > tgt) = hseek v h tgt
hseek v _ _ = v

-- | Compute the dictionary at a given instant in time.
loadDictVer :: DictRef -> T -> VTx Dict
loadDictVer dr tm = 
    loadDictHead dr >>= \ dh ->
    loadDictHist dr >>= \ hst ->
    return (hseek (dhDict dh) hst tm)

-- | Replace the dictionary at the head.
--
-- For update times, which double as a version number, I use a simple
-- strategy to guarantee a monotonic time: use time in seconds, or use
-- the previous time plus a millisecond (whichever is larger).
--
updateDict :: DictRef -> T -> Dict -> VTx ()
updateDict dr tUpd dv =
    loadDictHead dr >>= \ dh ->
    let dOld = dhDict dh in
    let tOld = dhMod dh in
    let tUpd' = max (ceilingSeconds tUpd) (tOld `addTime` 0.001) in -- 
    let rlu' = rluUpdate (dictDiff dv dOld) (d_rlu dh) in
    let dh' = DictHead { d_dict = dv, d_rlu = rlu', d_mod = tUpd' } in
    writePVar (d_head dr) dh' >>
    modifyPVar (d_hist dr) (LoB.cons (tUpd', dOld))

-- | The primary value associated with the dictionary.
dhDict :: DictHead -> Dict
dhDict = d_dict

-- | Reverse lookup index is always kept up-to-date.
dhRLU :: DictHead -> DictRLU
dhRLU = d_rlu

-- | When was the dictionary last updated? 
dhMod :: DictHead -> T
dhMod = d_mod



-- Thoughts:
--
-- For caching, I have at least a couple options. One option is to
-- indirect caching through a secure hash. Then I don't need to
-- worry about cache invalidation (simple expiration would work). 
-- And I could theoretically leverage a common cache across multiple
-- versions and branches of the dictionary.
--
-- OTOH, with VCache, I already gain a lot of implicit structure 
-- sharing. The lack of indirection might involve recomputing the
-- structure between branches and versions, but is simple.


instance VCacheable DictHead where
    put (DictHead d rlu tMod) = putWord8 1 >> put d >> put rlu >> put tMod
    get = getWord8 >>= \ v -> case v of
        1 -> DictHead <$> get <*> get <*> get
        _ -> fail "Wikilon.DictSet - unknown DictHead version"

instance VCacheable DictRef where
    put (DictRef dh hst) = putWord8 1 >> put dh >> put hst
    get = getWord8 >>= \ v -> case v of
        1 -> DictRef <$> get <*> get
        _ -> fail "Wikilon.DictSet - unknown DictRef version"

instance VCacheable DictSet where
    put (DictSet t) = putWord8 1 >> put t
    get = getWord8 >>= \ v -> case v of
        1 -> DictSet <$> get
        _ -> fail "Wikilon.DictSet - unknown DictSet version"
