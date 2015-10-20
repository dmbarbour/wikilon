{-# LANGUAGE DeriveDataTypeable #-}

-- | Wikilon hosts more than one dictionary. This is necessary for
-- DVCS-style forking and merging, to model 'working' dictionaries
-- apart from the more stable versions. Individually, each named
-- dictionary has a history. 
--
-- In terms of the web service, a dictionary has a URL \/d\/dictName.
-- And an individual word has URL \/d\/dictName\/w\/word. 
--
module Wikilon.Dict.Set
    ( DictName
    , DictSet
    , DictHead
    , DictRef

    , getDictRef
    , setDictRef
    , delDictRef
    , newDictRef

    , loadDictHead
    , loadDictHist
    , loadDictVer
    , updateDict

    , module Wikilon.Dict.Head
    , module Wikilon.Dict.Hist
    ) where

import Control.Applicative
import Data.Typeable (Typeable)
-- import qualified Data.ByteString as BS
import Data.VCache.Trie (Trie)
import qualified Data.VCache.Trie as Trie
import Database.VCache
import Wikilon.Dict.Head
import Wikilon.Dict.Hist

-- | Dictionary names should be valid words.
type DictName = Word

-- | Wikilon hosts a finite collection of named dictionaries.
newtype DictSet = DictSet (Trie DictRef)
    deriving (Eq, Typeable)

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
    , d_hist :: PVar DictHist   
    -- , d_auth :: PVar Auth
    } deriving (Typeable)

getDictRef :: DictName -> DictSet -> Maybe DictRef
getDictRef (Word w) (DictSet t) = Trie.lookup w t

setDictRef :: DictName -> DictRef -> DictSet -> DictSet
setDictRef (Word w) r (DictSet t) = DictSet (Trie.insert w r t)

delDictRef :: DictName -> DictSet -> DictSet
delDictRef (Word w) (DictSet t) = DictSet (Trie.delete w t)

-- | Create a new, empty DictRef. 
newDictRef :: T -> VTx DictRef
newDictRef tNow = 
    getVTxSpace >>= \ vc ->
    newPVar (dhCreate vc tNow) >>= \ _head ->
    newPVar (createDictHist vc) >>= \ _hist ->
    return $ DictRef
        { d_head = _head
        , d_hist = _hist
        }

-- seek a time-decreasing series for a particular version
hseek :: a -> [(T, a)] -> T -> a
hseek _ ((tm,v):h) tgt | (tm > tgt) = hseek v h tgt
hseek v _ _ = v

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
loadDictHist = fmap readDictHist . readPVar . d_hist

-- | Obtain the head version of the dictionary.
loadDictHead :: DictRef -> VTx DictHead
loadDictHead = readPVar . d_head

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
-- the previous time plus a millisecond (whichever is larger). If the
-- dictionary updates more frequently than 1000Hz, the time will not
-- be very accurate. But that's unlikely, and not a concern at this time.
--
updateDict :: DictRef -> T -> Dict -> VTx ()
updateDict dr tUpd dNew = 
    loadDictHead dr >>= \ dh ->
    let tOld = dhMod dh in
    let dOld = dhDict dh in
    let tUpd' = max (ceilingSeconds tUpd) (tOld `addTime` 0.001) in -- 
    let dh' = dhUpdate dh dNew tUpd' in
    writePVar (d_head dr) dh' >>
    modifyPVar (d_hist dr) (updateDictHist (tUpd', dOld))

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
