
-- | The Wiki has:
--
-- * an AO dictionary with words as pages
-- * active and inactive users (sessions)
-- * long-running services and behaviors
-- * persistent storage via acid-state
--
-- A special challenge of the Wiki is that users will frequently
-- require a historical view of the database; indeed, that will be
-- the normal case. OTOH, they'll also tend to jump to the most
-- recent version of the database.
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
    ( Wiki
    ) where

import Control.Arrow (second)
import Control.Applicative 
import Control.Monad.State.Strict

import qualified Data.Serialize as C
import qualified Data.Serialize.Get as C
import qualified Data.SafeCopy as SC
import qualified Codec.Compression.GZip as GZip

import qualified Data.List as L
import qualified Data.ByteString.Lazy as LBS

import qualified Wikilon.Base16 as B16
import Wikilon.Time
import Wikilon.Secret
import Wikilon.DictST
import Wikilon.DictTX
import Wikilon.Database

-- | The Wiki type for use with acid-state. There may be more than
-- one version, eventually, but I'll try to anticipate extensions.
type Wiki = Wiki0

data Dictionary = Dict 
    { _dictHist    :: ![DictTX]             -- primary content
    , _dictSize    :: {-# UNPACK #-} !Int   -- cached: length of history list
    , _dictState   :: !DictST               -- cached: current state of dictionary
    }


data Wiki0 = Wiki0
    { _secret   :: !Secret
    , _genSym   :: !Integer
    , _time     :: !T
    , _fullDict :: !Dictionary

    -- TODO:
    --   users
    --   auxillary state for multi-user services (e.g. IRC)
    --   logs and reports? likely embedded in auxillary state
    --   overrides for web-apps? try to embed in auxillary state
    --   cached computations? maybe separated from main Wiki? non-acidic
    }

-- The Wikilon decay rate is 10% per generation, but protecting the
-- first two tenths and cutting 1/8th of the last 8 tenths. This gives
-- a lot of extra stability to recent historical views of the database.
--
-- The relevant question, then, is how much history to keep. The half
-- life, and a lot of startup overhead, ultimately depends on the value
-- selected here. My intuition is that a history of 1000-2000 samples 
-- is suitable for most use cases. 
--
-- For now, I'll err on the side of excess.
-- 
defaultDecay :: Decay DictTX
defaultDecay = Decay
    { decay_merge = mergeHeuristicDTX
    , decay_freq = 8 
    , decay_keep = 400
    }
defaultDictHist :: Int
defaultDictHist = 2000

instance SC.SafeCopy Wiki0 where
    errorTypeName _ = "Wikilon.Wiki:Wiki0"
    version = 0
    kind = SC.base
    getCopy = SC.contain getWiki0
    putCopy = SC.contain . putWiki0

-- serialize the essential wiki content.
putWiki0 :: Wiki0 -> C.PutM ()
putWiki0 w = do
    C.put 'W'
    putTime (_time w)
    putSecret (_secret w)
    putGenSym (_genSym w)
    putDictZ (_dictHist (_fullDict w))
    putFakeStateZ -- hole for auxillary state
    putFakeUsersZ -- hole for user data

putSecret :: Secret -> C.PutM ()
putSecret s = C.put '$' >> C.put s

putGenSym :: Integer -> C.PutM ()
putGenSym n = C.put '#' >> C.put n

putTime :: T -> C.PutM ()
putTime = C.put . show

-- save a dictionary history in a compressed format. 
putDictZ :: [DictTX] -> C.PutM ()
putDictZ txs = C.put 'D' >> putZBytes (encodeDict txs)

-- save auxillary state (expect strong benefits from compression)
-- save user states (likely in a compressed format)
putFakeStateZ, putFakeUsersZ :: C.PutM ()
putFakeStateZ = C.put 'S' >> putZBytes (LBS.empty)
putFakeUsersZ = C.put 'U' >> putZBytes (LBS.empty)

getSecret :: C.Get Secret
getSecret = C.label "Secret" $ C.expect '$' >> C.get

getGenSym :: C.Get Integer
getGenSym = C.label "GenSym" $ C.expect '#' >> C.get

-- avoid conflict with original 'getTime'
loadTime :: C.Get T
loadTime = C.label "Time" $ 
    C.get >>= \ s ->
    case parseTime s of
        Nothing -> fail ("could not parse " ++ s ++ " as time.")
        Just t -> return t

getDictZ :: C.Get [DictTX]
getDictZ = C.label "Dict" $
    C.expect 'D' >>
    getZBytes >>= \ bytes ->
    case decodeDict bytes of
        Left _emsg -> fail _emsg
        Right txs  -> return txs

-- load user information and auxillary state
getFakeUsersZ, getFakeStateZ :: C.Get ()
getFakeStateZ = C.label "State" $ C.expect 'S' >> getZBytes >> return ()
getFakeUsersZ = C.label "Users" $ C.expect 'U' >> getZBytes >> return ()

-- 
getWiki0 :: C.Get Wiki0
getWiki0 = C.label "Wiki" $ do
    C.expect 'W'
    time <- loadTime
    secret <- getSecret
    gensym <- getGenSym
    txs <- getDictZ
    (_hist, dictST) <- runE (loadDict txs)
    _state <- getFakeStateZ
    _users <- getFakeUsersZ
    return $ Wiki0 
        { _secret   = secret
        , _time     = time
        , _genSym   = gensym
        , _fullDict = Dict 
                { _dictHist = txs
                , _dictSize = L.length txs
                , _dictState = dictST
                }
        }


-- Build a complete dictionary history, returning a list of states 
-- corresponding to the list of transactions, and a final state.
--
-- The intermediate states will be useful for initializing users -
-- their cached view of the dictionary.
--
-- The deep history will be useful for processing users, who must
-- obtain a near temporal snapshot. 
loadDict :: [DictTX] -> Either String ([DictST], DictST)
loadDict = (flip runStateT) emptyDictST . mapM addTx where
    addTx tx = StateT $ \ st ->
        updateDict tx st >>= \ st' ->
        return (st',st')

runE :: (Monad m) => Either String a -> m a
runE (Left _emsg) = fail _emsg
runE (Right a) = return a


-- | compress and put bytes, using both Base16 and GZip compression.
putZBytes :: LBS.ByteString -> C.PutM ()
putZBytes = C.put . gzc . b16c where
    gzc = GZip.compress
    b16c = LBS.pack . B16.compress . LBS.unpack

-- | decompress bytes, from both GZip and Base16 compression.
getZBytes :: C.Get LBS.ByteString
getZBytes = (b16x . gzx) <$> C.get where
    gzx = GZip.decompress
    b16x = LBS.pack . B16.decompress . LBS.unpack
