{-# LANGUAGE OverloadedStrings, ViewPatterns #-}

-- | A dictionary transaction represents a single, atomic update on
-- a dictionary. In Wikilon, we preserve history of a dictionary by
-- preserving a subset of transactions, occasionally merging them to
-- keep the space costs down (which eliminates intermediate values
-- for some definitions). 
--
-- 
module Wikilon.DictTX 
    ( DictTX(..), DTXMeta(..), DictUpd(..)
    , UpdateMap, RenameMap, AnnoWords
    , isUpdate, isDefine, isDelete
    , mergeUpdate, addUpdate
    , renameDTX, deepRenameDTX, mergeRenameMaps
    , mergeDTX, mergeHeuristicDTX, heuristicDTX
    , mapDictUpd
    , putDictTX, getDictTX
    , encodeDict, decodeDict
    ) where

--  serialization of transactions (Binary instance)

import Control.Monad (foldM,(>=>))
import Control.Arrow ((***))
import Control.Applicative ((<|>))
import qualified Data.List as L
import Data.Ratio

import qualified Data.ByteString.Lazy as LBS
import qualified Data.Binary as B
import qualified Data.Binary.Put as B
import qualified Data.Binary.Get as B
import qualified Codec.Binary.UTF8.Generic as UTF8
import qualified Wikilon.ParseUtils as P

import Wikilon.WordMap (WordMap)
import Wikilon.WordSet (WordSet)
import qualified Wikilon.WordMap as WM
import qualified Wikilon.WordSet as WS

import Wikilon.Time
import Wikilon.AO


-- | A transaction on a dictionary has some metadata and a few words
-- to update. Each word can have at most one update. Note that rename
-- applies strictly to words defined *before* this transaction.
data DictTX = DictTX
    { dtx_meta    :: !DTXMeta
    , dtx_rename  :: !RenameMap
    , dtx_update  :: !UpdateMap
    }
type RenameMap = WordMap Word
type UpdateMap = WordMap DictUpd

-- | Transaction metadata includes a little information about who,
-- when, where, why, and how many transactions have been performed.
--
-- Time gets special handling. But other metadata will have a simple
-- representation: a set of words. These words can represent some 
-- ad-hoc content, e.g. `user:dave` might be a describing a user
-- who performed the edit. And `project:awelon` might describe the
-- project associated with the edit. These words may be renamed or
-- deleted, and must exist in the dictionary (either before or after
-- the given transaction).
--
data DTXMeta = DTXMeta
    { dtx_tm_ini :: {-# UNPACK #-} !T -- when (start)
    , dtx_tm_fin :: {-# UNPACK #-} !T -- when (last merged)
    , dtx_count  :: {-# UNPACK #-} !Int -- number of merged transactions
    , dtx_anno   :: !AnnoWords -- words for who, where, why, etc.
    }
type AnnoWords = WordSet

-- | Dictionaries have a simple set of update operations:
-- 
--    define a new word
--    update an existing word
--    delete an existing word
--
-- The distinction between 'define' and 'update' exists so we can
-- halt propagation of 'rename' and 'delete' operations. To define a
-- word that already exists is an error. With only define, update, 
-- and delete, transactions have a nice property: idempotence. 
-- 
data DictUpd 
    = Define AO_Code    -- @define word code
    | Update AO_Code    -- @update word code
    | Delete            -- @delete word

-- Note: @append was considered and rejected.
-- * Pros: extend a definition for bulky word without repeating it
-- * Cons: lose idempotence, inflexible update, no strong use case

mapDictUpd :: (AO_Code -> AO_Code) -> DictUpd -> DictUpd
mapDictUpd f (Define ao) = Define (f ao)
mapDictUpd f (Update ao) = Update (f ao)
mapDictUpd _ Delete = Delete

isDefine :: DictUpd -> Bool
isDefine (Define _) = True
isDefine _ = False

isUpdate :: DictUpd -> Bool
isUpdate (Update _) = True
isUpdate _ = False

isDelete :: DictUpd -> Bool
isDelete Delete = True
isDelete _ = False

-- | mergeUpd older newer - compose or cancel two updates.
--     Define Delete → cancel
--     Define Update → Define (updated)
--     Delete Define → Update
mergeUpdate :: DictUpd -> DictUpd -> Maybe DictUpd
mergeUpdate (Define _) Delete = Nothing
mergeUpdate (Define _) (Update ao) = Just (Define ao)
mergeUpdate (Delete) (Define ao) = Just (Update ao)
mergeUpdate _ upd = Just upd

-- | Add an update to an atomic collection of updates.
-- Will merge with any existing update on same word.
addUpdate :: Word -> DictUpd -> UpdateMap -> UpdateMap
addUpdate w u m = 
    case WM.lookup w m of
        Nothing -> WM.insert w u m 
        Just u0 -> case mergeUpdate u0 u of
                      Nothing -> WM.delete w m
                      Just u' -> WM.insert w u' m

-- | Applying rename operations is one of the more challenging 
-- features for dictionary transactions. We must:
--
--   * rename words in our annotations
--   * rename mentions of a word in our dictionary
--   * subtract newly defined words from our rename map
--   * carefully compose rename maps
--
-- It is possible to rename foo→bar, bar→baz, baz→foo in one step.
-- So, the only option is to rename all the words atomically.
--
renameDTX :: RenameMap -> DictTX -> DictTX
renameDTX renameMap dtx =
    if WM.null renameMap then dtx else
    let rn w = maybe w id $ WM.lookup w renameMap in
    -- rename the annotations
    let meta = dtx_meta dtx in
    let anno' = WS.map rn (dtx_anno meta) in
    let meta' = meta { dtx_anno = anno' } in
    -- rename words within the updates!
    let rnu  = rn *** mapDictUpd (aoMapWords rn) in
    let update' = WM.fromList $ fmap rnu $ WM.toList $ dtx_update dtx in
    -- filter newly defined words from renameMap
    let newWords = WM.keys $ WM.filter isDefine update' in
    let renameMap' = L.foldl' (flip WM.delete) renameMap newWords in
    -- compose a new rename function.
    let rename' = mergeRenameMaps (dtx_rename dtx) renameMap' in
    -- generate the new transaction
    DictTX { dtx_meta = meta'
           , dtx_rename = rename'
           , dtx_update = update'
           }

-- | mergeRenameMaps old new; combine two rename maps!
-- 
-- The basic idea here is that we can collapse a sequence of renames.
-- For example, if we yesterday renamed foo to bar, and today we rename
-- bar to baz, we can collapse this to a single rename from foo to baz.
--
--    old map:  fromList [(foo,bar)]
--    new map:  fromList [(bar,baz)]
--    result:   fromList [(foo,baz)]
--
-- Of course, maps are only indexed on the first element. The only
-- way to use the index here is to search the new map for 'bar'.
--
-- It's possible a map contains (foo,bar) and (bar,baz), indicating
-- two words change at the same time. This results from (bar,baz)
-- performed before (foo,bar). It should work without changes, since
-- renameDTX applies only a single step. Even cycles are possible:
--
--    fromList [(baz,bazOld),(bar,baz),(foo,bar)]
--    fromList [(bazOld,foo)]
--    --------------
--    fromList [(baz,foo),(foo,bar),(bar,baz)]
--
-- Also, it's possible that yesterday we renamed foo to bar, and
-- today we renamed bar back to foo (changed our mind!). In that
-- case, we want to eliminate the identity rename (foo,foo).
--
mergeRenameMaps :: RenameMap -> RenameMap -> RenameMap
mergeRenameMaps old new =
    if WM.null new then old else
    L.foldl' mrn1 new (WM.toList old)

mrn1 :: RenameMap -> (Word,Word) -> RenameMap
mrn1 newMap (foo,bar) =
    case WM.lookup bar newMap of
        Nothing ->  WM.insert foo bar newMap
        Just baz ->
            let m' = WM.delete bar newMap in
            if (foo == baz) then m' else
            WM.insert foo baz m'

-- | deepRenameDTX will take a full database and apply all renames, 
-- and then return any leftover rename operations. If this is a
-- valid and complete dictionary, the leftover map will be empty.
--
-- It is unclear whether this offers any performance advantages over
-- the more incremental processes. deepRenameDTX doesn't leverage
-- any sort of reverse lookup index.
--
deepRenameDTX :: [DictTX] -> ([DictTX], RenameMap)
deepRenameDTX = dr [] WM.empty where
    dr z rn (t:ts) =
        let tRn = renameDTX rn t in 
        let rn' = dtx_rename tRn in
        let t' = tRn { dtx_rename = WM.empty } in
        t' `seq` dr (t':z) rn' ts
    dr z rn [] = (L.reverse z, rn)


-- | mergeDTX old new; compose two transactions into a third.
--
-- To merge two transactions, we'll first apply any name updates,
-- then combine the updates and annotations. A word that is both
-- defined and deleted will be fully removed at this point, and 
-- also removed from the set of annotations. 
mergeDTX :: DictTX -> DictTX -> DictTX
mergeDTX old new = fst $ mergeHeuristicDTX old new

-- | For Wikilon, I'm aiming for the following characteristics:
--
--   1) prefer to preserve many values for every word
--      Therefore: subtract 1 for each overlapping word in update.
--
--   2) prefer to merge updates from a single user or project
--      Assumption: user or project uses common annotations
--      Therefore: subtract 1 for each difference in annotations
--                 add 1 for each overlap in annotations
--
--   3) weakly prefer history be sampled evenly over real time
--      Therefore: subtract 1 for every 12 hours in scope
--
--   4) weakly prefer history be clustered evenly by event count
--      Therefore: subtract 1 for every 10 transactions in scope
--
-- The merge heuristic weakly adjusts which merges we favor in each
-- decay group, theoretically resulting in a smooth and interesting
-- history.
--
heuristicDTX :: DictTX -> DictTX -> Int
heuristicDTX old new = snd $ mergeHeuristicDTX old new

-- heuristic, assuming that no rename is needed.
heuristicDTX' :: DictTX -> DictTX -> Int
heuristicDTX' old new = score where
    score = wordScore + annoScore + timeScore + countScore
    wordScore = 
        let m1 = dtx_update old in
        let m2 = dtx_update new in
        negate $ WM.size $ WM.intersection m1 m2
    annoScore =
        let s1 = dtx_anno (dtx_meta old) in
        let s2 = dtx_anno (dtx_meta new) in
        let sn = WS.intersection s1 s2 in
        -- subtract overlap twice, but add thrice 
        (3 * WS.size sn) - (WS.size s1 + WS.size s2)
    countScore = 
        let ct1 = dtx_count (dtx_meta old) in
        let ct2 = dtx_count (dtx_meta new) in
        let ctn = ct1 + ct2 in
        negate $ max 0 (ctn `div` 10)
    timeScore = 
        let tm_ini = dtx_tm_ini (dtx_meta old) in
        let tm_fin = dtx_tm_fin (dtx_meta new) in
        let dt = (tm_fin `diffTime` tm_ini) in
        let hours = dtToPicos dt `div` picosInHour in
        fromInteger $ negate $ max 0 (hours `div` 12)
    

picosInHour :: Integer
picosInHour = picosPerSec * secsPerHour where
    picosPerSec = 1000 * 1000 * 1000 * 1000
    secsPerHour = 3600 
    
-- | Compute both a merged value and heuristic together, in the
-- format required by Wikilon.Database.
mergeHeuristicDTX :: DictTX -> DictTX -> (DictTX, Int)
mergeHeuristicDTX old new = (m, h) where
    old' = renameDTX (dtx_rename new) old
    new' = new { dtx_rename = WM.empty }
    m = mergeDTX'     old' new'
    h = heuristicDTX' old' new'

-- same as mergeDTX, but assumes that the new transaction does not
-- rename anything, and thus that names are consistent.
mergeDTX' :: DictTX -> DictTX -> DictTX
mergeDTX' old new = dtx' where
    dtx' = DictTX 
                { dtx_meta   = meta'
                , dtx_rename = rename'
                , dtx_update = update' 
                }
    meta' = DTXMeta
                { dtx_tm_ini = tm_ini'
                , dtx_tm_fin = tm_fin'
                , dtx_anno   = anno'
                , dtx_count  = count'
                }
    tm_ini' = min (dtx_tm_ini oldMeta) (dtx_tm_ini newMeta)
    tm_fin' = max (dtx_tm_fin oldMeta) (dtx_tm_fin newMeta)
    overflow = (maxBound - dtx_count oldMeta) <= dtx_count newMeta
    count'  | overflow = maxBound
            | otherwise = dtx_count oldMeta + dtx_count newMeta
    anno'    = WS.filter (not . lostWord) baseAnno
    baseAnno = dtx_anno oldMeta `WS.union` dtx_anno newMeta
    lostWord w = maybe False isDefine (WM.lookup w oldUpd) -- was defined
              && maybe False isDelete (WM.lookup w newUpd) -- now deleted
    rename' = dtx_rename old
    update' = L.foldl' addUpd oldUpd (WM.toList newUpd)
    addUpd m (w,u) = addUpdate w u m
    oldMeta = dtx_meta old
    newMeta = dtx_meta new
    oldUpd  = dtx_update old
    newUpd  = dtx_update new

-- serialization of DictTX
instance B.Binary DictTX where
    put = putDictTX
    get = getDictTX

instance Show DictTX where 
    show = UTF8.toString . B.encode
    showList = showString . UTF8.toString . encodeDict

-- | Output the dictionary transaction into a bytestring. 
--
-- A transaction consists of a list of actions indicated by '@' at a
-- new line, finally terminated by `@@`. For example:
--
--    @t0 "2014-10-21T18:34:31Z"
--    @tf "2014-10-21T18:34:31Z"
--    @ct 1
--    @an project:awelon 
--    @an user:david 
--    @rename wierd weird
--    @define bar 1 2 foo
--    @define foo +
--    @@
--
-- Every action parses as AO code, but has structural requirements.
-- E.g. @rename is followed by exactly two words. 
--
-- Transactions are serialized as UTF-8 text to make them easy for 
-- users to grok or eyeball for correctness. In practice, they'll 
-- be transmitted or stored in a compressed format.
--
putDictTX :: DictTX -> B.PutM ()
putDictTX dtx = mapM_ putAction allActions >> eotx where
    allActions = meta ++ renames ++ updates
    meta    = metaActions (dtx_meta dtx)
    renames = renameActions (dtx_rename dtx)
    updates = updateActions (dtx_update dtx) 
    eotx = B.put '@' >> B.put '@'
    putAction (command,code) = 
        B.put '@' >> 
        putAO' ((AO_Word command) : ao_code code) >> 
        B.put '\n'

type TXAction = (Word,AO_Code) -- command word & content
type TXActions = [TXAction]

metaActions :: DTXMeta -> TXActions
metaActions meta = actions where
    actions = ("t0", tmIni)
            : ("tf", tmFin)
            : ("ct", count)
            : annotations 
    tmIni = aoTime (dtx_tm_ini meta)
    tmFin = aoTime (dtx_tm_fin meta)
    count = aoNum (dtx_count meta)
    annotations = fmap toAN $ WS.toSortedList (dtx_anno meta) 
    toAN w = ("an", AO_Code [AO_Word w])

renameActions :: RenameMap -> TXActions
renameActions = fmap (uncurry toRNA) . WM.toSortedList where
    toRNA wa wb = ("rename", AO_Code [AO_Word wa, AO_Word wb])

updateActions :: UpdateMap -> TXActions
updateActions = fmap (uncurry toUPD) . WM.toSortedList where
    toUPD w (Define ao) = ("define", c w ao)
    toUPD w (Update ao) = ("update", c w ao)
    toUPD w Delete      = ("delete", AO_Code [AO_Word w])
    c w (AO_Code ao) = AO_Code (AO_Word w : ao)

aoStr :: String -> AO_Code
aoStr = AO_Code . (:[]) . AO_Text

aoTime :: T -> AO_Code
aoTime = aoStr . show

aoNum :: Int -> AO_Code
aoNum = AO_Code . (:[]) . AO_Num . fromIntegral

-- | Read a transaction from binary.
--
-- A transaction will be rejected if there is more than one update
-- on a single word, if any metadata is specified twice, or if any
-- word is renamed twice. Within these constraints, actions commute
-- within each transaction, though we should maintain deterministic
-- order for replication or naming purposes.
--
-- A transaction is rejected if an action is not recognized. While
-- transactions are feasibly extensible, it's important to know 
-- what every action means.
--
getDictTX :: B.Get DictTX
getDictTX =
    getTXActions >>= \ txs ->
    case buildTransaction txs of
        Left emsg -> fail emsg
        Right dtx -> return dtx

-- | Read the current set of actions. Each action must start with
-- '@' then read as AO. There isn't a particular requir
getTXActions :: B.Get TXActions
getTXActions = loop [] where
    loop ra = P.char '@' >> (eotx ra <|> next ra) 
    eotx ra = P.char '@' >> return (L.reverse ra)
    next ra = getAO' False >>= proc ra
    proc ra (True, (AO_Word w : ao)) = loop ((w, AO_Code ao):ra)
    proc _ra (False, ao) = fail $ "lacks terminal LF `@" ++ sc ao
    proc _ra (True, ao) = fail $ "invalid action word `@" ++ sc ao   
    sc = showCut 32

showCut :: (Show a) => Int -> a -> String
showCut n a =
    let (hd,tl) = L.splitAt n (show a) in
    if (L.null tl) then hd else hd ++ "…"

-- build a transaction from a set of actions, or report an error.
-- this is very ugly code. It uses a bunch of sentinel values, and
-- brute forces the parse.
buildTransaction :: TXActions -> Either String DictTX
buildTransaction = foldM (flip bt) dtx0 >=> validate where
    meta0 = DTXMeta { dtx_tm_ini = minBound, dtx_tm_fin = minBound
                    , dtx_count = minBound, dtx_anno = WS.empty }
    dtx0 = DictTX { dtx_meta = meta0, dtx_rename = WM.empty
                  , dtx_update = WM.empty }
    err = Left
    validate dtx =
        -- currently only va metadata
        let meta = dtx_meta dtx in
        let badT0 = (minBound == dtx_tm_ini meta) in
        let badTF = (minBound == dtx_tm_fin meta) in
        let badDT = (dtx_tm_ini meta > dtx_tm_fin meta) in
        let badCT = (dtx_count meta < 0) in
        if badT0 then err "t0 undefined or invalid" else
        if badTF then err "tf undefined or invalid" else
        if badDT then err "invalid timing; t0 > tf" else
        if badCT then err "ct undefined or invalid" else
        return dtx
    bt :: TXAction -> DictTX -> Either String DictTX
    bt ("t0",rdTime -> Just t0) = um $ \ m -> 
        let dupT0 = (minBound /= dtx_tm_ini m) in
        if dupT0 then err "t0 defined twice" else
        return $ m { dtx_tm_ini = t0 }
    bt ("tf",rdTime -> Just tf) = um $ \ m -> 
        let dupTF = (minBound /= dtx_tm_fin m) in
        if dupTF then err "tf defined twice" else
        return $ m { dtx_tm_fin = tf }
    bt ("ct",rdCount -> Just ct) = um $ \ m -> 
        let dupCT = (minBound /= dtx_count m) in
        if dupCT then err "ct defined twice" else
        return $ m { dtx_count = ct }
    bt ("an",rdWord -> Just an) = um $ \ m ->
        let dupAN = WS.member an (dtx_anno m) in
        if dupAN then err (show an ++ " annotated twice") else
        let anno' = WS.insert an (dtx_anno m) in
        return $ m { dtx_anno = anno' }
    bt ("rename", rdWord2 -> Just (foo,bar)) = \ dtx ->
        let dupRN = WM.member foo (dtx_rename dtx) in
        if dupRN then err (show foo ++ " renamed twice") else
        let rn' = WM.insert foo bar (dtx_rename dtx) in
        return $ dtx { dtx_rename = rn' }
    bt ("define", rdUpdate -> Just (w,ao)) = upd w (Define ao)
    bt ("update", rdUpdate -> Just (w,ao)) = upd w (Update ao)
    bt ("delete", rdWord -> Just w) = upd w Delete
    bt (w, AO_Code ao) = const $ 
        let actionAO = AO_Code $ AO_Word w : ao in
        err $ "unrecognized action: @" ++ show actionAO
    -- helper functions
    um fn dtx = 
        fn (dtx_meta dtx) >>= \ m' -> 
        return $ dtx { dtx_meta = m' }
    upd w v dtx =
        let dupUPD = WM.member w (dtx_update dtx) in
        if dupUPD then err (show w ++ " updated twice") else
        let upd' = WM.insert w v (dtx_update dtx) in
        return $ dtx { dtx_update = upd' }

-- simple AO structure readers
rdTime :: AO_Code -> Maybe T
rdTime (AO_Code [AO_Text t]) = parseTime t
rdTime _ = Nothing

rdWord :: AO_Code -> Maybe Word
rdWord (AO_Code [AO_Word w]) = Just w
rdWord _ = Nothing

rdWord2 :: AO_Code -> Maybe (Word,Word)
rdWord2 (AO_Code [AO_Word a, AO_Word b]) = Just (a,b)
rdWord2 _ = Nothing

rdUpdate :: AO_Code -> Maybe (Word,AO_Code)
rdUpdate (AO_Code (AO_Word w : ao')) = Just (w, AO_Code ao')
rdUpdate _ = Nothing

rdCount :: AO_Code -> Maybe Int
rdCount (AO_Code [AO_Num r]) | ok = Just n' where
    ok = (1 == denominator r) && (n > 0)
    n = numerator r
    overflow = n > toInteger (maxBound :: Int)
    n' | overflow = maxBound
       | otherwise = fromInteger n
rdCount _ = Nothing

-- | encode a list of transactions into a lazy ByteString.
-- Each transaction is separated by a newline.
encodeDict :: [DictTX] -> LBS.ByteString
encodeDict = B.runPut . en where
    en (x:xs) = putDictTX x >> more xs
    en [] = return ()
    more xs | L.null xs = return ()
            | otherwise = B.put '\n' >> en xs

-- | decode a list of transactions from a lazy ByteString.
-- Note that this will always parse to the end of input.
decodeDict :: LBS.ByteString -> Either String [DictTX]
decodeDict = tryDecode where
    tryDecode bs = case B.runGetOrFail dc bs of
        Left (_,_,emsg) -> Left emsg
        Right (_,_,txs) -> Right txs
    dc = done [] <|> next []
    next r = 
        getDictTX >>= \ tx ->
        let r' = (tx:r) in
        (done r' <|> (P.char '\n' >> next r'))
    done r = P.eof >> return (L.reverse r)

