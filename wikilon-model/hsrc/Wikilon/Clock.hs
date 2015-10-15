
-- | Reading time from within a transaction, without creating big 
-- synchronization bottlenecks, is a little tricky. For now, I'm
-- modeling this as a maintained time variable. A separate thread
-- updates it sometime after it is read by a transaction. Updates
-- are limited in frequency, too.
--
-- At the moment, this should be okay. Assuming Wikilon doesn't
-- read time too frequently. Use of `unsafePerformIO getTime` is
-- an alternative option if transactional properties aren't very 
-- relevant.
module Wikilon.Clock
    ( TimeVar
    , newTimeVar
    , readTimeVar
    ) where

import Control.Monad
import Control.Concurrent
import Control.Concurrent.STM
import System.IO.Unsafe (unsafeInterleaveIO)
import Wikilon.Time

data TimeVar = TimeVar !(TVar T) !(TVar Bool)

-- | Obtain an estimated current time. It is possible for several
-- concurrent transactions to read the same time. Between reads, the
-- time value will update. The returned time is actually computed 
-- when read (via unsafeInterleaveIO), so will be close to the first
-- read in a concurrent batch. 
--
-- Note: if a transaction fails after reading time, the time variable
-- will continue to hold the time of the attempted read. This is not
-- optimal, but also isn't a big problem in context of Wikilon.
readTimeVar :: TimeVar -> STM T
readTimeVar (TimeVar vTime vRead) = do
    bRead <- readTVar vRead
    unless bRead (writeTVar vRead True) 
    t <- readTVar vTime -- obtain the time, which may be lazy
    return $! t -- return approximate time of read

-- | Create a new time variable that provides time to an STM transaction.
-- This is automatically maintained by a background thread, and updates
-- only between transactions that read it (hence is no more active than
-- its readers). 
newTimeVar :: IO TimeVar
newTimeVar = do
    vTime <- newTVarIO minBound
    vRead <- newTVarIO True
    let updateTimeVar = do
            tUpd <- unsafeInterleaveIO getTime -- lazily get time
            atomically $ do
                check =<< readTVar vRead -- update after reading
                writeTVar vTime tUpd -- prepare to update current time
                writeTVar vRead False -- wait for another read
    updateTimeVar -- initialize the time variable
    let maintainTimeVar = updateTimeVar >> threadDelay tickLimit
    void $ forkIO $ forever maintainTimeVar
    return (TimeVar vTime vRead)

-- tickLimit is a tuning variable, a wait period after updating
-- the clock to limit the update frequency. For Wikilon, I can 
-- use a large tick limit because precise times don't matter.
tickLimit :: Int
tickLimit = 100 * 1000 {- microseconds -}

