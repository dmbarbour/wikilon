
-- | Create an automatically maintained time variable for access via
-- STM. This will update after it is read, but may hold constant for
-- several transactions.
module Wikilon.Store.TimeVar
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
readTimeVar (TimeVar vTime bRead) = do
    writeTVar bRead True -- signals update after transaction 
    t <- readTVar vTime -- obtain the time, which may be lazy
    return $! t -- return approximate time of read

-- | Create a new time variable that provides time to an STM transaction.
-- This is automatically maintained by a background thread, and updates
-- only between transactions that read it (hence is no more active than
-- its readers).
newTimeVar :: IO TimeVar
newTimeVar = do
    vTime <- newTVarIO minBound
    bRead <- newTVarIO True
    let updateTimeVar = do
            tUpd <- unsafeInterleaveIO getTime
            atomically $ do
                check =<< readTVar bRead -- update after reading
                writeTVar vTime tUpd -- update the current time
                writeTVar bRead False -- wait for another read
    updateTimeVar -- initialize the time variable
    void $ forkIO $ forever updateTimeVar -- maintain in future
    return (TimeVar vTime bRead)
