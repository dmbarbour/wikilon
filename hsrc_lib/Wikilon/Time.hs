
-- | Wikilon timestamp
--
-- Representation: Int64
-- Precision: 100ns 
-- Zero: Unix epoch
-- Range: ~29k years
-- 
module Wikilon.Time
    ( T(..), DT(..)
    , getTime, parseTime
    , addTime, subtractTime, diffTime
    , fromUTC, toUTC
    ) where

import Control.Applicative
import Data.Int (Int64)
import Data.Ratio (numerator,denominator)
import qualified Data.Time.Clock as Time
import qualified Data.Time.Calendar as Time
import qualified Data.Time.Format as Time
import qualified System.Locale as Time 
import Data.Function (on)
-- import Control.Exception (assert)
import qualified Data.Decimal as Dec

-- | Timestamp in Wikilon
newtype T = T Int64 deriving (Eq, Ord)

instance Bounded T where
    minBound = T minBound 
    maxBound = T maxBound 

dayLen, secLen :: Int64
dayLen = 86400 * secLen
secLen = 1000 * 1000 * 10  -- unit is 10^-7s, 0.1µs, 100ns

-- difference between Modified Julian Day and Unix Epoch
dayZero :: Int64
dayZero = 40587

toPicos :: Int64 -> Integer
toPicos dt = fromIntegral $ dt * 100000
    -- 100ns per time unit * 1000ps per ns

-- | Estimate of current time from operating system.
getTime :: IO T
getTime = fromUTC <$> Time.getCurrentTime

fromUTC :: Time.UTCTime -> T
fromUTC utc = T (timeDays + timeOfDay) where
    days = Time.toModifiedJulianDay (Time.utctDay utc)
    secs = toRational (Time.utctDayTime utc)
    timeDays = dayLen * (fromIntegral days - dayZero)
    timeOfDay = round $ (secs * fromIntegral secLen)

toUTC :: T -> Time.UTCTime
toUTC (T t) = Time.UTCTime  where
    (d,q) = t `divMod` dayLen
    days = Time.ModifiedJulianDay (fromIntegral $ d + dayZero)
    dt = Time.picosecondsToDiffTime (toPicos ps)

-- | DT - a representation of a difference in two times
-- Conversion functions treat as seconds. Precision is
-- fixpoint 0.1µs.
newtype DT = DT Int64 deriving (Eq, Ord)

-- | Add a difference in time to an absolute time.
addTime :: T -> DT -> T
addTime (T tm) (DT dt) = T (tm + dt)

-- | Subtract a difference in time from an absolute time
subtractTime :: T -> DT -> T
subtractTime (T tm) (DT dt) = T (tm - dt)

-- | Find the difference in time, diffTime a b = a - b
diffTime :: T -> T -> DT
diffTime (T tf) (T t0) = DT (tf - t0)

instance Num DT where
    (+) (DT a) (DT b) = DT (a + b)
    (-) (DT a) (DT b) = DT (a - b)
    (*) (DT a) (DT b) = DT ab where
        prod = (toInteger a * toInteger b) 
        lsec = toInteger secLen
        (dt,q) = prod `divMod` lsec
        c = if q > (lsec `div` 2) then 1 else 0
        ab = fromIntegral $ dt + c 
    negate (DT a) = DT (negate a)
    abs (DT a) = DT (abs a)
    signum (DT a) = signum a
    fromInteger = DT . (*) secLen . fromInteger

-- 'Fractional' is primarily for the 'fromRational' 
-- numeric conversions in seconds.
instance Fractional DT where
    (/) a b = picosToDt (q + c) 
        where pa = dtToPicos a
              pb = dtToPicos b
              (q,r) = (pa * picosInSec) `divMod` pb  -- 
              c = if (2 * r > pb) then 1 else 0  -- carry
    recip = (1 /) 
    fromRational rat = picosToDt (q + c)
        where (q,r) = (numerator rat * picosInSec) `divMod` denominator rat
              c = if (2 * r > denominator rat) then 1 else 0

-- show time for humans
instance Show T where
    -- basically iso8601, but with picosecond precision
    -- YYYY-MM-DDTHH:MM:SS.999999999999Z
    showsPrec _ = showString . Time.formatTime t_locale t_format . toUTC where

t_format :: String
t_format = "%Y-%m-%dT%H:%M:%S%QZ"

t_locale :: Time.TimeLocale
t_locale = Time.defaultTimeLocale

parseTime :: String -> Maybe T
parseTime = fmap fromUTC . Time.parseTime t_locale t_format

-- show difference in time for humans
instance Show DT where
    showsPrec _ dt = shows val . showString unit where
        val = Dec.normalizeDecimal $ Dec.Decimal places ps
        ps = dtToPicos dt
        pp = abs ps
        (places,unit) = 
            if (pp < 1000) then (0,"ps") else
            if (pp < 1000*1000) then (3,"ns") else
            if (pp < 1000*1000*1000) then (6,"µs") else
            if (pp < 1000*1000*1000*1000) then (9,"ms") else
            (12,"s")
