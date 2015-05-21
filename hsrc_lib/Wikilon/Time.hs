{-# LANGUAGE DeriveDataTypeable, CPP #-}

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
    , formatTime, parseTimeFormat, defaultTimeFormat
    , addTime, subtractTime, diffTime
    , fromUTC, toUTC
    , dtToPicos

    , ceilingDT, floorDT, roundDT
    , ceilingSeconds, ceilingMinutes
    , floorSeconds, floorMinutes
    , roundSeconds, roundMinutes
    ) where

import Control.Applicative
import Data.Int (Int64)
import Data.Ratio (numerator,denominator)

import qualified Data.Time.Clock as Time
import qualified Data.Time.Calendar as Time
import qualified Data.Time.Format as Time
#if !(MIN_VERSION_time(1,5,0))
import qualified System.Locale
#endif
-- import Control.Exception (assert)
import qualified Data.Decimal as Dec
import Data.Typeable (Typeable)
import Database.VCache

-- | Timestamp in Wikilon
newtype T = T Int64 deriving (Eq, Ord, Typeable)

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

dtToPicos :: DT -> Integer
dtToPicos (DT dt) = toPicos dt

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
toUTC (T t) = Time.UTCTime days timeInDay where
    (d,q) = t `divMod` dayLen
    days = Time.ModifiedJulianDay (fromIntegral $ d + dayZero)
    timeInDay = Time.picosecondsToDiffTime (toPicos q)

-- | DT - a representation of a difference in two times
-- Conversion functions treat as seconds. Precision is
-- fixpoint 0.1µs.
newtype DT = DT Int64 deriving (Eq, Ord, Typeable)

-- | Add a difference in time to an absolute time.
addTime :: T -> DT -> T
addTime (T tm) (DT dt) = T (tm + dt)

-- | Subtract a difference in time from an absolute time
subtractTime :: T -> DT -> T
subtractTime (T tm) (DT dt) = T (tm - dt)

-- | Find the difference in time, diffTime a b = a - b
diffTime :: T -> T -> DT
diffTime (T tf) (T t0) = DT (tf - t0)

-- | Round T up to an incremental step based on a DT value.
-- (assumes a positive DT)
ceilingDT :: DT -> T -> T
ceilingDT (DT dt) (T tm) = (T tm') where
    (steps, extras) = tm `divMod` dt
    steps' = if (extras > 0) then steps + 1 else steps 
    tm' = steps' * dt

-- | Round T down to an incremental step based on a DT value.
-- (assumes a positive DT)
floorDT :: DT -> T -> T
floorDT (DT dt) (T tm) = (T tm') where
    tm' = (tm `div` dt) * dt

-- | Round T to a nearest incremental step based on a DT value.
-- (assumes a positive DT)
roundDT :: DT -> T -> T
roundDT (DT dt) (T tm) = (T tm') where
    (steps, extras) = tm `divMod` dt
    steps' = if ((extras * 2) >= dt) then steps + 1 else steps
    tm' = steps' * dt

-- | common truncation operations for time
ceilingSeconds, ceilingMinutes :: T -> T
floorSeconds, floorMinutes :: T -> T
roundSeconds, roundMinutes :: T -> T

ceilingSeconds = ceilingDT 1
ceilingMinutes = ceilingDT 60
floorSeconds = floorDT 1
floorMinutes = floorDT 60
roundSeconds = roundDT 1
roundMinutes = roundDT 60

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
    signum (DT a) | (a > 0) = 1
                  | (0 == a) = 0
                  | otherwise = -1
    fromInteger = DT . (*) secLen . fromInteger

-- 'Fractional' is primarily for the 'fromRational' 
-- numeric conversions in seconds.
instance Fractional DT where
    (/) (DT a) (DT b) = DT (fromInteger q + c) where
        ua = toInteger a
        ub = toInteger b
        lsec = toInteger secLen
        (q,r) = (lsec * ua) `divMod` ub
        c = if ((2 * r) > ub) then 1 else 0
    recip = (1 /) 
    fromRational rat = DT (fromInteger q + c) where
        lsec = toInteger secLen
        (q,r) = (numerator rat * lsec) `divMod` denominator rat
        c = if ((2 * r) > denominator rat) then 1 else 0

-- show time for humans
instance Show T where
    -- basically iso8601, but with subsecond precision
    -- YYYY-MM-DDTHH:MM:SS.9999999Z
    showsPrec _ = showString . formatTime defaultTimeFormat

defaultTimeFormat :: String
defaultTimeFormat = "%Y-%m-%dT%H:%M:%S%QZ"

parseTime :: String -> Maybe T
parseTime = parseTimeFormat defaultTimeFormat

formatTime :: String -> T -> String
formatTime sFormat = Time.formatTime t_locale sFormat . toUTC

-- Need to deal with the ugly transition from time-1.4.2 to time-1.5
-- (current `stackage lts-2.8` uses time-1.4.2)
#if MIN_VERSION_time(1,5,0)
t_locale :: Time.TimeLocale
t_locale = Time.defaultTimeLocale

parseTimeFormat :: String -> String -> Maybe T
parseTimeFormat sFormat = fmap fromUTC . Time.parseTimeM True t_locale sFormat
#else
t_locale :: System.Locale.TimeLocale
t_locale = System.Locale.defaultTimeLocale

parseTimeFormat :: String -> String -> Maybe T
parseTimeFormat sFormat = fmap fromUTC . Time.parseTime t_locale sFormat
#endif


-- show difference in time for humans
instance Show DT where
    showsPrec _ (DT dt) = shows val . showString unit where
        val = Dec.normalizeDecimal $ Dec.Decimal places ps
        ps = toPicos dt
        pp = abs ps
        (places,unit) = 
            if (pp < 1000) then (0,"ps") else
            if (pp < 1000*1000) then (3,"ns") else
            if (pp < 1000*1000*1000) then (6,"µs") else
            if (pp < 1000*1000*1000*1000) then (9,"ms") else
            (12,"s")

-- convenient persistence for time values
instance VCacheable T where
    get = T . fromInteger <$> get
    put (T tm) = put (toInteger tm)
instance VCacheable DT where
    get = DT . fromInteger <$> get
    put (DT dt) = put (toInteger dt)



