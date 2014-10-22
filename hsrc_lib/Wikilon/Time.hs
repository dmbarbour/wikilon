
-- | A strict, unpacked, fixpoint representation of time, with 
-- picosecond precision; packs to 96 bits. Basically, a more 
-- efficient representation of UTCTime from Data.Time.Clock
-- (without any support for leap seconds...)
module Wikilon.Time
    ( T
    , tmDay,tmPicos
    , mkTime,timeFromDays
    , getTime, parseTime
    , DT
    , dtToPicos,picosToDt
    , addTime,subtractTime,diffTime
    , fromUTC, toUTC
    ) where

import Data.Int (Int32,Int64)
import Data.Ratio (numerator,denominator)
import qualified Data.Time.Clock as Time
import qualified Data.Time.Calendar as Time
import qualified Data.Time.Format as Time
import qualified System.Locale as Time 
import Data.Function (on)
-- import Control.Exception (assert)
import qualified Data.Decimal as Dec

-- | T - a fixpoint representation of time UTC with picosecond
-- precision, as a pair of integers. Time in Sirea is modeled as  
-- continuous, but the actual implementation is limited precision.
--    tmDay   - Modified Julian Day (days since Nov 17, 1858)
--    tmPicos - Picoseconds in the day. [0,86400*10^12)
-- Simplified. Strict. No leap seconds. Limited range, just over
-- plus or minus five million years. 
--
-- The choice of picoseconds is that it's the same as UTCTime. It is
-- also a lot more precision than we're ever likely to need; light 
-- in a vacuum travels only about 0.3mm in a picosecond.
-- 
-- Construct via mkTime, fromUTC, or getTime. 
data T = T {-# UNPACK #-} !Int32 {-# UNPACK #-} !Int64
    deriving (Eq, Ord)

_tmDay :: T -> Int32
_tmDay (T d _) = d

_tmPicos :: T -> Int64
_tmPicos (T _ p) = p

tmDay :: T -> Integer
tmDay = toInteger . _tmDay

tmPicos :: T -> Integer
tmPicos = toInteger . _tmPicos

instance Bounded T where
    minBound = T minBound 0
    maxBound = T maxBound (fromInteger (picosInDay - 1))

-- | `mkTime days picos`
-- smart constructor for time 
mkTime :: Integer -> Integer -> T
mkTime days picos =
    let (q,p) = picos `divMod` picosInDay in
    let d = days + q in
    T (fromInteger d) (fromInteger p)

-- | timeFromDays will convert a Modified Julian Day, stored as a
-- rational, to a T value. 
timeFromDays :: Rational -> T
timeFromDays r = mkTime days (picos + carry)
    where (days,dayFrac) = numerator r `divMod` denominator r
          (picos,picoFrac) = (dayFrac * picosInDay) `divMod` denominator r
          carry = if (picoFrac * 2 > denominator r) then 1 else 0

-- | Obtain estimate of current time from operating system.
getTime :: IO T
getTime = Time.getCurrentTime >>= return . fromUTC

fromUTC :: Time.UTCTime -> T
fromUTC utc =
    let d = Time.toModifiedJulianDay (Time.utctDay utc)
        r = toRational (Time.utctDayTime utc)
        n = (numerator r * picosInSec) `div` denominator r
    in mkTime d n

toUTC :: T -> Time.UTCTime
toUTC (T d ps) = Time.UTCTime days dt where
    days = Time.ModifiedJulianDay (fromIntegral d)
    dt = Time.picosecondsToDiffTime (fromIntegral ps)

-- | DT - a representation of a difference in two times, accessible
--   as a distance in picoseconds. 
newtype DT = DT { unDT :: T } deriving (Eq, Ord)

dtToPicos :: DT -> Integer
dtToPicos (DT tm) = (picosInDay * tmDay tm) + tmPicos tm

picosToDt :: Integer -> DT
picosToDt = DT . mkTime 0 

-- | Add a difference in time to an absolute time.
addTime :: T -> DT -> T
addTime (T tD tP) (DT (T dtD dtP)) =
    let p = tP + dtP in
    if (p < ppid) then T (tD + dtD) p
                  else T (tD + dtD + 1) (p - ppid)

-- | Subtract a difference in time from an absolute time
subtractTime :: T -> DT -> T
subtractTime tm (DT dt) = unDT (diffTime tm dt)

-- | Find the difference in time, diffTime a b = a - b
diffTime :: T -> T -> DT
diffTime (T da pa) (T db pb) =
    if (pa < pb) then DT (T ((da - db) - 1) ((pa - pb) + ppid))
                 else DT (T (da - db) (pa - pb))

ppid :: Int64
ppid = fromInteger picosInDay

picosInDay, secondsInDay, picosInSec :: Integer
picosInDay = secondsInDay * picosInSec
secondsInDay = 24 * 60 * 60
picosInSec = 1000 * 1000 * 1000 * 1000

instance Num DT where
    (+) (DT a) b = DT (addTime a b)
    (-) = diffTime `on` unDT
    (*) a b = picosToDt (q + c)
        where pa = dtToPicos a
              pb = dtToPicos b
              (q,r) = (pa * pb) `divMod` picosInSec
              c = if (r > (picosInSec `div` 2)) then 1 else 0
    negate (DT a) = 
        if (_tmPicos a == 0) 
            then DT (T (negate (_tmDay a)) 0) 
            else DT (T (negate (_tmDay a) - 1) (ppid - _tmPicos a))
    abs (DT a) = if (_tmDay a < 0) then negate (DT a) else (DT a)
    signum (DT a) = 
        if (_tmDay a < 0) 
            then -1 
            else  1
    fromInteger = picosToDt . (*) picosInSec

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
    showsPrec _ dt | signum dt < 0 = showChar '-' . shows (negate dt)
    showsPrec _ dt = shows val . showString unit where
        val = Dec.normalizeDecimal $ Dec.Decimal places ps
        ps = dtToPicos dt
        (places,unit) = 
            if (ps < 1000) then (0,"ps") else
            if (ps < 1000*1000) then (3,"ns") else
            if (ps < 1000*1000*1000) then (6,"µs") else
            if (ps < 1000*1000*1000*1000) then (9,"ms") else
            (12,"s")
