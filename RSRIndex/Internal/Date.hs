module RSRIndex.Internal.Date where

import Data.List
import Data.Time
import Data.Time.Format
import Statistics.Regression
import Statistics.Matrix.Types
import qualified Data.Vector.Unboxed as U
import qualified Data.Map as M
import qualified Data.Text as T hiding (foldr)
import RSRIndex.Types

getDateLimitsFromList :: Asset a => [(AssetSale a, AssetSale a)] -> (UTCTime , UTCTime)
getDateLimitsFromList [] = (read "9999-12-31 23:59:59.999999 UTC"::UTCTime, read "0000-01-01 00:00:00.000000 UTC"::UTCTime)
getDateLimitsFromList (x:xs)
	| (thisFirst < exFirst) && (thisSecond > exLast) = (thisFirst, thisSecond)
	| (thisFirst >= exFirst) && (thisSecond > exLast) = (exFirst, thisSecond)
	| (thisFirst < exFirst) && (thisSecond <= exLast) = (thisFirst, exLast)
	| otherwise  = (exFirst, exLast)
    where thisFirst = date (fst x)
          thisSecond = date (snd x)
          exFirst = fst (getDateLimitsFromList xs)
          exLast = snd (getDateLimitsFromList xs)

getRowSize :: (UTCTime, UTCTime) -> NominalDiffTime -> Int
getRowSize lim interval =  ceiling $ getIntervalsInDateRange lim interval

--given a date range, an interval size and an int, return the corresponding date
getIntAsDate :: Int -> NominalDiffTime -> (UTCTime, UTCTime) -> UTCTime
getIntAsDate a intLen lims= addUTCTime ((fromIntegral a)*intLen) (fst lims)
--intervalEnd a interval lim = addUTCTime (a*(realToFrac interval)) (fst lim)

--given a date, an interval size and a date range, return the corresponding int
getDateAsInt :: UTCTime -> NominalDiffTime -> (UTCTime, UTCTime) -> Int
getDateAsInt d intLen lims = ceiling $ realToFrac (diffUTCTime d (fst lims)) / realToFrac intLen

isInRange :: Ord a => a -> a -> a -> Bool
isInRange tgt start end
	| tgt >= start && tgt < end = True
	| otherwise = False

--NominalDiffTime gives diff in seconds when converted to Fractional
getIntervalsInDateRange :: (UTCTime, UTCTime) -> NominalDiffTime -> Double
getIntervalsInDateRange lims intLen = realToFrac (diffUTCTime (snd lims) (fst lims)) / realToFrac intLen

-- TODO: should probably be using maybe  in case of empty list so then this fn needs to use the monadic property
-- TODO: to be in range, date has to be less than the upper limit, so we should add a second to it
getDateLimits :: Asset a => SalePairSet a -> (UTCTime, UTCTime)
getDateLimits salePairs = getDateLimitsFromList (fst salePairs)