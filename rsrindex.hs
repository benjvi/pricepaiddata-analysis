{-# LANGUAGE MultiParamTypeClasses #-}
--{-# LANGUAGE ConstraintKinds #-}
module RSRIndex (
	generateSalePairs,
    getIndex,
    getPriceMatrix,
    getDateLimits,
    getLogPriceVector,
    getDateAsInt,
    AssetSale(..),
    Asset(..),
    SalePairSet,
    RSRIndex
)
where

import Data.List
import Data.Time
import Data.Time.Format
import Statistics.Regression
import Statistics.Matrix.Types
import qualified Data.Vector.Unboxed as U
import qualified Data.Map as M
import qualified Data.Text as T hiding (foldr)

--convenient alias

--TODO : this extra fn constraint is really pointless
class (Eq a, Show a, Ord a) => Asset a where
    self :: a -> a

data AssetSale a = AssetSale { date :: UTCTime
							, price :: Int
							, assetId :: a  } deriving (Eq, Show, Ord)

--fst: list of Asset Sale pairs (representing ownership)
--snd: residual unmatched sales associated with assets in the population
type SalePairSet a = ([(AssetSale a, AssetSale a)], M.Map a (AssetSale a))

type RSRIndex = (Vector, Double)

--returns an RSR Index normalized to 100
getIndex :: [[Int]] -> [Double]-> RSRIndex
getIndex priceMatrix logVector = normalizeIndex $ doRegression priceMatrix logVector 

--adapter to the olsRegress function - converting types
--for some reason the matrix is represented by a list of columns instead of the expected rows
doRegression :: [[Int]] -> [Double] -> RSRIndex
doRegression priceMatrix logVector = olsRegress ([(U.fromList ([fromIntegral i | i <- l])) | l <- (transpose priceMatrix)]) (U.fromList logVector)

normalizeIndex :: RSRIndex -> RSRIndex
normalizeIndex (index, fit) = (U.fromList [exp (i - (index U.! 0) + log 100) | i <- U.toList index], fit)

getPriceMatrix :: Asset a => SalePairSet a -> NominalDiffTime -> [[Int]]
getPriceMatrix salePairs interval = [mapSalePairToRSRRow p interval (getDateLimits salePairs) | p <- (fst salePairs)]

getLogPriceVector :: Asset a => SalePairSet a -> [Double]
getLogPriceVector salePairs = [ getLogPriceRatio p | p <- (fst salePairs)]

getLogPriceRatio :: Asset a => (AssetSale a, AssetSale a) -> Double
getLogPriceRatio salePair = log ((fromIntegral (price (snd salePair))) / (fromIntegral (price (fst salePair))))

generateSalePairs :: Asset a => [AssetSale a] -> SalePairSet a
-- inefficient (n^2 time) NAIVE SOLUTION:
--generatePropertySalePairs salesList = [(a,b) | a<- salesList, b<-salesList, (location a)==(location b), a/=b, (date b) > (date a) ]	
-- nicer fold solution (n log m) where m <=n (number of locations)
generateSalePairs salesList = foldr (matchSale) ([], M.empty) salesList

--for use in fold
--assuming base list is already sorted by date
matchSale :: Asset a => AssetSale a -> SalePairSet a -> SalePairSet a
matchSale sale existingSalePairs = 
	--Not sure if we need to filter: filter (\x -> (sale/=x && (date sale) > (date x)))
	case (M.lookup (assetId sale) (snd existingSalePairs)) of
	    -- if there is a sale with matching location we already encountered
	    Just value -> (((sale, value):(fst existingSalePairs)), (M.insert (assetId sale) sale (snd existingSalePairs)))
	    --if no sale for this location encountered
	    Nothing -> ((fst existingSalePairs), (M.insert (assetId sale) sale (snd existingSalePairs)))

-- TODO: should probably be using maybe  in case of empty list so then this fn needs to use the monadic property
-- TODO: to be in range, date has to be less than the upper limit, so we should add a second to it
getDateLimits :: Asset a => SalePairSet a -> (UTCTime, UTCTime)
getDateLimits salePairs = getDateLimitsFromList (fst salePairs)

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

--RSR Row is a list: -1 for the first sale, 1 for the second sale, 0 otherwise
mapSalePairToRSRRow :: Asset a => (AssetSale a, AssetSale a) -> NominalDiffTime ->  (UTCTime, UTCTime) -> [Int]
mapSalePairToRSRRow pair interval lim = 
    [ getRSRElem pair (getIntAsDate (x-1) interval lim) (getIntAsDate x interval lim) | x <- [1..(getRowSize lim interval)] ]
        where getRSRElem pair startdate enddate
		  	    --TODO: what if start and end periods are the same?
			    | isInRange (date (fst pair)) startdate enddate = -1
			    | isInRange (date (snd pair)) startdate enddate = 1
			    | otherwise = 0

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
