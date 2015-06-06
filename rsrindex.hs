{-# LANGUAGE MultiParamTypeClasses #-}
--{-# LANGUAGE ConstraintKinds #-}
module RSRIndex (
	generateSalePairs,
	getRSRIndex,
	makePricePrediction,
	getIdeosyncraticError,
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
import RSRIndex.Internal.Regression
import RSRIndex.Internal.Date
import RSRIndex.Internal.AssetSale
import RSRIndex.Types

-- | Return RSR Index of price trends for the sale pairs given, partitioned according to the given interval. Also returns the strength of the correlation (?)
getRSRIndex :: Asset a => SalePairSet a -> NominalDiffTime -> RSRIndex
getRSRIndex salePairs interval = getIndex (getPriceMatrix salePairs interval) (getLogPriceVector salePairs)

-- | Returns price prediction of asset in sale pair set, according to the RSRIndex calculated with a specified interval
makePricePrediction :: Asset a => SalePairSet a -> NominalDiffTime -> a -> RSRIndex -> Double
makePricePrediction salePairs interval asset rsrIndex = predictPrice lastSale rsrIndex (getDateLimits salePairs) interval
    where lastSale = getLastSale asset [snd salePair | salePair <- fst salePairs]              

-- | Returns the geometrical error in the rsr index for a particular buy/sale pair (i.e. the difference in the growth %)
getIdeosyncraticError :: Asset a => (AssetSale a, AssetSale a) -> RSRIndex -> (UTCTime,UTCTime) -> NominalDiffTime -> Double
getIdeosyncraticError salePair index limits interval = (priceTwo/priceOne) - (indexTwo/indexOne)
    where priceOne = fromIntegral $ price (fst salePair)
          priceTwo = fromIntegral $ price (snd salePair)
          indexOne = ((U.toList (fst index))!!(getDateAsInt (date (fst salePair)) interval limits))
          indexTwo = ((U.toList (fst index))!!(getDateAsInt (date (snd salePair)) interval limits))

-- | Given a date sorted list of asset sales, returns a type holding a list of sale pairs as well as a map of unpaired sales 
generateSalePairs :: Asset a => [AssetSale a] -> SalePairSet a
-- inefficient (n^2 time) NAIVE SOLUTION:
--generatePropertySalePairs salesList = [(a,b) | a<- salesList, b<-salesList, (location a)==(location b), a/=b, (date b) > (date a) ]	
-- nicer fold solution (n log m) where m <=n (number of locations)
generateSalePairs salesList = foldr (matchSale) ([], M.empty) salesList