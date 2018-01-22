module RSRIndex.Internal.Regression where

import qualified Data.Vector.Unboxed as U
import RSRIndex.Types
import RSRIndex.Internal.AssetSale
import RSRIndex.Internal.Date
import Data.Time
import Data.Time.Format
import Statistics.Regression
import Statistics.Matrix.Types
import Data.List

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