module RSRIndex.Internal.AssetSale where

import RSRIndex.Internal.Date
import RSRIndex.Types
import qualified Data.Map as M
import qualified Data.Vector.Unboxed as U
import Data.Time
import Data.Time.Format

--RSR Row is a list: -1 for the first sale, 1 for the second sale, 0 otherwise
mapSalePairToRSRRow :: Asset a => (AssetSale a, AssetSale a) -> NominalDiffTime ->  (UTCTime, UTCTime) -> [Int]
mapSalePairToRSRRow pair interval lim = 
    [ getRSRElem pair (getIntAsDate (x-1) interval lim) (getIntAsDate x interval lim) | x <- [1..(getRowSize lim interval)] ]
        where getRSRElem pair startdate enddate
		  	    --TODO: what if start and end periods are the same?
			    | isInRange (date (fst pair)) startdate enddate = -1
			    | isInRange (date (snd pair)) startdate enddate = 1
			    | otherwise = 0

--for use in fold (construction of sale pairs from a list)
--assuming base list is already sorted by date
matchSale :: Asset a => AssetSale a -> SalePairSet a -> SalePairSet a
matchSale sale existingSalePairs = 
	--Not sure if we need to filter: filter (\x -> (sale/=x && (date sale) > (date x)))
	case (M.lookup (assetId sale) (snd existingSalePairs)) of
	    -- if there is a sale with matching location we already encountered
	    Just value -> (((sale, value):(fst existingSalePairs)), (M.insert (assetId sale) sale (snd existingSalePairs)))
	    --if no sale for this location encountered
	    Nothing -> ((fst existingSalePairs), (M.insert (assetId sale) sale (snd existingSalePairs)))

-- | Returns the predicted price for the same property (according to the computed RSR Index) at the end of the indexed period
predictPrice :: Asset a => AssetSale a -> RSRIndex -> (UTCTime,UTCTime) -> NominalDiffTime -> Double
--we have Price_end/Price_sale=Index_end/Index_sale
-- find the index period corresponding to the last sale
--then compute the expression for Price_end = Price_sale * (Index_end/Index_sale)
predictPrice sale index limits interval = (fromIntegral (price sale)) * ((last (U.toList(fst index)))/ ((U.toList (fst index))!!(getDateAsInt (date sale) interval limits)))

--TODO: fails horribly if the list is 0-length? -> needs an error handling mechanism
getLastSale :: Asset a => a -> [AssetSale a] -> AssetSale a
getLastSale tgtLoc saleList = foldr (\acc x -> if ((date x)>(date acc)) then x else acc) (tgtSales!!0) tgtSales
                                  where tgtSales = filter (\x -> tgtLoc==(assetId x)) saleList