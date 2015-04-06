module PropertySale (
getPropertySalePairs,
mapSalePairToRSRRow,
getDateLimits,
getLogPriceRatio,
PropertySale,
PropertyLocation)
where

import System.Locale
import Data.Time
import Data.Time.Format

getPropertySalePairs x = (generatePropertySalePairs . parseListPropertySales) x

data PropertySale = PropertySale { date :: UTCTime
							, price :: Int
							, location :: PropertyLocation } deriving (Show, Eq)

--assuming postcode suffices to identify from street level upwards
data PropertyLocation = PropertyLocation { postCode :: String
							, pAON :: String
							, sAON :: String } deriving (Show, Eq)

mapPricePaidDataToPropertySale :: [String] -> PropertySale
mapPricePaidDataToPropertySale input = PropertySale { 
	date=parseSaleDate (input !! 2)
	, price=read (input !! 1)
	, location= PropertyLocation { 
		postCode=input !! 3
		, pAON=input !! 7
		, sAON=input !! 8
		}
	}

parseListPropertySales :: [[String]] -> [PropertySale]
parseListPropertySales strSalesList = [ mapPricePaidDataToPropertySale x | x <- strSalesList ]

generatePropertySalePairs :: [PropertySale] -> [(PropertySale, PropertySale)]
-- IMPERATIVE IMPLEMENTATION:
-- resutl :: [(PropertySale, PropertySale)]
-- unmatched :: Map propertyLocation, PropertySale
-- for sale in propertySale:
--		matchedsale = unmatched.get(sale.location)     
--		if map.get:
--			result.add((matchsale, sale))  
--			unmatched.put(sale) --overwrite the existing value in the map
-- unmatched :: [PropertySale]
-- inefficient (3 * n space, n^2 time) NAIVE SOLUTION:
generatePropertySalePairs salesList = [(a,b) | a<- salesList, b<-salesList, (location a)==(location b), a/=b, (date b) > (date a) ]	
-- TODO: think of a nice efficeint folding recursive function

-- TODO: should probably be using maybe  in case of empty list so then this fn needs to use the monadic property
-- TODO: to be in range, date has to be less than the upper limit, so we should add a second to it
getDateLimits :: [(PropertySale, PropertySale)] -> (UTCTime , UTCTime)
getDateLimits [] = (read "9999-12-31 23:59:59.999999 UTC"::UTCTime, read "0000-01-01 00:00:00.000000 UTC"::UTCTime)
getDateLimits (x:xs)
	| (thisFirst < exFirst) && (thisSecond > exLast) = (thisFirst, thisSecond)
	| (thisFirst >= exFirst) && (thisSecond > exLast) = (exFirst, thisSecond)
	| (thisFirst < exFirst) && (thisSecond <= exLast) = (thisFirst, exLast)
	| otherwise  = (exFirst, exLast)
    where thisFirst = date (fst x)
          thisSecond = date (snd x)
          exFirst = fst (getDateLimits xs)
          exLast = snd (getDateLimits xs)

--RSR Row is a list: -1 for the first sale, 1 for the second sale, 0 otherwise
mapSalePairToRSRRow :: (PropertySale, PropertySale) -> NominalDiffTime ->  (UTCTime, UTCTime) -> [Int]
mapSalePairToRSRRow pair interval lim = 
    [ getRSRElem pair (getIntervalAsDate (x-1) interval lim) (getIntervalAsDate x interval lim) | x <- [1..(getRowSize lim interval)] ]
        where getRSRElem pair startdate enddate
		  	    --TODO: what if start and end periods are the same?
			    | isInRange (date (fst pair)) startdate enddate = -1
			    | isInRange (date (snd pair)) startdate enddate = 1
			    | otherwise = 0 

getRowSize :: (UTCTime, UTCTime) -> NominalDiffTime -> Int
getRowSize lim interval =  ceiling $ getIntervalsInDateRange lim interval

--given a date range, an interval size and an int, return the corresponding date
getIntervalAsDate :: Int -> NominalDiffTime -> (UTCTime, UTCTime) -> UTCTime
getIntervalAsDate a interval lim= addUTCTime ((fromIntegral a)*interval) (fst lim)
--intervalEnd a interval lim = addUTCTime (a*(realToFrac interval)) (fst lim)

--NominalDiffTime gives diff in seconds when converted to Fractional
getIntervalsInDateRange :: Fractional a => (UTCTime, UTCTime) -> NominalDiffTime -> Double
getIntervalsInDateRange lim interval = realToFrac (diffUTCTime (snd lim) (fst lim)) / realToFrac interval


isInRange :: Ord a => a -> a -> a -> Bool
isInRange tgt start end
	| tgt >= start && tgt < end = True
	| otherwise = False

--know we have good quality input data so 
--we can use readTime and UTCTime instead of parseTime and Maybe UTCTime
parseSaleDate :: String -> UTCTime
parseSaleDate str = readTime defaultTimeLocale "%Y-%m-%d %H:%M" (str) 

getLogPriceRatio :: (PropertySale, PropertySale) -> Double
getLogPriceRatio salePair = log ((fromIntegral (price (snd salePair))) / (fromIntegral (price (fst salePair))))