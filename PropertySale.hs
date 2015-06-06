module PropertySale (
getPropertySalePairs,
parseListPropertySales,
PropertyLocation(..),
PropertySale
)
where

import System.Locale
import Data.Time
import Data.Time.Format
import qualified Data.Text as T hiding (foldr)
import qualified Data.Map as M
import RSRIndex

getPropertySalePairs :: [[T.Text]] -> PropertySalePairs
getPropertySalePairs x = (generateSalePairs . parseListPropertySales) x

--assuming postcode suffices to identify from street level upwards
-- we are only interested for the moment in uniquely idenitfying the properties
data PropertyLocation = PropertyLocation { postCode :: T.Text
							, pAON :: T.Text
							, sAON :: T.Text } deriving (Show, Eq, Ord)

instance Asset PropertyLocation where
	self a = a

type PropertySale = AssetSale PropertyLocation
type PropertySalePairs = SalePairSet PropertyLocation

mapPricePaidDataToPropertySale :: [T.Text] -> PropertySale
mapPricePaidDataToPropertySale input = AssetSale { 
	date=parseSaleDate (input !! 2)
	, price=read $ T.unpack (input !! 1) :: Int
	, assetId= PropertyLocation { 
		postCode=input !! 3
		, pAON=input !! 7
		, sAON=input !! 8
		}
	}

parseListPropertySales :: [[T.Text]] -> [PropertySale]	
parseListPropertySales strSalesList = [ mapPricePaidDataToPropertySale x | x <- strSalesList ]

--know we have good quality input data so 
--we can use readTime and UTCTime instead of parseTime and Maybe UTCTime
parseSaleDate :: T.Text -> UTCTime
parseSaleDate txt = readTime defaultTimeLocale "%Y-%m-%d %H:%M" (T.unpack txt) 

getLogPriceRatio :: (PropertySale, PropertySale) -> Double
getLogPriceRatio salePair = log ((fromIntegral (price (snd salePair))) / (fromIntegral (price (fst salePair))))
