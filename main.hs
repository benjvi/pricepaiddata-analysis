import ParseCSV
import PropertySale
import RSRIndex
import Statistics.Regression
import Statistics.Matrix.Types
import Data.Time
import Data.List
import Data.Either
import qualified Data.Vector.Unboxed as U
import qualified Data.Text as T hiding (foldr)

main = do 
  pricePrediction <- makeIndexFromCSV
	--print csvContent
	--unpack the either
  putStrLn $ show pricePrediction

parallelParsePricePaidCSVs :: IO [[T.Text]]
  --first step is to do a simple mapping from files to [[T.Text]] results
  --TODO:how to do a bind here (unpack the monad)
  --TODO: this is sequential (?)we didnt specify any parallelism/concurrency strategy
parallelParsePricePaidCSVs = do
  csv1995 <- parseCSVFromFile "C:\\Users\\ben\\Downloads\\pp-2014-test.csv"
  csv2014 <- parseCSVFromFile "C:\\Users\\ben\\Downloads\\pp-2014-test.csv"
  return $ concat $ rights [csv1995]
  --we now have [[[T.Text]]], want just [[T.Text]]

makePricePredictionFromCSV :: IO Double
makePricePredictionFromCSV = do
  csvContent <- parallelParsePricePaidCSVs
  return $ makePricePrediction csvContent

makeIndexFromCSV :: IO RSRIndex
makeIndexFromCSV = do
  csvContent <- parallelParsePricePaidCSVs
  return $ getIndex (getPriceMatrix (getPropertySalePairs csvContent) interval ) (getLogPriceVector (getPropertySalePairs csvContent))
    where interval = 1664000 :: NominalDiffTime

makePricePrediction :: [[T.Text]] -> Double
makePricePrediction x = predictPrice lastSale (getIndex (getPriceMatrix salePairs interval ) (getLogPriceVector salePairs)) (getDateLimits salePairs) interval
    where interval = 120400 :: NominalDiffTime -- 1664000.00 :: NominalDiffTime
          salePairs = getPropertySalePairs x 
          lastSale = getLastSale (PropertyLocation { postCode=(T.pack "B96 6BW"), pAON=(T.pack "EASTWOOD COURT, 2"), sAON=(T.pack "FLAT 21") }) (parseListPropertySales x)             

returnErrors :: [[T.Text]] -> [Double]
returnErrors x = getIdeosyncraticErrors salePairs index limits interval
    where interval = 120400 :: NominalDiffTime -- 1664000.00 :: NominalDiffTime
          salePairs = getPropertySalePairs x 
          limits = getDateLimits salePairs
          index = getIndex (getPriceMatrix salePairs interval ) (getLogPriceVector salePairs)
{-}
main = do
	csvContent <- parseCSVFromFile "C:\\Users\\ben\\Downloads\\pp-2014-test.csv"
	--print csvContent
	--unpack the either
	case csvContent of 
		Left  x -> putStrLn "invalid string"
		Right x -> print $ predictPrice (getLastSale filterLoc listSales) $ parseListPropertySales x
                      where filterLoc = PropertyLocation { postCode=(T.pack "B96 6BW"), pAON=(T.pack "EASTWOOD COURT, 2"), sAON=(T.pack "FLAT 21") }
-}
--TODO: fails horribly if the list is 0-length? -> needs an error handling mechanism
getLastSale :: PropertyLocation -> [PropertySale] -> PropertySale
getLastSale tgtLoc saleList = foldr (\acc x -> if ((date x)>(date acc)) then x else acc) (tgtSales!!0) tgtSales
                                  where tgtSales = filter (\x -> tgtLoc==(assetId x)) saleList

--we have Price_end/Price_sale=Index_end/Index_sale
-- find the index period corresponding to the last sale
--then compute the expression for Price_end = Price_sale * (Index_end/Index_sale)
predictPrice :: PropertySale -> RSRIndex -> (UTCTime,UTCTime) -> NominalDiffTime -> Double
predictPrice sale index limits interval = (fromIntegral (price sale)) * ((last (U.toList(fst index)))/ ((U.toList (fst index))!!(getDateAsInt (date sale) interval limits)))

--for each pair: err = Price_two/price_one - Index_two / Index_one
getIdeosyncraticErrors :: Asset a => SalePairSet a -> RSRIndex -> (UTCTime,UTCTime) -> NominalDiffTime -> [Double]
getIdeosyncraticErrors salePairs index limits interval = [ getIdeosyncraticError p index limits interval | p <- (fst salePairs)] 

getIdeosyncraticError :: Asset a => (AssetSale a, AssetSale a) -> RSRIndex -> (UTCTime,UTCTime) -> NominalDiffTime -> Double
getIdeosyncraticError salePair index limits interval = (priceTwo/priceOne) - (indexTwo/indexOne)
    where priceOne = fromIntegral $ price (fst salePair)
          priceTwo = fromIntegral $ price (snd salePair)
          indexOne = ((U.toList (fst index))!!(getDateAsInt (date (fst salePair)) interval limits))
          indexTwo = ((U.toList (fst index))!!(getDateAsInt (date (snd salePair)) interval limits))
