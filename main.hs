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
  index <- makeIndexFromCSV
	--print csvContent
	--unpack the either
  putStrLn $ show index

parallelParsePricePaidCSVs :: IO [[T.Text]]
  --first step is to do a simple mapping from files to [[T.Text]] results
  --TODO:how to do a bind here (unpack the monad)
  --TODO: this is sequential (?)we didnt specify any parallelism/concurrency strategy
parallelParsePricePaidCSVs = do
  csv1995 <- parseCSVFromFile "C:\\Users\\ben\\Downloads\\pp-2014-test.csv"
  csv2014 <- parseCSVFromFile "C:\\Users\\ben\\Downloads\\pp-2014-test.csv"
  return $ concat $ rights [csv1995]
  --we now have [[[T.Text]]], want just [[T.Text]]
{-}
makePricePredictionFromCSV :: IO Double
makePricePredictionFromCSV = do
  csvContent <- parallelParsePricePaidCSVs
  return $ makePricePrediction (getPropertySalePairs csvContent)
-}
makeIndexFromCSV :: IO RSRIndex
makeIndexFromCSV = do
  csvContent <- parallelParsePricePaidCSVs
  return $ getRSRIndex (getPropertySalePairs csvContent) interval
    where interval = 1664000 :: NominalDiffTime
{-}
--TODO RSRIndex must include date limits and interval
returnErrors :: [[T.Text]] -> [Double]
returnErrors x = getIdeosyncraticErrors salePairs index limits interval
    where interval = 120400 :: NominalDiffTime -- 1664000.00 :: NominalDiffTime
          salePairs = getPropertySalePairs x 
          limits = getDateLimits salePairs
          index = getRSRIndex salePairs

--for each pair: err = Price_two/price_one - Index_two / Index_one
getIdeosyncraticErrors :: Asset a => SalePairSet a -> RSRIndex -> (UTCTime,UTCTime) -> NominalDiffTime -> [Double]
getIdeosyncraticErrors salePairs index limits interval = [ getIdeosyncraticError p index limits interval | p <- (fst salePairs)] 
-}
