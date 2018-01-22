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
import Control.Parallel.Strategies

main = (parseCSVFromFile "C:\\Users\\ben\\Downloads\\pp-2014-test.csv" (T.pack "B")) >>= (\csv -> putStrLn $ show csv) 
{-}
parallelParsePricePaidCSVs :: IO [PropertySale]
  --first step is to do a simple mapping from files to [[T.Text]] results
  --TODO:how to do a bind here (unpack the monad)
  --TODO: this is sequential (?)we didnt specify any parallelism/concurrency strategy
parallelParsePricePaidCSVs = (sequence (runEval parallelParsingTest)) >>= (\allSales -> return (concat allSales))

parallelParsingTest :: Eval [IO [PropertySale]]
parallelParsingTest = (rpar (parseCSVToSalesList "C:\\Users\\ben\\Downloads\\pp-2014-test.csv")) >>= (\sales2014 ->
    (rpar (parseCSVToSalesList "C:\\Users\\ben\\Downloads\\pp-2014-test.csv")) >>= (\sales20142 ->
      rseq sales2014 >>= (\_ ->
        rseq sales20142 >>= (\_ ->
          return [sales2014,sales20142]
        )
      )
    )
  )

parallelParsing :: Eval [IO [PropertySale]]
parallelParsing = do 
  sales1995 <- rpar $ parseCSVToSalesList "/root/pp-1995.csv"
  sales1996 <- rpar $ parseCSVToSalesList "/root/pp-1996.csv"
  sales1997 <- rpar $ parseCSVToSalesList "/root/pp-1997.csv"
  sales1998 <- rpar $ parseCSVToSalesList "/root/pp-1998.csv"
  sales1999 <- rpar $ parseCSVToSalesList "/root/pp-1999.csv"
  sales2000 <- rpar $ parseCSVToSalesList "/root/pp-2000.csv"
  sales2001 <- rpar $ parseCSVToSalesList "/root/pp-2001.csv"
  sales2002 <- rpar $ parseCSVToSalesList "/root/pp-2002.csv"
  sales2003 <- rpar $ parseCSVToSalesList "/root/pp-2003.csv"
  sales2004 <- rpar $ parseCSVToSalesList "/root/pp-2004.csv"
  sales2005 <- rpar $ parseCSVToSalesList "/root/pp-2005.csv"
  sales2006 <- rpar $ parseCSVToSalesList "/root/pp-2006.csv"
  --  sales2007 <- rpar $ parseCSVToSalesList "/root/pp-2007.csv" hGetContents: invalid argument (invalid byte sequence)
  sales2008 <- rpar $ parseCSVToSalesList "/root/pp-2008.csv"
  sales2009 <- rpar $ parseCSVToSalesList "/root/pp-2009.csv"
  sales2010 <- rpar $ parseCSVToSalesList "/root/pp-2010.csv"
  sales2011 <- rpar $ parseCSVToSalesList "/root/pp-2011.csv"
  sales2012 <- rpar $ parseCSVToSalesList "/root/pp-2012.csv"
  sales2013 <- rpar $ parseCSVToSalesList "/root/pp-2013.csv"
  rseq sales1995
  rseq sales1996
  rseq sales1997
  rseq sales1998
  rseq sales1999
  rseq sales2000
  rseq sales2001
  rseq sales2002
  rseq sales2003
  rseq sales2004
  rseq sales2005
  rseq sales2006
  rseq sales2008
  rseq sales2009
  rseq sales2010
  rseq sales2011
  rseq sales2012
  rseq sales2013
  {-sales <- [rpar (parseCSVToSalesList x) | x <- ["/", "s"]]
  [rseq sale | sale <- sales ] -}
  return $ []

parseCSVToSalesList :: String -> IO [PropertySale]
parseCSVToSalesList csvPath = (parseCSVFromFile csvPath) >>= (\csv -> return (parseListPropertySales csv))

{-}
makePricePredictionFromCSV :: IO Double
makePricePredictionFromCSV = do
  csvContent <- parallelParsePricePaidCSVs
  return $ makePricePrediction (getPropertySalePairs csvContent)
-}

makeIndexFromCSV :: IO RSRIndex
makeIndexFromCSV = parallelParsePricePaidCSVs >>= (\sales -> return (getRSRIndex (generateSalePairs sales) interval))
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
-}