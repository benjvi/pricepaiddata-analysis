import ParseCSV
import PropertySale
import Statistics.Regression
import Statistics.Matrix.Types
import Data.Time
import Data.List
import qualified Data.Vector.Unboxed as U

main = do
	csvContent <- parseCSVFromFile "C:\\Users\\ben\\Downloads\\pp-2014-test.csv"
	--print csvContent
	--unpack the either
	case csvContent of 
		Left  x -> putStrLn "invalid string"
		Right x -> print $ normalizeIndex $ doRegression priceMatrix logPriceVector
                       where priceMatrix = [mapSalePairToRSRRow p interval (getDateLimits (getPropertySalePairs x)) | p <- (getPropertySalePairs x)]
				           where interval = 86400.00 :: NominalDiffTime                 	
                             logPriceVector = [ getLogPriceRatio p | p <- (getPropertySalePairs x)]
	--print [mapSalePairToRSRRow p interval (getDateLimits (getPropertySalePairs x)) | p <- (getPropertySalePairs x)]

--adapter to the olsRegress function - converting types
--for some reason the matrix is represented by a list of columns
doRegression :: [[Int]] -> [Double] -> (Vector, Double)
doRegression priceMatrix logVector = olsRegress ([(U.fromList ([fromIntegral i | i <- l])) | l <- (transpose priceMatrix)]) (U.fromList logVector)

normalizeIndex :: (Vector, Double) -> (Vector, Double)
normalizeIndex (index, fit) = (U.fromList [exp ((index U.! 0) - i + log 100) | i <- U.toList index], fit)