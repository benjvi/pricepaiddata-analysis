module ParseCSV (
parseCSVFromFile
) where

import qualified Data.Text.IO as T
import Data.Text
import Data.Functor
import qualified Text.ParseCSV as C

--result of type Either means this is not lazy
--targeted source data size is 3GB -> laziness would be nice 
--this is in theory not critical but memory usage seems to be many times the file size :/
-- file: pricepaid/SCC.hs
parseCSVFromFile :: String -> Text -> IO (Either String C.CSV)
parseCSVFromFile input prefix = (C.parseCSV prefix) <$> T.readFile input

