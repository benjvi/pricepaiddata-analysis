module ParseCSV (
parseCSVFromFile
) where

import qualified Data.Text.IO as T
import Data.Text
import Data.Functor
import qualified Text.ParseCSV as C

{-}
csvFile = endBy line eol
line = sepBy cell (char ',')
cell = quotedCell <|> many (noneOf ",\n\r")

quotedCell = 
    do char '"'
       content <- many quotedChar
       char '"' <?> "quote at end of cell"
       return content

quotedChar =
        noneOf "\""
    <|> try (string "\"\"" >> return '"')

eol =   try (string "\n\r")
    <|> try (string "\r\n")
    <|> string "\n"
    <|> string "\r"
    <?> "end of line"
-}
--result of type Either means this is not lazy
--targeted source data size is 3GB -> laziness would be nice 
--this is in theory not critical but memory usage seems to be many times the file size :/
-- file: pricepaid/SCC.hs
parseCSVFromFile :: String -> IO (Either String C.CSV)
parseCSVFromFile input = C.parseCSV <$> T.readFile input

