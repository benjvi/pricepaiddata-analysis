import System.IO
import System.Environment

readFiles = do
    [s] <- getArgs
    g   <- readFile s
    return g