module RSRIndex.Types where

import Data.Time
import Data.Time.Format
import qualified Data.Map as M
import Statistics.Matrix.Types

--TODO : this extra fn constraint is really pointless
class (Eq a, Show a, Ord a) => Asset a where
    self :: a -> a

data AssetSale a = AssetSale { date :: UTCTime
							, price :: Int
							, assetId :: a  } deriving (Eq, Show, Ord)

--fst: list of Asset Sale pairs (representing ownership)
--snd: residual unmatched sales associated with assets in the population
type SalePairSet a = ([(AssetSale a, AssetSale a)], M.Map a (AssetSale a))

type RSRIndex = (Vector, Double)