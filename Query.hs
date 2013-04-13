import qualified Data.Text as T
import Data.Binary
import Data.Hashable (Hashable)
import Data.List (sortBy)
import Data.Text.Binary
import System.Environment (getArgs)
import Data.Function (on)

import MinIR.SequentialDependence as SD
import MinIR.Types

main = do
    terms <- getArgs
    idx <- decodeFile "index" :: IO (SeqDepIndex String T.Text)
    mapM_ print $ take 10 $ topN idx $ map T.pack terms

topN :: (Ord term, Ord doc, Hashable term, Eq term)
     => SeqDepIndex doc term -> [term] -> [(doc, Score)]
topN idx = sortBy (flip compare `on` snd) . scoreTerms defaultParams idx
