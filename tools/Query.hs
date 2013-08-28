import qualified Data.Text as T
import Data.Binary
import Data.Hashable (Hashable)
import Data.List (foldl')
import Data.Text.Binary
import System.Environment (getArgs)
import Data.Function (on)

import MinIR.SequentialDependence as SD
import MinIR.Dictionary as D
import MinIR.Types
import Types
import NLP.Stemmer

main = do
    terms <- getArgs
    idx <- decodeFile "index" :: IO (SeqDepIndex T.Text T.Text)
    --termDict <- decodeFile "terms.dict" :: IO (SeqDepIndex Doc Term)
    --let terms' = D.lookupTerm key
    mapM_ print $ maxN (compare `on` snd) 20
                $ scoreTerms defaultParams idx $ map T.pack $ stemWords English terms

maxN :: (a -> a -> Ordering) -> Int -> [a] -> [a]
maxN f n = foldl' go []
  where go xs x = let (as,bs) = break (\y->f x y == GT) xs
                  in take n $ as++[x]++bs
