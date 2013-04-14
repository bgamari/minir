import qualified Data.Text as T
import Data.Binary
import Data.Hashable (Hashable)
import Data.List (foldl')
import Data.Text.Binary
import System.Environment (getArgs)
import Data.Function (on)

import MinIR.SequentialDependence as SD
import MinIR.Types
import NLP.Stemmer

main = do
    terms <- getArgs
    idx <- decodeFile "index" :: IO (SeqDepIndex T.Text T.Text)
    mapM_ print $ maxN (compare `on` snd) 20
                $ scoreTerms defaultParams idx $ map T.pack $ stemWords English terms

maxN :: (a -> a -> Ordering) -> Int -> [a] -> [a]
maxN f n = foldl' go []
  where go xs x = let (as,bs) = break (\y->f x y == GT) xs
                  in take n $ as++[x]++bs
