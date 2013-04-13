import Data.Function
import Data.List
import Data.Monoid
import qualified Data.Set as S
import Data.Foldable.Strict
import qualified Data.Map.Strict as M
import Control.Lens

import MinIR.CorpusStats as CS
import MinIR.TermIndex as TI
import MinIR.OrderedIndex as OI
import MinIR.UnorderedIndex as UI
import MinIR.SequentialDependence as SD

documents :: [(Int, [Int])]
documents =
    [ (100, [1,2,3,4])
    , (101, [1,2,3,4])
    , (102, [1,3])
    , (103, [1,3,5])
    ] ++ map (\i->(i,[10..20])) [200..30000]

main = do
    let idx = foldMap' (uncurry TI.fromTerms) documents
        oidx = foldMap' (uncurry $ OI.fromTerms 8) documents
        cstats = foldMap' (\(doc,terms)->CS.fromDocument doc (length terms)) documents

    Prelude.mapM_ (print . sortBy (flip compare `on` snd) . TI.termScore 0.1 cstats idx) [1..5]
    print $ take 10 $ sortBy (flip compare `on` snd)
          $ M.toList $ OI.termsScore 2 0.1 cstats oidx [1..8]
