import qualified Data.Set as S
import Data.Foldable
import SequentialDependence
import Control.Lens

documents :: [(Int, [Int])]
documents =
    [ (100, [1,2,3,4])
    , (101, [1,2,3,4])
    , (102, [1,3])
    , (103, [1,3,5])
    ] -- ++map (\i->(i,[10..20])) [200..3000]

main = do
    let stats = foldMap (uncurry indexTerms) documents
        idx = termStatsToIndex stats
    print $ idx ^. tFreq . at 1 . non S.empty . to S.toDescList
    --Prelude.mapM_ (print . termScore 0.1 idx) [1..5]
