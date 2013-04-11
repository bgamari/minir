import Data.Foldable
import SequentialDependence

documents :: [(Int, [Int])]
documents =
    [ (100, [1,2,3,4])
    , (101, [1,2,3,4])
    , (102, [1,3])
    , (103, [1,3,5])
    ]++map (\i->(i,[10..20])) [200..300000]

main = do
    let stats = foldMap (uncurry indexTerms) documents
        idx = termStatsToIndex stats
    print $ map (termScore 0.1 idx) [100,101,102,103,104]
