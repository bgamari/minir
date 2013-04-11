import Data.Foldable
import SequentialDependence

documents :: [(Int, [Int])]
documents =
    [ (100, [1,2,3,4])
    , (101, [1,2,3,4])
    , (102, [1,3])
    , (103, [1,3,5])
    ]++map (\i->(i,[10..20])) [200..3000]

main = do
    let stats = foldMap (uncurry indexTerms) documents
    print $ map (termScore 0.1 stats 3) [100,101,102,103,104]
