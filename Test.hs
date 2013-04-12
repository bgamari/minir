import Data.Function
import Data.List
import Data.Monoid
import qualified Data.Set as S
import Data.Foldable
import TermIndex
import SequentialDependence
import Control.Lens

documents :: [(Int, [Int])]
documents =
    [ (100, [1,2,3,4])
    , (101, [1,2,3,4])
    , (102, [1,3])
    , (103, [1,3,5])
    ] ++map (\i->(i,[10..20])) [200..300000]

main = do
    let idx = foldMap' (uncurry indexTerms) documents
    print $ idx ^. tTotalTerms
    Prelude.mapM_ (print . sortBy (flip compare `on` snd) . termScore 0.1 idx) [1..5]

foldMap' :: (Monoid m, Foldable f) => (a -> m) -> f a -> m
foldMap' f xs = Data.Foldable.foldl' (\a b->mappend a $ f b) mempty xs
