module Data.Foldable.Strict ( foldMap'
                            , module Data.Foldable
                            ) where

import Data.Foldable
import Data.Monoid

foldMap' :: (Monoid m, Foldable f) => (a -> m) -> f a -> m
foldMap' f xs = foldl' (\a b->mappend a $ f b) mempty xs
