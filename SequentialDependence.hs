{-# LANGUAGE TemplateHaskell, TupleSections, FlexibleInstances, BangPatterns #-}

module SequentialDependence ( OrderedIndex
                            , indexNGrams
                            ) where

import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import           Data.Set (Set)
import qualified Data.Set as S
import qualified Data.Vector as V
import Data.Hashable

import           Control.Lens
import Data.List (tails)
import Data.Foldable
import Data.Monoid
import Numeric.Log

import TermIndex

ngrams :: Int -> [a] -> [[a]]
ngrams n = map (take n) . tails

instance Hashable a => Hashable (V.Vector a) where
    hashWithSalt salt = hashWithSalt salt . V.toList

data OrderedIndex doc term
        = OIdx { _oFreq :: HashMap (V.Vector term) (HashMap doc Int)
                 }
        deriving (Show)
makeLenses ''OrderedIndex

instance (Hashable term, Eq term, Hashable doc, Ord doc) => Monoid (OrderedIndex doc term) where
    mempty = OIdx HM.empty
    a `mappend` b = OIdx (HM.unionWith (HM.unionWith (+)) (a^.oFreq) (b^.oFreq))

{-# INLINE indexNGrams #-}
indexNGrams :: (Hashable doc, Hashable term, Eq term, Ord doc)
            => Int -> doc -> [term] -> OrderedIndex doc term
indexNGrams n doc terms = foldMap' (indexNGram doc . V.fromList) $ ngrams n terms

indexNGram :: (Hashable doc, Hashable term)
           => doc -> V.Vector term -> OrderedIndex doc term
indexNGram doc ngram = OIdx (HM.singleton ngram $ HM.singleton doc 1)

foldMap' :: (Monoid m, Foldable f) => (a -> m) -> f a -> m
foldMap' f xs = foldl' (\a b->mappend a $ f b) mempty xs


data UnorderedStats doc term = UnorderedStats { _uFreq :: HashMap (Set term) (HashMap doc Int)
                                              }
                             deriving (Show)
makeLenses ''UnorderedStats
