{-# LANGUAGE TemplateHaskell, TupleSections, FlexibleInstances, BangPatterns #-}

module SequentialDependence ( OrderedIndex
                            , indexTerms
                            , nGramScore
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

import TermIndex hiding (indexTerms, termScore, termDocScore)

ngrams :: Int -> [a] -> [[a]]
ngrams n = map (take n) . tails

instance Hashable a => Hashable (V.Vector a) where
    hashWithSalt salt = hashWithSalt salt . V.toList

data OrderedIndex doc term
        = OIdx { _oFreq :: !(HashMap (V.Vector term) (HashMap doc Int))
               , _oTerms :: !(HashMap (V.Vector term) Int)
               }
        deriving (Show)
makeLenses ''OrderedIndex

instance (Hashable term, Eq term, Hashable doc, Ord doc)
         => Monoid (OrderedIndex doc term) where
    mempty = OIdx HM.empty HM.empty
    a `mappend` b = OIdx (HM.unionWith (HM.unionWith (+)) (a^.oFreq) (b^.oFreq))
                         (HM.unionWith (+) (a^.oTerms) (b^.oTerms))

{-# INLINE indexTerms #-}
indexTerms :: (Hashable doc, Hashable term, Eq term, Ord doc)
           => Int -> doc -> [term] -> OrderedIndex doc term
indexTerms n doc terms = foldMap' (indexNGram doc . V.fromList) $ ngrams n terms

indexNGram :: (Hashable doc, Hashable term)
           => doc -> V.Vector term -> OrderedIndex doc term
indexNGram doc ngram = OIdx (HM.singleton ngram $ HM.singleton doc 1)
                            (HM.singleton ngram 1)

foldMap' :: (Monoid m, Foldable f) => (a -> m) -> f a -> m
foldMap' f xs = foldl' (\a b->mappend a $ f b) mempty xs

nGramScore :: (Hashable doc, Eq doc, Hashable term, Eq term)
           => Double -> TermIndex doc term -> OrderedIndex doc term
           -> V.Vector term -> [(doc, Score)]
nGramScore alphaD idx oidx ngram =
    map (\doc->(doc, nGramDocScore alphaD idx oidx ngram doc))
    $ oidx ^. oFreq . at ngram . non HM.empty . to HM.keys

nGramDocScore :: (Hashable term, Eq term, Hashable doc, Eq doc)
              => Double -> TermIndex doc term -> OrderedIndex doc term
              -> V.Vector term -> doc -> Score
nGramDocScore alphaD idx oidx ngram doc =
    (1 - ad) * realToFrac tf / d + ad * cf / c
  where cf = oidx ^. oTerms . at ngram . non 0 . to realToFrac
        tf = oidx ^. oFreq . at ngram . non HM.empty . at doc . non 0 . to realToFrac
        d = idx ^. tDocs . at doc . non 0 . to realToFrac
        c = idx ^. tTotalTerms . to realToFrac
        ad = realToFrac alphaD


data UnorderedStats doc term = UnorderedStats { _uFreq :: HashMap (Set term) (HashMap doc Int)
                                              }
                             deriving (Show)
makeLenses ''UnorderedStats
