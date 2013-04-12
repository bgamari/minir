{-# LANGUAGE TemplateHaskell, TupleSections, TypeSynonymInstances, FlexibleInstances #-}

module SequentialDependence where

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

type Score = Log Double

-- | TermIndex is suitable for querying
type TermIndex doc term = TermIndex' (Set (Int,doc)) doc term
-- | TermStats is suitable for building up during indexing
type TermStats doc term = TermIndex' (HashMap doc Int) doc term

data TermIndex' postings doc term
         = TermIdx  { _tFreq :: !(HashMap term postings)
                      -- ^ Number of term mentions in each document
                    , _tTerms :: !(HashMap term Int)
                    , _tTotalTerms :: !Int
                      -- ^ Number of terms in entire collection
                    , _tDocs :: !(HashMap doc Int)
                      -- ^ Number of terms in each document
                    }
         deriving (Show)
makeLenses ''TermIndex'

instance (Hashable term, Eq term, Hashable doc, Ord doc) => Monoid (TermStats doc term) where
    mempty = TermIdx HM.empty HM.empty 0 HM.empty
    a `mappend` b = TermIdx (HM.unionWith (HM.unionWith (+)) (a^.tFreq) (b^.tFreq))
                            (HM.unionWith (+) (a^.tTerms) (b^.tTerms))
                            (a^.tTotalTerms + b^.tTotalTerms)
                            (HM.unionWith (+) (a^.tDocs) (b^.tDocs))

termScore :: (Hashable doc, Eq doc, Hashable term, Eq term)
          => Double -> TermIndex doc term -> term -> [(doc, Score)]
termScore alphaD idx term =
    let freqs = maybe [] S.toDescList $ idx^?tFreq.ix term
    in map (\a@(_,doc) -> (doc, termDocScore alphaD idx term a)) freqs

termDocScore :: (Hashable doc, Eq doc, Hashable term, Eq term)
             => Double -> TermIndex doc term -> term -> (Int,doc) -> Score
termDocScore alphaD idx term (tf,doc) =
    (1 - realToFrac alphaD) * realToFrac tf / d + realToFrac alphaD * cf / c
  where cf = idx ^. tTerms . at term . non 0 . to realToFrac
        d = idx ^. tDocs . at doc . non 0 . to realToFrac
        c = idx ^. tTotalTerms . to realToFrac

indexTerms :: (Hashable doc, Hashable term, Eq term, Ord doc)
           => doc -> [term] -> TermStats doc term
indexTerms doc terms = foldMap (indexTerm doc) terms

indexTerm :: (Hashable doc, Hashable term)
          => doc -> term -> TermStats doc term
indexTerm doc term = TermIdx (HM.singleton term $ HM.singleton doc 1)
                             (HM.singleton term 1)
                             1
                             (HM.singleton doc 1)

termStatsToIndex :: (Ord doc) => TermStats doc term -> TermIndex doc term
termStatsToIndex stats = stats & tFreq.mapped %~ invert
  where invert = foldMap (\(d,n)->S.singleton (n,d)) . HM.toList

termIndexToStats :: (Eq doc, Hashable doc) => TermIndex doc term -> TermStats doc term
termIndexToStats idx = idx & tFreq.mapped %~ invert
  where invert = foldMap (\(n,d)->HM.singleton d n) . S.toList

statsIndex :: (Hashable doc, Eq doc, Ord doc)
           => Iso' (TermStats doc term) (TermIndex doc term)
statsIndex = iso termStatsToIndex termIndexToStats

ngrams :: Int -> [a] -> [[a]]
ngrams n = map (take n) . tails

data OrderedStats doc term = OrderedStats { _oFreq :: HashMap (V.Vector term) (HashMap doc Int)
                                          }
                           deriving (Show)
makeLenses ''OrderedStats

data UnorderedStats doc term = UnorderedStats { _uFreq :: HashMap (Set term) (HashMap doc Int)
                                              }
                             deriving (Show)
makeLenses ''UnorderedStats
