{-# LANGUAGE TemplateHaskell, TupleSections #-}

module SequentialDependence where

import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import           Data.BiHashMap (BiHashMap)
import qualified Data.BiHashMap as BHM
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

data TermStats doc term = TermStats { _tFreq :: HashMap term (BiHashMap doc Int)
                                      -- ^ Number of term mentions in each document
                                    , _tTerms :: HashMap term Int
                                    , _tTotalTerms :: Int
                                      -- ^ Number of terms in entire collection
                                    , _tDocs :: HashMap doc Int
                                      -- ^ Number of terms in each document
                                    }
                        deriving (Show)
makeLenses ''TermStats

instance (Hashable term, Eq term, Hashable doc, Ord doc) => Monoid (TermStats doc term) where
    mempty = TermStats HM.empty HM.empty 0 HM.empty
    a `mappend` b = TermStats (HM.unionWith (BHM.unionWith (+)) (a^.tFreq) (b^.tFreq))
                              (HM.unionWith (+) (a^.tTerms) (b^.tTerms))
                              (a^.tTotalTerms + b^.tTotalTerms)
                              (HM.unionWith (+) (a^.tDocs) (b^.tDocs))

termScore :: (Hashable doc, Eq doc, Hashable term, Eq term)
          => Double -> TermStats doc term -> term -> doc -> Score
termScore alphaD stats term doc =
    (1 - realToFrac alphaD) * tf / d + realToFrac alphaD * cf / c
  where tf = maybe 0 realToFrac $ stats^?tFreq.ix term.to BHM.forward.ix doc
        cf = maybe 0 realToFrac $ stats^?tTerms.ix term
        d = maybe 0 realToFrac $ stats^?tDocs.ix doc :: Log Double
        c = stats^.tFreq^.to HM.size^.to realToFrac

indexTerms :: (Hashable doc, Hashable term, Eq term, Ord doc)
           => doc -> [term] -> TermStats doc term
indexTerms doc terms = foldMap (indexTerm doc) terms

indexTerm :: (Hashable doc, Hashable term)
          => doc -> term -> TermStats doc term
indexTerm doc term = TermStats (HM.singleton term (BHM.singleton doc 1))
                               (HM.singleton term 1)
                               1
                               (HM.singleton doc 1)


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
