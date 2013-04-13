{-# LANGUAGE TemplateHaskell, TupleSections, FlexibleInstances, BangPatterns, DeriveGeneric, RankNTypes #-}

module TermIndex ( TermIndex, tFreq
                 , Score, termScore, termDocScore
                 , fromTerms, fromTerm
                 ) where

import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as M

import GHC.Generics (Generic)
import Data.Binary (Binary)
import Control.Lens hiding (index)
import Data.Foldable
import Data.Monoid
import Numeric.Log

import FreqMap (fFreqs, fTotal, FreqMap)
import qualified FreqMap as FM
import CorpusStats (CorpusStats)
import qualified CorpusStats as CS

type Score = Log Double

newtype TermIndex doc term
        = TIdx { _tFreq :: Map term (FreqMap doc) }
        deriving (Show, Generic)
makeLenses ''TermIndex

instance (Binary doc, Binary term) => Binary (TermIndex doc term)

instance (Ord doc, Ord term) => Monoid (TermIndex doc term) where
    mempty = TIdx M.empty
    {-# INLINE mempty #-}
    TIdx a `mappend` TIdx b = TIdx (M.unionWith mappend a b)
    {-# INLINE mappend #-}

{-# INLINE fromTerms #-}
fromTerms :: (Ord doc, Ord term) => doc -> [term] -> TermIndex doc term
fromTerms doc terms = foldMap' (fromTerm doc) terms

{-# INLINE fromTerm #-}
fromTerm :: Ord doc => doc -> term -> TermIndex doc term
fromTerm doc term = TIdx $ M.singleton term $ FM.singleton doc 1

foldMap' :: (Monoid m, Foldable f) => (a -> m) -> f a -> m
foldMap' f xs = Data.Foldable.foldl' (\a b->mappend a $ f b) mempty xs

termScore :: (Ord doc, Ord term)
          => Double -> CorpusStats doc term -> TermIndex doc term -> term -> [(doc, Score)]
termScore alphaD stats idx term =
    map (\doc->(doc, termDocScore alphaD stats idx term doc))
    $ idx ^. tFreq . at term . def mempty . fFreqs . to M.keys

termDocScore :: (Ord doc, Ord term)
             => Double -> CorpusStats doc term -> TermIndex doc term -> term -> doc -> Score
termDocScore alphaD stats idx term doc =
    (1 - realToFrac alphaD) * realToFrac tf / d + realToFrac alphaD * cf / c
  where cf = idx ^. tFreq . at term . def mempty . fTotal . to realToFrac
        tf = idx ^. tFreq . at term . def mempty . fFreqs . at doc . def 0 . to realToFrac
        d = stats ^. CS.cDocs . at doc . def 0 . to realToFrac
        c = stats ^. CS.cTotalTerms . to realToFrac

def :: a -> Getter (Maybe a) a
def a = to (maybe a id)