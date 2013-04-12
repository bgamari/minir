{-# LANGUAGE TemplateHaskell, TupleSections, FlexibleInstances, BangPatterns, DeriveGeneric #-}

module TermIndex ( TermIndex
                 , tFreq, tTerms, tTotalTerms, tDocs
                 , Score, termScore, termDocScore
                 , indexTerms
                 ) where

import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import           Data.Set (Set)
import qualified Data.Set as S
import qualified Data.Vector as V

import Data.Hashable
import GHC.Generics (Generic)
import Data.Binary (Binary)
import Control.Lens
import Data.List (tails)
import Data.Foldable
import Data.Monoid
import Numeric.Log

type Score = Log Double

data TermIndex doc term
         = TermIdx  { _tFreq :: !(HashMap term (HashMap doc Int))
                      -- ^ Number of term mentions in each document
                    , _tTerms :: !(HashMap term Int)
                    , _tTotalTerms :: !Int
                      -- ^ Number of terms in entire collection
                    , _tDocs :: !(HashMap doc Int)
                      -- ^ Number of terms in each document
                    }
         deriving (Show, Generic)
makeLenses ''TermIndex

instance (Hashable term, Eq term, Hashable doc, Ord doc) => Monoid (TermIndex doc term) where
    mempty = TermIdx HM.empty HM.empty 0 HM.empty
    {-# INLINE mempty #-}
    a `mappend` b = TermIdx (HM.unionWith (HM.unionWith (+)) (a^.tFreq) (b^.tFreq))
                            (HM.unionWith (+) (a^.tTerms) (b^.tTerms))
                            (a^.tTotalTerms + b^.tTotalTerms)
                            (HM.unionWith (+) (a^.tDocs) (b^.tDocs))
    {-# INLINE mappend #-}

termScore :: (Hashable doc, Eq doc, Hashable term, Eq term)
          => Double -> TermIndex doc term -> term -> [(doc, Score)]
termScore alphaD idx term =
    map (\doc->(doc, termDocScore alphaD idx term doc))
    $ idx ^. tFreq . at term . non HM.empty . to HM.keys

termDocScore :: (Hashable doc, Eq doc, Hashable term, Eq term)
             => Double -> TermIndex doc term -> term -> doc -> Score
termDocScore alphaD idx term doc =
    (1 - realToFrac alphaD) * realToFrac tf / d + realToFrac alphaD * cf / c
  where cf = idx ^. tTerms . at term . non 0 . to realToFrac
        tf = idx ^. tFreq . at term . non HM.empty . at doc . non 0 . to realToFrac
        d = idx ^. tDocs . at doc . non 0 . to realToFrac
        c = idx ^. tTotalTerms . to realToFrac

{-# INLINE indexTerms #-}
indexTerms :: (Hashable doc, Hashable term, Eq term, Ord doc)
           => doc -> [term] -> TermIndex doc term
indexTerms doc terms = foldMap' (indexTerm doc) terms

foldMap' :: (Monoid m, Foldable f) => (a -> m) -> f a -> m
foldMap' f xs = foldl' (\a b->mappend a $ f b) mempty xs

indexTerm :: (Hashable doc, Hashable term)
          => doc -> term -> TermIndex doc term
indexTerm doc term = TermIdx (HM.singleton term $ HM.singleton doc 1)
                             (HM.singleton term 1)
                             1
                             (HM.singleton doc 1)
