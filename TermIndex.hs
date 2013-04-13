{-# LANGUAGE TemplateHaskell, TupleSections, FlexibleInstances, BangPatterns, DeriveGeneric #-}

module TermIndex ( TermIndex
                 , tFreq, tTerms, tTotalTerms, tDocs
                 , Score, termScore, termDocScore
                 , indexTerms
                 ) where

import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import           Data.Set (Set)
import qualified Data.Set as S
import qualified Data.Vector as V

import GHC.Generics (Generic)
import Data.Binary (Binary)
import Control.Lens
import Data.List (tails)
import Data.Foldable
import Data.Monoid
import Numeric.Log

type Score = Log Double

data TermIndex doc term
         = TermIdx  { _tFreq :: !(Map term (Map doc Int))
                      -- ^ Number of term mentions in each document
                    , _tTerms :: !(Map term Int)
                    , _tTotalTerms :: !Int
                      -- ^ Number of terms in entire collection
                    , _tDocs :: !(Map doc Int)
                      -- ^ Number of terms in each document
                    }
         deriving (Show, Generic)
makeLenses ''TermIndex

instance (Ord term, Ord doc) => Monoid (TermIndex doc term) where
    mempty = TermIdx M.empty M.empty 0 M.empty
    {-# INLINE mempty #-}
    a `mappend` b = TermIdx (M.unionWith (M.unionWith (+)) (a^.tFreq) (b^.tFreq))
                            (M.unionWith (+) (a^.tTerms) (b^.tTerms))
                            (a^.tTotalTerms + b^.tTotalTerms)
                            (M.unionWith (+) (a^.tDocs) (b^.tDocs))
    {-# INLINE mappend #-}

termScore :: (Ord doc, Ord term)
          => Double -> TermIndex doc term -> term -> [(doc, Score)]
termScore alphaD idx term =
    map (\doc->(doc, termDocScore alphaD idx term doc))
    $ idx ^. tFreq . at term . non M.empty . to M.keys

termDocScore :: (Ord doc, Ord term)
             => Double -> TermIndex doc term -> term -> doc -> Score
termDocScore alphaD idx term doc =
    (1 - realToFrac alphaD) * realToFrac tf / d + realToFrac alphaD * cf / c
  where cf = idx ^. tTerms . at term . non 0 . to realToFrac
        tf = idx ^. tFreq . at term . non M.empty . at doc . non 0 . to realToFrac
        d = idx ^. tDocs . at doc . non 0 . to realToFrac
        c = idx ^. tTotalTerms . to realToFrac

{-# INLINE indexTerms #-}
indexTerms :: (Ord doc, Ord term)
           => doc -> [term] -> TermIndex doc term
indexTerms doc terms = foldMap' (indexTerm doc) terms

foldMap' :: (Monoid m, Foldable f) => (a -> m) -> f a -> m
foldMap' f xs = foldl' (\a b->mappend a $ f b) mempty xs

indexTerm :: (Ord doc, Ord term)
          => doc -> term -> TermIndex doc term
indexTerm doc term = TermIdx (M.singleton term $ M.singleton doc 1)
                             (M.singleton term 1)
                             1
                             (M.singleton doc 1)
