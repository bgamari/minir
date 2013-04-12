{-# LANGUAGE TemplateHaskell, TupleSections, FlexibleInstances, BangPatterns, RankNTypes #-}

module OrderedIndex ( OrderedIndex
                    , indexTerms
                    , termsScore
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
import FreqMap (fFreqs, fTotal, FreqMap)
import qualified FreqMap as FM

ngrams :: Int -> [a] -> [[a]]
ngrams n = map (take n) . tails

instance Hashable a => Hashable (V.Vector a) where
    hashWithSalt salt = hashWithSalt salt . V.toList

newtype OrderedIndex doc term
        = OIdx { _oFreq :: (HashMap (V.Vector term) (FreqMap doc)) }
        deriving (Show)
makeLenses ''OrderedIndex

instance (Hashable term, Eq term, Hashable doc, Eq doc)
         => Monoid (OrderedIndex doc term) where
    mempty = OIdx HM.empty
    OIdx a `mappend` OIdx b = OIdx (HM.unionWith mappend a b)

{-# INLINE indexTerms #-}
indexTerms :: (Hashable doc, Hashable term, Eq term, Ord doc)
           => Int -> doc -> [term] -> OrderedIndex doc term
indexTerms n doc terms = foldMap' (indexNGram doc . V.fromList) $ ngrams n terms

indexNGram :: (Hashable doc, Hashable term)
           => doc -> V.Vector term -> OrderedIndex doc term
indexNGram doc ngram = OIdx (HM.singleton ngram $ FM.singleton doc 1)

firsts :: V.Vector a -> [V.Vector a]
firsts xs | V.length xs < 2 = [xs]
          | otherwise       = map (\n->V.take n xs) [2..V.length xs]

foldMap' :: (Monoid m, Foldable f) => (a -> m) -> f a -> m
foldMap' f xs = foldl' (\a b->mappend a $ f b) mempty xs

termsScore :: (Hashable doc, Eq doc, Hashable term, Eq term)
           => Int -> Double -> TermIndex doc term -> OrderedIndex doc term
           -> V.Vector term -> HM.HashMap doc Score
termsScore n alphaD idx oidx terms =
   let ngrams' = map V.fromList $ ngrams n $ V.toList terms
   in foldl' (HM.unionWith (+)) mempty $ do
                     ngram <- ngrams'
                     doc <- oidx ^. oFreq . at ngram . def mempty . fFreqs . to HM.keys
                     let score = nGramDocScore alphaD idx oidx ngram doc
                     return $ HM.singleton doc score

def :: a -> Iso' (Maybe a) a
def a = iso (maybe a id) Just

nGramDocScore :: (Hashable term, Eq term, Hashable doc, Eq doc)
              => Double -> TermIndex doc term -> OrderedIndex doc term
              -> V.Vector term -> doc -> Score
nGramDocScore alphaD idx oidx ngram doc =
    (1 - ad) * realToFrac tf / d + ad * cf / c
  where cf = oidx ^. oFreq . at ngram . def mempty . fTotal . to realToFrac
        tf = oidx ^. oFreq . at ngram . def mempty . fFreqs . at doc . non 0 . to realToFrac
        d = idx ^. tDocs . at doc . non 0 . to realToFrac
        c = idx ^. tTotalTerms . to realToFrac
        ad = realToFrac alphaD
