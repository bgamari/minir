{-# LANGUAGE TemplateHaskell, DeriveGeneric, RankNTypes #-}

module MinIR.UnorderedIndex ( UnorderedIndex
                            , fromTerms
                            , termsScore
                            ) where

import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import qualified Data.Set as S

import Data.Hashable
import GHC.Generics (Generic)
import Control.Lens
import Data.List (tails)
import Data.Foldable.Strict
import Data.Monoid
import Data.Binary
import Numeric.Log

import MinIR.Types
import MinIR.CorpusStats
import MinIR.FreqMap (fFreqs, fTotal, FreqMap)
import qualified MinIR.FreqMap as FM

ngrams :: Int -> Int -> [a] -> [[a]]
ngrams k n xs = error "MinIR.UnorderedIndex.ngrams: not implemented"

newtype UnorderedIndex doc term
        = UIdx { _oFreq :: Map (S.Set term) (FreqMap doc) }
        deriving (Show, Generic)
makeLenses ''UnorderedIndex

instance (Binary doc, Binary term) => Binary (UnorderedIndex doc term)

instance (Ord doc, Ord term)
         => Monoid (UnorderedIndex doc term) where
    mempty = UIdx M.empty
    UIdx a `mappend` UIdx b = UIdx (M.unionWith mappend a b)

{-# INLINE fromTerms #-}
fromTerms :: (Ord term, Eq term, Ord doc)
          => Int -> Int -> doc -> [term] -> UnorderedIndex doc term
fromTerms k n doc terms =
    foldMap' (fromNGram doc . S.fromList) $ ngrams k n terms

{-# INLINE fromNGram #-}
fromNGram :: (Ord doc)
          => doc -> S.Set term -> UnorderedIndex doc term
fromNGram doc ngram = UIdx (M.singleton ngram $ FM.singleton doc 1)

def :: a -> Iso' (Maybe a) a
def a = iso (maybe a id) Just

termsScore :: (Ord doc, Ord term, Eq term)
           => Int -> Int -> Double -> CorpusStats doc term -> UnorderedIndex doc term
           -> [term] -> M.Map doc Score
termsScore k n alphaD stats oidx terms =
   let ngrams' = map S.fromList $ ngrams k n terms
   in foldl' (M.unionWith (+)) mempty $ do
                     ngram <- ngrams'
                     doc <- oidx ^. oFreq . at ngram . def mempty . fFreqs . to M.keys
                     let score = nGramDocScore alphaD stats oidx ngram doc
                     return $ M.singleton doc score

nGramDocScore :: (Ord doc, Ord term, Eq term)
              => Double -> CorpusStats doc term -> UnorderedIndex doc term
              -> S.Set term -> doc -> Score
nGramDocScore alphaD stats oidx ngram doc =
    (1 - ad) * realToFrac tf / d + ad * cf / c
  where cf = oidx ^. oFreq . at ngram . def mempty . fTotal . to realToFrac
        tf = oidx ^. oFreq . at ngram . def mempty . fFreqs . at doc . non 0 . to realToFrac
        d = stats ^. cDocs . at doc . non 0 . to realToFrac
        c = stats ^. cTotalTerms . to realToFrac
        ad = realToFrac alphaD
