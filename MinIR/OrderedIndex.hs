{-# LANGUAGE TemplateHaskell, DeriveGeneric, RankNTypes #-}

module MinIR.OrderedIndex ( OrderedIndex
                          , fromTerms
                          , termsScore
                          ) where

import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import qualified Data.Map.Strict as M
import           Data.Set (Set)
import qualified Data.Set as S
import qualified Data.Vector as V
import Data.Hashable

import Control.Lens
import Data.List (tails)
import Data.Foldable.Strict
import Data.Monoid
import Data.Binary
import GHC.Generics (Generic)

import MinIR.Types
import MinIR.TermIndex (Score)
import MinIR.FreqMap (fFreqs, fTotal, FreqMap)
import MinIR.CorpusStats
import qualified MinIR.FreqMap as FM

ngrams :: Int -> [a] -> [[a]]
ngrams n = filter (\a->length a == 2) . map (take n) . tails

instance Hashable a => Hashable (V.Vector a) where
    hashWithSalt salt = hashWithSalt salt . V.toList

instance (Hashable k, Eq k, Binary k, Binary v) => Binary (HashMap k v) where
    get = HM.fromList `fmap` get
    put = put . HM.toList

instance (Binary v) => Binary (V.Vector v) where
    get = V.fromList `fmap` get
    put = put . V.toList

newtype OrderedIndex doc term
        = OIdx { _oFreq :: HashMap (V.Vector term) (FreqMap doc) }
        deriving (Show, Generic)
makeLenses ''OrderedIndex

instance (Hashable term, Eq term, Binary doc, Binary term)
         => Binary (OrderedIndex doc term)

instance (Hashable term, Eq term, Hashable doc, Eq doc, Ord doc)
         => Monoid (OrderedIndex doc term) where
    mempty = OIdx HM.empty
    OIdx a `mappend` OIdx b = OIdx (HM.unionWith mappend a b)

{-# INLINE fromTerms #-}
fromTerms :: (Hashable doc, Hashable term, Eq term, Ord doc)
          => Int -> doc -> [term] -> OrderedIndex doc term
fromTerms n doc terms = foldMap' (fromNGram doc . V.fromList) $ ngrams n terms

{-# INLINE fromNGram #-}
fromNGram :: (Hashable doc, Hashable term, Ord doc)
          => doc -> V.Vector term -> OrderedIndex doc term
fromNGram doc ngram = OIdx (HM.singleton ngram $ FM.singleton doc 1)

firsts :: V.Vector a -> [V.Vector a]
firsts xs | V.length xs < 2 = [xs]
          | otherwise       = map (\n->V.take n xs) [2..V.length xs]

termsScore :: (Ord doc, Hashable term, Eq term)
           => Int -> Double -> CorpusStats doc term -> OrderedIndex doc term
           -> [term] -> M.Map doc Score
termsScore n alphaD stats oidx terms =
   let ngrams' = map V.fromList $ ngrams n terms
   in foldl' (M.unionWith (+)) mempty $ do
                     ngram <- ngrams'
                     doc <- oidx ^. oFreq . at ngram . def mempty . fFreqs . to M.keys
                     let score = nGramDocScore alphaD stats oidx ngram doc
                     return $ M.singleton doc score

def :: a -> Iso' (Maybe a) a
def a = iso (maybe a id) Just

nGramDocScore :: (Ord doc, Hashable term, Eq term)
              => Double -> CorpusStats doc term -> OrderedIndex doc term
              -> V.Vector term -> doc -> Score
nGramDocScore alphaD stats oidx ngram doc =
    (1 - ad) * realToFrac tf / d + ad * cf / c
  where cf = oidx ^. oFreq . at ngram . def mempty . fTotal . to realToFrac
        tf = oidx ^. oFreq . at ngram . def mempty . fFreqs . at doc . non 0 . to realToFrac
        d = stats ^. cDocs . at doc . non 0 . to realToFrac
        c = stats ^. cTotalTerms . to realToFrac
        ad = realToFrac alphaD
