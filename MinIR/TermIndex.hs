{-# LANGUAGE TemplateHaskell, TupleSections, FlexibleInstances, BangPatterns, DeriveGeneric, RankNTypes #-}

module MinIR.TermIndex ( TermIndex, tFreq, termsSize
                       , Score, termsScore, termScore, termDocScore
                       , fromTerms, fromTerm
                       ) where

import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as M

import GHC.Generics (Generic)
import Data.Binary (Binary)
import Control.Lens hiding (index)
import Data.Foldable.Strict
import Data.Monoid

import MinIR.Types
import MinIR.FreqMap (fFreqs, fTotal, FreqMap)
import qualified MinIR.FreqMap as FM
import MinIR.CorpusStats (CorpusStats)
import qualified MinIR.CorpusStats as CS

-- | Inverted index over terms
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

-- | Construct a 'TermIndex' from a document and a set of observed terms
fromTerms :: (Ord doc, Ord term) => doc -> [term] -> TermIndex doc term
fromTerms doc terms = foldMap' (fromTerm doc) terms
{-# INLINE fromTerms #-}

-- | Construct a 'TermIndex' from a document and an observed term
fromTerm :: Ord doc => doc -> term -> TermIndex doc term
fromTerm doc term = TIdx $ M.singleton term $ FM.singleton doc 1
{-# INLINE fromTerm #-}

-- | Number of terms in the index
termsSize :: Ord term => TermIndex doc term -> Int
termsSize (TIdx a) = M.size a 
{-# INLINE termsSize #-}

termsScore :: (Ord doc, Ord term)
           => Double -> CorpusStats doc term -> TermIndex doc term -> [term] -> Map doc Score
termsScore alphaD stats idx =
    M.unionsWith (+) . map (M.fromList . termScore alphaD stats idx)

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
