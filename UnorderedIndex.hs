{-# LANGUAGE TemplateHaskell #-}

module UnorderedIndex where

import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import qualified Data.Set as S
import qualified Data.Vector as V
import Data.Hashable

import Control.Lens
import Data.List (tails)
import Data.Foldable
import Data.Monoid
import Numeric.Log

import TermIndex hiding (indexTerms, termScore, termDocScore)
import FreqMap (fFreqs, fTotal, FreqMap)
import qualified FreqMap as FM

instance Hashable a => Hashable (S.Set a) where
    hashWithSalt salt = hashWithSalt salt . S.toAscList

newtype UnorderedIndex doc term
        = UIdx { _oFreq :: (HashMap (S.Set term) (FreqMap doc)) }
        deriving (Show)
makeLenses ''UnorderedIndex

instance (Hashable term, Eq term, Hashable doc, Eq doc)
         => Monoid (UnorderedIndex doc term) where
    mempty = UIdx HM.empty
    UIdx a `mappend` UIdx b = UIdx (HM.unionWith mappend a b)
