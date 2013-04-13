{-# LANGUAGE TemplateHaskell, DeriveGeneric #-}

module UnorderedIndex where

import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import qualified Data.Vector as V

import Data.Hashable
import GHC.Generics
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
        = UIdx { _oFreq :: (Map (S.Set term) (FreqMap doc)) }
        deriving (Show, Generic)
makeLenses ''UnorderedIndex

instance (Ord doc, Ord term)
         => Monoid (UnorderedIndex doc term) where
    mempty = UIdx M.empty
    UIdx a `mappend` UIdx b = UIdx (M.unionWith mappend a b)
