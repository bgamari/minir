{-# LANGUAGE TemplateHaskell, DeriveGeneric #-}

module FreqMap ( FreqMap, fTotal, fFreqs, singleton ) where

import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Control.Lens
import Data.Monoid
import Data.Hashable
import GHC.Generics

data FreqMap doc = FreqMap { _fTotal :: !Int
                           , _fFreqs :: !(Map doc Int)
                           }
                 deriving (Show, Generic)
makeLenses ''FreqMap

singleton :: Ord doc => doc -> Int -> FreqMap doc
singleton doc n = FreqMap n (M.singleton doc n)

instance (Ord doc) => Monoid (FreqMap doc) where
    mempty = FreqMap 0 M.empty
    FreqMap na fa `mappend` FreqMap nb fb = FreqMap (na+nb) (M.unionWith (+) fa fb)
