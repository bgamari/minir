{-# LANGUAGE TemplateHaskell, DeriveGeneric #-}

module FreqMap ( FreqMap, fTotal, fFreqs, singleton ) where

import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import Control.Lens
import Data.Monoid
import Data.Hashable
import GHC.Generics

data FreqMap doc = FreqMap { _fTotal :: !Int
                           , _fFreqs :: !(HashMap doc Int)
                           }
                 deriving (Show, Generic)
makeLenses ''FreqMap

singleton :: Hashable doc => doc -> Int -> FreqMap doc
singleton doc n = FreqMap n (HM.singleton doc n)

instance (Hashable doc, Eq doc) => Monoid (FreqMap doc) where
    mempty = FreqMap 0 HM.empty
    FreqMap na fa `mappend` FreqMap nb fb = FreqMap (na+nb) (HM.unionWith (+) fa fb)
