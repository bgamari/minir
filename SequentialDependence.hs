{-# LANGUAGE TemplateHaskell, TupleSections, FlexibleInstances, BangPatterns, RankNTypes #-}

module SequentialDependence where

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
import OrderedIndex hiding (indexTerms, termScore, termDocScore)
