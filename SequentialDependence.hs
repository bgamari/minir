{-# LANGUAGE TemplateHaskell, TupleSections, FlexibleInstances, BangPatterns, RankNTypes #-}

module SequentialDependence where

import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import qualified Data.Map.Strict as M
import           Data.Set (Set)
import qualified Data.Set as S
import qualified Data.Vector as V
import Data.Hashable

import           Control.Lens
import Data.List (tails)
import Data.Foldable
import Data.Monoid
import Numeric.Log

import CorpusStats as CS
import TermIndex as TI
import OrderedIndex as OI
--import UnorderedIndex as UI

data SeqDepIndex doc term
    = SDIdx { cstats :: CorpusStats doc term
            , tIdx :: TermIndex doc term
            , oIdx :: OrderedIndex doc term
            }
    deriving (Show)

data SeqDepParams = SDParams { lambdaT, lambdaU, lambdaO :: Double }
                  deriving (Show)

instance (Ord term, Hashable term, Ord doc, Hashable doc)
         => Monoid (SeqDepIndex doc term) where
    mempty = SDIdx mempty mempty mempty
    SDIdx ca ta oa `mappend` SDIdx cb tb ob =
        SDIdx (ca <> cb) (ta <> tb) (oa <> ob)

fromDocument :: (Ord term, Hashable term, Ord doc, Hashable doc)
             => Int -> doc -> [term] -> SeqDepIndex doc term
fromDocument n doc terms =
    SDIdx (CS.fromDocument doc (length terms))
          (TI.fromTerms doc terms)
          (OI.fromTerms n doc terms)

scoreTerms :: (Ord term, Ord doc, Hashable term)
           => SeqDepParams -> SeqDepIndex doc term -> [term] -> [(doc, Score)]
scoreTerms (SDParams lt lu lo) (SDIdx cstats tIdx oIdx) terms =
    let t = TI.termsScore 0.1 cstats tIdx terms
        o = OI.termsScore 2 0.1 cstats oIdx terms
        docs = S.unions [M.keysSet t, M.keysSet o]
        f d = (d, realToFrac lt * t M.! d + realToFrac lo * o M.! d)
    in map f $ S.toList docs
