{-# LANGUAGE TemplateHaskell, DeriveGeneric #-}

module MinIR.SequentialDependence ( SeqDepIndex(..)
                                  , SeqDepParams(..)
                                  , defaultParams
                                  , fromDocument
                                  , scoreTerms
                                  ) where

import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Data.Hashable

import Control.Lens
import Data.List (tails)
import Data.Foldable
import Data.Monoid
import Data.Binary
import GHC.Generics (Generic)

import MinIR.Types
import qualified MinIR.CorpusStats as CS
import qualified MinIR.TermIndex as TI
import qualified MinIR.OrderedIndex as OI
import qualified MinIR.UnorderedIndex as UI

data SeqDepIndex doc term
    = SDIdx { cstats :: CS.CorpusStats doc term
            , tIdx :: TI.TermIndex doc term
            , oIdx :: OI.OrderedIndex doc term
            , uIdx :: UI.UnorderedIndex doc term
            }
    deriving (Show, Generic)

instance (Hashable term, Eq term, Binary doc, Binary term) => Binary (SeqDepIndex doc term)

data SeqDepParams = SDParams { lambdaT, lambdaU, lambdaO :: Double }
                  deriving (Show)

defaultParams = SDParams 0.1 0.1 0.1

instance (Ord term, Hashable term, Ord doc, Hashable doc)
         => Monoid (SeqDepIndex doc term) where
    mempty = SDIdx mempty mempty mempty mempty
    SDIdx ca ta oa ua `mappend` SDIdx cb tb ob ub =
        SDIdx (ca <> cb) (ta <> tb) (oa <> ob) (ua <> ub)

fromDocument :: (Ord term, Hashable term, Ord doc, Hashable doc)
             => Int -> doc -> [term] -> SeqDepIndex doc term
fromDocument n doc terms =
    SDIdx (CS.fromDocument doc (length terms))
          (TI.fromTerms doc terms)
          (OI.fromTerms n doc terms)
          (UI.fromTerms 8 n doc terms)

scoreTerms :: (Ord term, Ord doc, Hashable term)
           => SeqDepParams -> SeqDepIndex doc term -> [term] -> [(doc, Score)]
scoreTerms (SDParams lt lu lo) (SDIdx cstats tIdx oIdx uIdx) terms =
    let t = TI.termsScore 0.1 cstats tIdx terms
        o = OI.termsScore 2 0.1 cstats oIdx terms
        u = UI.termsScore 8 2 0.1 cstats uIdx terms
        docs = S.unions [M.keysSet t, M.keysSet o]
        f d = let psiT = M.findWithDefault 0 d t
                  psiO = M.findWithDefault 0 d o
                  psiU = M.findWithDefault 0 d u
              in (d, realToFrac lt * psiT + realToFrac lo * psiO + realToFrac lo * psiU)
    in map f $ S.toList docs
