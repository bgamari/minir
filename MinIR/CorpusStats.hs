{-# LANGUAGE TemplateHaskell, DeriveGeneric #-}

module MinIR.CorpusStats ( CorpusStats, cTotalTerms, cDocs
                         , fromDocument
                         ) where

import Data.Binary
import Data.Monoid
import Data.Map as M
import GHC.Generics (Generic)
import Control.Lens

data CorpusStats doc term
         = CStats { _cTotalTerms :: !Int
                    -- ^ Number of terms in entire collection
                  , _cDocs :: !(M.Map doc Int)
                    -- ^ Number of terms in each document
                  }
         deriving (Show, Generic)
makeLenses ''CorpusStats

instance (Ord doc, Ord term) => Monoid (CorpusStats doc term) where
    mempty = CStats 0 M.empty
    {-# INLINE mempty #-}
    CStats ta da `mappend` CStats tb db = CStats (ta+tb) (M.unionWith (+) da db)
    {-# INLINE mappend #-}

instance (Binary doc, Binary term) => Binary (CorpusStats doc term)

fromDocument :: doc -> Int -> CorpusStats doc term
fromDocument doc n = CStats n (M.singleton doc n)
