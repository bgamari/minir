{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Types where

import Data.Hashable
import Data.Binary
import Control.Lens
import Control.Monad.State.Strict
import MinIR.Dictionary as D

newtype Term = Term Int deriving (Eq, Show, Enum, Ord, Binary)

mapTerms :: (Eq term, Hashable term)
         => [(doc, [term])] -> ([(doc, [Term])], D.Dictionary Term term)
mapTerms terms = runState (terms & traverse . _2 . traverse %%~ (state . D.getKey)) (D.empty $ Term 0)
