module MinIR.PostingList.Types where

import Data.Binary
import Control.Applicative

data Posting doc = Posting !Int !doc
                 deriving (Show)
     
instance Binary doc => Binary (Posting doc) where
    get = Posting <$> get <*> get
    put (Posting count doc) = put count >> put doc
