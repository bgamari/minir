module MinIR.PostingList.Types where

import Data.Binary
import Control.Applicative

-- | A directory containing a posting list
type PostingListDir = FilePath

-- | A document and count
data Posting doc = Posting !Int !doc
                 deriving (Show)
     
instance Binary doc => Binary (Posting doc) where
    get = Posting <$> get <*> get
    put (Posting count doc) = put count >> put doc
